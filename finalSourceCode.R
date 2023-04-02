# List of all the needed package for the project
packagelist <- c("tidyverse", "dplyr","rvest", "stringr", "lubridate", "purrr","jsonlite", "caret",
                 "tidyr", "matrixStats", "xgboost", "pls", "zoo")

#check if package is install, it not, load it
not_installed <- packagelist[!packagelist %in% installed.packages()[ , "Package"]]
if(length(not_installed)) install.packages(not_installed, dependencies = TRUE)

lapply(packagelist, library, character.only = TRUE)

# Setting User Agent
options(HTTPUserAgent = "yourName   YourName@domain.com",
        timeout = Inf)

# Download the data from SEC
DownloadBulkData <- function(){ 
  url <- "https://www.sec.gov/Archives/edgar/daily-index/xbrl/companyfacts.zip"
  download.file(url, "companyfacts.zip", quiet = FALSE  )
}

# Fetch company list from wikipedia
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
h <- read_html(url)
SPlist <- html_nodes(h, "table")[[1]] %>% html_table(convert = FALSE)

# Keep only the oil related data and that are public for at least 10 years
OilList <- SPlist %>% filter(str_detect( .$`GICS Sub-Industry`, "Oil")) %>%
  mutate(across(`Date added`, as.Date)) %>% 
  filter(year(`Date added`) <= year(Sys.Date())-10)

# Store cik number and ticker symbol
oilCIK <- OilList %>% select(CIK)
oilTicker <- OilList %>% select(Symbol)

# Url Generator that we will use to fetch price
MakePriceUrl <- function(ticker){
  paste0("https://www.macrotrends.net/stocks/charts/",
         ticker,
         "/a/stock-price-history")
}

# Map the all the Oil Ticker symbol to the Url generator
PricesURls <- function(tick = oilTicker){
  urls <- sapply(tick, MakePriceUrl)
  data.frame(ticker = tick, urls = urls)
}

# Extract the price table from the url based website and convert into table 
FetchPrice <- function(url){
  h <- read_html(url) %>% html_nodes('table')
  df <- h[[1]] %>% html_table() %>% .[2:nrow(.),1:2]
  return(data.frame(year = pull(df[,1]), price = pull(df[,2])))
}

# Join the company price and the cik number
OilCompanyPrices <- map2(PricesURls()[[1]], PricesURls()[[2]], 
                         ~ FetchPrice(.y) %>% 
                           mutate(ticker = .x)) %>% 
  do.call(rbind , .) %>% 
  mutate(across(c(price,year),as.numeric)) %>%
  left_join(OilList %>% select(CIK, Symbol), by = c("ticker" = "Symbol")) %>%
  mutate(CIK = as.numeric(CIK))

# Check file name within company facts zip
allFileNames <- unzip( "companyfacts.zip", list = TRUE) %>% .$Name %>% as_tibble()

# Generate file name of the oil Company
filenames <- sapply(oilCIK, function(cik){
  paste0("CIK",cik,".json")
}) %>% as_tibble()

# Unzip the oil company file to the oilCompanies folder
unzip("companyfacts.zip",files = filenames %>% pull, exdir = "oilCompanies")

# Generate file name of the company wihtin the oilCompanies folder
thelist <- list.files("oilCompanies/") %>% sapply(function(x) {
  file.path(getwd(), "oilCompanies", x)})

# Json to dataframe converter
jsonToDf <- function(js) {
  js %>% as_tibble() %>% 
    unnest_longer("facts") %>%
    unnest_wider("facts") %>%
    unnest_longer("units") %>%
    unnest("units") %>%
    unnest_wider("units")
}

# Generate one table that contains all the information from the oilCompanies folder
FullTable <- map(thelist, function(x){
  read_json(x) %>% jsonToDf()
}) %>% do.call(rbind, .)

# Making the table wider.
Rectangling  <- function(table = FullTable){
  table %>% 
    mutate(end = ymd(end)) %>%
    group_by(accn, label) %>%
    filter(year(end) == fy) %>% 
    arrange(desc(end)) %>%
    slice_head() %>%
    ungroup() %>%
    select(c(entityName, label, fy, fp, form, val)) %>%
    pivot_wider(names_from = label, values_from = val)
}

# Check all the levels of the label
FullTable %>% mutate(across(label, as_factor)) %>% summarise(levels(label) %>% length()) %>% pull

# Select all the statement that are labeled as Revenue
FullTable %>% mutate(across(label, as_factor)) %>% summarise(lev = levels(label)) %>% 
  filter(str_detect(lev, "^Revenue"))

# Label occurrence statistics
facts_stats <- FullTable %>% group_by(accn) %>%
  filter(year(end) == fy) %>%
  ungroup() %>%
  filter(form == "10-K") %>%
  group_by(label) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>% filter(n >= 10)

top_n(facts_stats, 20)

# Generate price table with the price, company name and the year
pricetable <- OilCompanyPrices %>% left_join(FullTable, by = c("year" = "fy", "CIK" = "cik")) %>%
  select(year, price, entityName) %>% unique() %>% drop_na()

# Add the price to the financial table( Full table that has been rectangled and filter on the label
# of interest)
AddingStockPrice <- function(finTable){
  finTable %>% left_join(pricetable, by = c( "fy" = "year" , "entityName")) %>%
    return()
}

# Based on the statement that we want to study, we extract the data from FullTable and add the price
selectLabelPrice <- function(selectedLabel, drop_na = TRUE){
  X <- FullTable %>%  filter(form =='10-K' & label %in% selectedLabel) %>% 
    Rectangling() %>%
    mutate(across(-c(1,2,3,4),as.numeric)) %>% AddingStockPrice()
  ifelse(drop_na == TRUE, 
         return(X %>% drop_na()),
         return(X))
}

# Generate the dataset for the case of this project. We only keep the financial statement that are in
# the top 25 appearance. We fill the missing value with the column mean.
df <- selectLabelPrice(facts_stats %>% top_n(25) %>% .$label, drop_na = FALSE) %>%
  .[5:ncol(.)] %>% na.aggregate()

# Some statistics and visual exploration
dim(df) # dimension of the dataset
hist(df$price, nclass = 10) # histogram of the stock price
pairs(df %>% select(16:26)) # correlation between some label and the price
as.matrix(cor(df %>% select(-price))) %>% image # correlation as a heatmap

# Split the dataset into training and testing
set.seed(2023)
train_index <- createDataPartition(df$price, p = 0.9, list = FALSE)
train <- df[train_index,]
test <- df[-train_index,]

y_train <- df[train_index,] %>% select(price)
y_test <- df[-train_index,] %>% select(price) %>% pull

# RMSE for the error measurement
RMSE <- RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2))
}

# Linear model
control <- trainControl(method = "cv", number = 25) # 25 fold cross validation
model_lm <- train(price ~ ., method = "lm", data = train, trControl = control)
summary(model_lm)
rmse_lm <- RMSE(predict(model_lm, test), y_test)

# Principal component model
model_pca <- pcr(price ~ . , data = train, validation = "CV")
rmse_lm_pca <- RMSE(predict(model_pca, test, ncomp = 1), y_test)

# Random forest model
grid <- data.frame(mtry = c(1, 5, 10, 25)) # tuning grid for mtry hyperparameter
model_rf <- train(price ~ ., method = "rf", data = train, 
                  trControl = control, 
                  tuneGrid = grid,
                  ntree = 100)
rmse_rf <- RMSE(predict(model_rf, newdata = test),y_test)
plot(model_rf) # plot rmse score vs hyperparemeters

# Boosting Gradient
# Spliting training and testing set
xgb_train <- df[train_index,] %>% select(-price)
xgb_test <- df[-train_index,] %>% select(-price)

xgb_y_train <- df[train_index,] %>% select(price)
xgb_y_test <- df[-train_index,] %>% select(price) %>% pull

# tuning grid for eta and depth hyperparameters
tuneGridboost <- expand.grid(eta = seq(0,1, 0.01), depth = seq(10,30,5))

# gradient boost model
gradientModel <- function(etavalue, depthvalue){
  mod <- xgboost(data = as.matrix(xgb_train), label = as.matrix(xgb_y_train), 
                 max_depth = depthvalue, 
                 eta = etavalue, 
                 nrounds = 25,
                 objective = "reg:squarederror",
                 verbose = FALSE)
  rmse <- RMSE(predict(mod, as.matrix(xgb_test), reshape = TRUE), xgb_y_test)
  return(list(eta = etavalue,depth = depthvalue, rmse = rmse))
}

# Mapping the model to the tuning grid
tuning <- map2(tuneGridboost$eta, tuneGridboost$depth , gradientModel) %>%
  do.call(rbind, .) %>% as_tibble

tuning[which.min(tuning$rmse),] %>% glimpse

rmse_boost <- tuning[which.min(tuning$rmse),"rmse"] %>% unlist
# Model comparison
data.frame(rmse_lm, rmse_lm_pca, rmse_rf, rmse_boost = rmse_boost)
