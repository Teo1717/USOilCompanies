source("0-Libraries.R")
source("Functions.R")
library(shiny)

#set API Key for NASDAQ
Quandl.api_key("dJcrsGTMGsgV4SDjHcK6")

ui <- fluidPage(
  navbarPage("U.S. SEC Visual",
             column(4,textInput("tikr", label = h4("Choose a Tikker"), value = "AAPL")),
             column(4,uiOutput("company_facts_list")),
             column(4,radioButtons('YorQ', label = h5("Yearly or Quarterly"), 
                                   choices = list("Yearly" = '10-K', "Quartely" = '10-Q'), 
                                   selected = '10-K')),
             fluidRow(
               column(2, h1(textOutput("cik"))),
               column(10,textOutput("description"))
             ),
             
    tabPanel("Chart",
        fluidRow(     
          column(2, selectInput("rangeSelector", label = h5("Date range"),
                    choices = list("All" = Inf, "10 Years" = 10, "5 Years" = 5,
                                "3 Years" = 3, "1 Year" = 1),
                    selected = "All"
                    )),
          column(4, uiOutput("coef"))
        ),
        plotOutput("chart")
    ),
    
    tabPanel("Data", 
             uiOutput("fact_display")
    ),
    
    tabPanel("Ratios")
  )
)

server <- function(input, output) {

  
  
    output$cik <- renderText({
        CompanyInfos(input$tikr)[[2]]
    })
    
    
    output$company_facts_list <- renderUI({
      selectInput("fact", label = h5("Select a fact"), 
                  choices = CompanyFacts(input$tikr), 
                  selected = NULL)
    })
    
    data <- reactive({
      FactTable2(input$tikr, input$fact)[[1]] %>%
        filter(form == input$YorQ) %>%
        # filter(str_detect(frame,"^CY[\\d]{4}$")) %>%
        arrange(desc(end)) 
        # filter(!is.na(frame))
    })
    
    growth <- reactive({
       lm_model(entry = data(), rg = input$rangeSelector) %>%
        YoYGrowthRate()
    })
    
    output$fact_display <- renderTable({
      data() %>% mutate(across(everything(), as.character))
    })
    
    output$chart <- renderPlot({
      data() %>% PlotR(rg = input$rangeSelector)
    })
    
    output$description <- renderText({
      FactTable2(input$tikr, input$fact)[[3]]
    })
    
    output$coef <- renderPrint({
      paste("YoY Growth Rate: ",round(growth(),2))
      })

}
# Run the application 
shinyApp(ui = ui, server = server)

