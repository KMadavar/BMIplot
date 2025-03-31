#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(DT)
library(bslib)
#Define UI

# adsl <- tidyCDISC::adsl
# saveRDS(adsl, file = "C:/Users/karth/OneDrive/Desktop/R Practice/BMIplot/data/adsl.rds")
# save(adsl, file = "C:/Users/karth/OneDrive/Desktop/R Practice/BMIplot/data/adsl.RData")
# getwd()
# 
# load(file = "C:/Users/karth/OneDrive/Desktop/R Practice/BMIplot/data/adsl.RData")
# adsl <- readRDS(file = "C:/Users/karth/OneDrive/Desktop/R Practice/BMIplot/data/adsl.rds")

ui <- page_navbar( 
  nav_panel("BMI Calculator",

  sidebarLayout( 
    sidebarPanel( 
      #selectInput("weight", "Select Weight (kg):", choices = seq(40, 150, by = 1)), 
      #selectInput("height", "Select Height (cm):", choices = seq(140, 210, by = 1)) ), 
      numericInput( 
        "weight", 
        "Enter Weight", 
        value = 1, 
        min = 40, 
        max = 150 
      ), 
      numericInput( 
        "height", 
        "Enter Height", 
        value = 1, 
        min = 140, 
        max = 210 
      ),
      actionButton('submit', 'Submit the data')
    ),
    mainPanel( tags$h1('Bar Chart of BMI'),
          tags$br(),
          plotOutput("bmiPlot"), 
          tags$br(),
          tags$br(),
          tags$h2('BMI Calculated Value'),
          tags$br(),
          verbatimTextOutput("bmiValue"), 
          tags$br(),
          tags$br(),
          tags$h3('Table of height, weight, BMI'),
          tags$br(),
          dataTableOutput("table") 
    )
  )
  
),

nav_panel("BMI from ADSL data",
          
          sidebarLayout( 
            sidebarPanel( 
            
                fileInput("file1", "Choose R file", accept = c(".rds", ".RData")),
            ),
                mainPanel(
                  plotOutput("dataPlot"), 
                  tags$br(),
                  tags$br(),
                  dataTableOutput("contents")
                )
              )
          
          
),

title = "Applications", 
id = "page",
)

#Define server logic

server <- function(input, output) { 
  bmi <- reactive({ weight <- input$weight
  height <- input$height / 100
  # Convert cm to meters
  bmi_value <- weight / (height^2)
  return(bmi_value) })
  
  output$bmiValue <- renderText({
    bmi_val <- bmi()
    category <- ifelse(bmi_val < 18.5, "Underweight",
                       ifelse(bmi_val < 24.9, "Normal weight",
                              ifelse(bmi_val < 29.9, "Overweight", "Obese")))
    paste("Your BMI is:", round(bmi_val, 2), "-", category)
  }) %>% bindEvent(input$submit)
  
  bmidf <- reactive({
    data.frame(category=c('Height','Weight','BMI'),
               values=c(input$height , input$weight, bmi()))
  })
  
  output$bmiPlot <- renderPlot({
    ggplot2::ggplot(bmidf(), aes(x=category, y=values))+
      geom_col()
  }) %>% bindEvent(input$submit)
  
  output$table <- 
    renderDataTable({datatable(bmidf())
    }) %>% bindEvent(input$submit)
  
  
  database <- reactive({
    file <- input$file1
    # print(file)
    
    ext <- tools::file_ext(file$datapath)
    validate(need(ext == "rds", "Please upload a csv file"))
    
    readRDS(file$datapath)
  })
  
  
  output$dataPlot <- renderPlot({
    ggplot2::ggplot(database(), aes(x=HEIGHTBL, y=WEIGHTBL))+
      geom_point()
  })
  
  output$contents <- renderDataTable({
    database()
    
  })
  
}

#Run the application

shinyApp(ui = ui, server = server)

