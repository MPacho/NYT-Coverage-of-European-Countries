
## The New York Times coverage of European Countries in years 2000-2017
## Contains graphs by country and by NYT section
## Author: Magdalena Paszko


library(shiny)
library(plotly)

shinyUI(fluidPage(theme="style.css",
  
  # Application title
  titlePanel("The New York Times Coverage of European Countries"),
  
  # Sidebar with section and country
  fluidRow(
    column(2, "NYT Section",
           fluidRow(
             actionButton(inputId="check_all_sections", label="Check/Uncheck All")
           ),
           fluidRow(
             uiOutput("sectionControl")
           )
    ),
    
    column(2, "Country",
           fluidRow(
             actionButton(inputId="check_all_countries", label="Check/Uncheck All")
           ),
           fluidRow(
             uiOutput("countryControl")
           )
    ),
    
    column(8,
           tabsetPanel(type="tabs",
             tabPanel("By section",
                plotlyOutput("sectionPlot")),
             tabPanel("By country",
                plotlyOutput("countryPlot"))
           )
    )
  )
))
