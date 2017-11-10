
## The New York Times coverage of European Countries in years 2000-2017
## Contains graphs by country and by NYT section
## Author: Magdalena Paszko


library(shiny)
library(dplyr)
library(plotly)

df <- read.csv("NYT_Europe_News_Clean.csv", header=TRUE, stringsAsFactors=FALSE)

all_sections <- df %>% 
  group_by(section) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) %>%
  select(section)

all_countries <- unique(df$country)[order(unique(df$country))]


shinyServer(function(input, output, session) {
  
  # Render list of sections
  output$sectionControl <- renderUI({
    checkboxGroupInput(inputId="sel_sections", label="", choices=all_sections$section,
                       selected=all_sections$section)
  })
  # Update sections with a button
  observeEvent(
    eventExpr = input$check_all_sections,
    handlerExpr = {
    updateCheckboxGroupInput(
      session, "sel_sections", 
      selected = if (length(input$sel_sections) < length(all_sections$section)) all_sections$section
                  else 0
    )}
    )
  
  # Render list of countries
  output$countryControl <- renderUI({
    checkboxGroupInput(inputId="sel_countries", label="", choices=all_countries,
                       selected=all_countries)
  })
  # Update countries with a button
  observeEvent(
    eventExpr = input$check_all_countries,
    handlerExpr = {
      updateCheckboxGroupInput(
        session, "sel_countries", 
        selected = if (length(input$sel_countries) < length(all_countries)) all_countries
                    else 0
      )}
  )
  
  
  # Render graph by section
  output$sectionPlot <- renderPlotly({
    aggdf_section <- df %>% 
      filter(section %in% input$sel_sections &
             country %in% input$sel_countries) %>%
      group_by(year, section) %>%
      summarize(count=n_distinct(main))

    top5section <-  aggdf_section %>% 
      group_by(section) %>%
      summarize(total = sum(count)) %>%
      arrange(desc(total)) %>%
      mutate(rank = rank(desc(total)),
             top = ifelse(rank<=5, section, "other"),
             top = factor(top, levels=unique(top))) %>%
      select(section, top)
    
    left_join(aggdf_section, top5section, by="section") %>%
      group_by(section) %>%
      plot_ly(x = ~year, 
              y = ~count, 
              color=~top, colors="Set1", alpha=0.9,
              type="scatter",
              mode = 'lines',
              text=~section) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = 'Number of tagged articles'))
    
  })
  
  
  # Render graph by country
  output$countryPlot <- renderPlotly({
    aggdf_country <- df %>% 
      filter(section %in% input$sel_sections &
             country %in% input$sel_countries) %>%
      group_by(year, country) %>%
      summarize(count=n())
    
    top5country <-  aggdf_country %>% 
      group_by(country) %>%
      summarize(total = sum(count)) %>%
      arrange(desc(total)) %>%
      mutate(rank = rank(desc(total)),
             top = ifelse(rank<=5, country, "other"),
             top = factor(top, levels=unique(top))) %>%
      select(country, top)
    
    left_join(aggdf_country, top5country, by="country") %>%
      group_by(country) %>%
      plot_ly(x = ~year, 
              y = ~count, 
              color=~top, colors="Set1", alpha=0.9,
              type="scatter",
              mode = 'lines',
              text=~country) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = 'Number of tagged articles'))
    
  })
  
})
