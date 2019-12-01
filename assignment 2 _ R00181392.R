# R00181392, Svetlna Ivanov
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  
#   Visualisation of gapminder Data using plotly library in R on shinydashboard
#
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Loading packages

#install.packages("shinydashboard")
#install.packages("choroplethr")
#install.packages("choroplethrMaps")
library(ggplot2)
library(plotly)
library(plyr)
library(dplyr)
library(readr)
library(choroplethr)
library(choroplethrMaps)
library(crosstalk)
library(markdown)
library(shinydashboard)
library(shiny)
library(ggplot2)
require(markdown)
library(RColorBrewer)

# 
path = "C:/Users/User/Documents/R/CIT/Visualisation/Project1/GAPminder/"


# Reading data from files

df <-read_csv(file = paste(path, "my_data.csv", sep=""))
year_list <- unique(sort(df$year))

region_list <- unique(sort(df$Region))


# Define UI for application 
ui <- 

  dashboardPage(skin = 'blue',

                dashboardHeader(title = "Data Visualisation - Assignment 2: R00181392: Svetlana Ivanov",
                                titleWidth = 600),   #dashboardHeader
                dashboardSidebar(

                  conditionalPanel(
                    condition = "input.tabVals == 1",
                    br(),
                    h3("    My Data for visualisation"),
                    br(), br(),br(),

                  selectInput(
                    inputId="year_1",
                    label = "Select Year",
                    choices = year_list
                  ),
                  br(), br(), br(),

                   sliderInput(
                     inputId = "life",
                     label = "Life expectancy",
                     min = 0, max = 120,
                     value = c(30,100)
                   ) 
                    ),  #cond 1
                  
                  conditionalPanel(
                    condition = "input.tabVals == 2",
                    br(),
                    h3("Life expectancy by Region"),
                    br(), br(),br(),

                    selectInput(
                      inputId="year_2",
                      label = "Select Year",
                      choices = year_list
                    )
                    ),  #cond 2
                  
                  conditionalPanel(
                    condition = "input.tabVals == 3",
                    br(),
                    h3("Income vs Life expectancy by government spending for health"),
                    br(), br(),br(),

                    selectInput(
                      inputId="year_3",
                      label = "Select Year",
                      choices = year_list
                    )
                    
                    ),  #cond 3
                  
                  conditionalPanel(
                    condition = "input.tabVals == 4",
                    br(),
                    h3("Food supply versus income by countries and regions "),
                    br(), br(),br(),
                    
                    selectInput(
                      inputId="year_4",
                      label = "Select Year",
                      choices = year_list
                    )
                    ),  #cond 4
                  
                  conditionalPanel(
                    condition = "input.tabVals == 5",
                    br(),
                    h3("Food supply versus income by countries and regions"),
                    br(), br(),br()

                    #selectInput(
                    #  inputId="year_5",
                    #  label = "Year",
                    #  choices = year_list
                    #)
                    
                    ),  #cond 5
                  
                  conditionalPanel(
                    condition = "input.tabVals == 6",
                    br(),
                    h3("Life expectation vs Food and Life expectation vs Health"),
                    br(), br(),br(),

                    selectInput(
                      inputId="year_6",
                      label = "Select Year",
                      choices = year_list
                    ),
                    br(), br(),br(),
                    
                    selectInput(
                      inputId = "region",
                      label = "Select Region",
                      choice = region_list
                    ),
                    br(), br(),br(),
                    
                    sliderInput(
                      inputId = "life_6",
                      label = "Life expectancy",
                      min = 0, max = 120,
                      value = c(30,50)
                    )
                    
                    ),  #cond 6
                  
                  conditionalPanel(
                    condition = "input.tabVals == 7",
                    br(),
                    h3("Life expectation vs Food and Life expectation vs Health"),
                    br(), br(),br(),

                    selectInput(
                      inputId="year_7",
                      label = "Year",
                      choices = year_list
                    )
                    
                    ),  #cond 7
                  
                  conditionalPanel(
                    condition = "input.tabVals == 8",
                    br(),
                    h3("Choroplethr Maps"),
                    br(), br(),br(),

                    selectInput(
                      inputId="year_8",
                      label = "Select Year",
                      choices = year_list
                    ),
                    
                    br(), br(),br(),
                    radioButtons("selected_map", "Select type of map:",
                                 c("Life expectancy by country" = 1, 
                                   "Income per person by country" =2, 
                                   "Food supply kCal per person" = 3, 
                                   "Governement health spending" =4, 
                                   "Population growth annual percent" = 5, 
                                   "Population of the world "=6)
                    )
                    
                    )  #cond 8
                  
                  
                  
                ),  # dashboardSidebar
                dashboardBody(  tags$head(
                  tags$style(
                    HTML('
                         h2 {
                         font-weight: bold;
                         color: green;
                         font-style: italic;
                         text-align:center;
                         }
                    .main-sidebar {
                    background-color: linen;
                    font-style: strong;
                    text-align:center;
                   
                    }

                         ')
                    )
                ),
                  
                                          tabsetPanel(tabPanel(strong("Data"),
                                                   fluidRow(br()),
                                                   h2(textOutput('year_1')), br(), br(),
                                                   valueBoxOutput("min_lexp"), 
                                                   valueBoxOutput("min_country"),
                                                   valueBoxOutput("min_Region"),
                                                   br(), br(), br(),
                                                   
                                                   fluidRow(br()),
                                                   valueBoxOutput("max_lexp"), 
                                                   valueBoxOutput("max_country"),
                                                   valueBoxOutput("max_Region"),
                                                   br(), br(), br(),
                                                   
                                                   tableOutput('df'), value = 1   ),  # tabPanel1   
                                          
                                          tabPanel(strong("Static boxplot"),
                                                   fluidRow(br()),  
                                                   h2(textOutput('year_2')), br(), br(),
                                                   plotlyOutput('static_boxplot'),  value = 2),   # tabPanel 2

                                          tabPanel(strong("Static scatterpolt"),
                                                   fluidRow(br()),
                                                   h2(textOutput('year_3')), br(), br(),
                                                   plotlyOutput('static_scatterplot'), value = 3),    # tabPane3
                                          
                                          tabPanel(strong("Bubble scatterplot"),
                                                   fluidRow(br()),
                                                   h2(textOutput('year_4')), br(), br(),
                                                   plotlyOutput('static_bubble_scatterplot'), value = 4),  #tabPanel4
                                          
                                          tabPanel(strong("Animated scatterplot"),
                                                   fluidRow(br()), 
                                                   h2(textOutput('year_5')),
                                                    br(), br(),
                                                   plotlyOutput('animated_scatterplot'),  value = 5),   # tabPanel 5
                                          
                                          tabPanel(strong("Linked charts"),
                                                   fluidRow(br()), 
                                                   h2(textOutput('year_6')), br(), br(),
                                                   plotlyOutput('bscol_plot1'), 
                                                   br(), br(), br(),
                                                   plotlyOutput('bscol_plot2'), value = 6),   # tabPanel 6
                                          
                                          tabPanel(strong("Linked charts 2"),
                                                   fluidRow(br()), 
                                                   h2(textOutput('year_7')), br(), br(),
                                                   plotlyOutput('subplot_plot'),  value = 7),   # tabPanel 7
                                          
                                          tabPanel(strong("Maps"),
                                                   fluidRow(br()), 
                                                   h2(textOutput('year_8')), br(), br(),
                                                   plotOutput('maps_plot'),  value = 8),   # tabPanel 8
                                          
                                                         id = 'tabVals')   #tabsetPanel
                                                ) # tabsetPanel
                                               )  #dashboardBody
                                            


server <-   function(input,output) {

    output$year_1 <- renderText({
                  paste(" Dataset for year ", input$year_1,
                        " and Life Expectancy between ", input$life[1], 
                        " and ", input$life[2], " years", sep="")
                
    })
    output$min_lexp <- renderValueBox({
      my_data <-df %>% filter((year == input$year_1)& 
                                (lexp >= input$life[1]) & (lexp <=input$life[2]))
      min_lexp <-min(my_data$lexp, na.rm=TRUE)

      valueBox(
        value = min_lexp, 
        subtitle= "Minimal Life Expectancy for the choosen parameters",
        icon = icon("heartbeat"),
        color = "red"
      )
    
    })

    output$min_country <- renderValueBox({
      my_data <-df %>% filter((year == input$year_1)& 
                                (lexp >= input$life[1]) & (lexp <=input$life[2])) 
      min_lexp <-min(my_data$lexp, na.rm=TRUE)
      min_country <-my_data$country[my_data$lexp == min_lexp]
      
      valueBox(
        value = min_country, 
        subtitle= "Country with Manimal Life Expectancy",
        icon = icon("compass"),
        color = "orange"
      )
    })
    
    output$min_Region <- renderValueBox({
      my_data <-df %>% filter((year == input$year_1)& 
                                (lexp >= input$life[1]) & (lexp <=input$life[2])) 
      min_lexp <-min(my_data$lexp, na.rm=TRUE)
      min_country <-my_data$country[my_data$lexp == min_lexp]
      min_region <-my_data$Region[my_data$lexp == min_lexp]
      
      valueBox(
        value = min_region, 
        subtitle= "Region with Minimal Life Expectancy",
        icon = icon("globe"),
        color = "yellow"
      )
    })
    
    output$max_lexp <- renderValueBox({
      my_data <-df %>% filter((year == input$year_1)& 
                                (lexp >= input$life[1]) & (lexp <=input$life[2]))
      max_lexp <-max(my_data$lexp, na.rm=TRUE)
      
      valueBox(
        value = max_lexp, 
        subtitle= "Maximal Life Expectancy for the choosen parameters",
        icon = icon("heartbeat"),
        color = "lime"
      )
      
    })
    
    output$max_country <- renderValueBox({
      my_data <-df %>% filter((year == input$year_1)& 
                                (lexp >= input$life[1]) & (lexp <=input$life[2])) 
      max_lexp <-max(my_data$lexp, na.rm=TRUE)
      max_country <-my_data$country[my_data$lexp == max_lexp]
      
      valueBox(
        value = max_country, 
        subtitle= "Country with Maximal Life Expectancy",
        icon = icon("compass"),
        color = "green"
      )
    })
    
    output$max_Region <- renderValueBox({
      my_data <-df %>% filter((year == input$year_1)& 
                                (lexp >= input$life[1]) & (lexp <=input$life[2])) 
      max_lexp <-max(my_data$lexp, na.rm=TRUE)
      max_country <-my_data$country[my_data$lexp == max_lexp]
      max_region <-my_data$Region[my_data$lexp == max_lexp]
      
      valueBox(
        value = max_region, 
        subtitle= "Region with Maximal Life Expectancy",
        icon = icon("globe"),
        color = "olive"
      )
    })

    output$df <- renderTable({

     my_data <-df %>% filter((year == input$year_1)& 
                  (lexp >= input$life[1]) & (lexp <=input$life[2]))

            my_data
    })
    
    output$year_2 <- renderText({
      paste("Life expectancy by region in ", input$year_2, sep="")
    })  
    output$static_boxplot <- renderPlotly({
      
        df %>% filter(year==input$year_2)%>%   group_by(Region)%>%
        plot_ly( x= ~Region, y = ~ lexp, color = ~ Region) %>%
        add_boxplot()%>%
        layout(xaxis=list(title = "Regions"),
               yaxis=list(title = "Life Expectancy"))
               #title = "Life expectancy by regions in ", input$year_2)

      } )
    
    output$year_3 <- renderText({
      paste("Income vs Life expectancy by government spending for health in ", input$year_3, sep ="")
    })
    
    output$static_scatterplot <- renderPlotly({
      
      df %>% filter(year == input$year_3) %>%
        plot_ly(x=~gdp, y=~lexp, color=~health,
                hoverinfo = "text",
                text = ~paste("Country:", country, "<br>",
                              "Region:", Region, "<br>",
                              "GDPperCapita:", gdp, "<br>",
                              "Life Expectancy :", lexp, "<br>",
                              "Government health spending :", health )) %>%
        add_markers()%>%
        layout(xaxis=list(title = "Income per person, inflation adjusted", type="log"),
               yaxis=list(title = "Life Expectancy"))
               #title = "Income vs Life expectancy by government spending for health in year ", input$year_3)
      
    } )
  
    output$year_4 <- renderText({
      paste( "Food supply versus income by countries and regions in ", input$year_4, sep="")
    })
    
    output$static_bubble_scatterplot <- renderPlotly({
      
      df %>%
        filter(year == input$year_4) %>%
        plot_ly(x = ~gdp, y = ~food, 
                hoverinfo =  "text", 
                text = ~paste("Country:", country,
                              "<br> Income per person:", gdp,
                              "<br> kcal per person :", food,
                              "<br> Region :", Region)) %>%
        add_markers(size = ~pop, color = ~Region,
                    marker = list(opacity = 0.6,
                                  sizemode = "diameter",
                                  sizeref = 2)) %>%
        layout(xaxis=list(title = "Income per person, inflation adjusted", type="log"),
               yaxis=list(title = "Food supply kilocalories per person and day"))
               #title = "Food supply versus income for 2010 by countries and regions")
      
      
    } )
    
    output$year_5 <- renderText({
      #paste("Food supply versus income by countries and regions ", input$year_5, sep = "")
      "Food supply versus income by countries and regions "
    })
    
    output$animated_scatterplot <- renderPlotly({
      

     df%>%      plot_ly(x = ~gdp, y = ~food,
                hoverinfo =  "text", 
                text = ~paste("Country:", country,
                              "<br> Income per person:", gdp,
                              "<br> kcal per person :", food,
                              "<br> Region :", Region)) %>%
        add_text(x = 20000, y = 1700, text = ~year, frame = ~year,
                 textfont = list(color = toRGB("gray80"), size = 100)) %>%
        add_markers(frame = ~year, 
                    size = ~pop, 
                    color = ~Region,
                    marker = list(opacity = 0.6,
                                  sizemode = "diameter",
                                  sizeref = 2)) %>%
        layout(xaxis = list(title = "Income per person, inflation adjusted",type ="log"), 
               yaxis = list(title = "Food supply kilocalories per person and day", type ="log")) %>%
               #title = "Food supply versus income by countries and regions")%>%
        animation_slider(hide = TRUE) %>%
        animation_opts(frame = 1000,
                       transition = 300,
                       easing = "back",
                       redraw = TRUE)
      
      
    } )
    
    output$year_6 <- renderText({
      paste("Life expectation vs Food and Life expectation vs Health in ", input$year_6, sep="")
    }) 
    
    output$bscol_plot1 <- renderPlotly({
      
      df %>% 
        filter((year==input$year_6) &
                (Region == input$region) & 
                (lexp >= input$life_6[1]) & 
                (lexp <=input$life_6[2])) %>%
      
        plot_ly(x = ~food, y = ~lexp, color=~Region) %>%
        add_markers()%>%
        layout(xaxis = list(type ="log", title="Life expectation vs Food"))
      
      
      
    } )
    
    output$bscol_plot2 <- renderPlotly({
      
      df %>% 
        filter((year==input$year_6) &
                 (Region == input$region) & 
                 (lexp >= input$life_6[1]) & 
                 (lexp <=input$life_6[2])) %>%
        
        plot_ly(x = ~health, y = ~lexp, color = ~Region) %>%
        add_markers( title="Life expectation vs Health", showlegend = FALSE)
      
    } )
    
    output$year_7 <- renderText({
      paste("Life expectation vs Food and Life expectation vs Health in ", input$year_7, sep="")
    })
    
    output$subplot_plot <- renderPlotly({
      
          df %>% filter(year==input$year_6)
        
        shared_df <- SharedData$new(df)
        
        p1 <- shared_df %>% 
          plot_ly(x = ~food, y = ~lexp, color=~Region) %>%
          add_markers()%>%
          layout(xaxis = list(type ="log"))
        
        
        
        p2 <- shared_df %>%
          plot_ly(x = ~health, y = ~lexp, color = ~Region) %>%
          add_markers( showlegend = FALSE)
        
        ggplotly(subplot(p1, p2, titleX = TRUE, shareY = TRUE) %>% hide_legend()%>%
          highlight(on = "plotly_selected"))
    } )
    
    
    output$year_8 <- renderText({
      "Selected map"
    })
    
    output$maps_plot <- renderPlot({
      

      # prepare the data
      
          df_new <- df %>% filter(year == input$year_8 )

      
      if (input$selected_map==1) { 
        
        df_new <- subset(df_new, select=c("country", "lexp"))
        names(df_new)<-c("region", "value")
        
       plot_data <- df_new %>% mutate(region = tolower(region))
        
        
        country_choropleth(plot_data, num_colors=9) +
          scale_fill_brewer(palette="YlOrRd") +
          labs(title = "Life expectancy by country",
               subtitle = paste("Gapminder data ", input$year_8),
               caption = "source: https://www.gapminder.org",
               fill = "Years")
        
        
      } else if (input$selected_map==2){ 
        
        df_new <- subset(df_new, select=c("country", "gdp"))
        names(df_new)<-c("region", "value")
        
        plot_data <- df_new %>% mutate(region = tolower(region))
        
        
        country_choropleth(plot_data, num_colors=9) +
          scale_fill_brewer(palette="Blues") +
          labs(title = "Income per person by country",
               subtitle = paste("Gapminder data ", input$year_8),
               caption = "source: https://www.gapminder.org",
               fill = "$")
        
      } else if(input$selected_map==3){

        df_new <- subset(df_new, select=c("country", "food"))
        names(df_new)<-c("region", "value")
        
        plot_data <- df_new %>% mutate(region = tolower(region))
        
        
        country_choropleth(plot_data, num_colors=9) +
          scale_fill_brewer(palette="BuGn") +
          labs(title = "Food supply kcal per person",
               subtitle = paste("Gapminder data ", input$year_8),
               caption = "source: https://www.gapminder.org",
               fill = "kcal")
        
      } else if(input$selected_map==4){
        
        df_new <- subset(df_new, select=c("country", "health"))
        names(df_new)<-c("region", "value")
        
        plot_data <- df_new %>% mutate(region = tolower(region))
        
        
        country_choropleth(plot_data, num_colors=9) +
          scale_fill_brewer(palette="BuPu") +
          labs(title = "Governement health spending",
               subtitle = paste("Gapminder data ", input$year_8),
               caption = "source: https://www.gapminder.org",
               fill = "%")
        
      } else if (input$selected_map==5){
        
        df_new <- subset(df_new, select=c("country", "growth"))
        names(df_new)<-c("region", "value")
        
        plot_data <- df_new %>% mutate(region = tolower(region))
        
        
        country_choropleth(plot_data, num_colors=9) +
          scale_fill_brewer(palette="PuBuGn") +
          labs(title = "Population growth annual percent",
               subtitle = paste("Gapminder data ", input$year_8),
               caption = "source: https://www.gapminder.org",
               fill = "%")
        
      }else if (input$selected_map==6) {
        
        df_new <- subset(df_new, select=c("country", "pop"))
        names(df_new)<-c("region", "value")
        
        plot_data <- df_new %>% mutate(region = tolower(region))
        
        
        country_choropleth(plot_data, num_colors=9) +
          scale_fill_brewer(palette="Paired") +
          labs(title = "Population of the world ",
               subtitle = paste("Gapminder data ", input$year_8),
               caption = "source: https://www.gapminder.org",
               fill = " ")
        
      }
      
    } )
    
    
  } 

# Run the application 
shinyApp(ui = ui, server = server)

