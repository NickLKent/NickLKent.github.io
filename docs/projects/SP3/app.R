#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rsconnect)
library(readr)
library(dplyr)
library(plotly)
library(tidyverse)
library(lubridate)
library(rvest)
library(stringr)

load("Shinydata.rdata")

theme_gppr <- function(){ 
  font <- "Georgia"   #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 15,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 11,
        hjust = 0),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 10),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 9,angle=90),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}

# Define UI for application that draws a histogram
ui <- navbarPage("Liberal Arts Education at Macalester College",
                 tabPanel("Number of Majors",
                          # Sidebar with a drop down input for the division 
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("majors", label = h3("Number of Majors"),
                                           choices = c("1", "2", "3"),
                                           selected = "1")),
                            
                            # Show a plot of the generated distribution
                            mainPanel(plotOutput("noahPlot")))),
                            
                 tabPanel("Departments Participated In",
                                     # Sidebar with a drop down input for the division 
                         sidebarLayout(
                           sidebarPanel(
                              selectInput("MajorsDivision2", label = h3("Select a Division of Majors"), 
                                            choices = list("Interdisciplinary" = "Interdisciplinary", "Humanities" = "Humanities", 
                                              "Social Sciences" = "Social Sciences","Fine Arts" = "Fine Arts", 
                                              "Natural Sciences and Mathematics" = "Natural Sciences and Mathematics"), selected = "Interdisciplinary")),
                                       
                            # Show a plot of the generated distribution
                            mainPanel(plotOutput("quinnPlot")))),
                            
                  tabPanel("Average number of classes in each division",
                                     # Sidebar with a drop down input for the division 
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("ClassesDivision", label = h3("Select a Division of Classes"), 
                                             choices = list("Interdisciplinary" = 'countINT', "Humanities" = 'countHUM', 
                                               "Social Sciences" = 'countSOCSCI', "Fine Arts" = 'countFA', 
                                                "Natural Sciences and Mathematics" = 'countSCIMATH', 
                                                "Non-Divisional" = 'countNON'), selected = "countINT"),
                                         
                               selectInput("MajorsDivision", label = h3("Select a Division of Majors"), 
                                              choices = list("Interdisciplinary" = "Interdisciplinary", "Humanities" = "Humanities", 
                                                "Social Sciences" = "Social Sciences","Fine Arts" = "Fine Arts", 
                                                "Natural Sciences and Mathematics" = "Natural Sciences and Mathematics"), selected = "Interdisciplinary")),
                                       
                                # Show a plot of the generated distribution
                                mainPanel(plotOutput("nickPlot")))))


server <- function(input, output) {
  
  output$nickPlot <- renderPlot({
    #browser()
    majorsViz2%>%
      filter(major_division == input$MajorsDivision)%>%
      filter(division == input$ClassesDivision)%>%
      
      ggplot(aes(x = (fct_reorder(major, avgClasses)), y = avgClasses)) + 
      geom_col(fill = 'dark green', color = "white", alpha = 0.4) + 
      labs(x = 'Major', y = 'Avgerage Number of Classes', title = 'How Does Division of a Major Affect the Amount of Classes Taken Within Each Division?',
           subtitle = "Number of Classes Taken in Each Division Based on the Division of the Major") + 
      theme_gppr()+
      ylim(0, 40)+
      geom_text(aes(label = round(avgClasses)), vjust = 1.5, colour = "white")
    
    
  })
  output$quinnPlot <- renderPlot({
    avgPrefix2 %>% 
      filter(major1_division == input$MajorsDivision2)%>%
      ggplot(aes(x = major1, y = avgPrefix))+
      
      geom_col(aes(x=(fct_reorder(major1, avgPrefix)),y=avgPrefix), fill = 'dark green', color = "white", alpha = 0.4)+
      theme_gppr()+
      labs(x="Major",y="Average Departments Taken",title="How Varied is the Class Choice of Different Majors?",
           subtitle="Number of Unique Departments Taken Based on Major (single major students only)")+
      geom_text(aes(label = round(avgPrefix)), vjust = 1.5, colour = "white")
  })
  
  output$noahPlot <- renderPlot({
    majjors%>%
      filter(major_ct == input$majors)%>%
      
      ggplot(aes(x = (fct_reorder(division, value)), y = value)) + geom_col(fill = 'dark green', color = "white", alpha = 0.4) + theme_gppr() +
      scale_y_continuous(breaks = seq(0, 20, by = 2)) + labs(x = "Division", y = "Average Number of Classes", 
                                                             title = "How Does Number of Majors Affect Class Choice?", 
                                                             subtitle = "Average number of classes taken in each division based on the number of majors a student has")+
      geom_text(aes(label = round(value)), vjust = 1.5, colour = "white")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
