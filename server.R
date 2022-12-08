#######################################################
# Name: Magaritte Nguyen
# Date: 28NOV2022
# Class: ST 558 (601) Fall 2022 Data Science for Statisticians
# Assignment: Homework 11 - Dynamic UI
# File: server.R
#######################################################

# For this homework you will create an R Shiny App (two file approach please) and upload both the ui andserver file to wolfware.
# The purpose of this homework is to create an R Shiny app with some customizations.
# Creation of an app with a dynamic UI. Our goal will be able to create the the app you can find here (https://shiny.stat.ncsu.edu/jbpost2/Dynamic_UI/).
# The dataset used is about sleep habits of mammals. It is included in the ggplot2 package in a dataset called msleep.

# To do:

# • Create a folder ‘DynamicUI’.

# • In the folder save this starter ui file and this starter server file.

# • We want to allow for our UI to change based on user input.

# • Recall there are three major ways to do dynamic user interfaces (each will be used once in the new app):

 # – update* functions
 # – renderUI with uiOutput
 # – conditionalPanel

# • First if the user clicks on “Color code conservation status”, a new checkbox appears that changes the opacity of the points based on the REM sleep variable. Use a conditionalPanel to create this box and change the plot appropriately.

# • Notice that if the opacity checkbox is clicked then the slider changes its minimum value to 3 (so the opacity change can be seen). If the checkbox is unclicked the minimum goes back to 1. Use an update*function to do this.

# • The title changes based on which vore is selected. This is just a 1st level header (h1) being used to replace the titlePanel. Use renderUI/uiOutput to create this new title.

#code provided by Dr. Post
#Here is a start for your server.R file:

#access packages
library(shiny)
library(tidyverse)
library(ggplot2)

#data source - in R
#data("msleep")

#shiny server function
shinyServer(function(input, output, session) {
  
  #data source - in R
  getData <- reactive({
    newData <- msleep %>% 
                 filter(vore == input$vore)
  })
  
  #create plot
  output$sleepPlot <- renderPlot({
    
    #get filtered data
    newData <- getData()
    
    #create plot
    g <- ggplot(newData, aes(x = bodywt, y = sleep_total))
      
    #conservation T...
    if(input$conservation){
      #conservation T and REM T
      if(input$REM){ 
        g + geom_point(size = input$size, aes(col = conservation, alpha=sleep_rem))
      } else {
      
        #conservation T, but REM not T
        g + geom_point(size = input$size, aes(col = conservation))
      }
      
    } else {
      #base case -- conservation F, but REM F
      g + geom_point(size = input$size)
    }
  }) #end of the renderPlot() function
  
  #create text info
  output$info <- renderText({
    #get filtered data
    newData <- getData()
    
    paste("The average body weight for order", input$vore, "is", round(mean(newData$bodywt, na.rm = TRUE), 2), "and the average total sleep time is", round(mean(newData$sleep_total, na.rm = TRUE), 2), sep = " ")
  }) #end of renderText() function
  
  #create output of observations    
  output$table <- renderTable({
    getData()
  })
  
  #create title info
  output$title <- renderUI({
    paste0("Investigation of ",  tools::toTitleCase(input$vore), "vore Mammal Sleep Data")
  })  #end of renderUI() function

  #slider range update
  observe({
    updateSliderInput(session=session, inputId = "size",
                      min = ifelse(input$REM && input$conservation , 3, 1), max = 10, value = 5)

  })

}) #end of shiny server function 
