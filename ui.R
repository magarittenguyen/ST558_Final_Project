#######################################################
# Name: Magaritte Nguyen
# Date: 28NOV2022
# Class: ST 558 (601) Fall 2022 Data Science for Statisticians  
# Assignment: Homework 11 - Dynamic UI
# File: ui.R
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
#Here is a start for your ui.R file:

#access packages
library(shiny)
library(tidyverse)
library(ggplot2)

#data source - in R
#data("msleep")

#shiny UI function
shinyUI(fluidPage(
  
  # Application title
  #titlePanel("Investigation of Omnivore Mammal Sleep Data"),
  titlePanel(uiOutput(outputId = "title")),
  
  # Sidebar with options for the data set
  sidebarLayout(
    sidebarPanel(
      
      h3("Select the mammal's biological order:"),
      
      selectizeInput("vore", "Vore", selected = "omni", choices = levels(as.factor(msleep$vore))),
      
      br(),
      
      sliderInput("size", "Size of Points on Graph",
                  min = 1, max = 10, value = 5, step = 1),
      
      checkboxInput("conservation", h4("Color Code Conservation Status", style = "color:red;")),
      
      conditionalPanel(condition="input.conservation" , checkboxInput("REM", label = h5("Also change symbol based on REM sleep?")))
      
    ), #ind of sidebarPanel() function
    
    # Show outputs
    mainPanel(
      plotOutput("sleepPlot"),
      textOutput("info"),
      tableOutput("table")
    ) #end of mainPanel() function
  ) #end of sidebarLayout() function
)) #end of fluidPage() and shinyUI() functions, respectively