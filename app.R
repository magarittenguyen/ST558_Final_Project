#######################################################
# Name: Magaritte Nguyen
# Date: 9DEC2022
# Class: ST 558 (601) Fall 2022 Data Science for Statisticians  
# Assignment: Final Project - app file
# File: app.R
#######################################################

#data
#https://www.kaggle.com/code/kingabzpro/cosmetics-ingredients/data
#https://www.kaggle.com/datasets/kingabzpro/cosmetics-datasets/code?resource=download

#libraries
library(tidyverse)
library(corrplot)
library(ggplot2)
library(caret)
library(DT)

library(shiny)
library(shinydashboard)


#https://fontawesome.com/search?q=info&o=r

####################################### UI ################################################

ui <- dashboardPage(
  skin="blue",
  
  
  #add title
  dashboardHeader(title="Magaritte Nguyen - STT558 - Final Project - Cosmetics Datasets",
                  titleWidth=1000),
  
  #define sidebar items
  dashboardSidebar(
    sidebarMenu( id = "sidebarmenu",
                 #first menu item
                 #The About Page
                 menuItem("About", tabName = "about", icon = icon("box-archive")),
                 #second menu item
                 #The Data Exploration Page
                 menuItem("Data Exploration", tabName = "data-exploration", 
                          icon = icon("laptop")),
                 # 3rd menu item with - 3 subtabs
                 #The Modeling Page
                 menuItem("Modeling", tabName = "modeling", icon = icon("laptop"), 
                          #subitem 1
                          ## need to find out how to sub tab -- Modeling Info tab
                          menuSubItem ( "Modeling Info", 
                                        tabName = "modeling-info", 
                                        icon = icon("circle-info")), 
                          #subitem 2
                          ## need to find out how to sub tab -- Modeling Fitting tab
                          menuSubItem ( "Model Fitting", 
                                        tabName = "model-fitting", 
                                        icon = icon("chart-line")),
                          #subitem 3
                          ## need to find out how to sub tab -- Prediction tab
                          menuSubItem ( "Prediction", 
                                        tabName = "prediction", 
                                        icon = icon("check"))
                 ), #end of menuItem
                 #Data Page
                 menuItem("Data", tabName = "data", icon = icon("laptop"))
    )),
  
  #define the body of the app
  dashboardBody(
    tabItems(
      # First tab content -  An About page.
      tabItem(tabName = "about", 
              
              h4(
                "This app uses the",
                a(href = "https://www.kaggle.com/datasets/kingabzpro/cosmetics-datasets",
                  "Cosmetics dataset"),
                "provided on",
                a(href = "https://www.kaggle.com", "Kaggle.com"),
                "by the user",
                a(href = "https://www.kaggle.com/kingabzpro", "Abid Ali Awan", .noWS = "after"),
                "."
              ),
              
              fluidRow(
                #add in latex functionality if needed
                withMathJax(),
                
                #Include a picture related to the data 
                #added its own column...
                column(6,
                       tags$img(src='huda_beauty_product_spread.jpg', 
                                width='550px',height='400px')),
                # an image downloaded from web (b.jpg under www folder)
                
                
                #img(src='huda beauty product spread.png', align = "right")),
                
                #two columns for each of the two items
                column(6,
                       # Describe the purpose of the app
                       h1("What does this app do?"),
                       #box to contain description
                       box(background="blue",width=12,
                           h4("This application shows the relationship between the prior distribution and the posterior distribution for a simple Bayesian model."),
                           h4("The prior distribution is assumed to be a Beta distribution and the likelihood is a Binomial distribution with 30 trials (of which you can change the number of successes).  This yields a Beta distribution as the posterior. Note: As the prior distribution is in the same family as the posterior, we say the prior is conjugate for the likelihood."),
                           h4("This application corresponds to an example in ",span("Mathematical Statistics and Data Analysis",style = "font-style:italic"), "section 3.5, example E, by John Rice."),
                           h4("The goal of the example is to update our belief about the parameter \\(\\Theta\\) = the probability of obtaining a head when a particular coin is flipped.  The experiment is to flip the coin 30 times and observe the number of heads. The likelihood is then a binomial distribution. The prior is assumed to be a Beta distribution.")
                       )  #end of box() function -- line 65
                ), #end of column() function -- line 61
                
                column(6,
                       #How to use the app
                       h1("How to use the app?"),
                       #box to contain description
                       box(background="blue",width=12,
                           h4("The controls for the app are located to the left and the visualizations are available on the right."),
                           h4("To change the number of successes observed (for example the number of coins landing head side up), the slider on the top left can be used."),
                           h4("To change the prior distribution, the hyperparameters can be set using the input boxes on the left.  The changes in this distribution can be seen on the first graph."),
                           h4("The resulting changes to the posterior distribution can be seen on the second graph.")
                       ) #end of box() function
                ) #end of column() function 
              ) #end of fluidRow() function -- line 57
      ), #end of tabItem() function 
      
      
      
      
      
      #Second tab content
      #actual app layout      
      tabItem(tabName = "data-exploration",
              fluidRow( #summary of all vars - options 
                #also var i created in the data available
                box(width=12,background="blue",
                    selectizeInput(inputId="summary_select", 
                                   label="Summary of Cosmetic Data Set Variables",
                                   selected = "Price", #default value
                                   choices = colnames(cosmetics), 
                                   multiple = FALSE ),
                    
                    #outputting the data summary
                    mainPanel(
                      verbatimTextOutput("data_summaries")),
                    
                ), 
                
                #text finding in ingredients for products
                box(width=12, background="blue", 
                    textInput(inputId="find_ingredient", 
                              label="Serach Product by Ingredient",
                              value = "SUL(F|PH)(UR|ATE|A)"),
                    checkboxInput(inputId = "TF_ingredient_exclude",
                                  label = "Exclude Ingredient From Product?",
                                  value = FALSE ), #unchecked default
                    
                    #outputting results for incredients in produts
                    mainPanel(
                      #verbatimTextOutput("data_summaries"), 
                      verbatimTextOutput("inc_exc_ingredient")
                      #plotOutput("sleepPlot"),
                      #textOutput("info"),
                      #tableOutput("table")
                    ) #end of mainPanel() function
                    
                ) # end of box() function
                
              ), #end of fluidRow() function
              
              #charts and graphs - row names
              fluidRow( column(width=6 , #12 width is the max
                               box(width = 12, background = "blue", 
                                   #row var
                                   selectizeInput(inputId="contingency_table_row", 
                                                  label="Select Row Variable",
                                                  selected = "Brand", #default value
                                                  choices = # If you want to get both factors 
                                                            #and characters use
                                                colnames(cosmetics[, sapply(cosmetics, class) %in%                                                            c('character', 'factor')]),                                                              multiple = FALSE ),           
                                   
                                   #row var
                                   selectizeInput(inputId="contingency_table_col", 
                                                  label="Select Column Variable",
                                                  selected = "Label", #default value
                                                  choices = # If you want to get both factors 
                                                            #and characters use
                                               colnames(cosmetics[, sapply(cosmetics, class) %in%                                                            c('character', 'factor')]),                                                              multiple = FALSE ), 
                                                  #options = list(maxItems = 2
                                   
                                  ) #end of box()
                               
                               #output contingency table
                               
              ), #end of column() function
              
              #outputting contingency tables
              mainPanel(
                dataTableOutput("contingency_table"),
                plotOutput("correlation"), 

              ) #end of mainPanel() function
              
              ), #end of fluidRow() function
              
              br(), #add break in app output
              
 
      ), #end of tabItem() function -- item 2
      
      
      
      
      
      #Third tab content
      #modeling -- IS THERE SUPPOSED TO BE SOMETHING ON THIS PAGE?!?!?!
      tabItem(tabName = "modeling",
              fluidRow(
                column(width=3,
                       box(width=12,background="blue",sliderInput("yvalue","Y=Number of Modeling",min = 0,max = 30,value = 15)
                       )
                ),
                
              )
      ),  # end of item 3 - modeling  
      
      #3.1 sub tab content
      #modeling-info    
      tabItem(tabName = "modeling-info",
              fluidRow(
                column(width=3,
                       box(width=12,background="blue",sliderInput("yvalue","Y=Number of Modeling Info",min = 0,max = 30,value = 15)
                       )
                ),
                
              )
      ),   #end of item 3.1 - modeling-info  
      
      #3.2 sub tab content
      #model-fitting    
      tabItem(tabName = "model-fitting",
              fluidRow(
                column(width=3,
                       box(width=12,background="blue",sliderInput("yvalue","Y=Number of Model Fitting",min = 0,max = 30,value = 15)
                       )
                ),
                
              )
      ),   #end of item 3.2 - model-fitting  
      
      #3.3 sub tab content
      #prediction    
      tabItem(tabName = "prediction",
              fluidRow(
                column(width=3,
                       box(width=12,background="blue",sliderInput("yvalue","Y=Number of Prediction",min = 0,max = 30,value = 15)
                       )
                ),
                
              )
      ),   #end of item 3.3 - prediction                          
      
      #4 tab content
      #data    
      tabItem(tabName = "data",

             
      ) #end of item 4 - data 
      
      
      
      
      
      
      
      
      
      
      
      
    ) #end of tabItems() function
  ) #end of dashboardBody() function
) # end of the dashboardPage() function

####################################### SERVER ################################################
# Define server logic required to draw the plots
server <- shinyServer(function(input, output, session) {
  
  #read in cosmetic data
  #https://www.kaggle.com/datasets/kingabzpro/cosmetics-datasets/code?resource=download
  cosmetics0 <- read_csv("cosmetics.csv")
  
  #data source - in source above that was downloaded
  getData <- reactive({
    
    #create factors
    #name is too unique so remove
    #create var for main_ingredient -- 355... not too useful 
    cosmetics <- cosmetics0 %>%
      mutate(# type of skin -- factors for each
        combination_fctr = factor(Combination, c(0, 1), labels = c("No", "Yes")), 
        dry_fctr = factor(Dry, c(0, 1), labels = c("No", "Yes")), 
        normal_fctr = factor(Normal, c(0, 1), labels = c("No", "Yes")), 
        oily_fctr = factor(Oily, c(0, 1), labels = c("No", "Yes")), 
        sensitive_fctr = factor(Sensitive, c(0,1), labels = c("No", "Yes")), 
        #type of products - label -- factors
        label_fctr = factor(Label),
        #brand of product
        brand_fctr = factor(Brand)
      ) %>% #end of the mutate() function 
      separate (Ingredients, c("main_ingredient", NA), ",", remove =FALSE, extra = "merge",
                fill = "right") %>%
      select (! c("Combination", "Dry", "Normal", "Oily", "Sensitive", "Name") )
    
  })
  
  # creating summaries for variables in cosmetics data
  output$data_summaries <- renderText({
    
    cosmetics %>% 
      select (all_of(input$summary_select)) %>%
      summary()
    
  })
  
  #searching cosmetics dataset by ingredient that is text specified by user
  output$inc_exc_ingredient <- renderText({
    
    sum(str_detect(string=toupper(cosmetics$Ingredients), pattern = input$find_ingredient, 
                   negate=input$TF_ingredient_exclude ))
    
  })
  
  #graphical summeries...  
  #contingency tables, scatter plots, box plots, ...
  output$contingency_table <- renderDataTable({
    
    #e.g. label vs. brand
    validate(need(input$contingency_table_row,''),
             need(input$contingency_table_col,''))
    #xtabs(as.formula(paste0("~",input$contingency_table_row,"+",input$contingency_table_col)), 
          #cosmetics)
    f <- as.formula(paste0("~", input$contingency_table_row,"+",input$contingency_table_col ))
    pivot_wider(as_tibble(xtabs(f , cosmetics)), names_from = input$contingency_table_col, values_from = n)
    #pivot_wider(as_tibble(xtabs(~ Brand + Label, cosmetics)), names_from = Label, values_from = n)
  })
  
  #correlation
  #performing correlations between variables of interest
  #no input stuff because only two options...
  output$correlation <- renderPlot({
  cor_mat <- cor(cosmetics %>% 
                   select(Price, Rank), method = "pearson")
  
  #correlation plot below
  corrplot(cor_mat, 
           type = "lower",
           tl.pos = "lt",
           title = "Correlation Coefficients for Sephora Cosmetics Data",
           mar=c(0,0,2,0)
  ) #end of corrplot() function
  
  })
  

  
}) # end of the server function 

shinyApp(ui = ui, server = server)
