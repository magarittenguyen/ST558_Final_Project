#######################################################
# Name: Magaritte Nguyen
# Date: 9DEC2022
# Class: ST 558 (601) Fall 2022 Data Science for Statisticians  
# Assignment: Final Project - Creating a Shiny App - Cosmetics App
# File: app.R
#######################################################

#data
#https://www.kaggle.com/code/kingabzpro/cosmetics-ingredients/data
#https://www.kaggle.com/datasets/kingabzpro/cosmetics-datasets/code?resource=download

#libraries
library(shiny)
library(shinydashboard)

library(tidyverse)
library(corrplot)
library(ggplot2)
library(caret)
library(DT)

library(tree)
library(randomForest)


#read in cosmetic data
#https://www.kaggle.com/datasets/kingabzpro/cosmetics-datasets/code?resource=download
cosmetics0 <- read_csv("cosmetics.csv")

#data source - in source above that was downloaded
  
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
    select (! c("Combination", "Dry", "Normal", "Oily", "Sensitive", "Name") ) %>%
    #removing outliers -- i don't see any in the data first off, but dong for good measure
    na.omit()
  

####################################### UI #####################################
  
ui <- dashboardPage(
  skin="blue",
  
  
  #add title
  dashboardHeader(title="Cosmetics App",
                  titleWidth=1000),
  
  #define sidebar items
  dashboardSidebar(
    
  ################################### sidebar menu ############################  
  
  #use to find icons
  #https://fontawesome.com/search?q=info&o=r
    sidebarMenu( id = "sidebarmenu",
                 #first menu item
                 #The About Page
                 menuItem("About", tabName = "about", icon = icon("house")),
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
                                        icon = icon("magnifying-glass"))
                 ), #end of menuItem
                 #Data Page
                 menuItem("Data", tabName = "data", icon = icon("laptop"))
    )),

  ################################### dashboard body ###########################
  
  #define the body of the app
  dashboardBody(
    #many tab items...
    tabItems(
      ########## First tab content -  An About page.
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
              
              br(), 
              
              fluidRow(
                #Include a picture related to the data 
                #added its own column...
                column(6,
                       tags$img(src='makeup2.jpg', 
                                width='1250px',height='400px')),
                # an image downloaded from web (b.jpg under www folder)
                
                
                #img(src='huda beauty product spread.png', align = "right")),
              ),
              
              fluidRow(
                #add in latex functionality if needed
                withMathJax(),
                
                #two columns for each of the two items
                column(6,
                       # Describe the purpose of the app
                       h1("Purpose of The App"),
                       #box to contain description
                       box(background="purple",width=12,
                           
                           h4("This app is meant to predict the price of a product based on certian factor specified by the user. The reason for this is because when chosing a new cosmeic product to try can be very daunting. Not only are you gambling on something that might cost you a lot of money and it might not work, but you might also be taking a chance on your complection being ruined due to an allergic reaction or buying a product that is not compatible with your skin type. This app will aid in the selection of a product that will fit your budget, or atleast let you know how much you should expect to spend, with all the specs you're looking for."),
                           
                           h4("The app has several features, which allows the user to select certain varibles, subset the data, and do some data exploration. The app also allows the user to create numerical and graphical summmaries, as well as create different predictive models with the data provided. The predictive models used in this app will be (i) multiple linear regression, (ii) regression tree, and (iii) random forest. For the predictive models, the response variable of interest is Price, and the other explanitory variables from the data set will be used to predict the price of the cosmetic product."), 
                           
                           h4("Finally, we will compare these three models to see, which model has the best predicitive ability. This is assessed by the model with the lowest root mean squared error (RMSE)."),
                           
                           br(), #break
                           
                           h4("Below is the purpose of each tab/page in the app."), 
                           
                           h4("[The About Page]: This page describes the purpose of the app, some information about the data and its source (links to the dataset and additional information), a summary of what each tab/page contains, and it also includes a picture that is related to the data."),
                           
                           h4("[The Data Exploration Page]: This page allows users to filter and select certain variables in the data from a dropdown menu, radio buttons, and enter in free text in order to creates numerical and graphical summaries. Users can also play around with the various types of plots available (changes the type of plot shown) and the type of summary reported. Lastly, users can change the variables and filter the rows to change the data used in the plots/summaries. The page is meant to make it easy for users to explore and better understand the data available in the app."),
                           
                           h4("[The Modeling Page]: This page fits the three supervised learning models by offering a range of tools. These models are (i) multiple linear regression, (ii) regression tree, and (iii) random forest. 
                           
                           The Model Info Tab is a sub tabs that provide in depth information about each model, the pros and cons of each model, and some mathematical expression. 
                           
                           The Model Fitting Tab allows the user to also be able to pick how they would like to split their training and test data set, select what explanitory variables that they would like to include in their model, fit the models, and compare the models.
                           
                           The Prediction Tab allows the user to to selects the which model predicts the best. 
"),
                           
                           h4("[The Data Page]: This page allows for scrolling through the data set, subsetting this data set (rows and columns), and saving the (possibly subsetted) data as a .csv file.")
                       )  #end of box() function -- line 65
                ), #end of column() function -- line 61
                
                column(6,
                       #Data Details
                       h1("About The Data"),
                       #box to contain description
                       box(background="purple",width=12,
                           h4("This app uses data from a website called Kaggle, which is described as 'Your Machine Learning and Data Science Community'. It offers a no-setup, customizable, Jupyter Notebooks environment and access GPUs at no cost to you and a huge repository of community published data. For our purposes, I downloaded a data set called *cosmetics.csv*, which can be found", a(href = "https://www.kaggle.com/datasets/kingabzpro/cosmetics-datasets", "here"), " -- on the top right corner. This data set includes data from", a(href = "https://en.wikipedia.org/wiki/Sephora", "Sephora"), " and contains variables such as the type of cosmetic category, brand, product name, an ingredients list, rating, price, and the category of skin that these products are meant for (combinaiton, dry, normal, oily, sensitive)."),
                           
                           br(), #break
                           
                           h4("This data set consists of 11 variables (Label, Brand, Name, Price, Rank, Ingredients, Combination, Dry, Normal, Oily, Sensitive) and 1472 observations."),
                           
                           h4("> Label = Type of product"),   
                           h4("> Brand = Brand of product"),
                           h4("> Name = Name of Cosmetic"),
                           h4("> Price = Price in USD"),
                           h4("> Rank = Ranking"),
                           h4("> Ingredients = Ingredients"),  
                           h4("> Combination = Combination of Dry and oily"),
                           h4("> Dry = For Dry Skin"),
                           h4("> Normal = For Normal Skin "),
                           h4("> Oily = For Oily Skin"), 
                           h4("> Sensitive = For Sensitive Skin"),
                           
                       ) #end of box() function
                ) #end of column() function 
              ) #end of fluidRow() function -- line 57
      ), #end of tabItem() function 
      

      ########## Second tab content
      #actual app layout      
      tabItem(tabName = "data-exploration",
              
              fluidRow( #summary of all vars - options 
                # Summary of Cosmetic Data Set Variables
                h2("Summary of Cosmetic Data Set Variables"),
                #also var i created in the data available
                box(width=12,background="blue",
                    selectizeInput(inputId="summary_select", 
                                   label="Summary of Cosmetic Data Set Variables",
                                   selected = "Price", #default value
                                   choices = colnames(cosmetics), 
                                   multiple = FALSE )
                    
                ), #end of box()
                
                #outputting the data summary
                mainPanel(
                  verbatimTextOutput("data_summaries") )
                
              ), #end of fluidRow() function
                
                br(), #break

                
                fluidRow(
                # Serach Product by Ingredient
                h2("Serach Product by Ingredient"),
                #text finding in ingredients for products
                box(width=12, background="blue", 
                    textInput(inputId="find_ingredient", 
                              label="Serach Product by Ingredient",
                              value = "SUL(F|PH)(UR|ATE|A)"),
                    checkboxInput(inputId = "TF_ingredient_exclude",
                                  label = "Exclude Ingredient From Product?",
                                  value = FALSE ) #unchecked default
                    
                ), # end of box() function
                
                #outputting results for incredients in produts
                mainPanel(
                  #verbatimTextOutput("data_summaries"), 
                  verbatimTextOutput("inc_exc_ingredient")
                  #plotOutput("sleepPlot"),
                  #textOutput("info"),
                  #tableOutput("table")
                ) #end of mainPanel() function
                
              ), #end of fluidRow() function
              
              br(), #break
              
              #charts and graphs - row names
              fluidRow( column(width=6 , #12 width is the max
                               # Contingency Table
                               h2("Contingency Table"),
                               box(width = 12, background = "blue", 
                                   #row var
                                   selectizeInput(inputId="contingency_table_row", 
                                                  label="Select Row Variable",
                                                  selected = "Brand", #default value
                                                  choices = # If you want to get both factors 
                                                    #and characters use
                                                    colnames(cosmetics[, sapply(cosmetics, class) %in% 
                                                                         c('character', 'factor')]),   
                                                  multiple = FALSE ),           
                                   
                                   #row var
                                   selectizeInput(inputId="contingency_table_col", 
                                                  label="Select Column Variable",
                                                  selected = "Label", #default value
                                                  choices = # If you want to get both factors 
                                                    #and characters use
                                                    colnames(cosmetics[, sapply(cosmetics, class) %in%    
                                                                         c('character', 'factor')]),     
                                                  multiple = FALSE ), 
                                   #options = list(maxItems = 2
                                   
                               ) #end of box()
                               
                               #output contingency table
                               
              ), #end of column() function
              
              #outputting contingency tables
              mainPanel(
                dataTableOutput("contingency_table")
                #plotOutput("correlation"), 
                
              ) #end of mainPanel() function
              
              ), #end of fluidRow() function
              
              br(), #add break in app output

              
              fluidRow( 
              # Contingency Table
              h2("Correlation Plot"),
              #outputting contingency tables
              mainPanel(
                plotOutput("correlation")
                
              ), #end of mainPanel() function
              ), #end of fluidRow() function
              
              br(), #add break in app output

              fluidRow( 
                # Bar Table
                h2("Bar Plots"),
                #outputting bar charts
                mainPanel(
                  plotOutput("plot_freq_product")
                ), #end of mainPanel() function
                
                #inputs for my box plots that are separated by skin type
                #also used for avg price plot 
                selectizeInput(inputId="bar_plot_skintype", 
                               label="Select Skin Type Variable",
                               selected = "combination_fctr", #default value
                               choices = c("combination_fctr", "dry_fctr", "normal_fctr", 
                                           "oily_fctr", "sensitive_fctr"),
                               multiple = FALSE ), 
                #for the response variable = y = Price or Rank
                selectizeInput(inputId="bar_plot_response", 
                               label="Select Response Variable",
                               selected = "Price", #default value
                               choices = c("Price", "Rank"),
                               multiple = FALSE ), 
                
                #outputting freq tables
                mainPanel(
                  plotOutput("plot_freq_skintype"), 
                  plotOutput("plot_avgprice_skintype")
                  #tableOutput("debug")
                ), #end of mainPanel() function
                
                
              ), #end of fluidRow() function 
              
              br(), #break
              
              fluidRow( 
                # Scatter Table
                h2("Scatter Plots"),
                #outputting scatter plots - all product types
                #mainPanel(
                #plotOutput("plot_scatter_all")
                #), #end of mainPanel() function
                
                #scatter plot of Price vs. Rank -- possibility of subset of type of products
                selectizeInput(inputId="scatter_plot_sub", 
                               label="Select Subset Variable",
                               selected = "ALL", #default value
                               choices = c("ALL", "Moisturizer", "Cleanser", "Treatment",
                                           "Face Mask", "Eye cream", "Sun protect"),
                               multiple = FALSE ),
                
                checkboxInput(inputId="scatter_plot_chkbox",
                              label="Remove Rank Equal to 0",
                              value = FALSE
                ),
                
                checkboxInput(inputId="toggle_axis",
                              label="Fit The Plot by Zooming In",
                              value = FALSE
                ),
                
                #outputting scatter plots - subset by type of product
                mainPanel(
                  plotOutput("plot_scatter_subs"),
                  #verbatimTextOutput("test")
                ) #end of mainPanel() function
                
              ), #end of fluidRow() function      
              
              br(), #break
              
              fluidRow(
                # Box Table
                h2("Box Plots"),
                #might move into another fluid row
                selectizeInput(inputId="scatter_box_sub", 
                               label="Select Subset Variable",
                               selected = "ALL", #default value
                               choices = c("ALL", "combination_fctr", "dry_fctr", "normal_fctr",
                                           "oily_fctr", "sensitive_fctr"),
                               multiple = FALSE ),
                
                #outputting scatter plots - subset by type of product
                mainPanel(
                  plotOutput("scatter_box_sub")
                ) #end of mainPanel() function
                
              ), #end of fluidRow() function      
              
      ), #end of tabItem() function -- item 2
      
      
      
      
      
      ########## Third tab content
      #modeling -- IS THERE SUPPOSED TO BE SOMETHING ON THIS PAGE?!?!?!
      tabItem(tabName = "modeling",
              fluidRow(
                column(width=3,
                       box(width=12,background="blue",sliderInput("yvalue","Y=Number of Modeling",min = 0,max = 30,value = 15)
                       )
                ),
                
              )
      ),  # end of item 3 - modeling  
      
      ########## 3.1 sub tab content
      #modeling-info    
      tabItem(tabName = "modeling-info",
              fluidRow(
                column(width=3,
                       box(width=12,background="blue",sliderInput("yvalue","Y=Number of Modeling Info",min = 0,max = 30,value = 15)
                       )
                ),
                
              )
      ),   #end of item 3.1 - modeling-info  
      
      ########## 3.2 sub tab content
      #model-fitting    
      tabItem(tabName = "model-fitting",
              fluidRow(
                column(width=3,
                       box(width=12,background="blue",sliderInput("yvalue","Y=Number of Model Fitting",min = 0,max = 30,value = 15)
                       )
                ),
                
              )
      ),   #end of item 3.2 - model-fitting  
      
      ########## 3.3 sub tab content
      #prediction    
      tabItem(tabName = "prediction",
              fluidRow(
                column(width=3,
                       box(width=12,background="blue",sliderInput("yvalue","Y=Number of Prediction",min = 0,max = 30,value = 15)
                       )
                ),
                
              )
      ),   #end of item 3.3 - prediction                          
      
      ########## 4 tab content
      #data    
      tabItem(tabName = "data",
              fluidPage(sidebarPanel(width = 2, 
                                     checkboxGroupInput("display_vars", 
                                                        "Select Variables to Display",
                                                        choices = colnames(cosmetics), 
                                                        selected = setdiff(colnames(cosmetics),
                                                                           "Ingredients") ) , 
                                     numericInput("numRows",
                                                  "Number of Rows to Filter On:",
                                                  value =  5, min = 1, max = 1472),
                                     downloadButton("data_download", "Download Data (.csv)") ), 
                        mainPanel( width = 10, 
                                   DTOutput("data_table") ),
              ), #end of fluidPage() function
      ) #end of item 4 - data 
      
      

      
    ) #end of tabItems() function
  ) #end of dashboardBody() function
) # end of the dashboardPage() function
  
  
  

####################################### SERVER ################################################
  
# Define server logic required to draw the plots
server <- shinyServer(function(input, output, session) {
  
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
  
  #bar charts
  #Freq of Type of cosmetics
  output$plot_freq_product <- renderPlot({
    
    ggplot(data = cosmetics, aes(x = Label)) + 
      geom_bar(aes(fill= Label), show.legend = FALSE) + 
      ggtitle("Type of Cosmetics Available in Sephora Data") +
      labs(x="Type of Cosmetic", y="Frequency", fill="Type of Cosmetic")
    
  })
  
  #bar charts
  #Type of Cosmetics Available in Sephora Data for Combination Skin
  output$plot_freq_skintype <- renderPlot({
    
    x = switch(input$bar_plot_skintype, combination_fctr = "Combination Skin", 
               dry_fctr = "Dry Skin",
               normal_fctr = "Normal Skin",
               oily_fctr = "Oily Skin",
               sensitive_fctr = "Sensitive Skin")
    
    ggplot(data = cosmetics, aes(x = Label)) + 
      geom_bar(aes(fill= .data[[input$bar_plot_skintype]]), position="dodge") + 
      ggtitle(paste("Type of Cosmetics Available in Sephora Data for", x)) +
      labs(x="Type of Cosmetics", y="Frequency", fill=x)
    
  })
  
  #bar plots by average PRICE!
  #Type of cosmetic vs. Average Price of each type of cosmetic - Combination
  output$plot_avgprice_skintype <- renderPlot({
    
    x = switch(input$bar_plot_skintype, combination_fctr = "Combination Skin", 
               dry_fctr = "Dry Skin",
               normal_fctr = "Normal Skin",
               oily_fctr = "Oily Skin",
               sensitive_fctr = "Sensitive Skin")    
    
    d <- cosmetics %>%
      group_by(Label, get(input$bar_plot_skintype)) %>%
      summarize(avg=mean(get(input$bar_plot_response)) ) %>% 
      mutate(avg_fmt = paste0(ifelse( input$bar_plot_response == "Price" ,"$", ""), 
                              sprintf("%0.2f", round(avg, 2)) )) %>%
      rename( skintype  = `get(input$bar_plot_skintype)` )
    
    ggplot(data = d , 
           aes(x = Label, y=avg, group = skintype )) +  
      geom_col(aes(fill= skintype), position="dodge") + 
      ggtitle(paste("Type of Cosmetics Available vs. Average", input$bar_plot_response, 
                    "in Sephora Data for", x )) +
      labs(x="Type of Cosmetics", y=paste("Average",input$bar_plot_response ) , fill = x) +
      geom_label( aes(label = avg_fmt), position=position_dodge(width=0.9), size=rel(4) ) 
    
  })
  
  #bar plots by average PRICE! -- debuggingggg!
  #Type of cosmetic vs. Average Price of each type of cosmetic - Combination
  # output$debug <- renderTable({
  #   
  #     cosmetics %>%
  #     group_by(Label, get(input$bar_plot_skintype)) %>%
  #     summarize(avg_price=mean(Price) ) %>% # , .groups= "keep"
  #     mutate(avg_price_fmt = paste0("$", sprintf("%.2f", round(avg_price, 2)) )) %>%
  #     rename( skintype  = `get(input$bar_plot_skintype)` )
  #   
  # })    
  
  #scatter plots
  #everything
  # output$plot_scatter_all <- renderPlot({
  #   
  # #setting the stage to add layers - WHY DONT BOTH GRAPHS APPEAR?
  # g <- ggplot(cosmetics, aes(x = Rank, y = Price))
  # #scatter plot with linear regression line for width by weight
  # g + geom_point() +
  #   labs(title = "Rank vs. Price for All Types of Products") +
  #   geom_smooth(method = lm)
  # 
  # })
  
  #scatter plots
  #by type of product
  output$plot_scatter_subs <- renderPlot({
    
    #reformat output text
    y = switch(input$scatter_plot_sub,  "ALL"         = "All",
               "Moisturizer" = "Moisturizer", 
               "Cleanser"    = "Cleanser",
               "Treatment"   = "Treatment",
               "Face Mask"   = "Face Mask",
               "Eye cream"   = "Eye Cream",
               "Sun protect" = "Sun Protection" ) 
    
    #temp var 
    ALL <- c("Moisturizer", "Cleanser", "Treatment", "Face Mask", "Eye cream", "Sun protect")
    scatter_plot <- ifelse(input$scatter_plot_sub == "ALL", ALL, input$scatter_plot_sub)
    
    if (input$scatter_plot_sub == "ALL") { 
      
      data <- cosmetics } else { 
        
        data <- cosmetics %>% 
          filter(Label %in% scatter_plot)
      } 
    
    if (input$scatter_plot_chkbox == TRUE) {data <- data %>%
      filter (Rank != 0) }
    
    #Moisturizer
    #setting the stage to add layers
    g <- ggplot(data ,  #drop outliers... 
                # , Rank != 0 (need radio button? --  for both scatter plots...)
                aes(x = Rank, y = Price))
    #scatter plot with linear regression line
    p <- g + geom_point() +
      labs(title = paste("Rank vs. Price for", y, "Type of Products" )) +
      geom_smooth(method = lm) 
    # + xlim(0, 5)
    
    #chose to zoom into axis or not
    if (input$toggle_axis == TRUE) { 
      p } else {
        p + xlim(0, 5) 
      }
    
  })
  
  #Price - all
  #A scatter plot with boxplots
  output$scatter_box_sub <- renderPlot({
    
    x = switch(input$scatter_box_sub,               ALL  = "All Skin",
               combination_fctr = "Combination Skin", 
               dry_fctr = "Dry Skin",
               normal_fctr = "Normal Skin",
               oily_fctr = "Oily Skin",
               sensitive_fctr = "Sensitive Skin") 
    
    if (input$scatter_box_sub == "ALL") { 
      
      data <- cosmetics } else { 
        
        data <- cosmetics %>% 
          filter( get(input$scatter_box_sub) == "Yes" )
      } 
    
    #filtering for combination
    ggplot(data=data,
           aes(y=Price, x=Label)) + 
      labs(title = paste("Type of Products for", x, "vs. Price Broken Down By Sensitivity"), 
           x="Types of Skin Products", 
           y="Price") +
      geom_boxplot() + 
      geom_point(aes(shape = sensitive_fctr , color=Label), position="jitter", alpha=0.6, show.legend = c("shape"=TRUE, "color"=FALSE)) 
    
  })
  #debug
  #  output$test <- renderText({
  #    
  #  #temp var 
  #  ALL <- c("Moisturizer", "Cleanser", "Treatment", "Face Mask", "Eye cream", "Sun protect")
  # ifelse(input$scatter_plot_sub == "ALL", ALL, input$scatter_plot_sub)
  #  
  #  })
  
  ##########################################################################
  
  #data download
  #created reactive data
  d <- reactive( {cosmetics %>% 
      select(all_of(input$display_vars)) %>%
      slice(1:input$numRows) %>%
      as_tibble ( ) %>% 
      `[`( input$display_vars )
  } )
  
  #output for data table that we will display and let user interact with
  output$data_table <-renderDT( { d( ) %>%
      datatable ( options = list ( lengthMenu = list (c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All") ) ) )
    
  } )
  
  #downloading the data
  output$data_download <- downloadHandler( filename = function() { paste0( "cosmetics_", Sys.Date(), ".csv" ) }, content = function(file) { if(length(input$data_table_rows_selected) == 0) {write.csv(d(), file, row.names = FALSE) }else { write.csv( d()[input$data_table_rows_selected, ], file, row.names = FALSE ) } })
  
  
}) # end of the server function 

# Create Shiny Object
shinyApp(ui = ui, server = server)

#downloading // installing packages all in one...
#install.packages( c("shiny", "shinydashboard", "tidyverse", "corrplot", "ggplot2", "caret", "DT", "tree", "randomForest" ) )

#must run in the colsole
#can only be ran after you commit; otherwise, use the run app button on the top right
#shiny::runGitHub("ST558_Final_Project","magarittenguyen")
#shiny::runGitHub(repo="ST558_Final_Project",username="magarittenguyen")

#repo link
#https://github.com/magarittenguyen/ST558_Final_Project

