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
              
              fluidRow(
                box(background="purple",width=12,
              
              h4(
                "This app uses the",
                a(href = "https://www.kaggle.com/datasets/kingabzpro/cosmetics-datasets",
                  "Cosmetics dataset"),
                "provided on",
                a(href = "https://www.kaggle.com", "Kaggle.com"),
                "by the user",
                a(href = "https://www.kaggle.com/kingabzpro", "Abid Ali Awan", .noWS = "after"),
                "." ) 
              )
              
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
              
              br(), br(),
              
              fluidRow(
                #add in latex functionality if needed
                withMathJax(),
                
                #two columns for each of the two items
                column(6,
                       #box to contain description
                       box(background="purple",width=12,
                           
                           # Describe the purpose of the app
                           h1("Purpose of The App"),
                           br(),
                           
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
                       #box to contain description
                       box(background="purple",width=12,
                           #Data Details
                           h1("About The Data"),
                           br(), 
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
                box(width=12,background="green",
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
                box(width=12, background="green", 
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
                               box(width = 12, background = "green", 
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
              fluidPage(
                h4("testing...doesn't seem to have text here...")
              ),
      ),  # end of item 3 - modeling  
      
      ########## 3.1 sub tab content
      #modeling-info    
      tabItem(tabName = "modeling-info",
              fluidPage(
                #do this 3x
                #You should explain:
                #these three modeling approaches, 
                #the benefits of each, 
                #the drawbacks of each. 
                #You should include some type of math type in the explanation (you’ll need to include mathJax)
                
                box(background="blue",width=12,
                h4("This subsection tab/page has information on the three modeling approaches -- (i) multiple linear regression, (ii) regression tree, and (iii) random forest -- as well as benefits and drawbacks of each model used in this Cosmetics App.")
                ),
                
                br(), 
                
                ########## MLR
                column(width=4,
                       box(width=12,background="blue",
                           h3(strong("Multiple Linear Regression")),
                           
                           br(), 
                           h4("Multiple Linear Regression is a supervised learning statistical method that is an extention of the Simple Linear Regression Model and is used to examine the relationship between varibles and predict (Yi/response/dependent) the outcome of a variable based on the value of two or more (Xi/explanitory/indepentent) variables. This is done by minimizing the sum of squared residuals. The MLR can also include higher order terms. The easy interpretability of the linear model makes it widely used in the field of Statistics and Data Analysis."),
                           
                              br(),
                           
                              h4("Simple Linear Regression Formula:"),
                              
                              br(),
                           
                              h4(HTML("Yi = β0 + β1xi + Ei"), 
                              style="text-align:center"),
                              
                              br(),
                           
                              h4("Minimizing The Sum of Squares:"),
                              
                              br(),
                              
                              h4(HTML("min [ SUM (yi - β0 + β1xi)^2 ]"), # <b>Description</b>
                              style="text-align:center"),
                              
                              br(),
                              
h4("NOTE:",
br(),
" - Yi is the response variable for the i_th obervation",
br(),
" - Xi is the explanitory variable for the i_th obervation",
br(),
" - β1 is the y-intercept",
br(),
" - β0 is the slope", 
br(),
" - Ei is the error term",
                              
                           br(),br(),
), 
                           ########## MLR PROS
                           h4("PROS:", 
                              
                              br(), 
                              " >> The simplest equation used, which the relationship between the multiple predictor variables and predicted variable can be expressed.", 
                              br(),
                              " >> The modeling speed is efficient because it does not require complicated calculations and runs predictions fast when the amount of data is large; hence, it works well on any size data set.",
                              br(), 
                              " >> The ability interpret the output and to determine the relative influence of one or more explanitory variables to the response variable",
                              br(), 
                              " >> The ability to identify outliers or anomalies"),

                           br(), 
                           ########## MLR CONS
                           h4("CONS:", 
                              
                              br(),
                              " >> Often times complex data sets can lead to false conclusions if they aren't analyzed properly due to being too simplistic of a model to capture real world complexity." , 
                              br(),
                              " >> The output can be severly affected by outliers due to the line of best fit trying to minimize the MSE, resulting in a model that might not be able to capture the information inthe data",
                              br(),
                              " >> Assumes independence of variables -- that the predictor variables are not correlated which is rarely true",
                              br(),
                              " >> Falsely concludes that a correlation is causation", 
                              br(),
                              " >> The model assumes homoskedacity, which assumes a constant variance around the mean and this is unrealistic in most cases"),
                              br(),
                       )
                ),
                
                ########## RT
                column(width=4,
                       box(width=12,background="blue",
                           h3(strong("Regression Tree")),
                           
                           br(), 
                           h4("A Regression Tree, a variant of decision trees, is a statistical method thats goal is to predict a continuous response for a given distinct and non-overlapping region (predictor space), which usually uses the mean of observations as the prediction.", 
                           br(), br(),
                           "A tree is built by splitting the source set. Once that split is chosen, the same process is used to create the second split and so on. Generally this method grows a 'large' tree (many nodes). These trees are then usually pruned back using cost-complexity pruning. This helps with not overfitting the data. Although, pruning back increases bias, it decreases the variance; therefore, hopefully, improving prediciton. Generally, we can choose the number of nodes using the training/test set and compare or use a method like cross-validation to predict.",
                           br(), br(),
                           "Regression trees are used when the dependent variable is continuous whereas the classification tree is used when the dependent variable is categorical. Random Forests is one method that average across trees. It loseses interpetability, but gains in prediction!" 
                              ), 

                           br(), 
                           ########## RT PROS
                           h4("PROS:", 
                              
                              br(), 
                              " >> Simple to understand and very interpretable output -- easy to interpret, understand, and visualize ",
                              br(),
                              " >> Predictors don't need to be scaled", 
                              br(),
                              " >> Data preparation during pre-processing in a decision tree requires less effort and does not require normalization of data",
                              br(),
                              " >> No statistical assumptions necessary",
                              br(),
                              " >> Built in variable selection",
                              br(),
                              " >> Not largely influenced by outliers or missing values",
                              br(),
                              " >> Quick ways to identify relationships between variables and the most significant variable",

                           ),

                           br(), 
                           ########### RT CONS
                           h4("CONS:", 
                              
                              br(), 
                              " >> Small changes in data can vastly change tree structure, which causes instability",
                              br(),
                              " >> Greedy algorithm necessary (no optimal algorithm)",
                              br(),
                              " >> Need to prune (usually)",
                              br(),
                              " >> Smaller data sets provide poorer results due to the high variance single regression trees have; thus, poor/unstable predictive accuracy",
                              br(),
                              " >> Overfitting can easily occur (issue can be resolved by pruning and setting constraints on the model parameters)",
                              br(),
                              " >> Relatively expensive in comparison to other algorithms because the calculations involved can become complex and take longer to train the model", 
                              )
                       )
                ),
                
                ########## RF
                column(width=4,
                       box(width=12,background="blue",
                           h3(strong("Random Forest")),
                           
                           br(),
                           h4("A Random Forest is a statistical ensemble method which combines the creation of multiple decision trees, from bootstrap samples, to reach a single averaged result. This is an extension of the bagging method to create an uncorrelated forest of decision trees and is generally more accurate than just bagging itself. Bagged trees are created using bootstrap aggregation.",
                           br(), br(),
                           "The randomn part of this model generates a random subset of predictors for each bootstrap sample/tree fit, which ensures low correlation among decision trees (so, a good predictor or two won't dominate the tree fits). The key difference between decision trees and random forests is that, while decision trees consider all the possible feature splits, random forests only select a subset of those features. The reason why this is done is because if there is a situation where a really strong predictor exists, every bootstrap tree will probably use it for the first split. This makes the bagged trees predictions more correlated.",
                           br(), br(),
                           "NOTE:",
                           br(), 
                           "The process of bagged trees / random forest treats the sample as if it is the population.", 
                           br(), br(), 
                           " - creating the bootstrap sample (same size with replacement)",
                           br(),
                           " - training the tree on this sample (no pruning necessary)",
                           br(),
                           " - repeating the process a large number of times (e.g. B = 1000 times)",
                           br(),
                           " - the final prediction is the average of all these predictions for the regression trees model", 
                           
                           br(),br(),

"Finding the average of predictions decreases variance, which improves predictions, but unfortunately we lose interpretability."), 

                              br(), 

                           ########## RF PROS
                           h4("PROS:",
                              
                              br(),
                              " >> Provides flexibility because it can be used for both regression and classification tasks",
                              br(),
                              " >> Less susceptible to overfitting in decision trees than many other types of models and helps to improve the accuracy since the averaging of uncorrelated trees lowers the overall variance and prediction error",
                              br(),
                              " >> Easy to evaluate variable importance or contribution, to the model  ",
                              br(),
                              "  >> The trees created are not correlated", 
                              br(),
                              "  >> Not easily influenced by outliers or influential points",
                              br(),
                              "  >> The bias variance trade-off well is good"

                                                            ),
                           br(), 
                           ########## RF CONS
                           h4("CONS:", 
                              
                              br(), 
                              "  >> By doing this method, it loses interpretability due to complexity",
                              br(),
                              "  >> Time-consuming process due to requiring much computational power for each individual decision tree and resources to store data",
                              br(),
                              " >> May not always perform as well as other types of models on very very large datasets" ),
                           
                       )
                ),
                
              )
      ),   #end of item 3.1 - modeling-info  
      
      ########## 3.2 sub tab content
      #model-fitting    
      tabItem(tabName = "model-fitting",
              fluidPage(
                column(width=12,
                       
                       box(width=12,background="blue",
                           #set seed
                           numericInput(inputId = "setseed",
                                        label = "Set Seed for Reproducibility",
                                        value =  123, 
                                        min = 1, 
                                        max = 1000)
                           
                       ),
                       
                        box(width=12,background="blue",
                           # You’ll split your data into a training and test set. 
                           #Give the user the ability to choose the proportion of data used in each
                           sliderInput(inputId = "partition",
                                       label = "Partition of Train and Test Dataset, Respectively",
                                       min = 0.01,
                                       max = 0.99,
                                       value = 0.70), 
                           
                           #outputting scatter plots - subset by type of product
                           mainPanel(
                             #output text 
                             verbatimTextOutput("traintest_obs")
                           ) #end of mainPanel() function
                           
                       ),
                       
                        box(width=12,background="blue",
                           # numbers for model repeats
                           sliderInput("cvnum",
                                       "Number for CV",
                                       min = 0,
                                       max = 30,
                                       value = 5),
                           sliderInput("numrep",
                                       "Number of Reps",
                                       min = 0,
                                       max = 30,
                                       value = 3)
                       )
                ),
                
              )
      ),   #end of item 3.2 - model-fitting  
      
      ########## 3.3 sub tab content
      #prediction    
      tabItem(tabName = "prediction",
              fluidPage(

                br(),
                box(background="orange",width=12,
                h4("RMSE is a metric that tells us how far apart the predicted values are from the observed values in a dataset, on average. The lower the RMSE, the better a model fits a dataset."),
                br(),
                h4("As for the R^2 value we have (R-squared), this is a metric that indicates the proportion of the variance in the response variable of a regression model that can be explained by the predictor variables. The higher the R^2 value, the better a model fits a dataset. This value ranges from 0 to 1."),
                br(),
                h4("It should be noted that our best model is selected from a model that has results reflecting the lowest RMSE and the highest R^2 value. But, there are times when the results from the lowest RMSE and highest R^2 are not from the same model; therefore, we will use RMSE to pick our final winner.")
                ),  
                
                #outputting scatter plots - subset by type of product
                mainPanel(
                  #output text 
                  #verbatimTextOutput("predicting") #not showing up bc data not working and need input$xxx and need TrainTest()$xxx
                ) #end of mainPanel() function    
                
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
                                     
                                     downloadButton("data_download", "Download Data") ),
                        
                        mainPanel( width = 10, 
                                   DTOutput("data_table") ),
              ), #end of fluidPage() function
      ) #end of item 4 - data 
      
      

      
    ) #end of tabItems() function
  ) #end of dashboardBody() function
) # end of the dashboardPage() function
  
  
  

####################################### SERVER ################################################
  
########### Data Exploration 
  
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
  
  ########## Modeling 
  
  #Train and Test Data
  TrainTest <- reactive({
    
    # splitting into train and test data  
    #seed is set for reproducibility 
    set.seed(input$setseed)
    
    #indices to split on
    CosmeticsIndex <- createDataPartition(cosmetics$Price, p = input$partition, list = FALSE)
    
    #subset
    CosmeticsTrain <- cosmetics[ CosmeticsIndex, ]
    CosmeticsTest  <- cosmetics[-CosmeticsIndex, ]
    
    list(train = CosmeticsTrain , test = CosmeticsTest )
    
  })
  
  output$traintest_obs <- renderText({
    
    numsout <- sapply(TrainTest(), nrow )
    
    #try to output numbers with labesl...
    # numsout_tib <- as_tibble(numsout) 
    # 
    # test <-  numsout_tib %>%
    #            mutate(count1 = paste0( "Train = ", numsout_tib$value[1]),
    #                   count2 = paste0( "Test = " , numsout_tib$value[2]) )
    # 
    # final <- test[1,] %>%
    #   select(count1, count2)
    # 
    # final

    #names(numsout) <- c("Train", "Test")
    #numsout
    
  })
  
  ######################  MODELING  ########################
  
  #[[[ multiple linear regression ]]] 
  #training
  MLR_train <- reactive({
    
  #linear regression model 1
  MLR <- train(Price  ~ Rank +  Label + sensitive_fctr + I(Rank^2) + Rank*sensitive_fctr,
               data = TrainTest()$train, 
               method = "lm",
               preProcess = c("center", "scale"),
               trControl = trainControl(method= "cv", number = input$cvnum))
  
  #calling the l_m1 object
  MLR
  
  #summary statistics
  summary(MLR)
  
  })
  
  
  #[[[ regression tree ]]] 
  #training
  RT_train <- reactive({
  #Basic Tree Fit - Notes 3 Slide 199
  #rpart is also often used
  
  #factor predictors must have at most 32 levels
  treeFit <- tree(Price ~ Rank + label_fctr + combination_fctr+ sensitive_fctr , data = TrainTest()$train)
  plot(treeFit)
  text(treeFit) #first optimal split is 4.85
  
  cvTree <- cv.tree(treeFit)
  cvTree
  
  plot(cvTree$size ,cvTree$dev ,type="b")
  
  
  # RT_train <- reactive({
  #   
  #   RT <- train(Price ~ ., data = TrainTest()$train,
  #               method = "rpart",
  #               preProcess = c("center", "scale"),
  #               trControl = trainControl(method = "cv", number = 5)) #$input$cvnum
  #   RT
  #
  # })
  
  #classification_tree_fit <- train(Price ~ Rank + label_fctr + combination_fctr+ sensitive_fctr, 
  #                                 data = CosmeticsTrain, 
  #                                 method = "rpart",
  #                                 trControl=trctrl,
  #                                 preProcess = c("center", "scale"),
  #                                 tuneGrid = data.frame(cp=seq(0, 400, 10)) )
    
  })
  
  
  #[[[ random forest model ]]
  #training
  RF_train <- reactive({
    
    #Random Forest Model
    RF <- train(Price ~ Rank +  Label + sensitive_fctr ,
                data = TrainTest()$train, 
                method = "rf",
                trControl=trainControl(method = "cv", number = input$cvnum), 
                preProcess = c("center", "scale"),
                tuneGrid = data.frame(mtry = 1:3))
    
    #calling r_f object
    RF
    
  })
  
  
  #######################  PREDICTING #######################
  
  output$predicting <- reactive({
  
  #MLR
  #predicting on the ChannelTest data with linear regression model 1
  test_pred_MLR <- predict(MLR_train(), newdata = TrainTest()$test) 
  
  #best model chosen - RMSE reported explicitly0.
  #goal is to compare RMSE and see which one is the lowest!
  m1 <- postResample(test_pred_MLR, TrainTest()$test$Price) 
  #output object m1
  m1
  
  
  #RT
  trctrl <- trainControl(method = "repeatedcv", number = input$cvnum, repeats = input$numrep)  
  #Predictions using predict
  pred <- predict(RT_train(), newdata = dplyr::select(TrainTest()$test, -Price)) 
  #Root MSE
  TR_RMSE <- sqrt(mean((pred-TrainTest()$train$Price)^2)) 
  TR_RMSEs
  
  #RF
  #best model chosen - RMSE reported explicitly
  #goal is to compare RMSE and see which one is the lowest!
  test_pred_RF <- predict(RF_train(), newdata = TrainTest()$test) 
  m3 <- postResample(test_pred_RF, TrainTest()$test$Price) 
  #calling m3 object
  m3
  
  #Comparison
  #creating tibble with RMSE and R-squared values
  MLR <- tibble(model = c("Multiple Linear Regression"), RMSE = c(m1[[1]]), R2 = c(m1[[2]]))
  
  RT <- tibble(model = c("Regression Tree"), RMSE = c(TR_RMSE), R2 = NA )
  
  RF <- tibble(model = c("Random Forest"), RMSE = c(m3[[1]]), R2 = c(m3[[2]]))
  
  #creating tibble for all results for the 4 models
  RMSE_table <- rbind(MLR, RT, RF)
  RMSE_table
  
  
  #pick the smallest RMSE for the best model
  final_result <- RMSE_table %>%
    filter ( min(RMSE) == RMSE )
  
  final_result
  
  })
  
  
  ########### Data  
  
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

