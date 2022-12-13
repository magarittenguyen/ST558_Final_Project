ST558 Final Project - Cosmetics App
================
Magaritte Nguyen  
2022-12-02

<!-- 
Your README.md file should have the following things (you can create this in R markdown or just update the .md file that exists):

• Brief description of the app and its purpose.
• A list of packages needed to run the app.
• A line of code that would install all the packages used (so we can easily grab that and run it prior to
running your app).
• The shiny::runGitHub() code that we can copy and paste into RStudio to run your app.

You do not need to use github pages for this project (unless you want to).
 -->

<!-- TOC -->

# Table of Contents

-   <a href="#table-of-contents" id="toc-table-of-contents">Table of Contents</a>
-   <a href="#description-and-purpose" id="toc-description-and-purpose">Description and Purpose</a>
-   <a href="#required-packages" id="toc-required-packages">Required Packages</a>
-   <a href="#shiny-app-run-code" id="toc-shiny-app-run-code">Shiny App Run Code</a> 

<!--  Brief description of the app and its purpose  -->

# Description and Purpose

The purpose of this application project is for the solo efforts of Magaritte Nguyen on the NC State ST558 Final Project, Fall 2022.

The goal of this project is to create a nice looking shiny app that can be used to explore data and model it! We were also told that we get to chose our own data that was of interest to us. 

This app uses data from a website called Kaggle, which is described as "Your Machine Learning and Data Science Community". It offers a no-setup, customizable, Jupyter Notebooks environment and access GPUs at no cost to you and a huge repository of community published data. For our purposes, I downloaded a data set called *cosmetics.csv*, which can be found [here](https://www.kaggle.com/datasets/kingabzpro/cosmetics-datasets) -- on the top right corner. This data set includes data from [Sephora](https://en.wikipedia.org/wiki/Sephora) and contains variables such as the type of cosmetic category, brand, product name, an ingredients list, rating, price, and the category of skin that these products are meant for (combinaiton, dry, normal, oily, sensitive). 

I called my app the *Cosmetics App* and it is meant to predict the price of a product based on certian factor specified by the user. The reason for this is because when chosing a new cosmeic product to try can be very daunting. Not only are you gambling on something that might cost you a lot of money and might not work, but you might also be taking a chance on your complection being ruined due to an allergic reaction or buying a product that is not compatible with your skin type. This app will aid in the selection of a product that will fit your budget, or atleast let you know how much you should expect to spend, with all the specs you're looking for.

The app has several features, which allows the user to select certain varibles, subset the data, and do some data exploration. The app also allows the user to create numerical and graphical summmaries, as well as create different predictive models with the data provided. The predictive models used in this app will be (i) multiple linear regression, (ii) regression tree, and (iii) random forest. For the predictive models, the response variable is `Price`, and the other explanitory variables from the data set will be used to predict the price of the cosmetic product. 

Finally, we will compare these three models to see, which model has the best predicitive ability. This is assessed by the model with the lowest root mean squared error (RMSE).  

NOTE: 

I would like to mention the following for my app...

- The ingredients were hard to seperate due to commas within parenthases that were not being parsed correctly. There was no easy way I could this without errors, so I only made it possible to the user to subset products by ingredients and see how many products were available with the inputted character string(s). 
- Brand and Ingredients varaibles had too many options or were too unique on every row, respecively, to the point where their summaries were not meaningful. So, I had to chose varibles that could be grouped together when creating summaries and modeling.  
- I don't see any *NA* values in my data, but there seems to be some based on the warnings I see on the R Console when I when I ran my modeling scratch code. At the top of my app.R file, when reading in my data, I removed all the *NA* data via na.omit(). 

<!-- A list of packages needed to run the app -->

# Required Packages

The following packages are used for our data manipulation, prediction, etc.:

* `shiny`          : makes it easy to build interactive web apps straight from R.
* `shinydashboard` : makes it easy to use Shiny to create dashboards.
* `tidyverse`      : Tons of useful features for data manipulation and visualization!
* `corrplot`       : provides a visual exploratory tool on correlation matrix.
* `ggplot2`        : is a system for declaratively creating graphics, based on The Grammar of Graphics.
* `caret`          : used for predictive modelling.
* `DT`             : provides an R interface to the JavaScript library DataTables.
* `tree`           : is a package specifically designed to work with the decision trees.
* `randomForest`   : can be used for classification and regression. <!-- might remove -->

<!-- A line of code that would install all the packages used (so we can easily grab that and run it prior to running your app). -->

The following code will install the above packages. This can be run in the R Console.

#install packages   
install.packages(c("shiny", "shinydashboard", 
                   "tidyverse", "corrplot", "ggplot2", "caret", "DT", 
                   "tree", "randomForest"))

<!-- The shiny::runGitHub() code that we can copy and paste into RStudio to run your app -->

# Shiny App Run Code

The following code is required to run and output the Shiny App. This can be run in the R Console after a Commit to GitHub.

#run shiny app   
shiny::runGitHub(repo="ST558_Final_Project", username="magarittenguyen")


