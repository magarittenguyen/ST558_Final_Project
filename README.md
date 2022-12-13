ST558 Final Project
================
Magaritte Nguyen  
2022-12-02

<!-- In the repo’s README.md file (which doesn’t need to be created from a .Rmd file, just use the one you initialize into the repo if you want) give a brief description of the purpose of the repo, a list of R packages used, links to the generated analyses, and the code used to create the analyses from a single .Rmd file (i.e. the render() code). -->

The purpose of this repository is for the efforts of Magaritte Nguyen on the NC State ST558 Final Project, Fall 2022. Here, we will be reading in data pertaining to Mashable article sharing informatoin collected over the past 2 year. We will then create a summary report based on 4 different models (MLR model #1, MLR model #2, Random Forest, and Boosting) to be able to see which model has the best prediciton ability for sharing articles for 6 different channels. These channels cover topics like Lifestlye, Entertaiment, Business, Social Media, Tech, and World. 

Please see below for more information.

<!-- TOC -->

# Table of Contents

-   <a href="#table-of-contents" id="toc-table-of-contents">Table of Contents</a>
-   <a href="#introduction" id="toc-introduction">Introduction</a>
-   <a href="#required-packages" id="toc-required-packages">Required Packages</a>
-   <a href="#shiny-app-run-code" id="toc-shiny-app-run-code">Shiny App Run Code</a> 

<!-- Introduction -->

# Introduction 

Our goal with this project is to take the data about articles published by [Mashable](https://www.mashable.com) and create predictive models for the number 
of shares in social networks (popularity) then automating our Markdown reports. 

This dataset summarizes a heterogeneous set of features in a period of two years. 

Then we will do an Exploratory Data Analysis (EDA) and summarize the data and try to predict the number of shares in two linear regression models, a random
forest model, and a boosting model. Lastly we will compare the four models and declare a winner (the model with the lowest root mean squared error (RMSE)).  

The dataset we will be using is [Online News Popularity Data Set](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity).

<!-- Required Packages -->

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

The following code will install the above packages. This can be run in the R Console.

#install packages
install.packages(c("shiny", "shinydashboard", 
                   "tidyverse", "corrplot", "ggplot2", "caret", "DT", 
                   "tree", "randomForest"))

<!-- Shiny App Run Code -->

# Shiny App Run Code

The following code is required to run and output the Shiny App. This can be run in the R Console after a Commit to GitHub.

#run shiny app
shiny::runGitHub(repo="ST558_Final_Project", username="magarittenguyen")


