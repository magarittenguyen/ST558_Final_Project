#######################################################
# Name: Magaritte Nguyen
# Date: 01DEC2022
# Class: ST 558 (601) Fall 2022 Data Science for Statisticians  
# Assignment: Final Project - Creating a Shiny App
# File: Final Project Scratch Code.R
#######################################################

#https://www.kaggle.com/datasets/kingabzpro/cosmetics-datasets/code?resource=download

#check library
getwd()
setwd("D:/ST558/Final Project/Final_Project/ST558_Final_Project")

#libraries
library(tidyverse)
library(corrplot)
library(ggplot2)
library(caret)

#read in cosmetic data
#https://www.kaggle.com/datasets/kingabzpro/cosmetics-datasets/code?resource=download
cosmetics0 <- read_csv("cosmetics.csv")

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
             na.omit()


#A quick summary of different variables is as shown below:
# letting user define
var_names <- colnames(cosmetics) 

cosmetics %>% 
  select (all_of(var_names)) %>%
  summary()

#summary(cosmetics) 

#listing out unique items per variable

#this is to help me summarize the about the data part -- refernce this and make sure that tha app is not running by pressig stop int he console...
#type of cosmetic / skin care product
length(unique(cosmetics$Label))
unique(cosmetics$Label)

#unique brand names of products
length(unique(cosmetics$Brand))
unique(cosmetics$Brand)

#not too useful because each brand has its own name for their product
length(unique(cosmetics$Name))
unique(cosmetics$Name)

#look up ingredients of interest
#currently using products with carrot derivatives
sum(str_detect(string=toupper(cosmetics$Ingredients), pattern = "CAR(OTA|OTENE|ROT)", negate = FALSE )) 
#all non "Sulfur" products - bc this ingredient is excluded
#looks up english and british spelling / derivatives of the same ingredient
#regular expressions...
sum(str_detect(string=toupper(cosmetics$Ingredients), pattern = "SUL(F|PH)(UR|ATE|A)", negate=TRUE )) 
#products without water
#whittle down drop down box options to only have about 20 top allergens to avoid / 20 top ingredients of interest - to focus the project

#contingency tables

#label vs. brand
table(cosmetics$Brand, cosmetics$Label)
#done

#label vs. every skin type
table(cosmetics$Label, cosmetics$combination_fctr)
table(cosmetics$Label, cosmetics$dry_fctr)
table(cosmetics$Label, cosmetics$normal_fctr)
table(cosmetics$Label, cosmetics$oily_fctr)
table(cosmetics$Label, cosmetics$sensitive_fctr)

#brand, label, skin type
#might not do bc might break -- MAYBE LATER!!
table(cosmetics$Brand, cosmetics$Label, cosmetics$combination_fctr) #Yes and No for combination skin
table(cosmetics$Brand, cosmetics$Label, cosmetics$dry_fctr)
table(cosmetics$Brand, cosmetics$Label, cosmetics$normal_fctr)
table(cosmetics$Brand, cosmetics$Label, cosmetics$oily_fctr)
table(cosmetics$Brand, cosmetics$Label, cosmetics$sensitive_fctr)

#brand vs. every skin type
table(cosmetics$Brand, cosmetics$combination_fctr)
table(cosmetics$Brand, cosmetics$dry_fctr)
table(cosmetics$Brand, cosmetics$normal_fctr)
table(cosmetics$Brand, cosmetics$oily_fctr)
table(cosmetics$Brand, cosmetics$sensitive_fctr)

#sensitivity vs. skin types...?

#correlation
#performing correlations between variables of interest
cor_mat <- cor(cosmetics %>% 
                 select(Price, Rank), method = "pearson")

#correlation plot below
corrplot(cor_mat, 
         type = "lower",
         tl.pos = "lt",
         title = "Correlation Coefficients for Sephora Cosmetics Data",
         mar=c(0,0,2,0)
         ) #end of corrplot() function

#not useful??

#bar charts and scatter plots 

#bar charts
#Freq of Type of cosmetics
ggplot(data = cosmetics, aes(x = Label)) + 
  geom_bar(aes(fill= Label), show.legend = FALSE) + 
  ggtitle("Type of Cosmetics Available in Sephora Data") +
  labs(x="Type of Cosmetic", y="Frequency", fill="Type of Cosmetic")

#Type of Cosmetics Available in Sephora Data for Combination Skin
ggplot(data = cosmetics, aes(x = Label)) + 
  geom_bar(aes(fill= combination_fctr), position="dodge") + 
  ggtitle("Type of Cosmetics Available in Sephora Data for Combination Skin") +
  labs(x="Type of Cosmetics", y="Frequency", fill="Combination Skin")

#Type of Cosmetics Available in Sephora Data for Dry Skin
ggplot(data = cosmetics, aes(x = Label)) + 
  geom_bar(aes(fill= dry_fctr), position="dodge") + 
  ggtitle("Type of Cosmetics Available in Sephora Data for Dry Skin") +
  labs(x="Type of Cosmetics", y="Frequency", fill="Dry Skin")
  
#Type of Cosmetics Available in Sephora Data for Normal Skin
ggplot(data = cosmetics, aes(x = Label)) + 
  geom_bar(aes(fill= normal_fctr), position="dodge") + 
  ggtitle("Type of Cosmetics Available in Sephora Data for Normal Skin") +
  labs(x="Type of Cosmetics", y="Frequency", fill="Normal Skin")
  
#Type of Cosmetics Available in Sephora Data for Oily Skin
ggplot(data = cosmetics, aes(x = Label)) + 
  geom_bar(aes(fill= oily_fctr), position="dodge") + 
  ggtitle("Type of Cosmetics Available in Sephora Data for Oily Skin") +
  labs(x="Type of Cosmetics", y="Frequency", fill="Oily Skin")

#Type of Cosmetics Available in Sephora Data for Sensitive Skin
ggplot(data = cosmetics, aes(x = Label)) + 
  geom_bar(aes(fill= sensitive_fctr), position="dodge") + 
  ggtitle("Type of Cosmetics Available in Sephora Data for Sensitive Skin") +
  labs(x="Type of Cosmetics", y="Frequency", fill="Sensitive Skin")




#bar plots by average PRICE!
#Type of cosmetic vs. Average Price of each type of cosmetic - Combination
ggplot(data = cosmetics %>%
                group_by(Label, combination_fctr) %>%
                summarize(avg_price=mean(Price)) %>%
                mutate(avg_price_fmt = paste0("$", sprintf("%.2f", round(avg_price, 2)) )), 
  aes(x = Label, y=avg_price, group = combination_fctr)) + 
  geom_col(aes(fill= combination_fctr), position="dodge") + 
  ggtitle("Type of Cosmetics Available in Sephora Data for Combiation Skin") +
  labs(x="Type of Cosmetics", y="Average Price", fill="Combiation Skin") +
  geom_label( aes(label = avg_price_fmt), position=position_dodge(width=0.9), size=2.25 ) 

#Type of cosmetic vs. Average Price of each type of cosmetic - Dry
ggplot(data = cosmetics %>%
         group_by(Label, dry_fctr) %>%
         summarize(avg_price=mean(Price)) %>%
         mutate(avg_price_fmt = paste0("$", sprintf("%.2f", round(avg_price, 2)) )), 
       aes(x = Label, y=avg_price, group = dry_fctr)) + 
  geom_col(aes(fill= dry_fctr), position="dodge") + 
  ggtitle("Type of Cosmetics Available in Sephora Data for Dry Skin") +
  labs(x="Type of Cosmetics", y="Average Price", fill="Dry Skin") +
  geom_label( aes(label = avg_price_fmt), position=position_dodge(width=0.9), size=2.25 )

#Type of cosmetic vs. Average Price of each type of cosmetic - Normal
ggplot(data = cosmetics %>%
         group_by(Label, normal_fctr) %>%
         summarize(avg_price=mean(Price)) %>%
         mutate(avg_price_fmt = paste0("$", sprintf("%.2f", round(avg_price, 2)) )), 
       aes(x = Label, y=avg_price, group = normal_fctr)) + 
  geom_col(aes(fill= normal_fctr), position="dodge") + 
  ggtitle("Type of Cosmetics Available in Sephora Data for Normal Skin") +
  labs(x="Type of Cosmetics", y="Average Price", fill="Normal Skin") +
  geom_label( aes(label = avg_price_fmt), position=position_dodge(width=0.9), size=2.25 )

#Type of cosmetic vs. Average Price of each type of cosmetic - Oily
ggplot(data = cosmetics %>%
         group_by(Label, oily_fctr) %>%
         summarize(avg_price=mean(Price)) %>%
         mutate(avg_price_fmt = paste0("$", sprintf("%.2f", round(avg_price, 2)) )), 
       aes(x = Label, y=avg_price, group = oily_fctr)) + 
  geom_col(aes(fill= oily_fctr), position="dodge") + 
  ggtitle("Type of Cosmetics Available in Sephora Data for Oily Skin") +
  labs(x="Type of Cosmetics", y="Average Price", fill="Oily Skin") +
  geom_label( aes(label = avg_price_fmt), position=position_dodge(width=0.9), size=2.25 )

#Type of cosmetic vs. Average Price of each type of cosmetic - Sensitive
ggplot(data = cosmetics %>%
         group_by(Label, sensitive_fctr) %>%
         summarize(avg_price=mean(Price)) %>%
         mutate(avg_price_fmt = paste0("$", sprintf("%.2f", round(avg_price, 2)) )), 
       aes(x = Label, y=avg_price, group = sensitive_fctr)) + 
  geom_col(aes(fill= sensitive_fctr), position="dodge") + 
  ggtitle("Type of Cosmetics Available in Sephora Data for Sensitive Skin") +
  labs(x="Type of Cosmetics", y="Average Price", fill="Sensitive Skin") +
  geom_label( aes(label = avg_price_fmt), position=position_dodge(width=0.9), size=2.25 )


# some more bar plots... brand and rating.. maybe break down into type first...


#scatter plots
#everything
#setting the stage to add layers
g <- ggplot(cosmetics, aes(x = Rank, y = Price))
#scatter plot with linear regression line
g + geom_point() +
  labs(title = "Rank vs. Price for Combination Type Skin Products") +
  geom_smooth(method = lm) 

#by product type
#"Moisturizer" "Cleanser"    "Treatment"   "Face Mask"   "Eye cream"   "Sun protect"

#Moisturizer
#setting the stage to add layers 
g <- ggplot(cosmetics %>%
              filter(Label == "Moisturizer", Rank != 0) ,  # drop outliers...
            aes(x = Rank, y = Price))
#scatter plot with linear regression line 
g + geom_point() +
  labs(title = "Rank vs. Price for Combination Type Skin Products - Moisturizer") +
  geom_smooth(method = lm) 

# do later -- "Moisturizer", "Cleanser"    "Treatment"   "Face Mask"   "Eye cream"   "Sun protect"
#remove outlier though to test before drop...

#boxplots - for types of skin care products against Price and Rank...

#Price
#all
#A scatter plot with boxplots
ggplot(data=cosmetics, 
       aes(y=Price, x=Label)) + 
  labs(title = "Type of Skin Products vs. Price", 
       x="Types of Skin Products", 
       y="Price") +
  geom_boxplot() + 
  geom_point(aes(color=Label), position="jitter", alpha=0.25, show.legend = FALSE) 

#filtering for combination
ggplot(data=cosmetics %>%
              filter(combination_fctr == "Yes"), 
       aes(y=Price, x=Label)) + 
  labs(title = "Type of Productsfor Combination Skin vs. Price Broken Down By Sensitivinty", 
       x="Types of Skin Products", 
       y="Price") +
  geom_boxplot() + 
  geom_point(aes(shape = sensitive_fctr , color=Label), position="jitter", alpha=0.6, show.legend = c("shape"=TRUE, "color"=FALSE)) 

#combination, dry, normal, oily -- do price boxplots for these...

#Rank - change price to rank and then the titles... ^^
#unique(cosmetics$Label)


########################### MODELING ###############################;

# splitting into train and test data  
#seed is set for reproducibility 
set.seed(123)

#indices to split on
CosmeticsIndex <- createDataPartition(cosmetics$Price, p = 0.70, list = FALSE)

#subset
CosmeticsTrain <- cosmetics[ CosmeticsIndex, ]
CosmeticsTest  <- cosmetics[-CosmeticsIndex, ]

# – A Modeling page. You will fit three supervised learning models. Depending on your response you’ll fit a multiple linear regression or generalized linear regression model, regression or classification tree, and a random forest model. 

#This page should have three tabs to it:
#   ∗ Modeling Info tab: You should explain these three modeling approaches, the benefits of each, and the drawbacks of each. You should include some type of math type in the explanation (you’ll need to include mathJax).

#modeling - 
# you will fit 3 supervised learning models
# response = price 
#MLR or GLR, R or classification tree, random forest model

#You will fit three supervised learning models. 

#Depending on your response you’ll fit a 
# [[[ multiple linear regression ]]] OR generalized linear regression model, 
# [[[ regression tree ]]] OR classification tree, 
# a [[[ random forest model ]]]

#You should explain:
#these three modeling approaches, 
#the benefits of each, 
#the drawbacks of each. 
#You should include some type of math type in the explanation (you’ll need to include mathJax).


#use of MathJax - in Shiny Package...
#write LaTeX code in here...
#withMathJax(...)

## First Linear Model - MLR 
# 
# For this first model, we will model the number of shares using multiple linear regression, then summarize, and predict on the testing data called 'ChannelTest'. and use the `postResample()` function to get useful metrics. 
# 
# The selected independent variables for this model are:
#   
#   * "n_tokens_title"   (Number of words in the title),
# 
# * "n_tokens_content" (Number of words in the content)
# 
# * "num_imgs"         (Number of images) : we use the square of this variable
# 
# * "num_videos"       (Number of videos) : we use the square of this variable
# 
# * "is_weekend"       (Was the article published on the weekend?)

#unique(cosmetics$Brand)

#  [[[ multiple linear regression ]]] 
#linear regression model 1
MLR <- train(Price  ~ Rank +  Label + sensitive_fctr + I(Rank^2) + Rank*sensitive_fctr,
              data = CosmeticsTrain,
              method = "lm",
              preProcess = c("center", "scale"),
              trControl = trainControl(method= "cv", number = 5))
#calling the l_m1 object
MLR





#summary statistics
summary(MLR)

#predicting on the ChannelTest data with linear regression model 1
test_pred_MLR <- predict(MLR, newdata = CosmeticsTest)

#best model chosen - RMSE reported explicitly0.
#goal is to compare RMSE and see which one is the lowest!
m1 <- postResample(test_pred_MLR, CosmeticsTest$Price)
#output object m1
m1


#[[[ regression tree ]]] 
# Regression trees are used when the dependent variable is continuous whereas the classification tree is used when the dependent variable is categorical.

#a classification tree (use method = rpart: tuning parameter is cp, use values 0, 0.001, 0.002, . . . , 0.1)

#Here, we’ll look at predicting the same heart disease variable as well, just instead of using kNN we’ll use the a classification tree method (method = rpart). Use repeated 5 fold cross-validation, with the number of repeats being 3. You should also preprocess the data by centering and scaling. Lastly, set the tuneGrid so that you are considering values of cp, = 0, 0.001, 0.002, . . . , 0.1. 
#Results show that the most optimal model using the largest value for Accuracy (0.7909216) is the model where cp = 0.006 -- this is our final model.

#Basic Tree Fit - Notes 3 Slide 199
#install.packages("tree")
library(tree) #rpart is also often used
#suppressWarnings(as.numeric(vectr)) 

#factor predictors must have at most 32 levels
treeFit <- tree(Price ~ Rank + label_fctr + combination_fctr+ sensitive_fctr , data = CosmeticsTrain)
plot(treeFit)
text(treeFit) #first optimal split is 4.85

cvTree <- cv.tree(treeFit)
cvTree


plot(cvTree$size ,cvTree$dev ,type="b")


trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
#Predictions using predict
pred <- predict(treeFit, newdata = dplyr::select(CosmeticsTest, -Price))
#Root MSE
TR_RMSE <- sqrt(mean((pred-CosmeticsTest$Price)^2))
TR_RMSEs

#set.seed(3333)

#classification_tree_fit <- train(Price ~ Rank + label_fctr + combination_fctr+ sensitive_fctr, 
#                                 data = CosmeticsTrain, 
#                                 method = "rpart",
#                                 trControl=trctrl,
#                                 preProcess = c("center", "scale"),
#                                 tuneGrid = data.frame(cp=seq(0, 400, 10)) )
#classification_tree_fit

#Accuracy was used to select the optimal model using the largest value. The final value used for the model was cp = 0.006.


#[[[ random forest model ]]
## Random Forest Model
# 
# The idea behind the random forest model is the same as bagging, but we use a random subset of predictors for each bootstrap sample tree fit (indicated by "mtry"). 
# 
# More specifically, it involves:
#   
#   - creating a boothstrap sample (same size with replacement)
# - training the tree on this sample (no pruning necessary)
# - repeating the process a large number of times and the final prediction is the average of those predictions
# 
# Finding the average of predictions decreases variance, which improves predictions, but unfortunately we lose interpretability.
# 
# For our random forest, we model the number of shares by the selected independent variables:
#   
# * "num_hrefs" (Number of links)
# 
# * "n_tokens_content" (Number of words in the content)
# 
# * "num_videos" (Number of videos) 
# 
# * "is_weekend" (Was the article published on the weekend?)
 
#Random Forrest Model
RF <- train(Price ~ Rank +  Label + sensitive_fctr ,
             data = CosmeticsTrain, method = "rf",
             trControl=trainControl(method = "cv", number = 5),
             preProcess = c("center", "scale"),
             tuneGrid = data.frame(mtry = 1:3))
#calling r_f object
RF

#best model chosen - RMSE reported explicitly
#goal is to compare RMSE and see which one is the lowest!
test_pred_RF <- predict(RF, newdata = CosmeticsTest)
m3 <- postResample(test_pred_RF, CosmeticsTest$Price)
#calling m3 object
m3


#RMSE is a metric that tells us how far apart the predicted values are from the observed values in a dataset, on average. The lower the RMSE, the better a model fits a dataset.

#As for the R2 value we have (R-squared), this is a metric that indicates the proportion of the variance in the response variable of a regression model that can be explained by the predictor variables. The higher the R2 value, the better a model fits a dataset. This value ranges from 0 to 1.

#It should be noted that our best model is selected from a model that has results reflecting the lowest RMSE and the highest R2 value. But, there are times when the results from the lowest RMSE and highest R2 are not from the same model; therefore, we will use RMSE to pick our final winner.


# Comaprison

#The `postResample()` function was used to calculate useful statistics such as RMSE and R squared values for each one of the four models. We summarize them in the tibble below.

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

#When comparing all 4 of our models, we are looking for the smallest value of RMSE to tell us which model is the best. In this situation for the `r params$label` channel, we can say that the smallest RMSE value is `r final_result$RMSE` and this value comes from the `r final_result$model` model.
  