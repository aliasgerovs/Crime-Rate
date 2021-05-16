
#Case Study Week 7

# Imporing  libraries & dataset 
library(tidyverse) 
library(data.table)
library(rstudioapi)
library(recipes)
library(caret)
library(skimr)
library(purrr)
library(inspectdf)
library(mice)
library(graphics)
library(Hmisc)
library(glue)
library(highcharter)
library(plotly)
library(h2o)  
library(car)


my_dat <- fread("crimes.csv")

my_dat %>% skim()

nrows <- nrow(my_dat)
ncomplete = sum (complete.cases(my_dat))
ncomplete/nrows #answer is 1 it means we dont have any na value in our data


#1. Find multicollinearity by applying VIF; 

plot(my_dat)

my_cor <- cor(my_dat)

vif(model)

mean(vif(model)) 

#2. Standardize features; 

my_dat %>% glimpse()

my_dat$PctEmplProfServ <- my_dat$PctEmplProfServ %>% scale(center = TRUE, scale = TRUE)
my_dat$PctOccupManu <- my_dat$PctOccupManu %>% scale(center = TRUE, scale = TRUE)
my_dat$PctOccupManu <- my_dat$PctOccupMgmtProf %>% scale(center = TRUE, scale = TRUE)
my_dat$PctOccupManu <- my_dat$MalePctDivorce %>% scale(center = TRUE, scale = TRUE)
my_dat$MalePctNevMarr <- my_dat$MalePctNevMarr %>% scale(center = TRUE, scale = TRUE)
my_dat$FemalePctDiv <- my_dat$FemalePctDiv %>% scale(center = TRUE, scale = TRUE)
my_dat$TotalPctDiv <- my_dat$TotalPctDiv %>% scale(center = TRUE, scale = TRUE)
my_dat$PersPerFam <- my_dat$PersPerFam %>% scale(center = TRUE, scale = TRUE)


#3. Split data into train and test sets using seed=123; 

set.seed(123)

sample <- sample.split(my_dat$TotalPctDiv, SplitRatio = 0.70) 

# Training Data
train = subset(my_dat, sample == TRUE)

# Testing Data

test = subset(my_dat, sample == FALSE)

#4. Build linear regression model. p value of variables should be max 0.05; 

model <- lm(NumIlleg ~ . , data = train)

summary(model) # p-value: < 2.2e-16

predicts <- predict(model , test) %>% as.data.frame()
predicts$pred <- predicts$.

#5. Calculate RMSE 

test_set <- test %>% as.data.frame()
residuals = test_set$NumImmig - predicts$pred
RMSE = sqrt(mean(residuals^2))

#Adjusted R-squared; 

y_test_mean = mean(test_set$NumIlleg)

tss = sum((test_set$NumIlleg - y_test_mean)^2) #total sum of squares
rss = sum(residuals^2) #residual sum of squares

R2 = 1 - (rss/tss); R2

n <- test_set %>% nrow() #sample size
k <- features %>% length() #number of independent variables
Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1))

tibble(RMSE = round(RMSE,1),
       R2, Adjusted_R2)


#6. Check overfitting.

y_pred_train <- model %>% predict(newdata = train) %>% as.data.frame()

train_set <- train %>% as.data.frame()
residuals = train_set$NumIlleg - y_pred_train$predict

RMSE_train = sqrt(mean(residuals^2))
y_train_mean = mean(train_set$NumIlleg)

tss = sum((train_set$NumIlleg - y_train_mean)^2)
rss = sum(residuals^2)

R2_train = 1 - (rss/tss); R2_train

n <- train_set %>% nrow() #sample size
k <- features %>% length() #number of independent variables
Adjusted_R2_train = 1-(1-R2_train)*((n-1)/(n-k-1))