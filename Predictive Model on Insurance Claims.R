title: "HarvardX: PH125.9x Capstone - Predictive Model on Insurance Claims Project"
# author: "Andrew Chikunga"
#
# This script is a collection of code snippets from the accompanying Rmd file
# The code may include additional/modified comments but is in principle 
# equivalent to snippets in the Rmd file.
# Importing Essential Libraries for data wrangling operations and visualizations
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("mice", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("DataExplorer", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("VIM", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("reader", repos = "http://cran.us.r-project.org")
#################################################################################################
library(knitr)
library(tidyverse)
library(caret)
library(dslabs)
library(dplyr)
library(ggplot2)
library(matrixStats)
library(data.table)
library(lubridate)
library(lattice)
library(reshape2)
library(mice)
library(DataExplorer)
library(VIM)
library(readr)
##################################################################################################
# 2.Loading data

# loading the train_data set
train_data <- read.csv(("https://github.com/ANDREW-C1982/Predictive-Model-on-Insurance-Claims/raw/master/train_data.csv"), header=T)
head(train_data) # view the first six lines of the train_data dataset
summary(train_data) # view the summaries of the train_data dataset
str(myData) # view the variables of the train_data dataset

# 3.Exploratory Data Analysis

table (complete.cases (train_data)) # Checking for missing values on the train set
# look at the summary of all our variables and analyze which variables have the greatest number of missing values
md.pattern(train_data)
aggr_plot <- aggr(train_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(train_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
train_data <-na.omit(train_data) # removing missing values on train_data

## Continous variables

# Visualising distributions:Outlier Analysis or Clustering:
boxplot(train_data,outcol = "red",main = "features with outliers",col = "salmon", outcex = 1.5) # checking for outliers using box plots
# Removing the outliers on the Building.Dimension data variable
Q <- quantile(train_data$Building.Dimension, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(train_data$Building.Dimension) # Interquartile range
up <-  Q[2]+1.5*iqr # Upper Range
low<- Q[1]-1.5*iqr # Lower Range
new_train <- subset(train_data, Building.Dimension > (Q[1] - 1.5*iqr) & Building.Dimension < (Q[2]+1.5*iqr)) # removing the outlier below 25% and above 75%
boxplot(new_train,outcol = "red",main = "features without outliers",col ="salmon", outcex = 1.5) # box plot after removing main outlier

# Investigating variability in all the features of new_train Dataset

#  Histogram for  Building.Dimension > 50 square metres
new_train<-new_train%>%
filter(Building.Dimension > 50)
qplot(data= new_train, x = Building.Dimension,bins=50,main="Bar plot for building.dimension distribution",colour="red") + ylab("Count") 

# Histogram for Insured_period
qplot(data = new_train, x = Insured_Period,main="Bar plot for Insured_Period distribution",colour="red",width=0.7) + ylab("count")
new_train<-new_train%>% # removing all values <= 0.1 on  insured_period
filter(Insured_Period > 0.1)
qplot(data = new_train, x = Insured_Period,main="Bar plot for Insured_Period distribution",colour="red") + ylab("count") # final histogram on insured_period

# Histogram for Date_of_Occupancy
qplot(data = new_train, x = Date_of_Occupancy,main="Bar plot for Date_of_Occupancy distribution",colour="red") + ylab("count")
new_train<-new_train%>% # removing all values < 1900 on  Date_of_Occupancy
filter(Date_of_Occupancy >=1900)
qplot(data = new_train, x = Date_of_Occupancy,bins=50,main="Bar plot for Date_of_Occupancy distribution",colour="red") + ylab("count") # final histogram on Date_of_Occupancy

## Categorical Variables

ggplot(data=new_train, aes(x=NumberOfWindows, y=Claim, fill=Claim)) + geom_bar(stat="identity") + labs(title="Plot of NumberOfWindows per Claim") # Bar graph showing the NumberOfWindows
ggplot(data=new_train, aes(x=YearOfObservation, y=Claim, fill=Claim)) + geom_bar(stat="identity",width = 0.5) + labs(title="Plot of YearOfObservation  per Claim") # Bar graph showing the YearOfObservation.
ggplot(data=new_train, aes(x=Residential, y=Claim, fill=Claim)) + geom_bar(stat="identity",width = 0.2) + labs(title="Plot of Residential  per Claim") # Bar graph showing the Residential
ggplot(data=new_train, aes(x=Building_Painted, y=Claim, fill=Claim)) + geom_bar(stat="identity",width = 0.2) + labs(title="Plot of Building_Painted  per Claim") # Bar graph showing the Building_Paintedl
ggplot(data=new_train, aes(x=Building_Fenced, y=Claim, fill=Claim)) + geom_bar(stat="identity",width = 0.2) + labs(title="Plot of Building_Fenced per Claim") # Bar graph showing the Building_Fenced
ggplot(data=new_train, aes(x=Garden, y=Claim, fill=Claim)) + geom_bar(stat="identity",width = 0.3) + labs(title="Plot of Garden per Claim") # Bar graph showing the Garden
ggplot(data=new_train, aes(x=Settlement , y=Claim, fill=Claim)) + geom_bar(stat="identity",width = 0.2) + labs(title="Plot of Settlement per Claim") # Bar graph showing the Settlement
ggplot(data=new_train, aes(x=Building_Type, y=Claim, fill=Claim)) + geom_bar(stat="identity",width = 0.4) + labs(title="Plot of Building_Type per Claim") # Bar graph showing the Building_Type

## Transforming dataset into numeric
new_train <-sapply(new_train,as.numeric) ## Transforming data into numeric
new_train<-as.data.frame(new_train) # convert to data frame
introduce(new_train) # checking basic information about the numeric dataframe
plot_correlation(na.omit(new_train), maxcat = 5L) # correlation heat map
plot_histogram(new_train) # histograms of all variables in new_train

# 4.Feature Engineering
#drop columns with weak correlation
data <- data.frame(Insured_Period=new_train$Insured_Period,Residential=new_train$Residential,
                   Building.Dimension=new_train$Building.Dimension,Building_Type=new_train$Building_Type,NumberOfWindows=new_train$NumberOfWindows) #drop columns with weak correlation

# 5.Standardization
data <-scale(data)
data <-as.data.frame(data)
new_data <-mutate(data,new_train$Claim) # joining the target variable
colnames(new_data)[10] <- "Claim" # rename the target variable
new_data$Claim <-as.factor(new_data$Claim) 

# 6. Create training and testing sets

# testing set will be 30% of new_data
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = new_data$Claim, times = 1, p = 0.3, list = FALSE)
training_data <- new_data[-test_index,]
testing_data <- new_data[test_index,]

# 7. Fitting Logistic Regression Model
# logistic function
sigmoid = function(x) {
  1 / (1 + exp(-x))}
# you can use the equal sign as well, the programmer way.
x <- seq(-5, 5, 0.01)
plot(x, sigmoid(x), col='blue')
new_data_glm = train (Claim ~. , method = "gbm", data = new_data) # building the model
confusionMatrix (new_data$Claim, predict (new_data_glm, new_data)) # Checking the accuracy of the model using train data
log.model <- glm(Claim ~., data = new_data, family = binomial) # logistic equation
summary(log.model) # Summary of the model
plot(log.model) # plot the model

## Checking the accuracy of the model using train data:
training_data$Claim <- factor(training_data$Claim,levels=c(0,1))
testing_data$Claim <- factor(testing_data$Claim,levels=c(0,1))
training_glm = train (Claim ~. , method = "glm", data = training_data)
confusionMatrix (training_data$Claim, predict (training_glm, training_data)) # confusion matrix

## Building the model
log.model <- glm(Claim ~., data = training_data, family = binomial)
summary(log.model)
plot(log.model)

# 8. How to Predict on Testing Dataset
pred <- predict(log.model, newdata = testing_data, type = "response")

# Recode factors
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num)
y_act <- testing_data$Claim
mean(y_pred == y_act) #83% # Checking the accuracy
table(training_data$Claim) # inbalance check

# 8.Improving model accuracy by handling Class Imbalance with Upsampling.
# upscaling

%ni%' <- Negate('%in%')  # define 'not in' func
# set.seed(100)
up_train <- upSample(x = training_data[, colnames(training_data) %ni% "Claim"],
y = (training_data$Claim))
table(up_train$Class)

#upscale accuracy

upscale_pred <- predict(upscale_logitmod, newdata = testing_data, type = "response")
upscale_logitmod <- glm(Class ~., family = "binomial", data=up_train)
summary(upscale_logitmod)

upscale_pred_num <- ifelse(pred > 0.5, 1, 0)
upscale_pred <- factor(upscale_pred_num, levels=c(0, 1))
upscale_act <- testing_data$Claim
mean(upscale_pred == upscale_act) 

# ROC curve
library(pROC)
lr.predict <- predict(upscale_logitmod,up_train, probability = TRUE)
auc.gbm = roc(up_train$Class, lr.predict, plot = TRUE, col = "blue")















