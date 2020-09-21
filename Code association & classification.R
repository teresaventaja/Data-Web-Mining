getwd()

######################################################################################
######################################################################################
#################################     PART 1     #####################################
######################################################################################
######################################################################################

######### STEP 1 - Loading the data #########

# I install the packages
# and load the libraries

install.packages("tidyverse")
install.packages("readxl")
install.packages("knitr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("dplyr")
install.packages("plyr")
install.packages("arules")
install.packages("arulesViz")
install.packages("readxml")

library(tidyverse)
library(readxl)
library(readxml)
library(ggplot2)
library(lubridate)
library(dplyr)
library(arules)
library(arulesViz)
library(reshape2)
library(plyr)
library(reshape)
library(tidyr)

# Now for loading the data
# read.csv moves the position of columns number 33 and 34, 
# because the rows have a difference length.
# Therefore, I have converted the file into a .xlsx format
# and loaded it into RStudio this way

library(readxl)
groceries <- read_excel("Groceries.xlsx", col_names=FALSE, na="")

######### STEP 2 - Splitting the first column #########

# First I isolate the first column

first_column <- groceries[, 1]

# Then, I use separate() function (tidyr package)
# and try different combinations to obtain 2 columns:
# 1 containing the date, and a second containing the item

experiment <- separate(first_column, 1, sep = "(?<=000)|(?<=001)|(?<=002)",
         into = c("date", "item"), extra="merge")

# Next, I replace the first column in my original data set
# with the "item" column I've just created 

groceries$...1 <- experiment$item

# Now I have only items in cells

groceries

# Saving the result into a .csv file

write.csv(groceries, file="groceries2.csv", row.names=F)

######### STEP 3 - Association Rules and results #########

# Loading libraries for association rules

library(arules)
library(arulesViz)

# Reading the saved data as transactions
# I remove duplicates because the "apriori" algorithm only recognizes
# if an item is present, not how many times is present

groceries <- read.transactions("groceries2.csv", header=T, sep = ",", rm.duplicates = TRUE)

# The data at a glance

summary(groceries)

# The summary of the transactions indicates:

#########  1  #########
# That the most frequent items are
# vegetables, poultry, waffles, dishwashing liquid/detergent and ice cream

########  2  #########
# That the most frequent length distributions are
# transactions containing 20 items, 17 items, 18 items, 19 items and 14 items,
# followed by those containing 15, 16, 11, 21 and 9

########  3  #########

# That the minimum frequency list of items is 4, and the maximum is 27
# The median value transaction length is 15,
# the first quartile is 10, and the third quartile is 19

inspect(groceries[1:5])

# Plot for items meeting criteria 
# "support equal to or greater than 0.1"

itemFrequencyPlot(groceries, support = 0.1)

# Top 20 plot

itemFrequencyPlot(groceries, topN = 20)

# Matrix containing items contained in each transaction

image(groceries[1:5])

# y axis each transaction
# x axis what items are included in each transaction

# Matrix (items contained in each transaction) - Sample of 50

image(sample(groceries, 50))

# "apriori" algorithm with default parameters

res <- apriori(groceries)

# RESULT = We have created a set of 443 rules

summary(res)

#  2   3 
# 5 438 

# Defining different parameters

groceryrules <- apriori(groceries, parameter =  list(support = 0.14, confidence = 0.8, minlen = 2))

summary(groceryrules)

# RESULT: 41 rules
#2  3 
#5 36 

# groceryrules at a glance

inspect(groceryrules[1:5])

# Inspect first 10 results by lift

inspect(sort(groceryrules, by = "lift")[1:10])

# Check which rules contain "vegetable"
# This is relevant because this is the most frequent item in absolute terms

veggierules <- subset(groceryrules, items %in%  "vegetables")
inspect(veggierules)


######################################################################################
######################################################################################
#####################    PART 2 - CLASSIFICATION   ###################################
######################################################################################
######################################################################################

# Source link, and data description link

# https://archive.ics.uci.edu/ml/datasets/Occupancy+Detection+#
# https://code.datasciencedojo.com/datasciencedojo/datasets/tree/master/Occupancy%20Detection

######### STEP 1 - Pre-processing for KNN algorithm #########

# The original link contained the data divided into 3 files
# 1 for the test data, and 2 files for the training data
# I combined the 3 of them to do my own selection of test/training data
# resulting in room_oc.csv file

# Loading the data using read.csv function

room <- read.csv("room_oc.csv", header = TRUE, sep=",")

# Looking at the structure of the data and the top 6 rows

str(room)
head(room)

# Checking how many rooms are occupied and how many are not

table(room$Occupancy)

# 15810 are not occupied and 4750 are occupied

######### STEP 2 - Normalizing the data #########

# Set seed to allow reproducibility

set.seed(9850)

# Generate uniform random numbers, as many as the number of rows

ro <- runif(nrow(room))

# Using the order of those random numbers, 
# I relocate the original rows to a new position

room2 <- room[order(ro),]

# Now I looking at the structure 
# of the newly generated data and the top 6 rows

str(room2)
head(room2)

# Getting summary descriptive statistics
# for the attributes that I am going to use for the classification

summary(room2[,c(2,3,4,5)])

# The data is not normally distributed
# I am creating a function that it is going to be passed through parameter "x"
# and convert it into normally distributed data

normalize <- function(x) {
  return( (x - min(x)) / (max(x) - min(x)) )
}

# Test if the "normalize" function works with numbers 1:5

normalize(c(1,2,3,4,5))

# Using lapply() I am going to pass attributes Temperature, Humidity, Light and CO2
# through the function "normalize" in a loop
# This is going to convert every observation into normalized data

room_n <- as.data.frame(lapply(room2[,c(2,3,4,5)], normalize))

# Now I check the structure and the summary of the data

str(room_n)
summary(room_n)

# I have isolated the 4 variables I am going to work with
# and the data is normally distributed

##########     STEP 3 - kNN algorithm     ##########

# My data set is large enough to select a test sample of approximately
# 12% of the observations (2462 of the 20560)

room_n_train <- room_n[2463:20560,]
room_n_test <- room_n[1:2462,]

# The target data is not in my data frame containing the normalised data
# but instead it is in my original data frame that I created
# when I altered the order of the rows
# It is called "room2"
# and I am using the 7th column (Occupancy)
# to create the training and test target data

room_train_target <- room2[2463:20560, 7]

room_test_target <- room2[1:2462, 7]

# Loading the library for classification rules algorithm

library(class)

# Select k - the rule of thumb is the square root of the number of observations

sqrt(20560)

# selecting the closest odd number, I get 143

# Now I can build the model with knn() function

m1 <- knn(train=room_n_train, test=room_n_test, cl=room_train_target, k=143)
m1

# Finally, I create a table to check how well the model predicted the different classes

table(room_test_target, m1)
