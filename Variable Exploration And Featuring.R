library(tidyverse)
library(dplyr)
library(magrittr)
library(Amelia)

# Reading in the data
df.train = read_csv('train.csv')

# Create artificial train test dataset to make sure that dataset has all factors contained in 
# test
#   Read in test dataset
df.test = read_csv('test.csv')
#   Artificially created target variable to ncol matches
df.test = add_column(df.test, Survived = rep(0, nrow(df.test)))
#   Indicator variable for both train and test
df.train = add_column(df.train, TrainInd = rep(1, nrow(df.train)))
df.test = add_column(df.test, TrainInd = rep(0, nrow(df.test)))

#   creating train test table
df.train.test = rbind(df.train, df.test)
str(df.train.test)

# Finding variables with missing values
missmap(df.train.test)
table(df.train$FamilySize, exclude = NULL)
str(df.train.test)
#So Age and Embarked have missing values, keep this in mind when exploring variables

# Exploring Variables -----------------------------------------------------

# PassengerId variable
#   PassengerId is unique to each variable. Dispose
df.train.test$PassengerId = NULL

# PClass variable
table(df.train.test$Pclass)

#   Has three distinct levels, so encode as factor
df.train.test$Pclass = as.factor(df.train.test$Pclass)

# Name variable
df.train.test$Name

#   The title starts two commas after the comma and ends at the period
#   Extracting title from Name
loc.begin = str_locate(df.train.test$Name, ',')[, 1]
loc.end = str_locate(df.train.test$Name, '\\.')[, 1]
df.train.test$Title = str_sub(df.train.test$Name, loc.begin + 2, loc.end - 1)

#   Finding if the processing was done correctly
table(df.train.test$Title)
#   So looks like the code worked correctly. 

#   Encode Title as factor
df.train.test$Title = as.factor(df.train.test$Title)

#   Name does not have more useful information. Dispose
df.train.test$Name = NULL

# Sex variable should be factor
df.train.test$Sex = as.factor(df.train.test$Sex)

# Extracting lifestage from Age
find_lifestage = function(age){
  if (is.na(age)){
    return('Unknown')
  } else if (age<1){
    return('Baby<1')
  } else if (age<3){
    return('Baby<3')
  } else if  (age<7){
    return('Preschool')
  } else if(age<13){
    return('PrimarySchool')
  } else if(age < 18){
    return('HighSchool')
  } else if(age < 22){
    return('University')
  } else if (age < 30){
    return('Twenties')
  } else if (age < 40){
    return('Thirties')
  } else if (age < 50){
    return('Fourties')
  } else if (age < 60){
    return('Fifties')
  } else if (age < 70){
    return('Sixties')
  } else if (age < 80){
    return('Seventies')
  } else if (age < 90){
    return('Eighties')
  } else if (age < 100){
    return('Nineties')
  } else {
    return('>99')
  }
}
sapply(df.train.test$Age, find_lifestage)
df.train.test$LifeStage = sapply(df.train.test$Age, find_lifestage)
df.train.test$LifeStage = as.factor(df.train.test$LifeStage)

# Value from keeping age is marginal. Dispose
df.train.test$Age = NULL

#Create family size from Parch and SibSp
df.train.test$FamilySize = df.train.test$SibSp + df.train.test$SibSp
df.train.test$FamilySize = as.factor(df.train.test$FamilySize)

# SibSp should be factors 
df.train.test$SibSp = as.factor(df.train.test$SibSp)

# Parch should be factors
df.train.test$Parch = as.factor(df.train.test$Parch)

# Ticket variable
table(df.train.test$Ticket)

#   Looks like we could do some featuring, but for now ignore
df.train.test$Ticket = NULL

# Exploration for cabin
#   Extracting the cabin section
df.train.test$CabinSection = str_sub(df.train.test$Cabin, 1, 1)
table(is.na(df.train.test$CabinSection))

#   Setting unknown cabin sections to level 'Unknown'
df.train.test$CabinSection[is.na(df.train.test$CabinSection)] = 'Unknown'

#   Encoding CabinSection as a factor
df.train.test$CabinSection = as.factor(df.train.test$CabinSection)

#   Cabin may have more useful information, but dispose for now
df.train.test$Cabin = NULL

# Embarked variable
# Setting NA values to 'Unknown'
df.train.test$Embarked[is.na(df.train.test$Embarked)] = 'Unknown'

#   Encode Embarked as factor
df.train.test$Embarked = as.factor(df.train.test$Embarked)

# Making sure that number of levels in train and test is the same

str(df.train.test)

# Create family size from adding Parch
# Now that schema and features are consistent, split back into train and test

df.train = df.train.test %>% filter(TrainInd == 1)
df.test = df.train.test %>% filter(TrainInd == 0)

# Dropping Survived and Train Ind from df.test
df.test = df.test %>% select(-TrainInd, -Survived)

# Dropping TrainInd from df.train
df.train = df.train %>% select(-TrainInd)

str(df.train.test)

# https://stackoverflow.com/questions/17059432/random-forest-package-in-r-shows-error-during-prediction-if-there-are-new-fact
# Got idea to create df.train.test from above link
