
# Load Libraries ----------------------------------------------------------
library(caTools)
library(tidyverse)
library(xlsx)

# Load Data ---------------------------------------------------------------
dir_ex <- paste0("J:/deans/Presidents/SixSigma/Individual Folders",
                 "/Current Employees/Engineers/Anjelica Weber/Projects",
                 "/Data Mining Training Materials")
df_heart <- read.xlsx2(file = paste0(dir_ex, "/Datasets.xlsx"),
                     sheetName = "HeartFailure")

# Train / Test Data Split -------------------------------------------------
#data points for training and testing set selected using a random number generator (RNG)
#setting seed number for the RNG, this can be any arbitrary number 
set.seed(101)

#Split ratio is 70% for training 30% testing
#function creates boolean vector T for 70% of data F for 30%
#pass in any column from data set
sample <- sample.split(df$age, SplitRatio = 0.7)

train <- subset(df, sample == T)
test <- subset(df, sample == F)

# Training Model ----------------------------------------------------------
#equation y ~ x1 + x2 + etc, or y ~. for all variables
lr_model <- lm(target ~., train)

#display linear equation used for predictions
summary(lr_model)