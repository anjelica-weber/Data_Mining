
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

# Pre-Processing ----------------------------------------------------------
#formatting data types
df_heart <- df_heart %>%
  mutate(age = as.numeric(age),
         anaemia = as.factor(anaemia),
         creatinine_phosphokinase = as.numeric(creatinine_phosphokinase),
         diabetes = as.factor(diabetes),
         ejection_fraction = as.numeric(ejection_fraction),
         high_blood_pressure = as.factor(high_blood_pressure),
         platelets = as.numeric(platelets),
         serum_creatinine = as.numeric(serum_creatinine),
         serum_sodium = as.numeric(serum_sodium),
         sex = as.factor(sex),
         smoking = as.factor(smoking),
         time = as.numeric(time),
         DEATH_EVENT = as.factor(DEATH_EVENT))
#normalizing data


# Train / Test Data Split -------------------------------------------------
#data points for training and testing set selected using a random number generator (RNG)
#setting seed number for the RNG, this can be any arbitrary number 
#set.seed(101)

#Split ratio is 70% for training 30% testing
#function creates boolean vector T for 70% of data F for 30%
#pass in any column from data set
#sample <- sample.split(df_heart$age, SplitRatio = 0.7)

#train <- subset(df_heart, sample == T)
#test <- subset(df_heart, sample == F)

# Training Model ----------------------------------------------------------
#equation y ~ x1 + x2 + etc, or y ~. for all variables
log_model <- glm(DEATH_EVENT ~.,
                 data = df_heart,
                 family = binomial(link = "logit"))

#model summary
summary(log_model)

# Predictions -------------------------------------------------------------
head(predict(log_model, df_heart))
result_prob <- predict()