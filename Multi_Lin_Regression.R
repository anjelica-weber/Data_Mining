# Load Libraries ----------------------------------------------------------
library(caTools)
library(tidyverse)
library(xlsx)

# Import Data ---------------------------------------------------------------
dir_ex <- paste0("J:/deans/Presidents/SixSigma/Individual Folders",
                 "/Current Employees/Engineers/Anjelica Weber/Projects",
                 "/Data Mining Training Materials")
df_multi_lin <- read.xlsx2(file = paste0(dir_ex, "/Datasets.xlsx"),
                     sheetName = "LifeExpectancy")

# Pre-processing -----------------------------------------------------------
df_multi_lin <- df_multi_lin %>%
  mutate(Country = as.factor(Country),
         Year = as.numeric(Year),
         Status = as.factor(Status),
         Life.expectancy = as.numeric(Life.expectancy),
         Adult.Mortality = as.numeric(Adult.Mortality),
         infant.deaths = as.numeric(infant.deaths),
         Alcohol = as.numeric(Alcohol),
         percentage.expenditure = as.numeric(percentage.expenditure),
         Hepatitis.B = as.numeric(Hepatitis.B),
         Measles = as.numeric(Measles),
         BMI = as.numeric(BMI),
         under.five.deaths = as.numeric(under.five.deaths),
         Polio = as.numeric(Polio),
         Total.expenditure = as.numeric(Total.expenditure),
         Diphtheria = as.numeric(Diphtheria),
         HIV.AIDS = as.numeric(HIV.AIDS),
         GDP = as.numeric(GDP),
         Population = as.numeric(Population),
         thinness.1.19.years = as.numeric(thinness.1.19.years),
         thinness.5.9.years = as.numeric(thinness.5.9.years),
         Income.composition.of.resources = as.numeric(Income.composition.of.resources),
         Schooling = as.numeric(Schooling)) %>%
  drop_na()

# Building Model ----------------------------------------------------------
multi_lr_model <- lm(Life.expectancy ~ ., df_multi_lin)
summary(multi_lr_model)

# Evaluating Model --------------------------------------------------------
model_stats <- summary(multi_lr_model)
#coefficients used in linear equation to make predictions
model_stats$coefficients
#Measure of goodness of fit of model
#Describes how much variation is explained by the model. 1 is the best.
model_stats$r.squared

# Predictions -------------------------------------------------------------
head(predict(multi_lr_model, df_multi_lin))
