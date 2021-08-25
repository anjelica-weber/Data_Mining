
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(xlsx)
library(Amelia)

# Import Data -------------------------------------------------------------
dir_ex <- paste0("J:/deans/Presidents/SixSigma/Individual Folders",
                 "/Current Employees/Engineers/Anjelica Weber/Projects",
                 "/Data Mining Training Materials")
df_lin <- read.xlsx2(file = paste0(dir_ex, "/Datasets.xlsx"),
                     sheetName = "SimpleRegression 2")
df_multi_lin <- read.xlsx2(file = paste0(dir_ex, "/Datasets.xlsx"),
                           sheetName = "LifeExpectancy")

# Missing Values ----------------------------------------------------------
#Chart of missing values
missmap(df_multi_lin)

#Option 1 - Remove missing values
test_df <- df_lin %>% drop_na_()

#Option 2 - Replace missing values (note do not replace values for target variable)
#Option 2a - Replace with average / most frequent

#Option 2b - Replace with random value


# Data Normalization ------------------------------------------------------
#min - max 
#scales each column to 0(min value) to 1 (max value)
min_max_norm <- function(x){
  (x - min(x))/ (max(x)-min(x))
}
test_df <- as.data.frame(lapply(df_lin, min_max_norm))

#z-score 
# (x- mean)/ standard deviation
#assumes underlying variable is normally distributed
test_df <- as.data.frame(scale(df_lin))

# Data Types --------------------------------------------------------------
#Note: Categorical data must be factored
#Note: Meta data should not be used to train models

