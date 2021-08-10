
# Load Libraries ----------------------------------------------------------
library(caTools)
library(tidyverse)
library(xlsx)

# Import Data ---------------------------------------------------------------
dir_ex <- paste0("J:/deans/Presidents/SixSigma/Individual Folders",
                 "/Current Employees/Engineers/Anjelica Weber/Projects",
                 "/Data Mining Training Materials")
df_lin <- read.xlsx2(file = paste0(dir_ex, "/Datasets.xlsx"),
                     sheetName = "SimpleRegression 2")

# Pre-processing -----------------------------------------------------------
df_lin <- df_lin[1:10,] %>% mutate(xi = as.numeric(xi), yi = as.numeric(yi))

# Building Model ----------------------------------------------------------
lr_model <- lm(yi ~ xi, df_lin)

# Evaluating Model --------------------------------------------------------
model_stats <- summary(lr_model)
model_stats$coefficients
model_stats$r.squared
