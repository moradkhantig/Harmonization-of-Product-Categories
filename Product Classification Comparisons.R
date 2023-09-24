#### Read-Me ####
## This script takes in the predicted versus actual GPC brick codes and computes
## the number of correctly and incorrectly predicted GPC brick codes. These are
## reported as percentages

# Loading packages we might need
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr",repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(knitr)
library(kableExtra)

# The actual versus predicted GPC brick codes are in the form of excel files which are
# saved in the Clean Data folder on Github. These should be downloaded and saved
# locally.

setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Clean Data")

NB_ModelPredictions <- readxl::read_excel(path = "NaiveBayesModelPredictions.xlsx")
RF_ModelPrections <- readxl::read_excel(path = "RandomForestModelPredictions.xlsx")
knn_ModelPrections <- readxl::read_excel(path = "KnnModelPredictions.xlsx")
rp_ModelPrections <- readxl::read_excel(path = "ClassificationTreeModelPredictions.xlsx")

# In each datafrane create new column called Match which will display True if 
# the predicted and actual GPC brick codes are equal to each other and False if not.
NB_ModelPredictions$Match <- NB_ModelPredictions$PredictedGPCBrickCode %in% 
  NB_ModelPredictions$ActualGPCBrickCode

RF_ModelPrections$Match <- RF_ModelPrections$Predicted.GPCBrickCode %in%
  RF_ModelPrections$Actual.GPCBrickCode

knn_ModelPrections$Match <- knn_ModelPrections$Predicted.GPCBrickCode %in%
  knn_ModelPrections$Actual.GPCBrickCode

rp_ModelPrections$Match <- rp_ModelPrections$Predicted.GPCBrickCode %in%
  rp_ModelPrections$Actual.GPCBrickCode

# Report number of True and Fasle matches and create summary statistics dataframes
NB_PredictionTrue <- NB_ModelPredictions %>%
  filter(Match == "TRUE") %>%
  group_by(Match) %>%
  count(Match) %>%
  rename(TrueMatches = n)

NB_PredictionFalse <- NB_ModelPredictions %>%
  filter(Match == "FALSE") %>%
  group_by(Match) %>%
  count(Match) %>%
  rename(FalseMatches = n)

RF_PredictionTrue <-  RF_ModelPrections %>%
  filter(Match == "TRUE") %>%
  group_by(Match) %>%
  count(Match) %>%
  rename(TrueMatches = n)

RF_PredictionFalse <-  RF_ModelPrections %>%
  filter(Match == "FALSE") %>%
  group_by(Match) %>%
  count(Match) %>%
  rename(FalseMatches = n)

knn_PredictionTrue <- knn_ModelPrections %>%
  filter(Match == "TRUE") %>%
  group_by(Match) %>%
  count(Match) %>%
  rename(TrueMatches = n)

knn_PredictionFalse <- knn_ModelPrections %>%
  filter(Match == "FALSE") %>%
  group_by(Match) %>%
  count(Match) %>%
  rename(FalseMatches = n)

rp_PredictionTrue <- rp_ModelPrections %>%
  filter(Match == "TRUE") %>%
  group_by(Match) %>%
  count(Match) %>%
  rename(TrueMatches = n)

rp_PredictionFalse <- rp_ModelPrections %>%
  filter(Match == "FALSE") %>%
  group_by(Match) %>%
  count(Match) %>%
  rename(FalseMatches = n)


PredictionsStats <- data.frame("Model" = c("Multinomial Naive Bayes",
                                           "Random Forest",
                                           "Nearest Neigbor Algorithm",
                                           "Classification Tree"),
                               "True Predictions" = 
                                 c(NB_PredictionTrue$TrueMatches,
                                   RF_PredictionTrue$TrueMatches,
                                   knn_PredictionTrue$TrueMatches,
                                   rp_PredictionTrue$TrueMatches),
                               "False Predictions" = 
                                 c(NB_PredictionFalse$FalseMatches,
                                   RF_PredictionFalse$FalseMatches,
                                   knn_PredictionFalse$FalseMatches,
                                   rp_PredictionFalse$FalseMatches))
