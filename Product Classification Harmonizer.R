##### Read-Me ####
### This code is used for harmonization of product categories in the CPDat dataset
### Steps in the code include:
###   1.  Randomly selecting a subset of the CPDat dataset for purposes of increasing
###       computation time
###   2.  Performing pre-processing and word embedding of product categories in the CPDat
###       dataset
###   3.  Machine learning
###       a.  Splitting the CPDat dataset into a train and test set
###       b.  Training algorithms

##########################################################
# Start of code
#########################################################

# Load libraries we need

if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", 
                                      repos = "http://cran.us.r-project.org")
if(!require(writexl)) install.packages("writexl", 
                                       repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", 
                                       repos = "http://cran.us.r-project.org")
if(!require(superml)) install.packages("superml", 
                                       repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                       repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", 
                                     repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("naivebayes", 
                                          repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", 
                                          repos = "http://cran.us.r-project.org")

library(tidyverse)
library(readxl)
library(writexl)
library(stringr)
library(superml)
library(caret)
library(data.table)
library(naivebayes)
library(rpart)

####################
# Data preprocessing
####################

# Set working directory to read in the CPDat dataset. We need 
# to import are titled CPDat_FinalClean.

setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Clean Data")

CPDat <- read_excel(path = "CPDat_FinalClean.xlsx")

# Because the CPDat dataset is very large, we randomly select a smaller subset. This
# samller subset is saved as a new dataframe called CPDat_small. We randomly take
# 800 rows of CPDat and use those as the smaller subset of CPDat.

samp <- sample(nrow(CPDat),800)
CPDat_small <- CPDat[samp,]

# View the CPDat_small dataframe to verify that the sampling was correctly done
View(CPDat_small)

# Now we need to merge the data in ProductCategory1, ProductCategory2, and ProductCategory3
CPDat_small$ProductCategory <- paste(CPDat_small$ProductCategory1,
                                     CPDat_small$ProductCategory2,
                                     CPDat_small$ProductCategory3)

# Then drop ProductCategory1, ProductCategory2, and ProductCategory3
CPDat_small <- CPDat_small %>% select(-ProductCategory1,-ProductCategory2,-ProductCategory3)

# Finally we arrange the column order
CPDat_small <- CPDat_small %>% select(ProductName,ProductCategory,
                                      ProductUseCategoryDescription,
                                      ChemicalName,
                                      PREFERRED_NAME,
                                      CASRN)

# Lastly, we drop the ChemicaName column
CPDat_small <- CPDat_small %>% select(-ChemicalName)

# Now we export CPDat_small as a flat excel file to create a new column which will contain
# the GPC product category scheme to be used for machine learning algorithm training. We
# set the working directory as follows:

setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories")

write_xlsx(CPDat_small,"CPDat.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)

# Now we read the CPDat_small dataframe back in and this time we save it as CPDat_subset

CPDat_subset <- read_excel(path = "CPDatNew.xlsx")

##  We now use the following steps to preprocess the data in CPDat_subset. We perform
##  the preprocessing on the ProductName column as the entries in this
##  column will serve as the predictors.

# Convert uppercase to lowercase letters
CPDat_subset$ProductCategory <- tolower(CPDat_subset$ProductCategory)

# Trim leading and trailing spaces
CPDat_subset$ProductCategory <- trimws(CPDat_subset$ProductCategory,which = c("both"))

# Remove punctuation marks
CPDat_subset$ProductCategory <- gsub(",","",CPDat_subset$ProductCategory)
CPDat_subset$ProductCategory <- gsub(":","",CPDat_subset$ProductCategory)
CPDat_subset$ProductCategory <- gsub(";","",CPDat_subset$ProductCategory)
CPDat_subset$ProductCategory <- gsub("/","",CPDat_subset$ProductCategory)
CPDat_subset$ProductCategory <- gsub("|","",CPDat_subset$ProductCategory)

# Split CPDat_subset into test and train set
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = CPDat_subset$ProductName, times = 1, p = 0.2, list = FALSE)
train <- CPDat_subset[-test_index,]
test <- CPDat_subset[test_index,]

# Word embedding begins here
# Use the TfIdfVectorizer function to preprocess the ProductCategory column in
# CPDat_subset

# Generalize the class
tfv <- TfIdfVectorizer$new(max_features = 3000,remove_stopwords = TRUE,ngram_range = c(1,3))

# Fit on train data
tfv$fit(train$ProductCategory)

train_tf_features <- tfv$transform(train$ProductCategory)
test_tf_features <- tfv$transform(test$ProductCategory)

# Then need to convert train_tf_features and test_tf_features to either data.table or
# dataframes
train_tf_features <- as.data.frame(train_tf_features)
train_tf_features$GPCBrickCode <- as.factor(train$GPCBrickCode)
test_tf_features <- as.data.frame(test_tf_features)
test_tf_features$GPCBrickCode <- as.factor(test$GPCBrickCode)

##  Machine learning starts here. ##
# First train multinomial naive bayes
set.seed(30, sample.kind="Rounding")
NBnom <- multinomial_naive_bayes(x=train_tf_features[1:87],
                                 y=train_tf_features$GPCBrickCode)
NBnom_pred <- predict(NBnom,newdata = as.matrix(test_tf_features[1:87]))
confm_NBnom <- confusionMatrix(NBnom_pred,test_tf_features$GPCBrickCode)
NBnom_accuracy <- confm_NBnom$overall['Accuracy']

# As this is a multiclassification problem, the confusion matrix reports model
# performance statistics for each individual class. We are, however, interested
# in the overall performance statistics. To do that:
# First convert the confusion matrix to a matrix object
confm_NBnom_mat <- as.matrix(confm_NBnom)

# Set parameters, n and nc to be equal to the sum of instances and number of classes
n = sum(confm_NBnom_mat) # number of instances
nc = nrow(confm_NBnom_mat) # number of classes

# Calculate number of instances, number of predictions, and number of classified
# instances per class and store the results in rowsums, colsums, and diag.
rowsums = apply(confm_NBnom_mat, 1, sum) # number of instances per class
colsums = apply(confm_NBnom_mat, 2, sum) # number of predictions per class
diag = diag(confm_NBnom_mat)  # number of correctly classified instances per class 

# Report precision, recall, and the F1 score for each class
NB_precision = diag / colsums 
NB_recall = diag / rowsums 
NB_f1 = 2 * precision * recall / (precision + recall)

# Report the overall precision, recall, and the F1 score.
NB_macroPrecision = mean(NB_precision)
NB_macroRecall = mean(NB_recall)
NB_macroF1 = mean(NB_f1)

# Create two dataframes, ModelPerformance and NB_ModelPredictions. ModelPerformance will
# store the performance statistics and NB_ModelPredictions will store the predictions
# made by the multinomial naive bayes model and compare it with the actual values which are stored
# in test_tf_features.
ModelPerformance <- data.frame("Method" = c("Multinomial Naive Bayes"),
                               "Accuracy" = c(NBnom_accuracy),
                               "Precision" = c(NB_macroPrecision),
                               "Recall" = c(NB_macroRecall),
                               "F1 score" = c(NB_macroF1))

NB_ModelPredictions <- data.frame("PredictedGPCBrickCode" = test_tf_features$GPCBrickCode,
                                  "ActualGPCBrickCode" = NBnom_pred)

# Train Random Forest
set.seed(30, sample.kind="Rounding")

# First set tuning and control parameters.
#   For control parameters we choose a 10-fold cross validation repeated 3 times
#   The tuning parameter is mtry which we set as a vector running from 1 to the
#   square root of the total number of features in train_tf_features
trControl <- trainControl(method = "repeatedcv",number = 10,repeats = 3)

rf <- train(GPCBrickCode ~., data = train_tf_features,
            method = "rf",tuneGrid = data.frame(mtry = seq(1:sqrt(87))),
            trControl = trControl)
rf_pred <- predict(rf,newdata = test_tf_features)

# Calculate accuracy, precision, recall, and F1_score.
confm_rf <- confusionMatrix(rf_pred,test_tf_features$GPCBrickCode)
rf_accuracy <- confm_rf$overall['Accuracy']

confm_rf_matrix <- as.matrix(confm_rf)
n = sum(confm_rf_matrix)
nc = nrow(confm_rf_matrix)

rowsums = apply(confm_rf_matrix, 1, sum)
colsums = apply(confm_rf_matrix, 2, sum)
diag = diag(confm_rf_matrix)

rf_precision = diag / colsums 
rf_recall = diag / rowsums 
rf_f1 = 2 * precision * recall / (precision + recall)

# Report the overall precision, recall, and the F1 score.
rf_macroPrecision = mean(rf_precision)
rf_macroRecall = mean(rf_recall)
rf_macroF1 = mean(rf_f1)

# Add performance data to ModelPerformance and create a dataframe called
# RF_ModelPrections to store to predicted and actual GPC brick codes.
ModelPerformance <- data.frame("Method" = c("Multinomial Naive Bayes","Random Forest"),
                               "Accuracy" = c(NBnom_accuracy,rf_accuracy),
                               "Precision" = c(NB_macroPrecision,rf_macroPrecision),
                               "Recall" = c(NB_macroRecall,rf_macroRecall),
                               "F1 score" = c(NB_macroF1,rf_macroF1))

RF_ModelPrections <- data.frame("Predicted GPCBrickCode" = rf_pred,
                                "Actual GPCBrickCode" = test_tf_features$GPCBrickCode)

# Train nearest neighbor algorithm. Use the same train control parameters used above
# for random forest.
set.seed(30, sample.kind="Rounding")
trControl <- trainControl(method = "repeatedcv",number = 10,repeats = 3)
knn <- train(GPCBrickCode ~., data = train_tf_features,
      method = "knn",tuneGrid = data.frame(k = seq(1:25)),
      trControl = trControl)
knn_pred <- predict(knn,newdata = test_tf_features)

# Report accuracy, precision, recall, and the F1 score
confm_knn <- confusionMatrix(knn_pred,test_tf_features$GPCBrickCode)
knn_accuracy <- confm_knn$overall['Accuracy']

confm_knn_matrix <- as.matrix(confm_knn)
n = sum(confm_knn_matrix)
nc = nrow(confm_knn_matrix)

rowsums = apply(confm_knn_matrix, 1, sum)
colsums = apply(confm_knn_matrix, 2, sum)
diag = diag(confm_knn_matrix)

knn_precision = diag / colsums 
knn_recall = diag / rowsums 
knn_f1 = 2 * precision * recall / (precision + recall)

# Report the overall precision, recall, and the F1 score.
knn_macroPrecision = mean(knn_precision)
knn_macroRecall = mean(knn_recall)
knn_macroF1 = mean(knn_f1)

# Add performance statistics to ModelPerformance and create a dataframe called
# knn_ModelPrections to store to predicted and actual GPC brick codes.
ModelPerformance <- data.frame("Method" = c("Multinomial Naive Bayes",
                                            "Random Forest",
                                            "Nearest Neigbor Algorithm"),
                               "Accuracy" = c(NBnom_accuracy,
                                              rf_accuracy,
                                              knn_accuracy),
                               "Precision" = c(NB_macroPrecision,
                                               rf_macroPrecision,
                                               knn_macroPrecision),
                               "Recall" = c(NB_macroRecall,
                                            rf_macroRecall,
                                            knn_macroRecall),
                               "F1 score" = c(NB_macroF1,
                                              rf_macroF1,
                                              knn_macroF1))

knn_ModelPrections <- data.frame("Predicted GPCBrickCode" = knn_pred,
                                "Actual GPCBrickCode" = test_tf_features$GPCBrickCode)

# Train classification tree
set.seed(30, sample.kind="Rounding")
rp <- rpart(GPCBrickCode ~.,data = train_tf_features,
            method = "class")
rp_pred <- predict(rp,newdata = test_tf_features,type = "class")

# Report accuracy, precision, recall, and the F1 score
confm_rp <- confusionMatrix(rp_pred,test_tf_features$GPCBrickCode)
rp_accuracy <- confm_rp$overall['Accuracy']

confm_rp_matrix <- as.matrix(confm_rp)
n = sum(confm_rp_matrix)
nc = nrow(confm_rp_matrix)

rowsums = apply(confm_rp_matrix, 1, sum)
colsums = apply(confm_rp_matrix, 2, sum)
diag = diag(confm_rp_matrix)

rp_precision = diag / colsums 
rp_recall = diag / rowsums 
rp_f1 = 2 * precision * recall / (precision + recall)

# Report the overall precision, recall, and the F1 score.
rp_macroPrecision = mean(rp_precision)
rp_macroRecall = mean(rp_recall)
rp_macroF1 = mean(rp_f1)

# Add performance statistics to ModelPerformance and create a dataframe called
# rp_ModelPrections to store to predicted and actual GPC brick codes.
ModelPerformance <- data.frame("Method" = c("Multinomial Naive Bayes",
                                            "Random Forest",
                                            "Nearest Neigbor Algorithm",
                                            "Classification Tree"),
                               "Accuracy" = c(NBnom_accuracy,
                                              rf_accuracy,
                                              knn_accuracy,
                                              rp_accuracy),
                               "Precision" = c(NB_macroPrecision,
                                               rf_macroPrecision,
                                               knn_macroPrecision,
                                               rp_macroPrecision),
                               "Recall" = c(NB_macroRecall,
                                            rf_macroRecall,
                                            knn_macroRecall,
                                            rp_macroRecall),
                               "F1 score" = c(NB_macroF1,
                                              rf_macroF1,
                                              knn_macroF1,
                                              rp_macroF1))

rp_ModelPrections <- data.frame("Predicted GPCBrickCode" = rp_pred,
                                 "Actual GPCBrickCode" = test_tf_features$GPCBrickCode)

# Export model performance statistics and model predictions as flat files and
# save them in Clean Data
setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Clean Data")

write_xlsx(ModelPerformance,"ModelPerformanceStatistics.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)
write_xlsx(NB_ModelPredictions,"NaiveBayesModelPredictions.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)
write_xlsx(RF_ModelPrections,"RandomForestModelPredictions.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)
write_xlsx(knn_ModelPrections,"KnnModelPredictions.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)
write_xlsx(rp_ModelPrections,"ClassificationTreeModelPredictions.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)




