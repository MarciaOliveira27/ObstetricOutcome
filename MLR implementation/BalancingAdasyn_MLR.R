library(readxl)
library(writexl)
library(caret)
library(smotefamily)
library(dplyr)
library(MASS)
library(nnet)
library(pROC)
library(PRROC)
library(gt)


#Dividing

data <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_finished.xlsx")

set.seed(123)

#p = 0.7 - 70% of the data should be used for the training set, 
#while the remaining 30% should be used for the test set

split_indices <- createDataPartition(data$outcome, p = 0.7, list = FALSE)

train_db <- data[split_indices, ]
test_db <- data[-split_indices, ]

table(train_db$outcome)   # 0  1  2 =  589 318 19
table(test_db$outcome)    # 0  1  2 =  252 135 7

#classes 0 and 1
subset_ada1 <- train_db[train_db$outcome %in% c(0,1), ]  
adasyn_1 <- ADAS(subset_ada1[, -8], subset_ada1$outcome, K = 5)
adasyn_1 <- adasyn_1$data    # 0  1  = 589 545
table(adasyn_1$class)

#classes 0 and 2
subset_ada2 <- train_db[train_db$outcome %in% c(0,2), ]   
adasyn_2 <- ADAS(subset_ada2[, -8], subset_ada2$outcome, K = 5)
adasyn_2 <- adasyn_2$data  # 0  2  = 589 587
table(adasyn_2$class)

merged_adasyn <- full_join(adasyn_1, adasyn_2)
colnames(merged_adasyn)[8] <- "outcome"
table(merged_adasyn$outcome)       # 0  1  2 = 589 545 587


#Normalize the data

pre_adasyn <- preProcess(merged_adasyn[,c(1:2,4:7)], method = c("range"))
train_db_scaled <- predict(pre_adasyn, newdata = merged_adasyn)
test_db_scaled <- predict(pre_adasyn, newdata = test_db)
saveRDS(pre_smote, file = "pre_adasyn.rds")

train_db_scaled[, c(3)] <- round(train_db_scaled[, c(3)])
test_db_scaled[, c(3)] <- round(test_db_scaled[, c(3)])

#Convert to categorical variables

#"Fetalgender"

train_db_scaled$Fetalgender[train_db_scaled$Fetalgender == 0] <- "female"
train_db_scaled$Fetalgender[train_db_scaled$Fetalgender == 1] <- "male"
train_db_scaled$Fetalgender[train_db_scaled$Fetalgender == 2] <- "unknown"
test_db_scaled$Fetalgender[test_db_scaled$Fetalgender == 0] <- "female"
test_db_scaled$Fetalgender[test_db_scaled$Fetalgender == 1] <- "male"
test_db_scaled$Fetalgender[test_db_scaled$Fetalgender == 2] <- "unknown"

train_db_scaled$Fetalgender <- as.factor(train_db_scaled$Fetalgender)
test_db_scaled$Fetalgender <- as.factor(test_db_scaled$Fetalgender)

#"outcome"

train_db_scaled$outcome[train_db_scaled$outcome == 0] <- "newborn"
train_db_scaled$outcome[train_db_scaled$outcome == 1] <- "intra_fetal_death"
train_db_scaled$outcome[train_db_scaled$outcome == 2] <- "neonatal_death"
test_db_scaled$outcome[test_db_scaled$outcome == 0] <- "newborn"
test_db_scaled$outcome[test_db_scaled$outcome == 1] <- "intra_fetal_death"
test_db_scaled$outcome[test_db_scaled$outcome == 2] <- "neonatal_death"

train_db_scaled$outcome <- as.factor(train_db_scaled$outcome)
test_db_scaled$outcome <- as.factor(test_db_scaled$outcome)


write_xlsx(train_db_scaled, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_train_adasyn.xlsx")
write_xlsx(test_db_scaled, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_test_adasyn.xlsx")


################### MLR WITHOUT FEATURE SELECTON USING ADASYN ####################

data_train_adasyn <- train_db_scaled
data_test_adasyn <- test_db_scaled

data_train_adasyn$outcome  <- relevel(data_train_adasyn$outcome , ref = "newborn")
data_test_adasyn$outcome   <- relevel(data_test_adasyn$outcome , ref = "newborn")

model_adasyn <- multinom(outcome ~ ., data = data_train_adasyn, maxit=500, trace=T)
summary(model_adasyn)

coef(model_adasyn)
tidy_adasyn <- tidy(model_adasyn)
write.csv(tidy_adasyn, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Testing models/Coefs_models/coef_modeladasyn.csv", row.names = FALSE)

important_variables <- varImp(model_adasyn)
important_variables$variables <- row.names(important_variables)
important_variables <- important_variables[order(-important_variables$Overall),]

predictions_adasyn <- predict(model_adasyn, newdata = data_test_adasyn, type = "class")
predictions_adasyn

confusion_matrix_adasyn <- confusionMatrix(predictions_adasyn, data_test_adasyn$outcome)
confusion_matrix_adasyn

confusion_adasyn <- table(predictions_adasyn, data_test_adasyn$outcome)
confusion_adasyn

accuracy_adasyn <- sum(diag(confusion_adasyn)) / sum(confusion_adasyn)
accuracy_adasyn

recall_adasyn <- diag(confusion_adasyn) / rowSums(confusion_adasyn)
recall_adasyn

precision_adasyn <- diag(confusion_adasyn) / colSums(confusion_adasyn)
precision_adasyn

f1_adasyn <- 2 * (precision_adasyn * recall_adasyn) / (precision_adasyn + recall_adasyn)
f1_adasyn


#Definition of the response variable

data_test_adasyn$outcome <- factor(data_test_adasyn$outcome, levels = c("newborn", "intra_fetal_death", "neonatal_death"))
predictions_adasyn2 <- predict(model_adasyn, newdata = data_test_adasyn, type = "probs")
predictions_adasyn2

class_lbs <- c("newborn", "intra_fetal_death", "neonatal_death")
auc_pr_adasyn <- numeric(length(class_lbs))
auc_roc_adasyn <- numeric(length(class_lbs))

counter <- 1

for (class_lb in class_lbs) {
  #create a binary variable "binary response" for the current class
  binary_response_adasyn <- ifelse(data_test_adasyn$outcome == class_lb, 1, 0)
  
  #calculates the AUC-PR for each class
  auc_pr_curve_adasyn <- pr.curve(scores.class0 = as.numeric(predictions_adasyn2[, class_lb]), weights.class0 = binary_response_adasyn)
  auc_pr_adasyn[counter] <- auc_pr_curve_adasyn$auc.integral
  
  #calculates the AUC-ROC for each class
  auc_roc_adasyn[counter] <- auc(roc(response = binary_response_adasyn, predictor = as.numeric(predictions_adasyn2[, class_lb])))
  
  counter <- counter + 1
}

#display of results
counter <- 1
for (class_lb in class_lbs) {
  cat("Class:", class_lb, "\n")
  cat("AUC-PR:", auc_pr_adasyn[counter], "\n")
  cat("AUC-ROC:", auc_roc_adasyn[counter], "\n")

  counter <- counter + 1
}


#Table with the results

results_adasyn <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                      Accuracy = accuracy_adasyn,
                      Recall = recall_adasyn,
                      Precision = precision_adasyn,
                      F1 = f1_adasyn,
                      AUC_PR = auc_pr_adasyn,
                      AUC_ROC = auc_roc_adasyn)

table_adasyn <- gt(results_adasyn, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_adasyn


