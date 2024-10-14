library(readxl)
library(writexl)
library(caret)
library(smotefamily)
library(dplyr)
library(nnet)
library(broom)
library(pROC)
library(PRROC)
library(MLmetrics)
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

#Balancing - SMOTE

#K=5 is a common choice and often works well in practice, providing a good balance 
#between diversity and proximity of the synthetic examples.

#smote 0 and 1 classes
subset_smo1 <- train_db[train_db$outcome %in% c(0,1), ]  
smote_1 <- SMOTE(subset_smo1[, -8], subset_smo1$outcome, K = 5)
smote_1 <- smote_1$data  # 0  1  =  589 318
table(smote_1$class)

#smote 0 and 2 classes
subset_smo2 <- train_db[train_db$outcome %in% c(0,2), ]  
smote_2 <- SMOTE(subset_smo2[, -8], subset_smo2$outcome, K = 5)
smote_2 <- smote_2$data  # 0  2  =  589 589 
table(smote_2$class)

merged_smote <- full_join(smote_1, smote_2)
colnames(merged_smote)[8] <- "outcome"
table(merged_smote$outcome)       # 0  1  2 = 589 318 589


#Normalize the data

pre_smote <- preProcess(merged_smote[,c(1:2,4:7)], method = c("range"))
train_db_scaled <- predict(pre_smote, newdata = merged_smote)
test_db_scaled <- predict(pre_smote, newdata = test_db)
saveRDS(pre_smote, file = "pre_train.rds")

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

write_xlsx(train_db_scaled, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_train_smote.xlsx")
write_xlsx(test_db_scaled, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_test_smote.xlsx")


################## MLR WITHOUT FEATURE SELECTON USING SMOTE ######################

data_train_smote <- train_db_scaled
data_test_smote <- test_db_scaled

data_train_smote$outcome  <- relevel(data_train_smote$outcome , ref = "newborn")
data_test_smote$outcome   <- relevel(data_test_smote$outcome , ref = "newborn")

model_smote <- multinom(outcome ~ ., data = data_train_smote, maxit=500, trace=T)
summary(model_smote)

coefficients <- coef(model_smote)
odds_ratios <- exp(coefficients)
tidy_smote <- tidy(model_smote)
write.csv(tidy_smote, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Testing models/Coefs_models/coef_modelsmote.csv", row.names = FALSE)

important_variables <- varImp(model_smote)
important_variables$variables <- row.names(important_variables)
important_variables <- important_variables[order(-important_variables$Overall),]

predictions_smote <- predict(model_smote, newdata = data_test_smote, type = "class")
predictions_smote

confusion_matrix_smote <- confusionMatrix(predictions_smote, data_test_smote$outcome)
confusion_matrix_smote

confusion_smote <- table(predictions_smote, data_test_smote$outcome)
confusion_smote

accuracy_smote <- sum(diag(confusion_smote)) / sum(confusion_smote)
accuracy_smote

recall_smote <- diag(confusion_smote) / rowSums(confusion_smote)
recall_smote

precision_smote <- diag(confusion_smote) / colSums(confusion_smote)
precision_smote

f1_smote <- 2 * (precision_smote * recall_smote) / (precision_smote + recall_smote)
f1_smote

#Definition of the response variable

data_test_smote$outcome <- factor(data_test_smote$outcome, levels = c("newborn", "intra_fetal_death", "neonatal_death"))
predictions_smote2 <- predict(model_smote, newdata = data_test_smote, type = "probs")
predictions_smote2

class_lbs <- c("newborn", "intra_fetal_death", "neonatal_death")
auc_pr_smote <- numeric(length(class_lbs))
auc_roc_smote <- numeric(length(class_lbs))

counter <- 1

for (class_lb in class_lbs) {
  #create a binary variable "binary response" for the current class
  binary_response_smote <- ifelse(data_test_smote$outcome == class_lb, 1, 0)
  
  #calculates the AUC-PR for each class
  auc_pr_curve_smote <- pr.curve(scores.class0 = as.numeric(predictions_smote2[, class_lb]), weights.class0 = binary_response_smote)
  auc_pr_smote[counter] <- auc_pr_curve_smote$auc.integral
  
  #calculates the AUC-ROC for each class
  auc_roc_smote[counter] <- auc(roc(response = binary_response_smote, predictor = as.numeric(predictions_smote2[, class_lb])))

  counter <- counter + 1
}

#display of results
counter <- 1
for (class_lb in class_lbs) {
  cat("Class:", class_lb, "\n")
  cat("AUC-PR:", auc_pr_smote[counter], "\n")
  cat("AUC-ROC:", auc_roc_smote[counter], "\n")

  counter <- counter + 1
}


#Table with the results

results_smote <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                            Accuracy = accuracy_smote,
                            Recall = recall_smote,
                            Precision = precision_smote,
                            F1 = f1_smote,
                            AUC_PR = auc_pr_smote,
                            AUC_ROC = auc_roc_smote)

table_smote <- gt(results_smote, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_smote





