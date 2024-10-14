library(readxl)
library(writexl)
library(caret)
library(nnet)
library(pROC)
library(PRROC)
library(dplyr)
library(gt)
library(broom)


#Dividing

data <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_finished.xlsx")
table(data$outcome) 

set.seed(123)

#p = 0.7 - 70% of the data should be used for the training set, 
#while the remaining 30% should be used for the test set

split_indices <- createDataPartition(data$outcome, p = 0.7, list = FALSE)

train_db <- data[split_indices, ]
test_db <- data[-split_indices, ]

table(train_db$outcome)   # 0  1  2 =  589 318 19
table(test_db$outcome)    # 0  1  2 =  252 135 7


#Normalize the data

pre_wb <- preProcess(train_db[,c(1:2,4:7)], method = c("range"))
train_db_scaled <- predict(pre_wb, newdata = train_db)
test_db_scaled <- predict(pre_wb, newdata = test_db)

train_db_scaled[, c(1:2,4:7)] <- round(train_db_scaled[, c(1:2,4:7)],  digits = 2)
test_db_scaled[, c(1:2,4:7)] <- round(test_db_scaled[, c(1:2,4:7)],  digits = 2)

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

write_xlsx(train_db_scaled, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_train_wb.xlsx")
write_xlsx(test_db_scaled, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_test_wb.xlsx")


################### MLR WITHOUT BALANCING AND FEATURE SELECTON ####################

data_train_wb <- train_db_scaled
data_test_wb <- test_db_scaled


data_train_wb$outcome  <- relevel(data_train_wb$outcome , ref = "newborn")
data_test_wb$outcome   <- relevel(data_test_wb$outcome , ref = "newborn")

model_wb <- multinom(outcome ~ ., data = data_train_wb)
summary(model_wb)

coef(model_wb)
tidy_wb <- tidy(model_wb)
write.csv(tidy_wb, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Testing models/Coefs_models/coef_modelwb.csv", row.names = FALSE)

important_variables <- varImp(model_wb)
important_variables$variables <- row.names(important_variables)
important_variables <- important_variables[order(-important_variables$Overall),]


predictions_wb <- predict(model_wb, newdata = data_test_wb, type = "class")
predictions_wb

confusion_matrix_wb <- confusionMatrix(predictions_wb, data_test_wb$outcome)
confusion_matrix_wb

confusion_wb <- table(predictions_wb, data_test_wb$outcome)
confusion_wb

accuracy_wb <- sum(diag(confusion_wb)) / sum(confusion_wb)
accuracy_wb

recall_wb <- diag(confusion_wb) / rowSums(confusion_wb)
recall_wb

precision_wb <- diag(confusion_wb) / colSums(confusion_wb)
precision_wb

f1_wb <- 2 * (precision_wb * recall_wb) / (precision_wb + recall_wb)
f1_wb


#Definition of the response variable

data_test_wb$outcome <- factor(data_test_wb$outcome, levels = c("newborn", "intra_fetal_death", "neonatal_death"))
predictions_wb2 <- predict(model_wb, newdata = data_test_wb, type = "probs")
predictions_wb2

class_lbs <- c("newborn", "intra_fetal_death", "neonatal_death")
auc_pr_wb <- numeric(length(class_lbs))
auc_roc_wb <- numeric(length(class_lbs))

counter <- 1

for (class_lb in class_lbs) {
  
  #create a binary variable "binary response" for the current class
  binary_response_wb <- ifelse(data_test_wb$outcome == class_lb, 1, 0)
  
  #calculates the AUC-PR for each class
  auc_pr_curve_wb <- pr.curve(scores.class0 = as.numeric(predictions_wb2[, class_lb]), weights.class0 = binary_response_wb)
  auc_pr_wb[counter] <- auc_pr_curve_wb$auc.integral
  
  #calculates the AUC-ROC for each class
  auc_roc_wb[counter] <- auc(roc(response = binary_response_wb, predictor = as.numeric(predictions_wb2[, class_lb])))
  
  counter <- counter + 1
}

#display of results
counter <- 1
for (class_lb in class_lbs) {
  cat("Class:", class_lb, "\n")
  cat("AUC-PR:", auc_pr_wb[counter], "\n")
  cat("AUC-ROC:", auc_roc_wb[counter], "\n")
  
  counter <- counter + 1
}


#Table with the results

results_wb <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                             Accuracy = accuracy_wb,
                             Recall = recall_wb,
                             Precision = precision_wb,
                             F1 = f1_wb,
                             AUC_PR = auc_pr_wb,
                             AUC_ROC = auc_roc_wb)

table_wb <- gt(results_wb, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_wb
