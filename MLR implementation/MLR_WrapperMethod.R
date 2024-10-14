library(readxl)
library(caret)
library(nnet)
library(MASS)
library(broom)

#DATASET WITHOUT BALANCING

######################## WRAPPER METHOD - FORWARD #############################


d_train <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_train_wb.xlsx")
d_test <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_test_wb.xlsx")

d_train$outcome <- as.factor(d_train$outcome)
d_test$outcome <- as.factor(d_test$outcome)

d_train$outcome  <- relevel(d_train$outcome , ref = "newborn")
d_test$outcome   <- relevel(d_test$outcome , ref = "newborn")

model_fw <- multinom(outcome ~ ., data = d_train)
summary(model_fw)

model_fw_final <- step(model_fw, direction = "forward")
summary(model_fw_final)
coef(model_fw_final)

tidy_wb_fw <- tidy(model_fw_final)
write.csv(tidy_wb_fw, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Testing models/Coefs_models/coef_wb_fw.csv", row.names = FALSE)

used_vars <- colnames(attr(model_fw_final$terms, "factors"))
used_vars
important_variables <- varImp(model_fw_final)
important_variables$variables <- row.names(important_variables)
important_variables <- important_variables[order(-important_variables$Overall),]

predictions_fw <- predict(model_fw_final, newdata = d_test, type = "class")
predictions_fw

d_test$outcome <- factor(d_test$outcome, levels = levels(predictions_fw))

confusion_matrix_fw <- confusionMatrix(predictions_fw, d_test$outcome)
confusion_matrix_fw

confusion_fw <- table(predictions_fw, d_test$outcome)
confusion_fw

accuracy_fw <- sum(diag(confusion_fw)) / sum(confusion_fw)
accuracy_fw

recall_fw <- diag(confusion_fw) / rowSums(confusion_fw)
recall_fw

precision_fw <- diag(confusion_fw) / colSums(confusion_fw)
precision_fw

f1_fw <- 2 * (precision_fw * recall_fw) / (precision_fw + recall_fw)
f1_fw

predictions_fw2 <- predict(model_fw, newdata = d_test, type = "probs")
predictions_fw2

class_lbs <- c("newborn", "intra_fetal_death", "neonatal_death")
auc_pr_fw <- numeric(length(class_lbs))
auc_roc_fw <- numeric(length(class_lbs))

counter <- 1

for (class_lb in class_lbs) {
  #create a binary variable "binary response" for the current class
  binary_response_fw <- ifelse(d_test$outcome == class_lb, 1, 0)
  
  #calculates the AUC-PR for each class
  auc_pr_curve_fw <- pr.curve(scores.class0 = as.numeric(predictions_fw2[, class_lb]), weights.class0 = binary_response_fw)
  auc_pr_fw[counter] <- auc_pr_curve_fw$auc.integral
  
  #calculates the AUC-ROC for each class
  auc_roc_fw[counter] <- auc(roc(response = binary_response_fw, predictor = as.numeric(predictions_fw2[, class_lb])))
  
  counter <- counter + 1
}

#display of results
counter <- 1
for (class_lb in class_lbs) {
  cat("Class:", class_lb, "\n")
  cat("AUC-PR:", auc_pr_fw[counter], "\n")
  cat("AUC-ROC:", auc_roc_fw[counter], "\n")
  
  counter <- counter + 1
}


#Table with the results

results_fw <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                             Accuracy = accuracy_fw,
                             Recall = recall_fw,
                             Precision = precision_fw,
                             F1 = f1_fw,
                             AUC_PR = auc_pr_fw,
                             AUC_ROC = auc_roc_fw)

table_fw <- gt(results_fw, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_fw

######################## WRAPPER METHOD - BACKWARD #############################


model_bw <- multinom(outcome ~ ., data = d_train)
summary(model_bw)

model_bw_final <- stepAIC(model_bw, direction = "backward")
summary(model_bw_final)
coef(model_bw_final)

tidy_wb_bw <- tidy(model_bw_final)
write.csv(tidy_wb_bw, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Testing models/Coefs_models/coef_wb_bw.csv", row.names = FALSE)

used_vars <- colnames(attr(model_bw_final$terms, "factors"))
used_vars
important_variables <- varImp(model_bw_final)
important_variables$variables <- row.names(important_variables)
important_variables <- important_variables[order(-important_variables$Overall),]

predictions_bw <- predict(model_bw_final, newdata = d_test, type = "class")
predictions_bw

d_test$outcome <- factor(d_test$outcome, levels = levels(predictions_bw))

confusion_matrix_bw <- confusionMatrix(predictions_bw, d_test$outcome)
confusion_matrix_bw
confusion_bw <- table(predictions_bw, d_test$outcome)
confusion_bw

accuracy_bw <- sum(diag(confusion_bw)) / sum(confusion_bw)
accuracy_bw

recall_bw <- diag(confusion_bw) / rowSums(confusion_bw)
recall_bw

precision_bw <- diag(confusion_bw) / colSums(confusion_bw)
precision_bw

f1_bw <- 2 * (precision_bw * recall_bw) / (precision_bw + recall_bw)
f1_bw

predictions_bw2 <- predict(model_bw, newdata = d_test, type = "probs")
predictions_bw2

class_lbs <- c("newborn", "intra_fetal_death", "neonatal_death")
auc_pr_bw <- numeric(length(class_lbs))
auc_roc_bw <- numeric(length(class_lbs))

counter <- 1

for (class_lb in class_lbs) {
  #create a binary variable "binary response" for the current class
  binary_response_bw <- ifelse(d_test$outcome == class_lb, 1, 0)
  
  #calculates the AUC-PR for each class
  auc_pr_curve_bw <- pr.curve(scores.class0 = as.numeric(predictions_bw2[, class_lb]), weights.class0 = binary_response_bw)
  auc_pr_bw[counter] <- auc_pr_curve_bw$auc.integral
  
  #calculates the AUC-ROC for each class
  auc_roc_bw[counter] <- auc(roc(response = binary_response_bw, predictor = as.numeric(predictions_bw2[, class_lb])))
  
  counter <- counter + 1
}

#display of results
counter <- 1
for (class_lb in class_lbs) {
  cat("Class:", class_lb, "\n")
  cat("AUC-PR:", auc_pr_bw[counter], "\n")
  cat("AUC-ROC:", auc_roc_bw[counter], "\n")
  
  counter <- counter + 1
}


#Table with the results

results_bw <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                         Accuracy = accuracy_bw,
                         Recall = recall_bw,
                         Precision = precision_bw,
                         F1 = f1_bw,
                         AUC_PR = auc_pr_bw,
                         AUC_ROC = auc_roc_bw)

table_bw <- gt(results_bw, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_bw


######################## WRAPPER METHOD - BIDIRECTIONAL ############################

model_bd <- multinom(outcome ~ ., data = d_train)
summary(model_bd)

model_bd_final <- stepAIC(model_bd, direction = "both")
summary(model_bd_final)
coef(model_bd_final)

tidy_wb_bd <- tidy(model_bd_final)
write.csv(tidy_wb_bd, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Testing models/Coefs_models/coef_wb_bd.csv", row.names = FALSE)

used_vars <- colnames(attr(model_bd_final$terms, "factors"))
used_vars
important_variables <- varImp(model_bw_final)
important_variables$variables <- row.names(important_variables)
important_variables <- important_variables[order(-important_variables$Overall),]

predictions_bd <- predict(model_bd_final, newdata = d_test, type = "class")
predictions_bd

d_test$outcome <- factor(d_test$outcome, levels = levels(predictions_bd))

confusion_matrix_bd <- confusionMatrix(predictions_bd, d_test$outcome)
confusion_matrix_bd
confusion_bd <- table(predictions_bd, d_test$outcome)
confusion_bd

used_vars <- colnames(attr(model_bd_final$terms, "factors"))
used_vars

accuracy_bd <- sum(diag(confusion_bd)) / sum(confusion_bd)
accuracy_bd

recall_bd <- diag(confusion_bd) / rowSums(confusion_bd)
recall_bd

precision_bd <- diag(confusion_bd) / colSums(confusion_bd)
precision_bd

f1_bd <- 2 * (precision_bd * recall_bd) / (precision_bd + recall_bd)
f1_bd

predictions_bd2 <- predict(model_bd, newdata = d_test, type = "probs")
predictions_bd2

class_lbs <- c("newborn", "intra_fetal_death", "neonatal_death")
auc_pr_bd <- numeric(length(class_lbs))
auc_roc_bd <- numeric(length(class_lbs))

counter <- 1

for (class_lb in class_lbs) {
  #create a binary variable "binary response" for the current class
  binary_response_bd <- ifelse(d_test$outcome == class_lb, 1, 0)
  
  #calculates the AUC-PR for each class
  auc_pr_curve_bd <- pr.curve(scores.class0 = as.numeric(predictions_bd2[, class_lb]), weights.class0 = binary_response_bd)
  auc_pr_bd[counter] <- auc_pr_curve_bd$auc.integral
  
  #calculates the AUC-ROC for each class
  auc_roc_bd[counter] <- auc(roc(response = binary_response_bd, predictor = as.numeric(predictions_bd2[, class_lb])))
  
  counter <- counter + 1
}

#display of results
counter <- 1
for (class_lb in class_lbs) {
  cat("Class:", class_lb, "\n")
  cat("AUC-PR:", auc_pr_bd[counter], "\n")
  cat("AUC-ROC:", auc_roc_bd[counter], "\n")
  
  counter <- counter + 1
}


#Table with the results

results_bd <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                         Accuracy = accuracy_bd,
                         Recall = recall_bd,
                         Precision = precision_bd,
                         F1 = f1_bd,
                         AUC_PR = auc_pr_bd,
                         AUC_ROC = auc_roc_bd)

table_bd <- gt(results_bd, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_bd

#DATASET WITH SMOTE

######################## WRAPPER METHOD - FORWARD #############################

b_train <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_train_smote.xlsx")
b_test <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_test_smote.xlsx")

b_train$outcome <- as.factor(b_train$outcome)
b_test$outcome <- as.factor(b_test$outcome)

b_train$outcome  <- relevel(b_train$outcome , ref = "newborn")
b_test$outcome   <- relevel(b_test$outcome , ref = "newborn")

model_fw <- multinom(outcome ~ ., data = b_train)
summary(model_fw)

model_fw_final <- stepAIC(model_fw, direction = "forward")
summary(model_fw_final)
coef(model_fw_final)

tidy_smote_fw <- tidy(model_fw_final)
write.csv(tidy_smote_fw, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Testing models/Coefs_models/coef_smote_fw.csv", row.names = FALSE)

used_vars <- colnames(attr(model_fw_final$terms, "factors"))
used_vars
important_variables <- varImp(model_fw_final)
important_variables$variables <- row.names(important_variables)
important_variables <- important_variables[order(-important_variables$Overall),]

predictions_fw <- predict(model_fw_final, newdata = d_test, type = "class")
predictions_fw

d_test$outcome <- factor(d_test$outcome, levels = levels(predictions_fw))

confusion_matrix_fw <- confusionMatrix(predictions_fw, d_test$outcome)
confusion_matrix_fw

confusion_fw <- table(predictions_fw, d_test$outcome)
confusion_fw

accuracy_fw <- sum(diag(confusion_fw)) / sum(confusion_fw)
accuracy_fw

recall_fw <- diag(confusion_fw) / rowSums(confusion_fw)
recall_fw

precision_fw <- diag(confusion_fw) / colSums(confusion_fw)
precision_fw

f1_fw <- 2 * (precision_fw * recall_fw) / (precision_fw + recall_fw)
f1_fw

predictions_fw2 <- predict(model_fw, newdata = d_test, type = "probs")
predictions_fw2

class_lbs <- c("newborn", "intra_fetal_death", "neonatal_death")
auc_pr_fw <- numeric(length(class_lbs))
auc_roc_fw <- numeric(length(class_lbs))

counter <- 1

for (class_lb in class_lbs) {
  #create a binary variable "binary response" for the current class
  binary_response_fw <- ifelse(d_test$outcome == class_lb, 1, 0)
  
  #calculates the AUC-PR for each class
  auc_pr_curve_fw <- pr.curve(scores.class0 = as.numeric(predictions_fw2[, class_lb]), weights.class0 = binary_response_fw)
  auc_pr_fw[counter] <- auc_pr_curve_fw$auc.integral
  
  #calculates the AUC-ROC for each class
  auc_roc_fw[counter] <- auc(roc(response = binary_response_fw, predictor = as.numeric(predictions_fw2[, class_lb])))
  
  counter <- counter + 1
}

#display of results
counter <- 1
for (class_lb in class_lbs) {
  cat("Class:", class_lb, "\n")
  cat("AUC-PR:", auc_pr_fw[counter], "\n")
  cat("AUC-ROC:", auc_roc_fw[counter], "\n")
  
  counter <- counter + 1
}


#Table with the results

results_fw <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                         Accuracy = accuracy_fw,
                         Recall = recall_fw,
                         Precision = precision_fw,
                         F1 = f1_fw,
                         AUC_PR = auc_pr_fw,
                         AUC_ROC = auc_roc_fw)

table_fw <- gt(results_fw, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_fw

######################## WRAPPER METHOD - BACKWARD #############################

model_bw <- multinom(outcome ~ ., data = b_train)
summary(model_bw)

model_bw_final <- stepAIC(model_bw, direction = "backward")
summary(model_bw_final)
coef(model_bw_final)

tidy_smote_bw <- tidy(model_bw_final)
write.csv(tidy_smote_bw, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Testing models/Coefs_models/coef_smote_bw.csv", row.names = FALSE)

used_vars <- colnames(attr(model_bw_final$terms, "factors"))
used_vars
important_variables <- varImp(model_bw_final)
important_variables$variables <- row.names(important_variables)
important_variables <- important_variables[order(-important_variables$Overall),]

predictions_bw <- predict(model_bw_final, newdata = d_test, type = "class")
predictions_bw

d_test$outcome <- factor(d_test$outcome, levels = levels(predictions_bw))

confusion_matrix_bw <- confusionMatrix(predictions_bw, d_test$outcome)
confusion_matrix_bw
confusion_bw <- table(predictions_bw, d_test$outcome)
confusion_bw

accuracy_bw <- sum(diag(confusion_bw)) / sum(confusion_bw)
accuracy_bw

recall_bw <- diag(confusion_bw) / rowSums(confusion_bw)
recall_bw

precision_bw <- diag(confusion_bw) / colSums(confusion_bw)
precision_bw

f1_bw <- 2 * (precision_bw * recall_bw) / (precision_bw + recall_bw)
f1_bw

predictions_bw2 <- predict(model_bw, newdata = d_test, type = "probs")
predictions_bw2

class_lbs <- c("newborn", "intra_fetal_death", "neonatal_death")
auc_pr_bw <- numeric(length(class_lbs))
auc_roc_bw <- numeric(length(class_lbs))

counter <- 1

for (class_lb in class_lbs) {
  #create a binary variable "binary response" for the current class
  binary_response_bw <- ifelse(d_test$outcome == class_lb, 1, 0)
  
  #calculates the AUC-PR for each class
  auc_pr_curve_bw <- pr.curve(scores.class0 = as.numeric(predictions_bw2[, class_lb]), weights.class0 = binary_response_bw)
  auc_pr_bw[counter] <- auc_pr_curve_bw$auc.integral
  
  #calculates the AUC-ROC for each class
  auc_roc_bw[counter] <- auc(roc(response = binary_response_bw, predictor = as.numeric(predictions_bw2[, class_lb])))
  
  counter <- counter + 1
}

#display of results
counter <- 1
for (class_lb in class_lbs) {
  cat("Class:", class_lb, "\n")
  cat("AUC-PR:", auc_pr_bw[counter], "\n")
  cat("AUC-ROC:", auc_roc_bw[counter], "\n")
  
  counter <- counter + 1
}


#Table with the results

results_bw <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                         Accuracy = accuracy_bw,
                         Recall = recall_bw,
                         Precision = precision_bw,
                         F1 = f1_bw,
                         AUC_PR = auc_pr_bw,
                         AUC_ROC = auc_roc_bw)

table_bw <- gt(results_bw, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_bw


######################## WRAPPER METHOD - BIDIRECTIONAL ############################

model_bd <- multinom(outcome ~ ., data = b_train)
summary(model_bd)

model_bd_final <- stepAIC(model_bd, direction = "both")
summary(model_bd_final)
coef(model_bd_final)

tidy_smote_bd <- tidy(model_bd_final)
write.csv(tidy_smote_bd, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Testing models/Coefs_models/coef_smote_bd.csv", row.names = FALSE)

used_vars <- colnames(attr(model_bd_final$terms, "factors"))
used_vars
important_variables <- varImp(model_bw_final)
important_variables$variables <- row.names(important_variables)
important_variables <- important_variables[order(-important_variables$Overall),]

predictions_bd <- predict(model_bd_final, newdata = d_test, type = "class")
predictions_bd

d_test$outcome <- factor(d_test$outcome, levels = levels(predictions_bd))

confusion_matrix_bd <- confusionMatrix(predictions_bd, d_test$outcome)
confusion_matrix_bd
confusion_bd <- table(predictions_bd, d_test$outcome)
confusion_bd

used_vars <- colnames(attr(model_bd_final$terms, "factors"))
used_vars

accuracy_bd <- sum(diag(confusion_bd)) / sum(confusion_bd)
accuracy_bd

recall_bd <- diag(confusion_bd) / rowSums(confusion_bd)
recall_bd

precision_bd <- diag(confusion_bd) / colSums(confusion_bd)
precision_bd

f1_bd <- 2 * (precision_bd * recall_bd) / (precision_bd + recall_bd)
f1_bd

predictions_bd2 <- predict(model_bd, newdata = d_test, type = "probs")
predictions_bd2

class_lbs <- c("newborn", "intra_fetal_death", "neonatal_death")
auc_pr_bd <- numeric(length(class_lbs))
auc_roc_bd <- numeric(length(class_lbs))

counter <- 1

for (class_lb in class_lbs) {
  #create a binary variable "binary response" for the current class
  binary_response_bd <- ifelse(d_test$outcome == class_lb, 1, 0)
  
  #calculates the AUC-PR for each class
  auc_pr_curve_bd <- pr.curve(scores.class0 = as.numeric(predictions_bd2[, class_lb]), weights.class0 = binary_response_bd)
  auc_pr_bd[counter] <- auc_pr_curve_bd$auc.integral
  
  #calculates the AUC-ROC for each class
  auc_roc_bd[counter] <- auc(roc(response = binary_response_bd, predictor = as.numeric(predictions_bd2[, class_lb])))
  
  counter <- counter + 1
}

#display of results
counter <- 1
for (class_lb in class_lbs) {
  cat("Class:", class_lb, "\n")
  cat("AUC-PR:", auc_pr_bd[counter], "\n")
  cat("AUC-ROC:", auc_roc_bd[counter], "\n")
  
  counter <- counter + 1
}


#Table with the results

results_bd <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                         Accuracy = accuracy_bd,
                         Recall = recall_bd,
                         Precision = precision_bd,
                         F1 = f1_bd,
                         AUC_PR = auc_pr_bd,
                         AUC_ROC = auc_roc_bd)

table_bd <- gt(results_bd, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_bd

#DATASET WITH ADASYN

######################## WRAPPER METHOD - FORWARD #############################


b_train <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_train_adasyn.xlsx")
b_test <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_test_adasyn.xlsx")

b_train$outcome <- as.factor(b_train$outcome)
b_test$outcome <- as.factor(b_test$outcome)

b_train$outcome  <- relevel(b_train$outcome , ref = "newborn")
b_test$outcome   <- relevel(b_test$outcome , ref = "newborn")

model_fw <- multinom(outcome ~ ., data = b_train)
summary(model_fw)

model_fw_final <- stepAIC(model_fw, direction = "forward")
summary(model_fw_final)
coef(model_fw_final)

tidy_adasyn_fw <- tidy(model_fw_final)
write.csv(tidy_adasyn_fw, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Testing models/Coefs_models/coef_adasyn_fw.csv", row.names = FALSE)

used_vars <- colnames(attr(model_fw_final$terms, "factors"))
used_vars
important_variables <- varImp(model_fw_final)
important_variables$variables <- row.names(important_variables)
important_variables <- important_variables[order(-important_variables$Overall),]


predictions_fw <- predict(model_fw_final, newdata = d_test, type = "class")
predictions_fw

d_test$outcome <- factor(d_test$outcome, levels = levels(predictions_fw))

confusion_matrix_fw <- confusionMatrix(predictions_fw, d_test$outcome)
confusion_matrix_fw

confusion_fw <- table(predictions_fw, d_test$outcome)
confusion_fw

accuracy_fw <- sum(diag(confusion_fw)) / sum(confusion_fw)
accuracy_fw

recall_fw <- diag(confusion_fw) / rowSums(confusion_fw)
recall_fw

precision_fw <- diag(confusion_fw) / colSums(confusion_fw)
precision_fw

f1_fw <- 2 * (precision_fw * recall_fw) / (precision_fw + recall_fw)
f1_fw

predictions_fw2 <- predict(model_fw, newdata = d_test, type = "probs")
predictions_fw2

class_lbs <- c("newborn", "intra_fetal_death", "neonatal_death")
auc_pr_fw <- numeric(length(class_lbs))
auc_roc_fw <- numeric(length(class_lbs))

counter <- 1

for (class_lb in class_lbs) {
  #create a binary variable "binary response" for the current class
  binary_response_fw <- ifelse(d_test$outcome == class_lb, 1, 0)
  
  #calculates the AUC-PR for each class
  auc_pr_curve_fw <- pr.curve(scores.class0 = as.numeric(predictions_fw2[, class_lb]), weights.class0 = binary_response_fw)
  auc_pr_fw[counter] <- auc_pr_curve_fw$auc.integral
  
  #calculates the AUC-ROC for each class
  auc_roc_fw[counter] <- auc(roc(response = binary_response_fw, predictor = as.numeric(predictions_fw2[, class_lb])))
  
  counter <- counter + 1
}

#display of results
counter <- 1
for (class_lb in class_lbs) {
  cat("Class:", class_lb, "\n")
  cat("AUC-PR:", auc_pr_fw[counter], "\n")
  cat("AUC-ROC:", auc_roc_fw[counter], "\n")
  
  counter <- counter + 1
}


#Table with the results

results_fw <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                         Accuracy = accuracy_fw,
                         Recall = recall_fw,
                         Precision = precision_fw,
                         F1 = f1_fw,
                         AUC_PR = auc_pr_fw,
                         AUC_ROC = auc_roc_fw)

table_fw <- gt(results_fw, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_fw


######################## WRAPPER METHOD - BACKWARD #############################

model_bw <- multinom(outcome ~ ., data = b_train)
summary(model_bw)

model_bw_final <- stepAIC(model_bw, direction = "backward")
summary(model_bw_final)
coef(model_bw_final)

tidy_adasyn_bw <- tidy(model_bw_final)
write.csv(tidy_adasyn_bw, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Testing models/Coefs_models/coef_adasyn_bw.csv", row.names = FALSE)

used_vars <- colnames(attr(model_bw_final$terms, "factors"))
used_vars
important_variables <- varImp(model_bw_final)
important_variables$variables <- row.names(important_variables)
important_variables <- important_variables[order(-important_variables$Overall),]


predictions_bw <- predict(model_bw_final, newdata = d_test, type = "class")
predictions_bw

d_test$outcome <- factor(d_test$outcome, levels = levels(predictions_bw))

confusion_matrix_bw <- confusionMatrix(predictions_bw, d_test$outcome)
confusion_matrix_bw
confusion_bw <- table(predictions_bw, d_test$outcome)
confusion_bw

used_vars <- colnames(attr(model_bw_final$terms, "factors"))
used_vars

accuracy_bw <- sum(diag(confusion_bw)) / sum(confusion_bw)
accuracy_bw

recall_bw <- diag(confusion_bw) / rowSums(confusion_bw)
recall_bw

precision_bw <- diag(confusion_bw) / colSums(confusion_bw)
precision_bw

f1_bw <- 2 * (precision_bw * recall_bw) / (precision_bw + recall_bw)
f1_bw

predictions_bw2 <- predict(model_bw, newdata = d_test, type = "probs")
predictions_bw2

class_lbs <- c("newborn", "intra_fetal_death", "neonatal_death")
auc_pr_bw <- numeric(length(class_lbs))
auc_roc_bw <- numeric(length(class_lbs))

counter <- 1

for (class_lb in class_lbs) {
  #create a binary variable "binary response" for the current class
  binary_response_bw <- ifelse(d_test$outcome == class_lb, 1, 0)
  
  #calculates the AUC-PR for each class
  auc_pr_curve_bw <- pr.curve(scores.class0 = as.numeric(predictions_bw2[, class_lb]), weights.class0 = binary_response_bw)
  auc_pr_bw[counter] <- auc_pr_curve_bw$auc.integral
  
  #calculates the AUC-ROC for each class
  auc_roc_bw[counter] <- auc(roc(response = binary_response_bw, predictor = as.numeric(predictions_bw2[, class_lb])))
  
  counter <- counter + 1
}

#display of results
counter <- 1
for (class_lb in class_lbs) {
  cat("Class:", class_lb, "\n")
  cat("AUC-PR:", auc_pr_bw[counter], "\n")
  cat("AUC-ROC:", auc_roc_bw[counter], "\n")
  
  counter <- counter + 1
}


#Table with the results

results_bw <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                         Accuracy = accuracy_bw,
                         Recall = recall_bw,
                         Precision = precision_bw,
                         F1 = f1_bw,
                         AUC_PR = auc_pr_bw,
                         AUC_ROC = auc_roc_bw)

table_bw <- gt(results_bw, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_bw


######################## WRAPPER METHOD - BIDIRECTIONAL ############################


model_bd <- multinom(outcome ~ ., data = b_train)
summary(model_bd)

model_bd_final <- stepAIC(model_bd, direction = "both")
summary(model_bd_final)
coef(model_bd_final)

tidy_adasyn_bd <- tidy(model_bd_final)
write.csv(tidy_adasyn_bd, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Testing models/Coefs_models/coef_adasyn_bd.csv", row.names = FALSE)

used_vars <- colnames(attr(model_bd_final$terms, "factors"))
used_vars
important_variables <- varImp(model_bd_final)
important_variables$variables <- row.names(important_variables)
important_variables <- important_variables[order(-important_variables$Overall),]


predictions_bd <- predict(model_bd_final, newdata = d_test, type = "class")
predictions_bd

d_test$outcome <- factor(d_test$outcome, levels = levels(predictions_bd))

confusion_matrix_bd <- confusionMatrix(predictions_bd, d_test$outcome)
confusion_matrix_bd
confusion_bd <- table(predictions_bd, d_test$outcome)
confusion_bd

used_vars <- colnames(attr(model_bd_final$terms, "factors"))
used_vars

accuracy_bd <- sum(diag(confusion_bd)) / sum(confusion_bd)
accuracy_bd

recall_bd <- diag(confusion_bd) / rowSums(confusion_bd)
recall_bd

precision_bd <- diag(confusion_bd) / colSums(confusion_bd)
precision_bd

f1_bd <- 2 * (precision_bd * recall_bd) / (precision_bd + recall_bd)
f1_bd

predictions_bd2 <- predict(model_bd, newdata = d_test, type = "probs")
predictions_bd2

class_lbs <- c("newborn", "intra_fetal_death", "neonatal_death")
auc_pr_bd <- numeric(length(class_lbs))
auc_roc_bd <- numeric(length(class_lbs))

counter <- 1

for (class_lb in class_lbs) {
  #create a binary variable "binary response" for the current class
  binary_response_bd <- ifelse(d_test$outcome == class_lb, 1, 0)
  
  #calculates the AUC-PR for each class
  auc_pr_curve_bd <- pr.curve(scores.class0 = as.numeric(predictions_bd2[, class_lb]), weights.class0 = binary_response_bd)
  auc_pr_bd[counter] <- auc_pr_curve_bd$auc.integral
  
  #calculates the AUC-ROC for each class
  auc_roc_bd[counter] <- auc(roc(response = binary_response_bd, predictor = as.numeric(predictions_bd2[, class_lb])))
  
  counter <- counter + 1
}

#display of results
counter <- 1
for (class_lb in class_lbs) {
  cat("Class:", class_lb, "\n")
  cat("AUC-PR:", auc_pr_bd[counter], "\n")
  cat("AUC-ROC:", auc_roc_bd[counter], "\n")
  
  counter <- counter + 1
}


#Table with the results

results_bd <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                         Accuracy = accuracy_bd,
                         Recall = recall_bd,
                         Precision = precision_bd,
                         F1 = f1_bd,
                         AUC_PR = auc_pr_bd,
                         AUC_ROC = auc_roc_bd)

table_bd <- gt(results_bd, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_bd
