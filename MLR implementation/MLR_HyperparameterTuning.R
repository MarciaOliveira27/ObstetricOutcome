library(readxl)
library(caret)
library(glmnet)
library(pROC)
library(PRROC)
library(gt)

#SMOTE
#ELASTIC NET combining cross-validation and fine-tuning of hyperparameters

b_train <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_train_smote.xlsx")
b_test <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_test_smote.xlsx")

X_train <- b_train[,1:7]
y_train <- b_train$outcome
X_test <- b_test[,1:7]
y_test <- b_test$outcome

X_train$Fetalgender <- as.factor(X_train$Fetalgender)
X_test$Fetalgender <- as.factor(X_test$Fetalgender)

cgl_cols <- which(sapply(X_train, is.factor))
X_train[, cgl_cols] <- lapply(X_train[, cgl_cols], factor)
X_train_matrix <- model.matrix(~ . - 1, data = X_train)

cgl_cols <- which(sapply(X_test, is.factor))
X_test[, cgl_cols] <- lapply(X_test[, cgl_cols], factor)
X_test_matrix <- model.matrix(~ . - 1, data = X_test)

y_train <- factor(y_train)
y_test <- factor(y_test)

y_train  <- relevel(y_train , ref = "newborn")
y_test   <- relevel(y_test , ref = "newborn")

tctl <- trainControl(method = "cv", number = 10)
initial_lambda_grid <- seq(0.001, 1, length.out = 100)
alpha_grid = seq(0.1, 0.9, by = 0.1)

enetFit_initial <- train(x = X_train_matrix, y = y_train, 
                         method = "glmnet",
                         trControl = tctl,
                         tuneGrid = expand.grid(alpha = alpha_grid, lambda = initial_lambda_grid))

best_alpha_initial <- enetFit_initial$bestTune$alpha
best_lambda_initial <- enetFit_initial$bestTune$lambda

refined_lambda_grid <- seq(best_lambda_initial * 0.5, best_lambda_initial * 1.5, length.out = 100)

enetFit_refined <- train(X_train_matrix, y_train, method = "glmnet", 
                         trControl = tctl, tuneGrid = expand.grid(alpha = alpha_grid, lambda = refined_lambda_grid))

best_alpha_refined <- enetFit_refined$bestTune$alpha
best_lambda_refined <- enetFit_refined$bestTune$lambda

predictions <- predict(enetFit_refined, newdata = X_test_matrix, type = "prob")
predicted_classes <- colnames(predictions)[apply(predictions, 1, which.max)]
predicted_classes_factor <- factor(predicted_classes, levels = levels(y_test))

confusion <- table(predicted_classes_factor, y_test)
accuracy <- sum(diag(confusion)) / sum(confusion)  
precision <- diag(confusion) / colSums(confusion)
recall <- diag(confusion) / rowSums(confusion)
f1 <- 2 * (precision * recall) / (precision + recall)


class_lbs <- c("newborn", "intra_fetal_death", "neonatal_death")
auc_pr <- numeric(length(class_lbs))
auc_roc <- numeric(length(class_lbs))

counter <- 1

for (class_lb in class_lbs) {
  #create a binary variable "binary response" for the current class
  binary_response <- ifelse(y_test == class_lb, 1, 0)
  
  #calculates the AUC-PR for each class
  auc_pr_curve <- pr.curve(scores.class0 = as.numeric(predictions[, class_lb]), weights.class0 = binary_response)
  auc_pr[counter] <- auc_pr_curve$auc.integral
  
  #calculates the AUC-ROC for each class
  auc_roc[counter] <- auc(roc(response = binary_response, predictor = as.numeric(predictions[, class_lb])))
  
  counter <- counter + 1
}

#display of results
counter <- 1
for (class_lb in class_lbs) {
  cat("Class:", class_lb, "\n")
  cat("AUC-PR:", auc_pr[counter], "\n")
  cat("AUC-ROC:", auc_roc[counter], "\n")
  
  counter <- counter + 1
}

performance_metrics <- enetFit_refined$results
performance_metrics

#Table with the results

results_en_h <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                           Accuracy = accuracy,
                           Recall = recall,
                           Precision = precision,
                           F1 = f1,
                           AUC_PR = auc_pr,
                           AUC_ROC = auc_roc)

table_en_h <- gt(results_en_h, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_en_h

#Coefficients/OR
coefficients_ht <- coef(enetFit_refined$finalModel, s = best_lambda_refined)

convert_sparse_ht <- function(sparse_matrix) {
  as.data.frame(as.matrix(sparse_matrix))
}
ht_intra_fetal_death <- convert_sparse_ht(coefficients_ht$intra_fetal_death)
ht_neonatal_death <- convert_sparse_ht(coefficients_ht$neonatal_death)
OR_intra_fetal_death_ht <- exp(ht_intra_fetal_death)
OR_neonatal_death_ht <- exp(ht_neonatal_death)
print(OR_intra_fetal_death_ht)
print(OR_neonatal_death_ht)

#Graphs with the most important variables
#intra_fetal_death
coef_intra_fetal_death <- as.matrix(coefficients_ht$intra_fetal_death)

df_intra_fetal_death <- data.frame(
  Variable = rownames(coef_intra_fetal_death),
  Coefficient = coef_intra_fetal_death[, 1],
  Class = "intra_fetal_death"
)
df_intra_fetal_death <- df_intra_fetal_death[df_intra_fetal_death$Variable != "(Intercept)", ]
df_intra_fetal_death <- df_intra_fetal_death[order(-df_intra_fetal_death$Coefficient), ]
ggplot(df_intra_fetal_death, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "#FFD700") +
  coord_flip() +
  labs(title = "Importance of variables for the intrauterine fetal death class",
       x = "Variables",
       y = "Coefficients")

#neonatal_death
coef_neonatal_death <- as.matrix(coefficients_ht$neonatal_death)
df_neonatal_death <- data.frame(
  Variable = rownames(coef_neonatal_death),
  Coefficient = coef_neonatal_death[, 1],
  Class = "neonatal_death"
)
df_neonatal_death <- df_neonatal_death[df_neonatal_death$Variable != "(Intercept)", ]
df_neonatal_death <- df_neonatal_death[order(-df_neonatal_death$Coefficient), ]
ggplot(df_neonatal_death, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "#CCFF99") +
  coord_flip() +
  labs(title = "Importance of variables for the neonatal death class",
       x = "Variables",
       y = "Coefficients")


#Neural Networks with Repeated Cross Validation
#Tuning hyperparameters in nnet

d_train <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_train_smote.xlsx")
d_test <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_test_smote.xlsx")

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,           
                     repeats = 3,            
                     classProbs = TRUE,      
                     savePredictions="all",
                     summaryFunction = multiClassSummary,  
                     selectionFunction = "oneSE"  
)

model <- train(outcome ~ ., data=d_train,
               method = "nnet",
               trControl = ctrl,
               tuneGrid = expand.grid(.size = 1:10, .decay = c(0, 0.1, 0.01)),  #Varyies the size and decay of the hidden layer
               trace = FALSE)

model
model$bestTune

predictions <- predict(model, newdata = test_data)

confusion <- table(predictions, test_data$outcome)
accuracy <- sum(diag(confusion)) / sum(confusion)
recall <- diag(confusion) / rowSums(confusion)
precision <- diag(confusion) / colSums(confusion)
f1 <- 2 * (precision * recall) / (precision + recall)


predictions_prob <- predict(model, newdata = test_data, type = "prob")
class_lbs <- c("intra_fetal_death", "neonatal_death","newborn")
auc_pr <- numeric(length(class_lbs))
auc_roc <- numeric(length(class_lbs))

counter <- 1

for (class_lb in class_lbs) {
  #create a binary variable "binary response" for the current class
  binary_response <- ifelse(test_data$outcome == class_lb, 1, 0)
  
  #calculates the AUC-PR for each class
  auc_pr_curve <- pr.curve(scores.class0 = as.numeric(predictions_prob[, class_lb]), weights.class0 = binary_response)
  auc_pr[counter] <- auc_pr_curve$auc.integral
  
  #calculates the AUC-ROC for each class
  auc_roc[counter] <- auc(roc(response = binary_response, predictor = as.numeric(predictions_prob[, class_lb])))
  
  counter <- counter + 1
}

#display of results
counter <- 1
for (class_lb in class_lbs) {
  cat("Class:", class_lb, "\n")
  cat("AUC-PR:", auc_pr[counter], "\n")
  cat("AUC-ROC:", auc_roc[counter], "\n")
  
  counter <- counter + 1
}


#Table with the results

results_nnet_s <- data.frame(Class = c("intra_fetal_death", "neonatal_death","newborn"),
                             Accuracy = accuracy,
                             Recall = recall,
                             Precision = precision,
                             F1 = f1,
                             AUC_PR = auc_pr,
                             AUC_ROC = auc_roc)

table_nnet_s <- gt(results_nnet_s, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_nnet_s

#ADASYN
#ELASTIC NET combining cross-validation and fine-tuning of hyperparameters

b_train <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_train_adasyn.xlsx")
b_test <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_test_adasyn.xlsx")

X_train <- b_train[,1:7]
y_train <- b_train$outcome
X_test <- b_test[,1:7]
y_test <- b_test$outcome

X_train$Fetalgender <- as.factor(X_train$Fetalgender)
X_test$Fetalgender <- as.factor(X_test$Fetalgender)

cgl_cols <- which(sapply(X_train, is.factor))
X_train[, cgl_cols] <- lapply(X_train[, cgl_cols], factor)
X_train_matrix <- model.matrix(~ . - 1, data = X_train)

cgl_cols <- which(sapply(X_test, is.factor))
X_test[, cgl_cols] <- lapply(X_test[, cgl_cols], factor)
X_test_matrix <- model.matrix(~ . - 1, data = X_test)

y_train <- factor(y_train)
y_test <- factor(y_test)

y_train  <- relevel(y_train , ref = "newborn")
y_test   <- relevel(y_test , ref = "newborn")

tctl <- trainControl(method = "cv", number = 10)
initial_lambda_grid <- seq(0.001, 1, length.out = 100)
alpha_grid = seq(0.05, 0.95, by = 0.05)


enetFit_initial <- train(x = X_train_matrix, y = y_train, 
                         method = "glmnet",
                         trControl = tctl,
                         tuneGrid = expand.grid(alpha = alpha_grid, lambda = initial_lambda_grid))

best_alpha_initial <- enetFit_initial$bestTune$alpha
best_lambda_initial <- enetFit_initial$bestTune$lambda


refined_lambda_grid <- seq(best_lambda_initial * 0.5, best_lambda_initial * 1.5, length.out = 100)


enetFit_refined <- train(X_train_matrix, y_train, method = "glmnet", 
                         trControl = tctl, tuneGrid = expand.grid(alpha = alpha_grid, lambda = refined_lambda_grid))

best_alpha_refined <- enetFit_refined$bestTune$alpha
best_lambda_refined <- enetFit_refined$bestTune$lambda

coeficientes <- coef(enetFit_refined$finalModel, s = best_lambda_refined)

predictions <- predict(enetFit_refined, newdata = X_test_matrix, type = "prob")
predicted_classes <- colnames(predictions)[apply(predictions, 1, which.max)]
predicted_classes_factor <- factor(predicted_classes, levels = levels(y_test))


confusion <- table(predicted_classes_factor, y_test)
accuracy <- sum(diag(confusion)) / sum(confusion)  
precision <- diag(confusion) / colSums(confusion)
recall <- diag(confusion) / rowSums(confusion)
f1 <- 2 * (precision * recall) / (precision + recall)


class_lbs <- c("newborn", "intra_fetal_death", "neonatal_death")
auc_pr <- numeric(length(class_lbs))
auc_roc <- numeric(length(class_lbs))

counter <- 1

for (class_lb in class_lbs) {
  #create a binary variable "binary response" for the current class
  binary_response <- ifelse(y_test == class_lb, 1, 0)
  
  #calculates the AUC-PR for each class
  auc_pr_curve <- pr.curve(scores.class0 = as.numeric(predictions[, class_lb]), weights.class0 = binary_response)
  auc_pr[counter] <- auc_pr_curve$auc.integral
  
  #calculates the AUC-ROC for each class
  auc_roc[counter] <- auc(roc(response = binary_response, predictor = as.numeric(predictions[, class_lb])))
  
  counter <- counter + 1
}

#display of results
counter <- 1
for (class_lb in class_lbs) {
  cat("Class:", class_lb, "\n")
  cat("AUC-PR:", auc_pr[counter], "\n")
  cat("AUC-ROC:", auc_roc[counter], "\n")
  
  counter <- counter + 1
}

performance_metrics <- enetFit_refined$results
performance_metrics

#Table with the results

results_enh_a <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                            Accuracy = accuracy,
                            Recall = recall,
                            Precision = precision,
                            F1 = f1,
                            AUC_PR = auc_pr,
                            AUC_ROC = auc_roc)

table_enh_a <- gt(results_enh_a, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_enh_a


#Neural Networks with Repeated Cross Validation

d_train <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_train_adasyn.xlsx")
d_test <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_test_adasyn.xlsx")

#Tuning hyperparameters in nnet

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,           
                     repeats = 3,            
                     classProbs = TRUE,      
                     savePredictions="all",
                     summaryFunction = multiClassSummary,  
                     selectionFunction = "oneSE"  
)

model <- train(outcome~., data=d_train,
               method = "nnet",
               trControl = ctrl,
               tuneGrid = expand.grid(.size = 1:10, .decay = c(0, 0.1, 0.01)),  #Varyies the size and decay of the hidden layer
               trace = FALSE)

#size= 9 / decay= 0.01

model
model$bestTune

predictions <- predict(model, newdata = test_data)

confusion <- table(predictions, test_data$outcome)
accuracy <- sum(diag(confusion)) / sum(confusion)
recall <- diag(confusion) / rowSums(confusion)
precision <- diag(confusion) / colSums(confusion)
f1 <- 2 * (precision * recall) / (precision + recall)


predictions_prob <- predict(model, newdata = test_data, type = "prob")
class_lbs <- c("intra_fetal_death", "neonatal_death","newborn")
auc_pr <- numeric(length(class_lbs))
auc_roc <- numeric(length(class_lbs))

counter <- 1

for (class_lb in class_lbs) {
  #create a binary variable "binary response" for the current class
  binary_response <- ifelse(test_data$outcome == class_lb, 1, 0)
  
  #calculates the AUC-PR for each class
  auc_pr_curve <- pr.curve(scores.class0 = as.numeric(predictions_prob[, class_lb]), weights.class0 = binary_response)
  auc_pr[counter] <- auc_pr_curve$auc.integral
  
  #calculates the AUC-ROC for each class
  auc_roc[counter] <- auc(roc(response = binary_response, predictor = as.numeric(predictions_prob[, class_lb])))
  
  counter <- counter + 1
}

#display of results
counter <- 1
for (class_lb in class_lbs) {
  cat("Class:", class_lb, "\n")
  cat("AUC-PR:", auc_pr[counter], "\n")
  cat("AUC-ROC:", auc_roc[counter], "\n")
  
  counter <- counter + 1
}


#Table with the results

results_nnet_a <- data.frame(Class = c("intra_fetal_death", "neonatal_death","newborn"),
                           Accuracy = accuracy,
                           Recall = recall,
                           Precision = precision,
                           F1 = f1,
                           AUC_PR = auc_pr,
                           AUC_ROC = auc_roc)

table_nnet_a <- gt(results_nnet_a, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_nnet_a

