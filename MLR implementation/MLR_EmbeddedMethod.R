library(readxl)
library(caret)
library(glmnet)
library(pROC)
library(PRROC)
library(Matrix)
library(gt)
library(plotmo)
library(doParallel)


#DATASET WITHOUT BALANCING

######################## EMBEDDED METHOD - RIDGE #############################


b_train <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_train_wb.xlsx")
b_test <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_test_wb.xlsx")

X_train <- b_train[,1:7]
y_train <- b_train$outcome
X_test <- b_test[,1:7]
y_test <- b_test$outcome

#converting categorical variables to factors
X_train$Fetalgender <- as.factor(X_train$Fetalgender)
X_test$Fetalgender <- as.factor(X_test$Fetalgender)

cgl_cols <- which(sapply(X_train, is.factor))
X_train[, cgl_cols] <- lapply(X_train[, cgl_cols], factor)
cgl_cols <- which(sapply(X_test, is.factor))
X_test[, cgl_cols] <- lapply(X_test[, cgl_cols], factor)

#creating dummy matrices
X_train_matrix <- model.matrix(~ . - 1, data = X_train)
X_test_matrix <- model.matrix(~ . - 1, data = X_test)

#converting the response variable to factor
y_train <- factor(y_train)
y_test <- factor(y_test)

y_train  <- relevel(y_train , ref = "newborn")
y_test   <- relevel(y_test , ref = "newborn")

train_ctl <- trainControl(method = "cv", number = 10)

ridgeFit <- train(X_train_matrix, y_train, method = "glmnet", 
                  trControl = train_ctl, tuneGrid = data.frame(alpha=0, lambda=seq(0.1,0.9,0.05)))

best_lambda <- ridgeFit$bestTune$lambda
coeficientes <- coef(ridgeFit$finalModel, s = best_lambda)

predictions <- predict(ridgeFit, newdata = X_test_matrix, type = "prob")
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

#Table with the results

results_rg_wb <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                         Accuracy = accuracy,
                         Recall = recall,
                         Precision = precision,
                         F1 = f1,
                         AUC_PR = auc_pr,
                         AUC_ROC = auc_roc)

table_rg_wb <- gt(results_rg_wb, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_rg_wb

######################## EMBEDDED METHOD - LASSO #############################    
b_train <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_train_wb.xlsx")
b_test <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_test_wb.xlsx")

X_train <- b_train[,1:7]
y_train <- b_train$outcome
X_test <- b_test[,1:7]
y_test <- b_test$outcome

X_train$Fetalgender <- as.factor(X_train$Fetalgender)
X_test$Fetalgender <- as.factor(X_test$Fetalgender)
y_train <- factor(y_train)
y_test <- factor(y_test)
y_train  <- relevel(y_train , ref = "newborn")
y_test   <- relevel(y_test , ref = "newborn")
levels(y_train)
levels(y_test)

cgl_cols <- which(sapply(X_train, is.factor))
X_train[, cgl_cols] <- lapply(X_train[, cgl_cols], factor)
X_train_matrix <- model.matrix(~ . - 1, data = X_train)

cgl_cols <- which(sapply(X_test, is.factor))
X_test[, cgl_cols] <- lapply(X_test[, cgl_cols], factor)
X_test_matrix <- model.matrix(~ . - 1, data = X_test)

lassoFit <- glmnet(x=X_train_matrix, y=y_train, family = "multinomial", alpha=1,  type.multinomial = "grouped")
plot(lassoFit)
plot_glmnet(lassoFit, label = TRUE, nresponse = 3) 

#selected variables
rownames(coefficients$newborn)[which(coefficients$newborn != 0)]
rownames(coefficients$intra_fetal_death)[which(coefficients$intra_fetal_death != 0)]
rownames(coefficients$neonatal_death)[which(coefficients$neonatal_death != 0)]

#cross-validation for Lasso
lassoCV <- cv.glmnet(x=X_train_matrix, y=y_train, family="multinomial", alpha= 1, type.multinomial='grouped', parallel = TRUE, nfolds = 10)

#variable deviance vs. shrinkage parameter
plot(lassoCV)  

lambda_min <- lassoCV$lambda.min
lambda_1se <- lassoCV$lambda.1se


lassomodel <- glmnet(X_train_matrix,
                     y_train, alpha = 1, lambda = lambda_1se , family="multinomial")

lassomodel
coef(lassomodel)


predictions <- predict(lassomodel, newx = X_test_matrix, type = "response")
predictions <- data.frame(predictions)
colnames(predictions) <- c("newborn","intra_fetal_death", "neonatal_death")

predicted_classes <- colnames(predictions)[apply(predictions, 1, which.max)]
predicted_classes_factor <- factor(predicted_classes, levels = levels(y_test))

confusion <- table(predicted_classes_factor, y_test)
accuracy <- sum(diag(confusion)) / sum(confusion)  
precision <- diag(confusion) / colSums(confusion)
recall <- diag(confusion) / rowSums(confusion)
f1 <- 2 * (precision * recall) / (precision + recall)

class_lbs <- c("newborn","intra_fetal_death", "neonatal_death")
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

#Table with the results

results_ls_wb <- data.frame(Class = c("newborn","intra_fetal_death", "neonatal_death"),
                            Accuracy = accuracy,
                            Recall = recall,
                            Precision = precision,
                            F1 = f1,
                            AUC_PR = auc_pr,
                            AUC_ROC = auc_roc)

table_ls_wb <- gt(results_ls_wb, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_ls_wb

######################## EMBEDDED METHOD - ELASTIC NET ############################# 
b_train <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_train_wb.xlsx")
b_test <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_test_wb.xlsx")

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

hyperparameter_grid <- expand.grid(alpha = seq(0.05, 0.95, by = 0.05),  
                                   lambda = seq(0.1, 0.9, 0.05))

elasticFit <- train(X_train_matrix, y_train, method = "glmnet", 
                    trControl = tctl, tuneGrid = hyperparameter_grid)

best_alpha <- elasticFit$bestTune$alpha
best_lambda <- elasticFit$bestTune$lambda

coefficients <- coef(elasticFit$finalModel, s = best_lambda)

predictions <- predict(elasticFit, newdata = X_test_matrix, type = "prob")
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


#Table with the results

results_en_wb <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                         Accuracy = accuracy,
                         Recall = recall,
                         Precision = precision,
                         F1 = f1,
                         AUC_PR = auc_pr,
                         AUC_ROC = auc_roc)

table_en_wb <- gt(results_en_wb, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_en_wb

#DATASET WITH SMOTE

######################## EMBEDDED METHOD - RIDGE #############################

b_train <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_train_smote.xlsx")
b_test <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_test_smote.xlsx")

X_train <- b_train[,1:7]
y_train <- b_train$outcome
X_test <- b_test[,1:7]
y_test <- b_test$outcome

#converting categorical variables to factors
X_train$Fetalgender <- as.factor(X_train$Fetalgender)
X_test$Fetalgender <- as.factor(X_test$Fetalgender)

cgl_cols <- which(sapply(X_train, is.factor))
X_train[, cgl_cols] <- lapply(X_train[, cgl_cols], factor)
cgl_cols <- which(sapply(X_test, is.factor))
X_test[, cgl_cols] <- lapply(X_test[, cgl_cols], factor)

#creating dummy matrices
X_train_matrix <- model.matrix(~ . - 1, data = X_train)
X_test_matrix <- model.matrix(~ . - 1, data = X_test)

#converting the response variable to factor
y_train <- factor(y_train)
y_test <- factor(y_test)

y_train  <- relevel(y_train , ref = "newborn")
y_test   <- relevel(y_test , ref = "newborn")

train_ctl <- trainControl(method = "cv", number = 10)

ridgeFit <- train(X_train_matrix, y_train, method = "glmnet", 
             trControl = train_ctl, tuneGrid = data.frame(alpha=0, lambda=seq(0.1,0.9,0.05)))

best_lambda <- ridgeFit$bestTune$lambda

predictions <- predict(ridgeFit, newdata = X_test_matrix, type = "prob")
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

#Table with the results

results_rg <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                         Accuracy = accuracy,
                         Recall = recall,
                         Precision = precision,
                         F1 = f1,
                         AUC_PR = auc_pr,
                         AUC_ROC = auc_roc)

table_rg <- gt(results_rg, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_rg

######################## EMBEDDED METHOD - LASSO #############################    

b_train <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_train_smote.xlsx")
b_test <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_test_smote.xlsx")

X_train <- b_train[,1:7]
y_train <- b_train$outcome
X_test <- b_test[,1:7]
y_test <- b_test$outcome

X_train$Fetalgender <- as.factor(X_train$Fetalgender)
X_test$Fetalgender <- as.factor(X_test$Fetalgender)
y_train <- factor(y_train)
y_test <- factor(y_test)
y_train  <- relevel(y_train , ref = "newborn")
y_test   <- relevel(y_test , ref = "newborn")
levels(y_train)
levels(y_test)

cgl_cols <- which(sapply(X_train, is.factor))
X_train[, cgl_cols] <- lapply(X_train[, cgl_cols], factor)
X_train_matrix <- model.matrix(~ . - 1, data = X_train)

cgl_cols <- which(sapply(X_test, is.factor))
X_test[, cgl_cols] <- lapply(X_test[, cgl_cols], factor)
X_test_matrix <- model.matrix(~ . - 1, data = X_test)

lassoFit <- glmnet(x=X_train_matrix, y=y_train, family = "multinomial", alpha=1,  type.multinomial = "grouped")
plot(lassoFit)
plot_glmnet(lassoFit, label = TRUE, nresponse = 3) 

#selected variables
rownames(coefficients$newborn)[which(coefficients$newborn != 0)]
rownames(coefficients$intra_fetal_death)[which(coefficients$intra_fetal_death != 0)]
rownames(coefficients$neonatal_death)[which(coefficients$neonatal_death != 0)]

#cross-validation for Lasso
lassoCV <- cv.glmnet(x=X_train_matrix, y=y_train, family="multinomial", alpha= 1, type.multinomial='grouped', parallel = TRUE, nfolds = 10)

#variable deviance vs. shrinkage parameter
plot(lassoCV)  

lambda_min <- lassoCV$lambda.min
lambda_1se <- lassoCV$lambda.1se

lassomodel <- glmnet(X_train_matrix,
                y_train, alpha = 1, lambda = lambda_min, family="multinomial")

coef(lassomodel)

predictions <- predict(lassomodel, newx = X_test_matrix, type = "response")
predictions <- data.frame(predictions)
colnames(predictions) <- c("newborn","intra_fetal_death", "neonatal_death")

predicted_classes <- colnames(predictions)[apply(predictions, 1, which.max)]
predicted_classes_factor <- factor(predicted_classes, levels = levels(y_test))

confusion <- table(predicted_classes_factor, y_test)
accuracy <- sum(diag(confusion)) / sum(confusion)  
precision <- diag(confusion) / colSums(confusion)
recall <- diag(confusion) / rowSums(confusion)
f1 <- 2 * (precision * recall) / (precision + recall)

class_lbs <- c("newborn","intra_fetal_death", "neonatal_death")
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

#Table with the results

results_ls <- data.frame(Class = c("newborn","intra_fetal_death", "neonatal_death"),
                         Accuracy = accuracy,
                         Recall = recall,
                         Precision = precision,
                         F1 = f1,
                         AUC_PR = auc_pr,
                         AUC_ROC = auc_roc)

table_ls <- gt(results_ls, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_ls

######################## EMBEDDED METHOD - ELASTIC NET #############################    

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


hyperparameter_grid <- expand.grid(alpha = seq(0.1, 0.9, by = 0.05),  
                                   lambda = seq(0.1, 0.7, 0.05))

elasticFit <- train(X_train_matrix, y_train, method = "glmnet", 
                  trControl = tctl, tuneGrid = hyperparameter_grid)

best_alpha <- elasticFit$bestTune$alpha
best_lambda <- elasticFit$bestTune$lambda

predictions <- predict(elasticFit, newdata = X_test_matrix, type = "prob")
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

#Table with the results

results_en <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                         Accuracy = accuracy,
                         Recall = recall,
                         Precision = precision,
                         F1 = f1,
                         AUC_PR = auc_pr,
                         AUC_ROC = auc_roc)

table_en <- gt(results_en, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_en

#Coefficients/OR
coefficients_en <- coef(elasticFit$finalModel, s = best_lambda)

convert_sparse_en <- function(sparse_matrix) {
  as.data.frame(as.matrix(sparse_matrix))
}
en_intra_fetal_death <- convert_sparse_en(coefficients_en$intra_fetal_death)
en_neonatal_death <- convert_sparse_en(coefficients_en$neonatal_death)
OR_intra_fetal_death <- exp(en_intra_fetal_death)
OR_neonatal_death <- exp(en_neonatal_death)
print(OR_intra_fetal_death)
print(OR_neonatal_death)

#Graphs with the most important variables
#intra_fetal_death
coef_intra_fetal_death <- as.matrix(coefficients_en$intra_fetal_death)
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
coef_neonatal_death <- as.matrix(coefficients_en$neonatal_death)
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

#DATASET WITH ADASYN

######################## EMBEDDED METHOD - RIDGE #############################

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
cgl_cols <- which(sapply(X_test, is.factor))
X_test[, cgl_cols] <- lapply(X_test[, cgl_cols], factor)

X_train_matrix <- model.matrix(~ . - 1, data = X_train)
X_test_matrix <- model.matrix(~ . - 1, data = X_test)

y_train <- factor(y_train)
y_test <- factor(y_test)

y_train  <- relevel(y_train , ref = "newborn")
y_test   <- relevel(y_test , ref = "newborn")

train_ctl <- trainControl(method = "cv", number = 10)

ridgeFit <- train(X_train_matrix, y_train, method = "glmnet", 
                  trControl = train_ctl, tuneGrid = data.frame(alpha=0, lambda=seq(0.1,0.9,0.05)))


best_lambda <- ridgeFit$bestTune$lambda

coeficientes <- coef(ridgeFit$finalModel, s = best_lambda)

predictions <- predict(ridgeFit, newdata = X_test_matrix, type = "prob")
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

#Table with the results

results_rg_ad <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                            Accuracy = accuracy,
                            Recall = recall,
                            Precision = precision,
                            F1 = f1,
                            AUC_PR = auc_pr,
                            AUC_ROC = auc_roc)

table_rg_ad <- gt(results_rg_ad, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_rg_ad

######################## EMBEDDED METHOD - LASSO #############################    

b_train <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_train_adasyn.xlsx")
b_test <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_test_adasyn.xlsx")

X_train <- b_train[,1:7]
y_train <- b_train$outcome
X_test <- b_test[,1:7]
y_test <- b_test$outcome

X_train$Fetalgender <- as.factor(X_train$Fetalgender)
X_test$Fetalgender <- as.factor(X_test$Fetalgender)
y_train <- factor(y_train)
y_test <- factor(y_test)
y_train  <- relevel(y_train , ref = "newborn")
y_test   <- relevel(y_test , ref = "newborn")

cgl_cols <- which(sapply(X_train, is.factor))
X_train[, cgl_cols] <- lapply(X_train[, cgl_cols], factor)
X_train_matrix <- model.matrix(~ . - 1, data = X_train)

cgl_cols <- which(sapply(X_test, is.factor))
X_test[, cgl_cols] <- lapply(X_test[, cgl_cols], factor)
X_test_matrix <- model.matrix(~ . - 1, data = X_test)


lassoFit <- glmnet(x=X_train_matrix, y=y_train, family = "multinomial", alpha=1,  type.multinomial = "grouped")
plot(lassoFit)
plot_glmnet(lassoFit, label = TRUE, nresponse = 3) 

#selected variables
rownames(coefficients$newborn)[which(coefficients$newborn != 0)]
rownames(coefficients$intra_fetal_death)[which(coefficients$intra_fetal_death != 0)]
rownames(coefficients$neonatal_death)[which(coefficients$neonatal_death != 0)]

#cross-validation for Lasso
lassoCV <- cv.glmnet(x=X_train_matrix, y=y_train, family="multinomial", alpha= 1, type.multinomial='grouped', parallel = TRUE, nfolds = 10)

#variable deviance vs. shrinkage parameter
plot(lassoCV)  

lambda_min <- lassoCV$lambda.min
lambda_1se <- lassoCV$lambda.1se


lassomodel <- glmnet(X_train_matrix,
                       y_train, alpha = 1, lambda = lambda_1se, family="multinomial")

coef(lassomodel)

predictions <- predict(lassomodel, newx = X_test_matrix, type = "response")
predictions <- data.frame(predictions)
colnames(predictions) <- c("newborn","intra_fetal_death", "neonatal_death")

predicted_classes <- colnames(predictions)[apply(predictions, 1, which.max)]
predicted_classes_factor <- factor(predicted_classes, levels = levels(y_test))

confusion <- table(predicted_classes_factor, y_test)
accuracy <- sum(diag(confusion)) / sum(confusion)  
precision <- diag(confusion) / colSums(confusion)
recall <- diag(confusion) / rowSums(confusion)
f1 <- 2 * (precision * recall) / (precision + recall)

class_lbs <- c("newborn","intra_fetal_death", "neonatal_death")
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

#Table with the results

results_ls_ad <- data.frame(Class = c("newborn","intra_fetal_death", "neonatal_death"),
                            Accuracy = accuracy,
                            Recall = recall,
                            Precision = precision,
                            F1 = f1,
                            AUC_PR = auc_pr,
                            AUC_ROC = auc_roc)

table_ls_ad <- gt(results_ls_ad, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_ls_ad

######################## EMBEDDED METHOD - ELASTIC NET ############################# 

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

hyperparameter_grid <- expand.grid(alpha = seq(0.1, 0.9, by = 0.05),  
lambda = seq(0.1, 0.9, 0.05))

elasticFit <- train(X_train_matrix, y_train, method = "glmnet", 
                    trControl = tctl, tuneGrid = hyperparameter_grid)

best_alpha <- elasticFit$bestTune$alpha
best_lambda <- elasticFit$bestTune$lambda

coeficientes <- coef(elasticFit$finalModel, s = best_lambda)


predictions <- predict(elasticFit, newdata = X_test_matrix, type = "prob")
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

#Table with the results

results_en_ad <- data.frame(Class = c("newborn", "intra_fetal_death", "neonatal_death"),
                         Accuracy = accuracy,
                         Recall = recall,
                         Precision = precision,
                         F1 = f1,
                         AUC_PR = auc_pr,
                         AUC_ROC = auc_roc)

table_en_ad <- gt(results_en_ad, rowname_col = "Class") %>%
  
  fmt_number(
    columns = c(Accuracy:AUC_ROC),
    decimals = 3
  )

table_en_ad




















