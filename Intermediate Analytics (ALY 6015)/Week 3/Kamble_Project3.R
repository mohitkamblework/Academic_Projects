cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session




# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>Week - 3<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

install.packages("ISLR")
library('ISLR')

# College Dataset
attach(College)
View(College)
names(College)

clg <- data.frame(College)

# Q1)
summary(clg)
str(clg)

# Histogram of Enrollment
hist(clg$Enroll, main = "Histogram of Enrollment", col = "orange", border = "black")


# Barplot of the distribution of Private and Public Colleges
barplot(table(clg$Private), col = c("seagreen", "orange"), 
        main = "Distribution of Private and Public Colleges",
        xlab = "College Type", ylab = "Count", names.arg = c("Public", "Private"))



# Correlation matrix
cor_matrix <- cor(clg[, c("Enroll", "Accept", "Grad.Rate", "Apps")])

install.packages("corrplot")
library(corrplot)

corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black",
         diag = FALSE, type = "upper", order = "hclust", tl.srt = 45)


# Q2)
install.packages("caTools")
library(caTools)

set.seed(123)

split_vec <- sample.split(clg$Private, SplitRatio = 0.7)

# Training and Test sets based on the split vector
train_data <- subset(clg, split_vec == TRUE)
test_data <- subset(clg, split_vec == FALSE)

View(train_data)
View(test_data)
head(train_data)
head(test_data)


# Q3)
install.packages("caret")
library(caret)


response_variable <- "Private"
predictors <- c("Accept", "Enroll")

# logistic regression model
mod1 <- glm(Private ~ Accept + Enroll, data = train_data, family = binomial(link = 'logit'))


# Display the summary of the logistic regression model
summary(mod1)

# Q4)
# Predictions on the training set
train_predictions <- predict(mod1, newdata = train_data, type = "response")

# Convert probabilities to binary predictions (0 or 1)
train_predictions_binary <- ifelse(train_predictions > 0.5, 1, 0)

# Confusion matrix
cm_train <- table(Actual = train_data$Private, Predicted = train_predictions_binary)


# Q5)
# Accuracy, Precision, Recall, and Specificity metrics for Train Data
accuracy_train <- round(sum(diag(cm_train)) / sum(cm_train),2)
sensitivity_train <- round(cm_train[2, 2] / sum(cm_train[2, ]),2)
specificity_train <- round(cm_train[1, 1] / sum(cm_train[1, ]),2)
precision_train <- round(cm_train[2, 2] / sum(cm_train[, 2]),2)
recall_train <- round(sensitivity_train,2)
f1_score_train <- round(2 * (precision_train * recall_train) / (precision_train + recall_train),2)

# Creating a table to store the metrics for training set
metrics_table_train <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy_train, sensitivity_train, specificity_train, precision_train, recall_train, f1_score_train)
)


# Q6)
# Predictions on the test set
test_predictions <- predict(mod1, newdata = test_data, type = "response")

# Convert probabilities to binary predictions (0 or 1)
test_predictions_binary <- ifelse(test_predictions > 0.5, 1, 0)

# Create confusion matrix for the test set
cm_test <- table(Actual = test_data$Private, Predicted = test_predictions_binary)

# Accuracy, Precision, Recall, and Specificity metrics for Test Data
accuracy_test <- round(sum(diag(cm_test)) / sum(cm_test),2)
sensitivity_test <- round(cm_test[2, 2] / sum(cm_test[2, ]),2)
specificity_test <- round(cm_test[1, 1] / sum(cm_test[1, ]),2)
precision_test <- round(cm_test[2, 2] / sum(cm_test[, 2]),2)
recall_test <- round(sensitivity_test,2)
f1_score_test <- round(2 * (precision_test * recall_test) / (precision_test + recall_test),2)


# Creating a table to store the metrics for test set
metrics_table_test <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy_test, sensitivity_test, specificity_test, precision_test, recall_test, f1_score_test)
)



# Q7)
install.packages("pROC")
library(pROC)

roc_curve <- roc(test_data$Private, test_predictions)

plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2, cex.main = 1.2, col.main = "darkblue", lty = 1,
     xlab = "False Positive Rate (Specificity)", ylab = "True Positive Rate (Sensitivity)")
text(0.5, 0.5, paste("AUC = ", round(auc(roc_curve), 2)), col = "seagreen", cex = 1.2)
legend("bottomright", legend = c("ROC Curve"), col = "blue", lty = 1, lwd = 2)


# Q8)
area_uc <- round(auc(roc_curve),2)
# Area Under Curve is mentioned in ROC curve plot