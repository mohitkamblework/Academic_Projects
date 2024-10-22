cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Final Project<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(glmnet)
library(readr)
library(Metrics)


boston <- read.csv("D:/NEU STUDY/2nd Quarter/Intermediate Analytics (ALY 6015)/Group Project/fy19fullpropassess.csv")

# Data Overview
str(boston)
summary(boston)


# Stats Tables
# Descriptive Statistics for Numeric Variables
num_vars <- c("AV_LAND", "AV_BLDG", "AV_TOTAL", "GROSS_TAX")
summary_stats_numeric <- summary(boston[, num_vars])


# Property Condition Distribution (exterior, interior, overall)
exterior_condition_counts <- table(boston$R_EXT_CND)
interior_condition_counts <- table(boston$R_INT_CND)
overall_condition_counts <- table(boston$R_OVRALL_CND)


condition_counts_table <- data.frame(
  Condition = c("Exterior", "Overall", "Interior"),
  Average = c(exterior_condition_counts["A"], overall_condition_counts["A"], interior_condition_counts["A"]),
  Excellent = c(exterior_condition_counts["E"], overall_condition_counts["E"], interior_condition_counts["E"]),
  Good = c(exterior_condition_counts["G"], overall_condition_counts["G"], interior_condition_counts["G"]),
  Fair = c(exterior_condition_counts["F"], overall_condition_counts["F"], interior_condition_counts["F"]),
  Poor = c(exterior_condition_counts["P"], overall_condition_counts["P"], interior_condition_counts["P"])
)
condition_counts_table

# Heat Type and Air Conditioning Distribution
heat_type_counts <- table(boston$R_HEAT_TYP)
ac_counts <- table(boston$R_AC)

heat_ac_counts_table <- data.frame(
  Type = c("Heat Type", "Air Conditioning"),
  Electric = c(heat_type_counts["E"], ac_counts["C"]),
  Other = c(heat_type_counts["O"], ac_counts["D"]),
  Hot_Water = c(heat_type_counts["W"], ac_counts["N"]),
  Forced_Air = c(heat_type_counts["F"], ac_counts[""]),
  None = c(heat_type_counts["N"], ac_counts["N"]),
  Space_Heater = c(heat_type_counts["S"], ac_counts[""])
)

# Number of Rooms, Bedrooms, and Bathrooms Distribution
rooms_counts <- table(boston$R_TOTAL_RMS)
bedrooms_counts <- table(boston$R_BDRMS)
full_baths_counts <- table(boston$R_FULL_BTH)
half_baths_counts <- table(boston$R_HALF_BTH)

room_counts_table <- data.frame(
  Category = c("Rooms", "Bedrooms", "Full Baths", "Half Baths"),
  One = c(rooms_counts["1"], bedrooms_counts["1"], full_baths_counts["1"], half_baths_counts["1"]),
  Two = c(rooms_counts["2"], bedrooms_counts["2"], full_baths_counts["2"], half_baths_counts["2"]),
  Three = c(rooms_counts["3"], bedrooms_counts["3"], full_baths_counts["3"], half_baths_counts["3"]),
  Four = c(rooms_counts["4"], bedrooms_counts["4"], full_baths_counts["4"], half_baths_counts["4"])
)



# Charts
# bar plot for the distribution of land use types
# lu_chart color palette
lu_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#b2DF8a",
               "#fb9a99", "#fdbf6f", "#cab2d6", "#ffff99", "#8dd3c7", "#d9d9d9", "#bc80bd", "#ccebc5")


lu_chart <- ggplot(subset(boston, LU != "AH"), aes(x = reorder(factor(LU), table(LU)[LU]))) +
  geom_bar(fill = lu_colors, color = "black", width = 0.7, show.legend = FALSE) +
  labs(title = "Distribution of Land Use Types",
       x = "Land Use Type",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_fill_manual(values = lu_colors) +
  coord_flip()


# Create a histogram for the distribution of owner-occupied status (OWN_OCC)
# OOC color palette
ooc_colors <- c("#3498db", "#2c3e50")

# Create a bar plot for the distribution of owner-occupied status
ooc_status_chart <- ggplot(boston, aes(x = factor(OWN_OCC), fill = factor(OWN_OCC))) +
  geom_bar(color = "black", show.legend = FALSE) +
  scale_fill_manual(values = ooc_colors) +
  labs(title = "Distribution of Owner-Occupied Status",
       x = "Owner-Occupied",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "none")



# Property Condition Distribution Chart
# Property Condition color palette
prop_cond_colors <- c("#64B5F6", "#4CAF50", "#FFEB3B")

# Create a bar plot for the distribution of property conditions
condition_plot <- ggplot(condition_counts_table, aes(x = Condition, y = Average, fill = Condition)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  labs(title = "Property Condition Distribution",
       x = "Condition", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values = prop_cond_colors)





# Heat Type and Air Conditioning Distribution
# Heat Type color palette
ht_ac_colors <- c("#64B5F6", "#E57373")

# Create a bar plot for the distribution of heat type and air conditioning
heat_ac_plot <- ggplot(heat_ac_counts_table, aes(x = Type, y = Electric, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  labs(title = "Heat Type and Air Conditioning Distribution",
       x = "Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values = ht_ac_colors)




# T-test
# T-Test for AV_TOTAL between properties with and without air conditioning
# Create a binary variable for air conditioning
boston$Has_AC <- ifelse(boston$R_AC %in% c("C", "D") & boston$R_AC != "N", "With AC", "Without AC")


# Perform t-test for AV_TOTAL between properties with and without air conditioning
t_test_ac <- t.test(boston$AV_TOTAL ~ boston$Has_AC)



# Correlation matrix for selected numeric variables
numeric_vars_selected <- c("GROSS_TAX", "AV_LAND", "AV_BLDG", "AV_TOTAL")
correlation_matrix <- cor(boston[, numeric_vars_selected])

# Corrplot
corrplot(correlation_matrix, method = "color", addCoef.col = "#FFA500", tl.col = "black", tl.srt = 45)


# Feature Engineering
mean_LIVING_AREA <- mean(boston$LIVING_AREA, na.rm = TRUE)
boston$LIVING_AREA[is.na(boston$LIVING_AREA)] <- mean_LIVING_AREA

mean_NUM_FLOORS <- mean(boston$NUM_FLOORS, na.rm = TRUE)
boston$NUM_FLOORS[is.na(boston$NUM_FLOORS)] <- mean_NUM_FLOORS

View(boston)
names(boston)


# Total Living Area
boston$TOTAL_LIVING_AREA <- boston$LIVING_AREA * boston$NUM_FLOORS


# LASSO Regression
# Splitting data into train and test sets
set.seed(123)
trainIndex <- sample(x = nrow(boston), size = nrow(boston) * 0.7)
train <- boston[trainIndex,]
test <- boston[-trainIndex,]

# Prepare predictors and response variables
x_train <- as.matrix(train[, c("GROSS_TAX", "LIVING_AREA")])
x_test <- as.matrix(test[, c("GROSS_TAX", "LIVING_AREA")])
y_train <- train$AV_TOTAL
y_test <- test$AV_TOTAL

# Plotting ridge and lasso
# Impute missing values with mean
x_train[is.na(x_train)] <- colMeans(x_train, na.rm = TRUE)
ridge <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 10)

lambda_min_ridge <- ridge$lambda.min
lambda_1se_ridge <- ridge$lambda.1se

plot(ridge, main = "", sub = "Ridge Regression CV Results")
abline(v = log(lambda_min_ridge), col = "red", lty = 2)
abline(v = log(lambda_1se_ridge), col = "blue", lty = 2)
legend("bottomright", legend = c("lambda.min", "lambda.1se"), col = c("red", "blue"), lty = 2)

lasso <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)

lambda_min_lasso <- lasso$lambda.min
lambda_1se_lasso <- lasso$lambda.1se

# plot
plot(lasso, main = "", sub = "LASSO Regression CV Results")
abline(v = log(lambda_min_lasso), col = "red", lty = 2)
abline(v = log(lambda_1se_lasso), col = "blue", lty = 2)
legend("topleft", legend = c("lambda.min", "lambda.1se"), col = c("red", "blue"), lty = 2)

# Train set predictions for Ridge
model.ridge <- glmnet(x_train, y_train, alpha = 0, lambda = ridge$lambda.min)
preds.train.ridge <- predict(model.ridge, newx = x_train)
ridge_train_rmse <- rmse(y_train, preds.train.ridge)

# Check for missing values in x_test and impute if necessary
if (anyNA(x_test)) {
  x_test[is.na(x_test)] <- colMeans(x_test, na.rm = TRUE)
}

# Ensure dimensions match
if (ncol(x_test) != ncol(x_train)) {
  stop("Number of predictors in test set does not match the training set.")
}

# Make predictions on test set
preds_test <- predict(ridge, newx = x_test, s = "lambda.min")

# Calculate RMSE on test set if y_test has variability
if (var(y_test) != 0) {
  ridge_test_rmse <- rmse(y_test, preds_test)
  cat("Ridge Regression RMSE - Test:", ridge_test_rmse, "\n")
} else {
  cat("Response variable in test set is constant. Cannot calculate RMSE.\n")
}


# Test set predictions for Ridge
preds.test.ridge <- predict(model.ridge, newx = x_test)
ridge_test_rmse <- rmse(y_test, preds.test.ridge)

# Compare RMSE values for Ridge
cat("Ridge Regression RMSE - Train:", ridge_train_rmse, "\n")
cat("Ridge Regression RMSE - Test:", ridge_test_rmse, "\n")

# Train set predictions for Lasso
model.lasso <- glmnet(x_train, y_train, alpha = 1, lambda = lasso$lambda.min)
preds.train.lasso <- predict(model.lasso, newx = x_train)

# Test set predictions for Lasso
preds.test.lasso <- predict(model.lasso, newx = x_test)

# Check for missing values in preds.test.lasso
missing_values <- any(is.na(preds.test.lasso))

# Handle missing values if present
if (missing_values) {
  # Impute missing values with mean
  preds.test.lasso[is.na(preds.test.lasso)] <- mean(preds.test.lasso, na.rm = TRUE)
}

# Calculate RMSE for Lasso test set
lasso_test_rmse <- rmse(y_test, preds.test.lasso)

# Print the RMSE for Lasso test set
cat("Lasso Regression RMSE - Test:", lasso_test_rmse, "\n")


# Calculate RMSE for Lasso
lasso_train_rmse <- rmse(y_train, preds.train.lasso)
lasso_test_rmse <- rmse(y_test, preds.test.lasso)

# Compare RMSE values for Lasso
cat("Lasso Regression RMSE - Train:", lasso_train_rmse, "\n")
cat("Lasso Regression RMSE - Test:", lasso_test_rmse, "\n")

# Model Comparisons
rmse_results <- data.frame(
  Model = c("Ridge", "Lasso"),
  Train_RMSE = c(ridge_train_rmse, lasso_train_rmse),
  Test_RMSE = c(ridge_test_rmse, lasso_test_rmse)
)
















