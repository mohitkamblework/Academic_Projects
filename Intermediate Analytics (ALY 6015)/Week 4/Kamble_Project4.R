cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session




# >>>>>>>>>>>>>>>>>>>>>>>>>>>>Week 4<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



install.packages("ISLR")
install.packages("glmnet")
install.packages("Metrics")
library(ISLR)
library(glmnet)
library(Metrics)

# Loading College Dataset
data("College")
names(College)
View(College)

str(College)
summary(College)


# Splitting the data
set.seed(123)
tr_ind <- sample(x = nrow(College), size = nrow(College) * 0.7)

trn <- College[tr_ind,]
tst <- College[-tr_ind,]

trn_x <- model.matrix(Grad.Rate ~., trn)[,-1]
tst_x <- model.matrix(Grad.Rate ~., tst)[,-1]

trn_y <- trn$Grad.Rate
tst_y <- tst$Grad.Rate

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>Ridge<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Finding the best values for lambda using cross-validation
# alpha = 0 for Ridge (L1)

set.seed(123)
ridge_cv <- cv.glmnet(trn_x, trn_y, alpha = 0, nfolds = 10)

round(log(ridge_cv$lambda.min), 2)
round(log(ridge_cv$lambda.1se), 2)

# optimal values of lambda, minimizes the prediction error
# lambd.min - minimizes out of sample loss
# lambda.1se - largest value of lambda within 1 standard error of lambda.min

lambda_min_ridge <- ridge_cv$lambda.min
lambda_1se_ridge <- ridge_cv$lambda.1se

# plot
plot(ridge_cv, main = "", sub = "Ridge Regression CV Results")
abline(v = log(lambda_min_ridge), col = "red", lty = 2)
abline(v = log(lambda_1se_ridge), col = "blue", lty = 2)
legend("bottomright", legend = c("lambda.min", "lambda.1se"), col = c("red", "blue"), lty = 2)


# Fitting models based on lambda
# Fitting the model on the training data using lambda.min
mod.min_ridge <- glmnet(trn_x, trn_y, alpha = 0, lambda = ridge_cv$lambda.min)

# Regression coefficients (lambda.min)
round(coef(mod.min_ridge), 2)

# Fitting the model on the training data using lambda.1se
mod.1se_ridge <- glmnet(trn_x, trn_y, alpha = 0, lambda = ridge_cv$lambda.1se)


# Regression coefficients (lambda.1se)
round(coef(mod.1se_ridge), 2)


# Train data prediction
prd.trn_ridge <- round(predict(mod.1se_ridge, newx = trn_x), 2)
trn.rmse.ridge <- round(rmse(trn_y, prd.trn_ridge), 2)


# Test data prediction
prd.tst_ridge <- round(predict(mod.1se_ridge, newx = tst_x),2)
tst.rmse.ridge <- round(rmse(tst_y, prd.tst_ridge), 2)


# Results of Ridge regression

ridge_results <- matrix(
  c(trn.rmse.ridge,tst.rmse.ridge),
  nrow = 1,
  ncol = 2,
  byrow = TRUE
)
row.names(ridge_results) <- c("Ridge")
colnames(ridge_results) <- c("Train", "Test")

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>Lasso<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Finding the best values for lambda using cross-validation
# alpha = 1 for Lasso (L2)

set.seed(123)
lasso_cv <- cv.glmnet(trn_x, trn_y, alpha = 1, nfolds = 10)

round(log(lasso_cv$lambda.min), 2)
round(log(lasso_cv$lambda.1se), 2)

# optimal values of lambda, minimizes the prediction error
# lambd.min - minimizes out of sample loss
# lambda.1se - largest value of lambda within 1 standard error of lambda.min

lambda_min_lasso <- lasso_cv$lambda.min
lambda_1se_lasso <- lasso_cv$lambda.1se

# plot
plot(lasso_cv, main = "", sub = "LASSO Regression CV Results")
abline(v = log(lambda_min_lasso), col = "red", lty = 2)
abline(v = log(lambda_1se_lasso), col = "blue", lty = 2)
legend("topleft", legend = c("lambda.min", "lambda.1se"), col = c("red", "blue"), lty = 2)

# Fitting models based on lambda
# Fitting the model on the training data using lambda.min
mod.min_lasso <- glmnet(trn_x, trn_y, alpha = 1, lambda = lasso_cv$lambda.min)

# Regression coefficients (lambda.min)
round(coef(mod.min_lasso), 2)

# Fitting the model on the training data using lambda.1se
mod.1se_lasso <- glmnet(trn_x, trn_y, alpha = 1, lambda = lasso_cv$lambda.1se)


# Regression coefficients (lambda.1se)
round(coef(mod.1se_lasso), 2)


# Train data prediction
prd.trn_lasso <- round(predict(mod.1se_lasso, newx = trn_x), 2)
trn.rmse.lasso <- round(rmse(trn_y, prd.trn_lasso), 2)


# Test data prediction
prd.tst_lasso <- round(predict(mod.1se_lasso, newx = tst_x),2)
tst.rmse.lasso <- round(rmse(tst_y, prd.tst_lasso), 2)


# Results of Lasso regression

lasso_results <- matrix(
  c(trn.rmse.lasso,tst.rmse.lasso),
  nrow = 1,
  ncol = 2,
  byrow = TRUE
)
row.names(lasso_results) <- c("LASSO")
colnames(lasso_results) <- c("Train", "Test")




# Comparing RMSE values for Ridge (L1) and  Lasso (L2)

final_results <- matrix(
  c(trn.rmse.ridge, trn.rmse.lasso, tst.rmse.ridge, tst.rmse.lasso),
  nrow = 2,
  ncol = 2,
  byrow = TRUE
)
row.names(final_results) <- c("Train", "Test")
colnames(final_results) <- c("Ridge", "Lasso")

# Difference of RMSE Train and Test for Ridge L1 and Lasso L2
dfr_ride <- (trn.rmse.ridge - tst.rmse.ridge)
dfr_lasso <- (trn.rmse.lasso - tst.rmse.lasso)



