############################LASSO REGRESSION MODEL#################################
library(glmnet)

# Fit model
lasso.fit <- cv.glmnet(x_train_isOpen, data_train$is_open, alpha = 1, nfolds = 10)

# View coefficients
coef(lasso.fit)

# Calculate Train MSE
yhat_train_lasso <- predict(lasso.fit, x_train_isOpen, s = lasso.fit$lambda.min)
mse_train_lasso <- mean((data_train$is_open - yhat_train_lasso)^2)

# Calculate Test MSE
yhat_test_lasso <- predict(lasso.fit, x_test_isOpen, s = lasso.fit$lambda.min)
mse_test_lasso <- mean((data_test$is_open - yhat_test_lasso)^2)

# Compare MSEs
mse_train_lasso
mse_test_lasso

# Positive significant variables:
#### stars - review_count - amb_casual - GoodForKids - park_lot

# Negative significant variables:
#### price_range - full_bar - BikeParking - park_street