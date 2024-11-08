library(MASS)
data(Boston)


# Summary statistics
summary(Boston)

# Pairwise correlations between variables
cor(Boston)

# Visual exploration
pairs(Boston)

# Linear regression with NOX, CRIM, and RM as predictors for MEDV
linear_model <- lm(medv ~ nox + crim + rm, data = Boston)
summary(linear_model)
plot(linear_model)

# Visual exploration
pairs(~nox + crim + rm + medv, data = Boston)

# Diagnostic plots
par(mfrow = c(2, 2))  # Organize plots in a 2x2 grid
plot(linear_model)


# Ridge regression
install.packages("glmnet")

library(glmnet)

# Prepare the model matrix for glmnet
x <- model.matrix(medv ~ nox + crim + rm - 1, data = Boston)  # -1 removes the intercept
y <- Boston$medv

# Fit Ridge regression model
grid <- 10^seq(10, -2, length = 100)
ridge_model <- cv.glmnet(x, y, alpha = 0, lambda = grid, standardize = TRUE)
plot(ridge_model)
coef(ridge_model, s = "lambda.min")


# Fit Lasso regression model
lasso_model <- cv.glmnet(x, y, alpha = 1, lambda = grid, standardize = TRUE)
plot(lasso_model)
coef(lasso_model, s = "lambda.min")
summary(lasso_model)


# Non Parametric Regression
# B-spline
library(splines)
library(MASS)


# For simplicity, placing knots at the quartiles of each predictor variable
knots_rm <- quantile(Boston$rm, probs = c(0.25, 0.5, 0.75))
knots_nox <- quantile(Boston$nox, probs = c(0.25, 0.5, 0.75))
knots_crim <- quantile(Boston$crim, probs = c(0.25, 0.5, 0.75))

# cubic B-spline regression model with multiple predictors
spline_model <- lm(medv ~ bs(rm, knots = knots_rm, degree = 3) + 
                     bs(nox, knots = knots_nox, degree = 3) + 
                     bs(crim, knots = knots_crim, degree = 3), 
                   data = Boston)
summary(spline_model)
plot(spline_model)
coef(spline_model)

# a grid of values for plotting rm
rm_grid <- with(Boston, expand.grid(rm = seq(min(rm), max(rm), length.out = 100),
                                    nox = mean(nox),
                                    crim = mean(crim)))

# Predicting MEDV over the grid
rm_pred <- predict(spline_model, newdata = rm_grid)

# Plot rm
plot(Boston$rm, Boston$medv, main = "Spline Fit for RM", xlab = "Average number of rooms per dwelling (RM)", ylab = "Median value of homes (MEDV)")
lines(rm_grid$rm, rm_pred, col = "blue", lwd = 2)



#nox grid
nox_grid <- with(Boston, expand.grid(nox = seq(min(nox), max(nox), length.out = 100),
                                    rm = mean(rm),
                                    crim = mean(crim)))

# Predict MEDV over the grid
nox_pred <- predict(spline_model, newdata = nox_grid)

# Plot
plot(Boston$nox, Boston$medv, main = "Spline Fit for NOX", xlab = "Average NOX", ylab = "Median value of homes (MEDV)")
lines(nox_grid$nox, nox_pred, col = "green", lwd = 2)

install.packages("nlme")
library(mgcv)
library(MASS) 

# Fit a GAM model with splines for RM, NOX, and CRIM
gam_model <- gam(medv ~ s(rm) + s(nox) + s(crim), data = Boston, method = "REML")

# Summary of the model
summary(gam_model)


#Estimating the unkown function
newdata_rm <- with(Boston, expand.grid(rm = seq(min(rm), max(rm), length.out = 200),
                                       nox = mean(nox), crim = mean(crim)))
newdata_nox <- with(Boston, expand.grid(nox = seq(min(nox), max(nox), length.out = 200),
                                        rm = mean(rm), crim = mean(crim)))
newdata_crim <- with(Boston, expand.grid(crim = seq(min(crim), max(crim), length.out = 200),
                                         rm = mean(rm), nox = mean(nox)))
# Predict medv
preds_rm <- predict(gam_model, newdata_rm, type="response")
preds_nox <- predict(gam_model, newdata_nox, type="response")
preds_crim <- predict(gam_model, newdata_crim, type="response")

par(mfrow = c(3, 1))

# Plot for RM
plot(newdata_rm$rm, preds_rm, type = "l", main = "Effect of RM on MEDV",
     xlab = "Average Number of Rooms", ylab = "Predicted MEDV", col = "blue")
abline(h = mean(Boston$medv), lty = 2)

# Plot for NOX
plot(newdata_nox$nox, preds_nox, type = "l", main = "Effect of NOX on MEDV",
     xlab = "Nitric Oxides Concentration", ylab = "Predicted MEDV", col = "red")
abline(h = mean(Boston$medv), lty = 2)

# Plot for CRIM
plot(newdata_crim$crim, preds_crim, type = "l", main = "Effect of CRIM on MEDV",
     xlab = "Per Capita Crime Rate", ylab = "Predicted MEDV", col = "green")
abline(h = mean(Boston$medv), lty = 2)

par(mfrow = c(1, 1))

# The horizontal dashed line representing the overall mean of medv isn't visible in the plot for CRIM,
# it's likely because the range of predicted values for medv based on CRIM doesn't include the overall mean of medv

# Calculate the overall mean of MEDV
medv_mean <- mean(Boston$medv)

# Adjusting the plot for CRIM to ensure the mean line is visible
plot(newdata_crim$crim, preds_crim, type = "l", main = "Effect of CRIM on MEDV",
     xlab = "Per Capita Crime Rate", ylab = "Predicted MEDV", col = "green",
     ylim = c(min(preds_crim, medv_mean) - 2, max(preds_crim, medv_mean) + 2)) 

# Add a horizontal line for the overall mean of MEDV
abline(h = medv_mean, lty = 2)


# For example, plotting with confidence intervals for 'rm'
plot(gam_model, select = 1, shade = TRUE, se = TRUE, main = "Effect of RM on MEDV with Confidence Interval")


# Predictions with confidence intervals
preds_with_ci <- predict(gam_model, newdata = Boston, type = "link", se.fit = TRUE)

# Calculate upper and lower bounds of the 95% confidence interval
ci_lower <- preds_with_ci$fit - 1.96 * preds_with_ci$se.fit
ci_upper <- preds_with_ci$fit + 1.96 * preds_with_ci$se.fit









