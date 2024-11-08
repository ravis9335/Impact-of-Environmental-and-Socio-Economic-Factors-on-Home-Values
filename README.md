# Impact-of-Environmental-and-Socio-Economic-Factors-on-Home-Values

Project Overview

This project explores how environmental and socio-economic factors influence the median value of homes using the Boston Housing dataset. Focusing on key variables like nitric oxide concentration (NOX), crime rate per capita (CRIM), and the average number of rooms (RM), this analysis aims to understand how these factors individually and collectively impact housing prices.

Data

The dataset is a subset of the Boston Housing dataset, with primary variables including:
	•	NOX: Nitric oxide levels, serving as an environmental quality indicator.
	•	CRIM: Crime rate per capita, reflecting neighborhood safety.
	•	RM: Average number of rooms, indicating housing space.

Code Examples

Below are some essential code snippets used in this analysis:

Data Exploration

To gain initial insights, we started by exploring the data with summary statistics and visualizations:
```
library(MASS)
data(Boston)

# Summary statistics
summary(Boston)

# Pairwise correlation analysis
cor(Boston)

# Pairwise scatter plot
pairs(~nox + crim + rm + medv, data = Boston) 
```

Linear Regression Model

A baseline linear regression model was created to quantify the relationships between predictors and median home values (MEDV):

```
# Linear regression with NOX, CRIM, and RM as predictors for MEDV
linear_model <- lm(medv ~ nox + crim + rm, data = Boston)
summary(linear_model)
plot(linear_model)
```

Ridge Regression

To improve model stability and address multicollinearity, we applied ridge regression, which applies regularization to the linear model:
```
# Install and load glmnet for ridge regression
install.packages("glmnet")
library(glmnet)

# Prepare data for glmnet
x <- model.matrix(medv ~ nox + crim + rm - 1, data = Boston)  # -1 removes the intercept
y <- Boston$medv

# Fit Ridge regression model
grid <- 10^seq(10, -2, length = 100)
ridge_model <- cv.glmnet(x, y, alpha = 0, lambda = grid, standardize = TRUE)
plot(ridge_model)
coef(ridge_model, s = "lambda.min")
```

Key Findings

•	Environmental Impact: Higher NOX levels correlate with lower home values, suggesting pollution is a major deterrent.
•	Crime Rate: Crime per capita negatively affects property values, though less strongly than NOX.
•	Housing Size: Both smaller and larger homes command premium prices, indicating niche market preferences.

Conclusion

This project reveals that non-linear modeling techniques like Generalized Additive Models (GAMs) can significantly improve model performance, offering nuanced insights into housing markets that linear models may overlook. The findings suggest that environmental quality, represented by NOX levels, may impact home values more significantly than crime rates in the Boston area.
