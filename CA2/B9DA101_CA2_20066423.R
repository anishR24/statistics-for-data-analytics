#============================#
# Name - Anish Rao
# Student number - 20066423
# Course - B9DA101
# Group C
#============================#

library(caret)
library(dplyr)
library(forecast)
library(ggplot2)
library(tseries)
set.seed(86)

data("mtcars")
head(mtcars)
str(mtcars)
summary(mtcars)

data("EuStockMarkets")
head(EuStockMarkets)
str(EuStockMarkets)
summary(EuStockMarkets)


# QUESTION 1
# Consider a relational dataset and specify your input and output variables:
# ==============================================================================
# a) Train the model using 80% of this dataset
#    and suggest an appropriate GLM to model ouput to input variables.
# ==============================================================================

mtcars$am = as.factor(mtcars$am)
mtcars$cyl = as.factor(mtcars$cyl)

data_split = createDataPartition(mtcars$am, p = 0.8, list = FALSE)
train_data = mtcars[data_split, ]
test_data = mtcars[-data_split, ]

model = glm(am ~ mpg + hp + wt + cyl, data = train_data, family = binomial)

# ==============================================================================
# b) Specify the significant variables on the output variable
#    at the level of 𝛼=0.05 and explore the related hypotheses test.
#    Estimate the parameters of your model.
# ==============================================================================

summary(model)

# No variables are significant at 𝛼=0.05
# Null hypothesis H0: No effect on output variable "am"
# Alternative hypothesis H1: effects output variable "am"
# All p-values > 0.05, we cant reject null hypothesis for any variables
# Estimated parameters:
# (Intercept): -1280.950
# mpg        : 40.998
# hp         : 6.413
# wt         : -98.671
# cyl6       : -5.707
# cyl8       : -480.280

# ==============================================================================
# c) Predict the output of the test dataset using the trained model.
#    Provide the functional form of the optimal predictive model.
# ==============================================================================

predicted_prob = predict(model, newdata = test_data, type = "response")

predicted_class = ifelse(predicted_prob > 0.5, 1, 0)
actual_class = test_data$am

print(data.frame(Actual = actual_class, Predicted = predicted_class))

# ==============================================================================
# d) Provide the confusion matrix and obtain the probability of correctness 
#    of predictions.
# ==============================================================================

predicted_class = as.factor(predicted_class)
actual_class = as.factor(actual_class)

confusion_matrix = confusionMatrix(predicted_class, actual_class)

print(confusion_matrix)

# ==============================================================================
# ==============================================================================

# QUESTION 2
# Let x_1,…,x_10 are identically independently distributed (iid) with Poisson(λ)
# ==============================================================================
# a) Compute the likelihood function (LF).
# ==============================================================================

x = rpois(10, lambda = 3)
print(x)

likelihood = function(lambda, data) {
  n = length(data)
  sum_x = sum(data)
  return((lambda ^ sum_x) * exp(-n * lambda))
}

lambda_values = seq(1, 6, by = 0.1)
likelihood_values = sapply(lambda_values, likelihood, data = x)

plot(lambda_values, likelihood_values,
     type = "l",
     col = "red",
     main = "Likelihood Function",
     xlab = "Lambda", ylab = "Likelihood")

# ==============================================================================
# b) Adopt the appropriate conjugate prior to the parameter λ.
#   (Hint: Choose hyperparameters optionally within the support of distribution)
# ==============================================================================

alpha = 2
beta = 1
lambda_vals = seq(0, 10, by = 0.1)

prior_density = dgamma(lambda_vals, shape = alpha, rate = beta)

plot(lambda_vals, prior_density,
     type = "l",
     col = "red",
     main = "Gamma Prior for λ (α = 2, β = 1)",
     xlab = "Lambda", ylab = "Density")

# ==============================================================================
# c) Using (a) and (b), find the posterior distribution of λ.
# ==============================================================================

sum_x = sum(x)
n = length(x)

post_alpha = alpha + sum_x
post_beta = beta + n

cat("Posterior: Gamma(", post_alpha, ",", post_beta, ")\n")

# ==============================================================================
# d) Compute the minimum Bayesian risk estimator of λ.
# ==============================================================================

lambda_bayes = post_alpha / post_beta

cat("Bayesian Estimator of λ:", lambda_bayes, "\n")

# ==============================================================================
# ==============================================================================

# QUESTION 3
# Use a particular stock market dataset 
# and apply the following steps to accomplish the time series analysis:
# ==============================================================================
# a) Check whether the time series is stationary in mean and variance.
# ==============================================================================

dax = EuStockMarkets[, "DAX"]

plot(dax,
     type = "l", 
     col = "red",
     main = "DAX Index Time Series", 
     xlab = "Time", ylab = "Price")

adf_result = adf.test(dax)
print(adf_result)

sd_full = sd(dax)
sd_first_half = sd(dax[1:(length(dax)/2)])
sd_second_half = sd(dax[((length(dax)/2) + 1):length(dax)])

cat("Full SD:", sd_full, "\n")
cat("First Half SD:", sd_first_half, "\n")
cat("Second Half SD:", sd_second_half, "\n")

# ==============================================================================
# b) Use acf() and pacf() functions to identify the order of AR and MA.
# ==============================================================================

dax_diff = diff(dax)

plot(dax_diff, 
     type = "l", 
     col = "red",
     main = "Differenced DAX Series", 
     xlab = "Time", ylab = "Diff Price")

acf(dax_diff, main = "ACF")

pacf(dax_diff, main = "PACF")

# ==============================================================================
# c) Use auto.arima() to learn the best ARIMA model.
# ==============================================================================

best_model = auto.arima(dax)
print(best_model)

# ==============================================================================
# d) Forecast h=10 step ahead prediction of the time series variable
#    and plot it with the original time series.
# ==============================================================================

forecast_result = forecast(best_model, h = 10)

plot(forecast_result,
     main = "10 step Forecast",
     xlab = "Time", ylab = "DAX Value")

# ==============================================================================
# ==============================================================================