#============================#
# Name - Anish Rao           #
# Student number - 20066423  #
# Group C                    #
# Dataset - Diamonds         #
#============================#

# Setup
library(ggplot2)
data("diamonds")

# Question 2
# ==============================================================================
# a) Select four variables of the dataset, and propose an appropriate 
#    probability model to quantify uncertainty of each variable.
# ==============================================================================

# Normal distribution:
# Price (Continuous)
# Carat (Continuous)

# Multinomial distribution:
# Cut (Categorical)
# Color (Categorical)

# ==============================================================================
# b) For each model in part (a), estimate the parameters of model.
# ==============================================================================

# Continuous Variables - Normal Model
price_mean = mean(diamonds$price)
price_sd = sd(diamonds$price)

carat_mean = mean(diamonds$carat)
carat_sd = sd(diamonds$carat)

cat(" Normal distribution parameters for Price:\n", 
    "Mean:", price_mean, "\n", "SD:", price_sd, "\n\n", 
    "Normal distribution parameters for Carat:\n", 
    "Mean:", carat_mean, "\n", "SD:", carat_sd, "\n\n")

# Categorical Variables - Multinomial Model
cut_freq = table(diamonds$cut)
cut_prob = prop.table(cut_freq)

color_freq = table(diamonds$color)
color_prob = prop.table(color_freq)

cat("Estimated probabilities for Cut:\n"); print(cut_prob)
cat("\nEstimated probabilities for Color:\n"); print(color_prob)

# ==============================================================================
# c) Express the way in which each model can be used for the 
#    predictive analytics, then find the prediction for each attribute.
# ==============================================================================

# Predictions for Continuous Variables
predicted_price = price_mean
predicted_carat = carat_mean
cat("Predicted Price (expected value):", predicted_price, 
    "\nPredicted Carat (expected value):", predicted_carat, "\n\n")

# Predictions for Categorical Variables
predicted_cut = names(which.max(cut_prob))
predicted_color = names(which.max(color_prob))
cat("Predicted Cut (most common):", predicted_cut, 
    "\nPredicted Color (most common):", predicted_color)