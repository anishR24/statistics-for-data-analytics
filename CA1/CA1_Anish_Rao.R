#============================#
# Name - Anish Rao           #
# Student number - 20066423  #
# Group C                    #
# Dataset - Diamonds         #
#============================#

# Setup
library(ggplot2)
data("diamonds")
str(diamonds)
summary(diamonds)


# QUESTION 1
# ==============================================================================
# a) Describe the dataset using appropriate plots/curves/charts,…
# ==============================================================================

ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Diamond Prices", x = "Price", y = "Frequency")

ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.1, fill = "tomato", color = "black") +
  labs(title = "Histogram of Diamond Carat", x = "Carat", y = "Frequency")

ggplot(diamonds, aes(x = cut)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Distribution of Diamond Cut", x = "Cut", y = "Count")

ggplot(diamonds, aes(x = color)) +
  geom_bar(fill = "pink") +
  labs(title = "Distribution of Diamond Color", x = "Color", y = "Count")

# ==============================================================================
# b) Consider one of continuous attributes, and compute central
#    and variational measures.
# ==============================================================================

price = diamonds$price
price_mean = mean(price)
price_median = median(price)
price_range = range(price)
price_variance = var(price)
price_sd = sd(price)
price_IQR = IQR(price)

cat("Central Measures for Price: \nMean:", price_mean, 
    "\nMedian:", price_median, 
    "\n\nVariational Measures for Price: \nRange:", 
    price_range[1], "to", price_range[2], 
    "\nVariance:", price_variance, 
    "\nStandard Deviation:", price_sd,
    "\nIQR:", price_IQR)

# ==============================================================================
# c) For a particular variable of the dataset, use Chebyshev's rule,
#    and propose one-sigma interval.
#    Based on your proposed interval, specify the outliers if any.
# ==============================================================================

lower_bound = price_mean - price_sd
upper_bound = price_mean + price_sd
cat("One-sigma interval (Mean ± SD): [", lower_bound, ",", upper_bound, "]\n")

price_outliers = diamonds[diamonds$price < lower_bound 
                          | diamonds$price > upper_bound, ]
cat("Number of outliers:", nrow(price_outliers), "\n")

# ==============================================================================
# d) Explain how the box-plot technique can be used to detect outliers.
#    Apply this technique for one attribute of the dataset
# ==============================================================================

first_quartile = quantile(diamonds$price, 0.25)
third_quartile = quantile(diamonds$price, 0.75)
price_IQR = IQR(diamonds$price)

lower_limit = first_quartile - 1.5 * price_IQR
upper_limit = third_quartile + 1.5 * price_IQR

price_outlier_values = diamonds$price[diamonds$price < lower_limit | 
                                        diamonds$price > upper_limit]
num_outliers = length(price_outlier_values)
if(num_outliers == 0) {
  cat("No outliers detected using the boxplot method.\n")
} else {
  cat("Outliers detected using the boxplot method:", num_outliers, "\n")
}

boxplot(diamonds$price,
        main = "Boxplot for Diamond Prices",
        col = "lightgreen",
        horizontal = TRUE,
        xlab = "Price")

# ==============================================================================
# ==============================================================================

# QUESTION 2
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

# ==============================================================================
# ==============================================================================

# Question 3
# ==============================================================================
# a) Consider two categorical variables of the dataset, 
#    develop a binary decision making strategy to check whether two variables 
#    are independent at the significant level alpha=0.01.  To do so,
# ==============================================================================

# =========================
# (i) State the hypotheses.
# =========================
# H₀: Cut and Color are independent.
# Hₐ: Cut and Color are not independent.

# ============================================
# (ii) Find the statistic and critical values.
# ============================================
contingency_table = table(diamonds$cut, diamonds$color)
chi_test = chisq.test(contingency_table)

chi_stat = chi_test$statistic
df = chi_test$parameter

# critical value = qchisq(1 - alpha, df)
critical_value = qchisq(0.99, df)

cat("Chi-square Statistic:", chi_stat, 
    "\nDegrees of Freedom:", df, 
    "\nCritical Value (alpha = 0.01):", critical_value, "\n")

# ==================================================
# (iii) Explain your decision and Interpret results.
# ==================================================

if (chi_stat > critical_value) {
  cat("Reject H₀: chi_stat =", round(chi_stat, 2), 
      ">", round(critical_value, 2),
      "; Cut and Color are dependent.\n")
} else {
  cat("Fail to reject H₀: chi_stat =", round(chi_stat, 2), 
      "<=", round(critical_value, 2),
      "; insufficient evidence of dependency.\n")
}

# ==============================================================================
# b) Consider one categorical variable, apply goodness of fit test to evaluate 
#    whether a candidate set of probabilities can be appropriate to quantify 
#    the uncertainty of class frequency at the significant level alpha=0.05.
# ==============================================================================

# Hypotheses:
# H₀: The observed frequencies of 'Cut' follow the candidate probabilities.
# Hₐ: They do not.

observed = table(diamonds$cut)
n_levels = length(observed)
candidate_prob = rep(1/n_levels, n_levels)

gof = chisq.test(observed, p = candidate_prob)
gof_stat = gof$statistic
df = gof$parameter
critical_value = qchisq(0.95, df)

cat("Chi-square Statistic:", round(gof_stat, 2), 
    "\nDegrees of Freedom:", df, 
    "\nCritical Value (alpha = 0.05):", round(critical_value, 2), "\n")

if (gof_stat > critical_value) {
  cat("Reject H₀: gof_stat =", round(gof_stat, 2), 
      ">", round(critical_value, 2),
      "; the candidate probabilities do not fit the data.\n")
} else {
  cat("Fail to reject H₀: gof_stat =", round(gof_stat, 2), 
      "<=", round(critical_value, 2),
      "; the candidate probabilities are appropriate.\n")
}

# ==============================================================================
# c) Consider one continuous variable in the dataset, and apply test of mean 
#    for a proposed candidate of μ at the significant level alpha=0.05.  
# ==============================================================================

candidate_mean = 4000
t_test_result = t.test(diamonds$price, mu = candidate_mean)

t_stat = t_test_result$statistic
df = t_test_result$parameter
critical_value = qt(0.975, df)

cat("T-statistic:", round(t_stat, 2), 
    "\nDegrees of Freedom:", df, 
    "\nCritical Value (alpha = 0.05):", round(critical_value, 2), "\n\n")

if (abs(t_stat) > critical_value) {
  cat("Reject H₀: |t_stat| =", round(abs(t_stat), 2), 
      ">", round(critical_value, 2),
      "; significant evidence that the mean is not", candidate_mean, "\n")
} else {
  cat("Fail to reject H₀: |t_stat| =", round(abs(t_stat), 2), 
      "<=", round(critical_value, 2),
      "; insufficient evidence to conclude the mean differs from", 
      candidate_mean, "\n")
}

# ==============================================================================
# ==============================================================================