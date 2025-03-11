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

# Question 1
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

price_outliers = diamonds[diamonds$price < lower_bound | diamonds$price > upper_bound, ]
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