# -------------------------
# Name - Anish Rao
# Student number - 20066423
# Group C
# Dataset - Iris
# 2 categorical variables: Species, Region
# 4 continuous variables: Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
# -------------------------

data(iris)
head(iris)
summary(iris)

# Question 1
# ---------------------------------------------------------------------
# a) Describe the dataset using appropriate plots/curves/charts,…
# ---------------------------------------------------------------------

hist(iris$Sepal.Length, main='Histogram of Sepal Length', 
     xlab='Sepal Length', col='skyblue');
hist(iris$Sepal.Width, main='Histogram of Sepal Width', 
     xlab='Sepal Width', col='lightgreen');
boxplot(iris$Petal.Length, main='Boxplot of Petal Length', col='orange')
boxplot(iris$Petal.Width, main='Boxplot of Petal Width', col='pink')

species_count = table(iris$Species)
barplot(species_count, main='Species Distribution', 
        xlab='Species', ylab='Count', col='yellow')

# ---------------------------------------------------------------------
# b) Consider one of continuous attributes, and compute central
#    and variational measures.
# ---------------------------------------------------------------------

sepal_length = iris$Sepal.Length

mean_sepal_length = mean(sepal_length)
median_sepal_length = median(sepal_length)

range_sepal_length = range(sepal_length)
variance_sepal_length = var(sepal_length)
sd_sepal_length = sd(sepal_length)
iqr_sepal_length = IQR(sepal_length)

cat("Central Measures for Sepal Length: \nMean:", mean_sepal_length, 
    "\nMedian:", median_sepal_length, 
    "\n\nVariational Measures for Sepal Length: \nRange:", 
    range_sepal_length[1], "to", range_sepal_length[2], 
    "\nVariance:", variance_sepal_length, 
    "\nStandard Deviation:", sd_sepal_length,
    "\nIQR:", iqr_sepal_length)

# ---------------------------------------------------------------------
# c) For a particular variable of the dataset, use Chebyshev's rule,
#    and propose one-sigma interval.
#    Based on your proposed interval, specify the outliers if any.
# ---------------------------------------------------------------------

lower_bound = mean_sepal_length - sd_sepal_length
upper_bound = mean_sepal_length + sd_sepal_length
cat('One-sigma interval (Mean ± SD): [', lower_bound, ', ', upper_bound, ']')

cat("One-Sigma Interval for Sepal Width: 
    [", lower_bound, ",", upper_bound, "]\n")

outliers = sepal_length[sepal_length < lower_bound | sepal_length > upper_bound]
outliers_species = iris[iris$Sepal.Length %in% outliers, 
                      c("Species", "Sepal.Length")]
num_outliers = nrow(outliers_species)

cat("Outliers based on the one-sigma interval (Number of outliers:", 
    num_outliers, "):\n"); print(outliers_species)

# ---------------------------------------------------------------------
# d) Explain how the box-plot technique can be used to detect outliers.
#    Apply this technique for one attribute of the dataset
# ---------------------------------------------------------------------

Q1 = quantile(sepal_length, 0.25)
Q3 = quantile(sepal_length, 0.75)
IQR = IQR(sepal_length)
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR

boxplot_outliers = sepal_length[sepal_length < lower_bound | 
                                sepal_length > upper_bound]
boxplot_outliers_species = iris[iris$Sepal.Length %in% boxplot_outliers, c("Species", "Sepal.Length")]
num_boxplot_outliers = nrow(boxplot_outliers_species)

if(length(boxplot_outliers) == 0) {
  cat("No outliers found using the boxplot method for Sepal.Length.")
} else {
  cat("Outliers based on the boxplot method (Number of outliers:", 
      num_boxplot_outliers, "):\n"); print(boxplot_outliers_species)
}
boxplot(sepal_length, main = 'Boxplot for Sepal.Length', col = 'lightblue', horizontal = TRUE)