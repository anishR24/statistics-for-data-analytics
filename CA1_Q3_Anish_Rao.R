#============================#
# Name - Anish Rao           #
# Student number - 20066423  #
# Group C                    #
# Dataset - Diamonds         #
#============================#

# Setup
data("diamonds")

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

cat("Goodness of fit Statistic:", round(gof_stat, 2), 
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
#    for a proposed candidate of  at the significant level alpha=0.05.  
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