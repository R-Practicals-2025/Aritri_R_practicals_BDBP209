# III. Two sample tests
# (1) Two sample Z test:
# (a) Write a function to perform a two sample Z test given data sets x1, x2, their respec-tive standard deviations, siginficance level and null hypothesis. As above, the func-
# tion should return the conclusions of the test along with the statistic values. The function should be defined as two_sample_Z_test(x1,x2,sigma_x1,sigma_x2,alpha,null)

two_sample_Z_test <- function(x1, x2, sigma_x1, sigma_x2, alpha = 0.05, null = "equal") {
  n1 <- length(x1)
  n2 <- length(x2)
  mean1 <- mean(x1)
  mean2 <- mean(x2)
  
  # Standard error
  se <- sqrt((sigma_x1^2 / n1) + (sigma_x2^2 / n2))
  
  # Z-statistic
  Z <- (mean1 - mean2) / se
  
  # Calculate p-value based on null type
  if (null == "equal") {
    p_val <- 2 * (1 - pnorm(abs(Z)))
    conclusion <- ifelse(p_val < alpha, "Reject H0: μ1 = μ2", "Fail to reject H0")
  } else if (null == "greater_than_or_equal") {
    p_val <- pnorm(Z)
    conclusion <- ifelse(p_val < alpha, "Reject H0: μ1 ≥ μ2 (Accept μ1 < μ2)", "Fail to reject H0")
  } else if (null == "less_than_or_equal") {
    p_val <- 1 - pnorm(Z)
    conclusion <- ifelse(p_val < alpha, "Reject H0: μ1 ≤ μ2 (Accept μ1 > μ2)", "Fail to reject H0")
  } else {
    stop("Invalid null hypothesis type. Use 'equal', 'greater_than_or_equal', or 'less_than_or_equal'")
  }
  
  return(list(
    Z_statistic = Z,
    p_value = p_val,
    conclusion = conclusion
  ))
}


# (b) Use the data given in two-sample.dat for this problem with σx1 = 24.6 and
# σx2 = 27.8 with α = 0.05 to test the null hypothesis that the μ1 ≥ μ2.

# Read the data file (adjust path and format as necessary)
data <- read.table("two-sample.dat", header = FALSE)  # Or use read.csv()

x1 <- data$V1
x2 <- data$V2

# Given standard deviations and alpha
sigma_x1 <- 24.6
sigma_x2 <- 27.8
alpha <- 0.05

# Test H0: μ1 ≥ μ2
result <- two_sample_Z_test(x1, x2, sigma_x1, sigma_x2, alpha, null = "greater_than_or_equal")

# Show result
print(result)


###############################################################################################################################

# (2) Two sample t-test: In R, both one-sample and two-sample t-tests can be performed with
# the library function t.test() with the following arguments:
# t.test(x,y,alternative,mu,paired,var.equal,conf.level) where x,y are the data
# vectors, alternative is a character string specifying the alternate hypothesis of 3 pos-sible values: two.sided, less and greater. One can also input a vector with all three,
# in which case all the 3 hypotheses are tested, the default value is two.sided. For one-
# sample, mu is the hypothesized mean of the population, and for two samples, it is the
# hypothesized difference of the means. A conf.level=0.95 sets the confidence level at 95%.
# (a) Welsch’s test: use the data sets to carry out the t-test for equality of means as H0.
# Print the results summary.
# Xvar=c(4.95,5.37,4.70,4.96,4.72,5.17,5.28,5.12,5.26,5.48)
# Yvar=c(4.65,4.86,4.57,4.56,4.96,4.63,5.04,4.92,5.37,4.58,4.26,4.40)

# Given data
Xvar <- c(4.95,5.37,4.70,4.96,4.72,5.17,5.28,5.12,5.26,5.48)
Yvar <- c(4.65,4.86,4.57,4.56,4.96,4.63,5.04,4.92,5.37,4.58,4.26,4.40)

# Perform Welch's t-test (default is var.equal = FALSE)
t_test_result <- t.test(Xvar, Yvar, alternative = "two.sided", var.equal = FALSE)

# Print the summary
print(t_test_result)



# (b) Dependent variables: Use the data sets below to carry out a paired t-test for a
# significance level of 0.05. In this case, do we need to input μ?
# data_before = c(95,106,79,71,90,79,71,77,103,103,92,63,82,76)
# data_after = c(97,116,82,81,82,86,107,86,94,91,85,98,91,87)

# Given paired data
data_before <- c(95,106,79,71,90,79,71,77,103,103,92,63,82,76)
data_after  <- c(97,116,82,81,82,86,107,86,94,91,85,98,91,87)

# Perform paired t-test
paired_test_result <- t.test(data_before, data_after,
                             alternative = "two.sided",
                             paired = TRUE,
                             conf.level = 0.95)

# Print summary
print(paired_test_result)

###############################################################################################################################

# (3) Two-sample proportion test: In R, the function prop.test() can perform proportion
# tests for one, two or more proportions. Here we will also learn the Fisher’s test applicable
# for small samples. The input is changed as follows:
# prop.test(x,n,p=NULL,alternative="two.sided",correct=TRUE)
# fisher.test(x,alternative="two.sided",conf.int=TRUE,conf.level=0.95)
# where x is a 2x2 matrix containing the values of the contingency table under different cat-egories for the Fisher’s test and a data vector of counts of successes for the prop.test(),
# n is a data vector containing number of trials in which x successes were observed, p is a
# vector of probabilites of successes. The alternative can be two.sided, less or more.
# (a) Perform a prop.test() test for the following problem with H0 that the proportions
# are equal. In a health survey, 520 out of 600 men and 550 out of 600 women
# questioned said they use antibiotics whenever fever continues for more than 2 days.
# We want to test whether there is a significant difference in the fraction of men and
# women who start taking antibotics after 2 days of fever.

# Number of successes
x <- c(520, 550)

# Number of trials
n <- c(600, 600)

# Two-sample proportion test (with continuity correction)
prop_test_result <- prop.test(x = x, n = n, alternative = "two.sided", correct = TRUE)

# Print results
print(prop_test_result)



# (b) Perform a Fisher’s exact test for the following data to find whether the patients from
# higher income group indulge in tobacco abuse in a significantly different proportion
# than the patients from the lower income group.
# 
# Higher-income Lower-income
# Tobacco abuse 11 17
# No abuse 42 39

# 2x2 contingency matrix
tobacco_matrix <- matrix(c(11, 42, 17, 39), nrow = 2, byrow = TRUE)

# Fisher's exact test
fisher_test_result <- fisher.test(tobacco_matrix, alternative = "two.sided", conf.int = TRUE)

# Print results
print(fisher_test_result)

#As p-value > 0.05, then there's no significant difference in tobacco use between income groups.
##############################################################################################################################

# (4) Two-sample variance test: In R, we will use the F-distribution functions (qf()) to carry
# out the two-sample variance test.
# (a) Write a function of the form two_sample_variance_test(x,y,alpha) that out-
# puts the statistical conclusion along with the statistical values of F and p-values.


two_sample_variance_test <- function(x, y, alpha = 0.05) {
  # Compute variances and sample sizes
  s1_sq <- var(x)
  s2_sq <- var(y)
  n1 <- length(x)
  n2 <- length(y)
  
  # Ensure F statistic is >1 by putting the larger variance in the numerator
  if (s1_sq > s2_sq) {
    F_stat <- s1_sq / s2_sq
    df1 <- n1 - 1
    df2 <- n2 - 1
  } else {
    F_stat <- s2_sq / s1_sq
    df1 <- n2 - 1
    df2 <- n1 - 1
  }
  
  # p-value from F-distribution (two-tailed)
  p_value <- 2 * min(
    pf(F_stat, df1, df2, lower.tail = FALSE),
    pf(F_stat, df1, df2, lower.tail = TRUE)
  )
  
  # Decision
  conclusion <- if (p_value < alpha) {
    "Reject the null hypothesis: Variances are significantly different."
  } else {
    "Fail to reject the null hypothesis: No significant difference in variances."
  }
  
  # Return results
  return(list(F_value = F_stat, p_value = p_value, conclusion = conclusion))
}



# (b) Use the data sets below to carry out this test with α = 0.05.
# x = c(1067.7, 984.3,998.8,1025.9,1060.9,959.1,1013.8,
#       1047.0,987.8,1051.0,885.2,
#       1049.5,1098.2,1001.5,1011.1,991.6)
# 
# y = c(957.6, 981.8, 1096.5, 984.4, 1074.3, 929.4, 1056.0,
#       1012.3, 1040.7, 1099.5,
#       1006.1, 1064.3, 865.6, 944.4, 1091.8, 952.1)

x <- c(1067.7, 984.3,998.8,1025.9,1060.9,959.1,1013.8,
       1047.0,987.8,1051.0,885.2,1049.5,1098.2,1001.5,1011.1,991.6)

y <- c(957.6, 981.8, 1096.5, 984.4, 1074.3, 929.4, 1056.0,
       1012.3, 1040.7, 1099.5, 1006.1, 1064.3, 865.6, 944.4, 1091.8, 952.1)

# Run the test
result <- two_sample_variance_test(x, y, alpha = 0.05)
print(result)

###############################################################################################################################


# (5) Wilcoxon signed rank test for two dependent samples: This is carried out using wilcox.test()
# function in R again, and the parameters are already described above in the one sample
# tests. Carry out this test for the following data with conf.level=0.95 for the null hy-pothesis that the mean for the paired sample is greater than 0, i.e. the two samples have different means.
# Pre_therapy : 74, 72, 62, 58, 59, 65, 54, 63, 80, 66, 65, 64, 79, 60
# Post_therapy : 79, 55, 53, 53, 74, 55, 64, 55, 39, 44, 37, 68, 54, 54

# Data for Pre-therapy and Post-therapy
Pre_therapy <- c(74, 72, 62, 58, 59, 65, 54, 63, 80, 66, 65, 64, 79, 60)
Post_therapy <- c(79, 55, 53, 53, 74, 55, 64, 55, 39, 44, 37, 68, 54, 54)

# Wilcoxon signed rank test
result <- wilcox.test(Pre_therapy, Post_therapy, alternative = "greater", conf.level = 0.95)

# Print result
print(result)

#As p-value < 0.05, we reject the null hypothesis, suggesting that the Pre-therapy values are significantly higher than the Post-therapy values.

#####################################################################################################################################


# (6) Wilcoxon rank sum test for unpaired samples and Mann-Whitney test: Use the wilcox.test()
# function to carry out the Wilcoxon rank sum test for two independent samples given be-low with the alternate hypothesis that the placebo population has a smaller mean than
# that exposed to the drug. Use a confidence level of 95%.
# drug : 31.7,75.0,101.1,60.5,62.8,59.3,58.9,91.3,99.1,52.0,39.1
# placebo : 59.3,72.7,100.5,64.7,69.0,72.7,69.6,97.4,100.6,65.1,65.7


# Data for drug and placebo
drug <- c(31.7, 75.0, 101.1, 60.5, 62.8, 59.3, 58.9, 91.3, 99.1, 52.0, 39.1)
placebo <- c(59.3, 72.7, 100.5, 64.7, 69.0, 72.7, 69.6, 97.4, 100.6, 65.1, 65.7)

# Wilcoxon rank sum test (Mann-Whitney test)
result <- wilcox.test(drug, placebo, alternative = "less", conf.level = 0.95)

# Print result
print(result)

#As p-value > 0.05, we fail to reject the null hypothesis, suggesting no significant difference between the drug and placebo populations.


###############################################################################################################################


# (7) Kruskal Wallis test: In R, this test is performed by kruska.test() function.
# Group-1 : 220 214 203 184 186 200 165
# Group-2 : 262 193 225 200 164 266 179
# Group-3 : 272 192 190 208 231 235 141
# Group-4 : 190 255 247 278 230 269 289
# Reform the data above into a (x,y) form where x stands for the value and y is the
# category of the group (use rep() function to label each data point according to the
# group), then use the above R function with arguments x and y. Print the results output
# by the function.


# Data for each group
Group_1 <- c(220, 214, 203, 184, 186, 200, 165)
Group_2 <- c(262, 193, 225, 200, 164, 266, 179)
Group_3 <- c(272, 192, 190, 208, 231, 235, 141)
Group_4 <- c(190, 255, 247, 278, 230, 269, 289)

# Combine all data into a single vector (x) for the values
x <- c(Group_1, Group_2, Group_3, Group_4)

# Create a vector (y) for the group labels using rep()
y <- rep(1:4, times = c(length(Group_1), length(Group_2), length(Group_3), length(Group_4)))

# Kruskal-Wallis test
result <- kruskal.test(x ~ y)

# Print the result
print(result)

#As p-value > 0.05, we fail to reject the null hypothesis, suggesting no significant difference between the groups.

##############################################################################################################################


# (8) Chi-square GoF test: Based on what we learnt in class, write a function to perform the
# GoF test based on input data of expected and observed values. We will use qchisq()
# function to get the critical value and pchisq() to get the p-value. Use the function to
# carry out the test for the following data:
# Observed : 32, 82, 77, 49
# Expected : 40,80,80,40

# Function to perform Chi-square Goodness of Fit test
chi_square_gof_test <- function(observed, expected, alpha = 0.05) {
  # Calculate the Chi-square statistic
  chi_square_stat <- sum((observed - expected)^2 / expected)
  
  # Degrees of freedom: (Number of categories - 1)
  df <- length(observed) - 1
  
  # Get the critical value from the Chi-square distribution
  critical_value <- qchisq(1 - alpha, df)
  
  # Calculate the p-value
  p_value <- 1 - pchisq(chi_square_stat, df)
  
  # Conclusion based on p-value
  conclusion <- ifelse(p_value < alpha, "Reject the null hypothesis", "Fail to reject the null hypothesis")
  
  # Return the results
  return(list(
    chi_square_statistic = chi_square_stat,
    degrees_of_freedom = df,
    critical_value = critical_value,
    p_value = p_value,
    conclusion = conclusion
  ))
}

# Observed and Expected values
observed <- c(32, 82, 77, 49)
expected <- c(40, 80, 80, 40)

# Perform the Chi-square Goodness of Fit test
result <- chi_square_gof_test(observed, expected)

# Print the result
print(result)

#As p_value >= 0.05, we fail to reject the null hypothesis, meaning the observed frequencies are consistent with the expected frequencies.


#################################################################################################################################################

























