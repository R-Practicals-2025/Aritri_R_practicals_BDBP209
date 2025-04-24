
# I. Error bars, covariance and correlation
# An error is a small line segment around each point (X,Y) representing the uncertainity in the
# measured value Y. The bar might be any one of the following: (a) standard deviation sy, (b)
# standard error of the mean σy/√n orsy/√n and (c) confidence interval around the (unknown)
# population mean given by Z1−α/2σy/√n or if the population variance is not known, T1−α/2sy/√n.
# 
# For this we will use the arrows() function:
#   arrows(x0, y0, x1, y1, length, angle, code = 2, col, lty, lwd ) where the arrow is
# drawn from (x0,y0) to (x1,y1), with given length, angle between the arrow head and arrow
# head, and code to specify whether arrow heads at one end or both.
# (1) Error bars on bar plots: Enter the following data into R:
#   means = c(20.34,19.49,25.68)
# stderr = c(0.83,1.51,1.39)
# Make a barplot with the following features: the three means should be labeled as ‘A’,
# ‘B’ and ‘C’, grey filled bars, plot title as ’Errors on bar plot’. Use the arrows() function
# to plot the error bars as follows:
#   arrows(<barplotobject>, means+stderr, <barplotobject>,means-stderr,
#          angle=90,code=3,length=0.06,col=’red’)


# Given data
means <- c(20.34, 19.49, 25.68)
stderr <- c(0.83, 1.51, 1.39)

# Create a barplot with grey bars and labels A, B, C
bar_positions <- barplot(
  means,
  names.arg = c("A", "B", "C"),
  col = "grey",
  ylim = c(0, max(means + stderr) + 2),  # Add some space for error bars
  main = "Errors on bar plot",
  ylab = "Mean Value"
)

# Add error bars using arrows()
arrows(
  x0 = bar_positions, y0 = means + stderr,
  x1 = bar_positions, y1 = means - stderr,
  angle = 90, code = 3, length = 0.06, col = "red"
)
box()



# (2) Error bars on (x,y) plots: Enter the following data into R. The errors provided are
# standard errors on the mean
# x = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
# y = c(5, 9, 13, 17, 20, 24, 26, 27, 28, 27)
# errors = c(0.5, 0.9, 1.4, 1.5, 2.0, 2.2, 2.3, 2.5, 2.9, 3.0)
# Plot (x,y) with points and xlabel ‘concentration’ and ylabel ‘optical activity’ and plot title
# as ‘Error bars on data points”. Again we can use the arrows() function to plot the error
# bars, only difference is in the first 4 arguments: arrows(x,y+errors,x,y-errors,.....)


# Clean start: redefine all
x <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
y <- c(5, 9, 13, 17, 20, 24, 26, 27, 28, 27)
errors <- c(0.5, 0.9, 1.4, 1.5, 2.0, 2.2, 2.3, 2.5, 2.9, 3.0)

# Plot the points
plot(
  x, y,
  pch = 16,
  xlab = "concentration",
  ylab = "optical activity",
  main = "Error bars on data points",
  ylim = c(min(y - errors), max(y + errors))
)

# Add error bars
arrows(
  x0 = x, y0 = y + errors,
  x1 = x, y1 = y - errors,
  angle = 90, code = 3, length = 0.05, col = "blue"
)

# (3) Covariance and Pearson’s correlation coefficient: For a univariate sample, the functions
# cov() and cor() return a number, for multivariate samples, these functions returns a
# matrix. Try the following:
#   x : 10,20,30,40,50,60,70,80,90,100
# y : 95, 220, 279, 424, 499, 540, 720, 880, 950, 1200
# cov(x,y)
# cor(x,y)
# cor(longley) #multivariate data

x <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
y <- c(95, 220, 279, 424, 499, 540, 720, 880, 950, 1200)
cov(x, y)
cor(x, y)
cor(longley)

#########################################################################


# II. One sample tests
# (1) One sample Z test:
# (a) Write a function to perform one sample Z test, where given a data set x that is
# randomly drawn from a Gaussian distribution of population mean μ and standard
# deviation σ the function returns the conclusions of the test along with the computed
# statistic values. The function should be defined as
# one_sample_Ztest(x,sigma,muzero, alpha,null) where x is the data vector,
# sigma is the population standard deviation, muzero is the population mean for
# comparison, alpha is the significance level and null is a string indicating type of
# null hypothesis. The possible values of null should be equal, less_than_or_equal
# or more_than_or_equal. The function should return a vector with two numbers
# and the conclusion: p-value and the Z-value and the statistical conclusion.
 


one_sample_Ztest <- function(x, sigma, muzero, alpha = 0.05, null = "equal") {
  
  # Step 1: Compute sample mean and size
  xbar <- mean(x)
  n <- length(x)
  
  # Step 2: Compute Z statistic
  Z <- (xbar - muzero) / (sigma / sqrt(n))
  
  # Step 3: Compute p-value based on hypothesis type
  if (null == "equal") {
    p_val <- 2 * (1 - pnorm(abs(Z)))  # Two-tailed
    conclusion <- ifelse(p_val < alpha,
                         "Reject the null hypothesis (mean ≠ muzero)",
                         "Fail to reject the null hypothesis (mean = muzero)")
  } else if (null == "less_than_or_equal") {
    p_val <- 1 - pnorm(Z)  # Right-tailed test
    conclusion <- ifelse(p_val < alpha,
                         "Reject the null hypothesis (mean > muzero)",
                         "Fail to reject the null hypothesis (mean ≤ muzero)")
  } else if (null == "more_than_or_equal") {
    p_val <- pnorm(Z)  # Left-tailed test
    conclusion <- ifelse(p_val < alpha,
                         "Reject the null hypothesis (mean < muzero)",
                         "Fail to reject the null hypothesis (mean ≥ muzero)")
  } else {
    stop("Invalid 'null' argument. Use 'equal', 'less_than_or_equal', or 'more_than_or_equal'.")
  }
  
  # Step 4: Return result
  return(list(
    Z_value = Z,
    p_value = p_val,
    conclusion = conclusion
  ))
}



# (b) Use the data set to test the null hypothesis that μ = μ0:
#   x = c(141.5, 152.3, 121.2, 123.0, 151.6, 124.8, 138.9,
#         137.4, 145.6, 135.6, 135.4, 121.5)
# and μ0 of 124.6 and σ = 14.5 with 0.05 significance level.
# Define the data
x <- c(141.5, 152.3, 121.2, 123.0, 151.6, 124.8, 138.9,
       137.4, 145.6, 135.6, 135.4, 121.5)

# Run the Z-test using the function we created
result <- one_sample_Ztest(x, sigma = 14.5, muzero = 124.6, alpha = 0.05, null = "equal")

# Print the results
print(result)

###################################################################################################################################


# (2) One sample t- test:
# (a) Write a function to perform a one sample t-test given a data set x that is randomly
# drawn from a Gaussian distribution of population mean μ and standard deviation
# σ. The function should return the conclusions of the test along with the statistic
# values. Function should be defined as
# one_sample_t_test(x,muzero,alpha,null) where the arguments have the same
# meaning as above.
one_sample_t_test <- function(x, muzero, alpha = 0.05, null = "equal") {
  
  # Sample statistics
  xbar <- mean(x)
  s <- sd(x)
  n <- length(x)
  t_stat <- (xbar - muzero) / (s / sqrt(n))
  df <- n - 1
  
  # Determine p-value and conclusion
  if (null == "equal") {
    p_val <- 2 * (1 - pt(abs(t_stat), df))  # Two-tailed test
    conclusion <- ifelse(p_val < alpha,
                         "Reject the null hypothesis (mean ≠ muzero)",
                         "Fail to reject the null hypothesis (mean = muzero)")
  } else if (null == "less_than_or_equal") {
    p_val <- 1 - pt(t_stat, df)  # Right-tailed test
    conclusion <- ifelse(p_val < alpha,
                         "Reject the null hypothesis (mean > muzero)",
                         "Fail to reject the null hypothesis (mean ≤ muzero)")
  } else if (null == "more_than_or_equal") {
    p_val <- pt(t_stat, df)  # Left-tailed test
    conclusion <- ifelse(p_val < alpha,
                         "Reject the null hypothesis (mean < muzero)",
                         "Fail to reject the null hypothesis (mean ≥ muzero)")
  } else {
    stop("Invalid 'null' argument. Use 'equal', 'less_than_or_equal', or 'more_than_or_equal'.")
  }
  
  # Return result
  return(list(
    t_value = t_stat,
    p_value = p_val,
    df = df,
    conclusion = conclusion
  ))
}

# (b) Use the data set below to test the null hypothesis that μ = μ0, where μ0 = 100 for
# 0.05 significance level.
# x = c(96.0, 104.0, 99.1, 97.6, 99.4, 92.8, 105.6, 97.2,
#       96.8, 92.1, 100.6, 101.5, 100.7, 97.3, 99.6, 105.9)

# Given data
x <- c(96.0, 104.0, 99.1, 97.6, 99.4, 92.8, 105.6, 97.2,
       96.8, 92.1, 100.6, 101.5, 100.7, 97.3, 99.6, 105.9)

# Run the one-sample t-test
result <- one_sample_t_test(x, muzero = 100, alpha = 0.05, null = "equal")

# Print the result
print(result)

#########################################################################################################################################

# (3) One sample proportion test: In R, the functions binom.test() and prop.test() per-
#   forms the one sample proportion test. The former computes the exact binomial probabil-
#   ity, and is used when the sample sizes are small. The latter uses a normal approximation
# to the binomial distribution and can be used when n > 30. The functions are
# binom.test(x,n,p,alternative)
# prop.test(x,n,p,alternative,correct)
# where x is the number of successes, n is the total number of trials, p proportion to test
# against (i.e., hypothesized value), alternative is the string value indicating type of null
# hypothesis as “two-sided”, “less”, “greater”, and correct is a logical variable indicating
# whether a correction should be applied for small sample sizes (the default is TRUE).
# Print the results of both the tests with x = 710, n = 2600, p = 0.25 and alternative=greater.

# Given values
x <- 710
n <- 2600
p_null <- 0.25

# Exact binomial test
binom_result <- binom.test(x = x, n = n, p = p_null, alternative = "greater")

# Proportion test (normal approx.)
prop_result <- prop.test(x = x, n = n, p = p_null, alternative = "greater", correct = TRUE)

# Print results
cat("=== binom.test() Result ===\n")
print(binom_result)

cat("\n=== prop.test() Result ===\n")
print(prop_result)


#################################################################################################################################

# (4) One sample variance test:
# (a) Write a function with the following structure to output the statistical conclusion
# and properties (p-value and σ2 value):one_sample_variance_test(x,test_sigma,alpha), where the function should
# compute the χ2 test statistic and get the appropriate limits by using the qchisq()
# function and deciding the conclusion.

one_sample_variance_test <- function(x, test_sigma, alpha = 0.05) {
  n <- length(x)
  s2 <- var(x)                    # Sample variance
  sigma2_0 <- test_sigma^2       # Hypothesized variance
  
  # Chi-squared test statistic
  chi_sq <- (n - 1) * s2 / sigma2_0
  
  # Degrees of freedom
  df <- n - 1
  
  # Critical values for two-tailed test
  lower_crit <- qchisq(alpha / 2, df)
  upper_crit <- qchisq(1 - alpha / 2, df)
  
  # p-value
  p_val <- 2 * min(pchisq(chi_sq, df), 1 - pchisq(chi_sq, df))
  
  # Conclusion
  conclusion <- ifelse(chi_sq < lower_crit | chi_sq > upper_crit,
                       "Reject the null hypothesis (σ² ≠ σ₀²)",
                       "Fail to reject the null hypothesis (σ² = σ₀²)")
  
  return(list(
    chi_sq_statistic = chi_sq,
    sample_variance = s2,
    p_value = p_val,
    df = df,
    critical_values = c(lower_crit, upper_crit),
    conclusion = conclusion
  ))
}


# (b) Perform the above test for the data set given below for hypothesized σ = 29 and for
# 0.05 significance level. The points are from a normal distribution with mean=140
# and standard deviation of 20.
# x = c(142.8, 135.0, 157.5, 148.4, 135.9, 153.4, 149.0, 130.2,
#       156.0, 189.7, 151.6, 156.5, 123.8, 152.9, 118.4, 145.8)

# Provided data
x <- c(142.8, 135.0, 157.5, 148.4, 135.9, 153.4, 149.0, 130.2,
       156.0, 189.7, 151.6, 156.5, 123.8, 152.9, 118.4, 145.8)

# Hypothesized standard deviation = 29
result <- one_sample_variance_test(x, test_sigma = 29, alpha = 0.05)

# Show the result
print(result)


########################################################################################################################################

# (5) One sample Wilcoxon signed rank test: In R, the function wilcox.test() carries out
# one- and two-sample non-parametric tests. The arguments are
# wilcox.test(x,y=NULL,alternative,mu=0,paired=FALSE,exact=NULL,correct=TRUE,conf.int=FALSE,conf.level=0.95)
# where for two samples, y will not be NULL. Perform this test for the data set with μ = 160
# and confidence level of 95% and H0 as μ >= μ0.
# x = c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7,
#       156.6, 174.5, 184.4, 165.2, 147.8, 177.8, 160.1, 161.5)

# Given data
x <- c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7,
       156.6, 174.5, 184.4, 165.2, 147.8, 177.8, 160.1, 161.5)

# Perform the one-sample Wilcoxon signed-rank test
wilcox.test(
  x,
  mu = 160,
  alternative = "less",       # H0: median >= 160 vs H1: median < 160
  conf.int = TRUE,
  conf.level = 0.95
)



