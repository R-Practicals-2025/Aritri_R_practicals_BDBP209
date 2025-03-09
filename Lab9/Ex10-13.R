#Ex10: Plot the PDF and CPDF for the uniform distribution U(1,2). Find a way to shade the region under the PDF up to x = 1.5.
# Define the range
x <- seq(0, 3, length.out = 100)
# PDF of U(1,2)
pdf_uniform <- ifelse(x >= 1 & x <= 2, 1, 0)
# CDF of U(1,2)
cdf_uniform <- pmax(0, pmin(x - 1, 1))
# Plot PDF
plot(x, pdf_uniform, type = "l", col = "blue", lwd = 2, ylab = "Density", xlab = "x", main = "PDF and CDF of U(1,2)")
polygon(c(1, seq(1, 1.5, length.out = 50), 1.5), c(0, rep(1, 50), 0), col = rgb(1, 0, 0, 0.5), border = NA) # Shading
lines(x, pdf_uniform, col = "blue", lwd = 2)
# Plot CDF
plot(x, cdf_uniform, type = "l", col = "red", lwd = 2, ylab = "Cumulative Probability", xlab = "x", main = "CDF of U(1,2)")




#Ex11:Plot the PDF and CPDF for the exponential distribution with λ = 10. Shade the region under the PDF up to x = 2.8.
# Define the range
x <- seq(0, 5, length.out = 100)
# PDF and CDF of Exponential(10)
pdf_exp <- dexp(x, rate = 10)
cdf_exp <- pexp(x, rate = 10)
# Plot PDF
plot(x, pdf_exp, type = "l", col = "blue", lwd = 2, ylab = "Density", xlab = "x", main = "PDF and CDF of Exp(10)")
polygon(c(0, seq(0, 2.8, length.out = 50), 2.8), c(0, dexp(seq(0, 2.8, length.out = 50), rate = 10), 0), col = rgb(1, 0, 0, 0.5), border = NA)
lines(x, pdf_exp, col = "blue", lwd = 2)
# Plot CDF
plot(x, cdf_exp, type = "l", col = "red", lwd = 2, ylab = "Cumulative Probability", xlab = "x", main = "CDF of Exp(10)")




#Ex12: Plot the PDF and CPDF for the Gamma distribution with α = 5 and θ = 3.
# Define the range
x <- seq(0, 30, length.out = 100)
# PDF and CDF of Gamma(5,3)
pdf_gamma <- dgamma(x, shape = 5, scale = 3)
cdf_gamma <- pgamma(x, shape = 5, scale = 3)
# Plot PDF
plot(x, pdf_gamma, type = "l", col = "blue", lwd = 2, ylab = "Density", xlab = "x", main = "PDF and CDF of Gamma(5,3)")
# Plot CDF
plot(x, cdf_gamma, type = "l", col = "red", lwd = 2, ylab = "Cumulative Probability", xlab = "x", main = "CDF of Gamma(5,3)")



#Ex13: Plot the PDF and CPDF for the Chi-square distribution for 20 degrees of freedom. Shade the region under the PDF up to x = 1.0.
# Define the range
x <- seq(0, 50, length.out = 100)
# PDF and CDF of Chi-square(20)
pdf_chisq <- dchisq(x, df = 20)
cdf_chisq <- pchisq(x, df = 20)
# Plot PDF
plot(x, pdf_chisq, type = "l", col = "blue", lwd = 2, ylab = "Density", xlab = "x", main = "PDF and CDF of Chi-square(20)")
polygon(c(0, seq(0, 1, length.out = 50), 1), c(0, dchisq(seq(0, 1, length.out = 50), df = 20), 0), col = rgb(1, 0, 0, 0.5), border = NA)
lines(x, pdf_chisq, col = "blue", lwd = 2)
# Plot CDF
plot(x, cdf_chisq, type = "l", col = "red", lwd = 2, ylab = "Cumulative Probability", xlab = "x", main = "CDF of Chi-square(20)")











