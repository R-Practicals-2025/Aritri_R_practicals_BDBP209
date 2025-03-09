#Ex9: Load the csv file for heights and weights of 25000 people and do the following:
#(i) Plot a histogram of the height variable and determine it’s mean and standard deviation
data=read.csv("C:/Users/Aritri Baidya/Downloads/SOCR-HeightWeight.csv")
print(dim(data))
print(colnames(data))
print(rownames(data))
print(head(data,30))
height <- data$Height.Inches.  

mean_height <- mean(height, na.rm = TRUE)
sd_height <- sd(height, na.rm = TRUE)

hist(height, 
     breaks = 30,  # Set an initial bin size
     col = "skyblue", 
     border = "black",
     main = "Histogram of Heights",
     xlab = "Height",
     ylab = "Frequency")
abline(v = mean_height, col = "red", lwd = 2, lty = 2)  # Add a vertical line for the mean
print(mean_height)
print(sd_height)


#(ii) Plot a histogram of the weight variable and determine it’s mean and standard deviation
weight <- data$Weight.Pounds.  

mean_weight <- mean(weight, na.rm = TRUE)
sd_weight <- sd(weight, na.rm = TRUE)

hist(weight, 
     breaks = 30,  # Set an initial bin size
     col = "lightgreen", 
     border = "black",
     main = "Histogram of Weights",
     xlab = "Weight",
     ylab = "Frequency")
abline(v = mean_weight, col = "red", lwd = 2, lty = 2)  # Add a vertical line for the mean
print(mean_weight)
print(sd_weight)

#(iii) Draw a Gaussian curve (recall the Gaussian PDF) with the above calculated mean and standard deviation for both height and weight variables as Z vs P(Z) (i.e. Ztransformed).
#Plot using either plot() function or curve() function.
gaussian_curve <- function(z, mu, sigma) {
  (1 / (sigma * sqrt(2 * pi))) * exp(-((z - mu)^2) / (2 * sigma^2))
}

# Generate Z values for height
z_height <- seq(mean_height - 4 * sd_height, mean_height + 4 * sd_height, length = 100)
p_height <- gaussian_curve(z_height, mean_height, sd_height)

# Plot Gaussian curve for Height
plot(z_height, p_height, 
     type = "l", col = "blue", lwd = 2,
     main = "Gaussian Curve for Heights",
     xlab = "Z-Transformed Height", ylab = "P(Z)")

# Generate Z values for weight
z_weight <- seq(mean_weight - 4 * sd_weight, mean_weight + 4 * sd_weight, length = 100)
p_weight <- gaussian_curve(z_weight, mean_weight, sd_weight)

# Plot Gaussian curve for Weight
lines(z_weight, p_weight, col = "red", lwd = 2)  # Add weight Gaussian to the same plot
legend("topright", legend = c("Height", "Weight"), col = c("blue", "red"), lty = 1, lwd = 2)

#(iv) What happens when you decrease the size of the bins in the histogram plots? Make a 1x3 grid of 3 plots that show the trend for decreasing bin sizes.
par(mfrow = c(1, 3))  # Set up a 1x3 grid for plots

# Plot histogram with fewer bins (wide bins)
hist(height, breaks = 10, col = "skyblue", main = "Bins = 10", xlab = "Height", ylab = "Frequency")

# Plot histogram with moderate bins
hist(height, breaks = 30, col = "skyblue", main = "Bins = 30", xlab = "Height", ylab = "Frequency")

# Plot histogram with more bins (narrow bins)
hist(height, breaks = 100, col = "skyblue", main = "Bins = 100", xlab = "Height", ylab = "Frequency")

# Reset plot layout
par(mfrow = c(1, 1))













