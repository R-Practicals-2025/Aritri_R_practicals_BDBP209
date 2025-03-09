# Ex1: Plot the point (2,4) with square point character type and magenta color.
plot(2,4,pch=0,col="magenta")


# Ex2: Create a seqence of function values corresponding to sin(x) and cos(x) function from −π to π and plot the two functions on the same graph with appropriate titles, axes labels,
#blue color for the sine curve, red color for the cosine curve, star point character type for sine, cross type point character for cosine overlaid by lines joining the points.
x <- seq(-pi, pi, 0.1)  # Generate x values from -π to π
y1 <- sin(x)            # Compute sine values
y2 <- cos(x)            # Compute cosine values

# Plot sine function with blue star points
plot(x, y1,type='o', col="blue", pch=8, xlab="x", ylab="y = sin(x) / y = cos(x)", 
     main="Sine vs Cos Function")

# Add cosine function with red cross points and connecting lines
#points(x, y2, col="red", pch=4)
lines(x, y2, col="red",type='o',pch=4)

# Add legend to the plot
legend("topright", inset=0.05, lty=1, legend=c("Sine", "Cos"), 
       col=c("blue", "red"), pch=c(8, 4), title="Sine/Cos Function")


#Ex3: Reproduce the bar graph type of plot in Fig. 4.2.1 in the Biostatistics book by Daniel using the appropriate settings for the appearance.

# Define the number of programs and their frequencies
programs <- c(1, 2, 3, 4, 5, 6, 7, 8)
frequency <- c(62, 47, 39, 39, 58, 37, 4, 11)

# Convert to probability
total_families <- sum(frequency)
probability <- frequency / total_families

# Create the bar plot
bp <- barplot(probability, names.arg = programs, col = "gray90", border = "black",
              xlab = expression(italic(x) ~ "(number of assistance programs)"), 
              ylab = "Probability", 
              main = "", # No title in image
              ylim = c(0, 0.25), space = 0.5, axes=FALSE)

# Add X-axis with inward ticks
axis(1, at = bp, labels = programs, line = 0, col = "black", tck = 0.02)

# Add Y-axis with inward ticks
axis(2, at=seq(0,0.25,0.05), tck = 0.02, las=1)

# Explicitly add the X-axis line at y=0
abline(h=-0.0002, col="black", lwd=3, lty=1)


## Add grid lines
#grid(nx = NA, ny = NULL, lty = "dotted", col = "darkgray")

## Add frequency values above bars
#text(x = bp, y = frequency+2, labels = frequency, cex = 0.8)


#Ex 4
# Set up a 2x3 plotting grid
par(mfrow=c(2,3))

# (i) x vs cos(x) with red color and lines
x <- seq(-pi, pi, length.out=100)
plot(x, cos(x), type='l', col='red', lwd=2, main="Cos(x)", xlab="x", ylab="cos(x)")

# (ii) x vs (x^2 / 3) + 4.2 with violet color, points and lines, linewidth 2 and linetype 1
x <- seq(-5, 5, length.out=100)
y <- (x^2 / 3) + 4.2
plot(x, y, type='o', col='purple', lwd=2, lty=1, pch=16, main="Quadratic", xlab="x", ylab="(x^2 / 3) + 4.2")

# (iii) and (iv)

# Custom function to compute binomial probability using factorial
binomial_prob <- function(k, n, p) {
  binomial_coeff <- factorial(n) / (factorial(k) * factorial(n - k))
  return(binomial_coeff * (p^k) * ((1 - p)^(n - k)))
}

# Set parameters for first plot (p=0.3)
n <- 12
p <- 0.3
x_vals <- 0:n

# Compute probabilities
probs <- sapply(x_vals, function(k) binomial_prob(k, n, p))

# Plot histogram for Binomial distribution (p=0.3)
barplot(probs, names.arg = x_vals, col = "gray", border = "black",
        main = paste("Binomial Dist (n=", n, ", p=", p, ")"), 
        xlab = "Number of Successes", ylab = "Probability")

# Set parameters for second plot (p=0.8)
p <- 0.8
probs <- sapply(x_vals, function(k) binomial_prob(k, n, p))

# Plot histogram for Binomial distribution (p=0.8)
barplot(probs, names.arg = x_vals, col = "gray", border = "black",
        main = paste("Binomial Dist (n=", n, ", p=", p, ")"), 
        xlab = "Number of Successes", ylab = "Probability")


# (v) Histogram plot using type='h' option
x <- seq(1, 10, 0.5)
y <- 50*x / (x + 2)
colors <- rep(c("blue", "orange"), length.out=length(x))
plot(x, y, type='h', col=colors, lwd=2, main="Histogram", xlab="x", ylab="50x / (x+2)")

# (vi) x vs log(x) with orange color and ‘step’ linetype
x <- seq(1, 10, length.out=100)  # Avoid log(0)
plot(x, log(x), type='s', col='orange', lwd=2, main="Log(x) Step", xlab="x", ylab="log(x)")

par(mfrow = c(1, 1))