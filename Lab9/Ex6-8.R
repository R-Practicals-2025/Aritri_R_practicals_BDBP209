#Ex6: Plot a histogram representation of hypergeometric distribution with N=500, K=50 and n=30

# Function to compute factorial
factorial_custom <- function(n) {
  if (n == 0) return(1)
  prod(1:n)
}

# Function to compute combinations (nCr)
combinations <- function(n, r) {
  if (r > n) return(0)
  exp(lfactorial(n) - (lfactorial(r) + lfactorial(n - r))) # Uses logarithm of factorials
}


# Function to compute hypergeometric distribution
hypergeom_distribution_manual <- function(N, K, n) {
  x <- 0:n
  probabilities <- numeric(length(x)) # Initialize vector to store probabilities
  
  for (i in 1:length(x)) {
    num_successes <- x[i]
    numerator <- combinations(K, num_successes) * combinations(N - K, n - num_successes)
    denominator <- combinations(N, n)
    probabilities[i] <- numerator / denominator
  }
  
  return(list(x = x, probabilities = probabilities))
}

# Parameters
N <- 500  # Population size
K <- 50   # Number of successes in the population
n <- 30   # Number of draws

# Compute the hypergeometric distribution
hypergeom_data <- hypergeom_distribution_manual(N, K, n)

# Plot the histogram
barplot(hypergeom_data$probabilities, 
        names.arg = hypergeom_data$x,
        col = "skyblue", 
        border = "black",
        main = "Histogram of Hypergeometric Distribution",
        xlab = "Number of Successes",
        ylab = "Probability",
        ylim = c(0, max(hypergeom_data$probabilities) * 1.2))
grid()



#Ex7: Write few lines of script to explore what happens to a hypergeomtric distribution when n is increased and gets closer to N. Show that it approaches the binomial distribution by
#plotting histograms of both hypergeometric and binomial on the same plot. Use a 3x3 grid of 9 graphs to show the convergence of hypergeometric to binomial distribution.

# Function to compute factorial
factorial_custom <- function(n) {
  if (n == 0) return(1)
  prod(1:n)
}

# Function to compute combinations (nCr)
combinations <- function(n, r) {
  if (r > n) return(0)
  factorial_custom(n) / (factorial_custom(r) * factorial_custom(n - r))
}

# Function to compute hypergeometric probabilities
hypergeom_distribution <- function(N, K, n, x) {
  numerator <- combinations(K, x) * combinations(N - K, n - x)
  denominator <- combinations(N, n)
  return(numerator / denominator)
}


# Function to compute binomial probabilities
binom_distribution <- function(n, p, x) {
  combinations(n, x) * (p^x) * ((1 - p)^(n - x))
}

# Plot function without libraries
plot_histogram <- function(hyper_probs, binom_probs, x_vals, n) {
  plot(NULL, xlim = c(min(x_vals), max(x_vals)), ylim = c(0, max(c(hyper_probs, binom_probs))),
       xlab = "Number of Successes", ylab = "Probability",
       main = paste("n =", n), type = "n")
  
  # Plot hypergeometric probabilities
  rect(x_vals - 0.25, 0, x_vals, hyper_probs, col = "skyblue", border = "black")
  
  # Plot binomial probabilities
  rect(x_vals, 0, x_vals + 0.25, binom_probs, col = "orange", border = "black")
  
  legend("topright", legend = c("Hypergeometric", "Binomial"), fill = c("skyblue", "orange"))
}

# Parameters
N <- 100  # Population size
K <- 40   # Number of successes in the population
n_values <- seq(10, N, length.out = 9) # Sample sizes to explore
p <- K / N # Probability of success in binomial distribution

# Create a 3x3 grid of plots
par(mfrow = c(3, 3)) # Setup 3x3 plotting grid
for (n in n_values) {
  x_vals <- 0:n
  
  # Compute hypergeometric probabilities
  hyper_probs <- sapply(x_vals, function(x) hypergeom_distribution(N, K, n, x))
  
  # Compute binomial probabilities
  binom_probs <- sapply(x_vals, function(x) binom_distribution(n, p, x))
  
  # Plot the histograms
  plot_histogram(hyper_probs, binom_probs, x_vals, n)
}

# Reset the plotting grid
par(mfrow = c(1, 1))


#Ex8: On the same plot, draw 3 Poisson distributions with λ values of 3,20,45 (Code the probability distribution function).

# Function to compute factorial
factorial_custom <- function(n) {
  if (n == 0) return(1)
  prod(1:n)
}

# Function to compute Poisson probability distribution function
poisson_pmf <- function(lambda, x) {
  (lambda^x * exp(-lambda)) / factorial_custom(x)
}

# Define the range of x values
x_vals <- 0:60  # Choose a range wide enough to capture all distributions

# Define lambda values
lambdas <- c(3, 20, 45)

# Compute probabilities for each lambda
poisson_probs <- lapply(lambdas, function(lambda) sapply(x_vals, function(x) poisson_pmf(lambda, x)))

# Set up the plot
plot(x_vals, poisson_probs[[1]], type = "h", lwd = 2, col = "blue",
     main = "Poisson Distributions (λ = 3, 20, 45)",
     xlab = "x (Number of Events)", ylab = "Probability",
     ylim = c(0, max(unlist(poisson_probs))))
lines(x_vals, poisson_probs[[2]], type = "h", lwd = 2, col = "green")
lines(x_vals, poisson_probs[[3]], type = "h", lwd = 2, col = "red")

# Add a legend
legend("topright", legend = c("λ = 3", "λ = 20", "λ = 45"),
       fill = c("blue", "green", "red"))














