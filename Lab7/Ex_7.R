#Write a function to replace all the negative values in a vector by zeros
replace_negatives <- function(vec) {
  vec[vec < 0] <- 0
  return(vec)
}
replace_negatives(c(-1,3,-5,-8,6))

#Write a function to calculate the factorial of a number using the Stirling’s approximation
stirling_factorial <- function(n){
  if (n < 0){
    print("Factorial is not defined for negative numbers.")
}
  sqrt(2 * pi * n) * (n / exp(1))^n * (1 + 1 / (12 * n) + 1 / (288 * n^2) - 139 / (51840 * n^3) - 571 / (2488320 * n^4))
}
stirling_factorial(6)

#Write a function to sum the digits of a number
sum_digits <- function(num) {
  digits <- as.numeric(unlist(strsplit(as.character(num), "")))
  return(sum(digits))
}
sum_digits(123)