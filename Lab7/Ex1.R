# Creating the first matrix with byrow = TRUE
amat <- matrix(seq(10, 120, by=10), nrow=3, ncol=4, byrow=TRUE)
print(amat)

# Creating the second matrix with byrow = FALSE
amat2 <- matrix(seq(10, 120, by=10), nrow=3, ncol=4, byrow=FALSE)
print(amat2)
#The elements in amat are filled row by row, while the elements in amat2 are filled column by column.
#As a result, the two matrices have the same elements but arranged in a different order.

# Assigning row names and column names to the matrix 'amat'
rownames(amat) <- c("R1", "R2", "R3")
colnames(amat) <- c("C1", "C2", "C3", "C4")
print(amat)

# Creating Matrix A with the specified elements
A <- matrix(c(2, 5, 7, 3, 1, 8, 9, 10, 1, 12, 5, 10, 4, 17, 15, 11), nrow=4, ncol=4)
# Creating Matrix B with the specified elements
B <- matrix(c(12, 5, 3, 17, 1, 18, 9, 10, 1, 12, 5, 10, 4, 15, 15, 4), nrow=4, ncol=4)
print(A)
print(B)
# Element-wise multiplication of matrices A and B
result <- A * B
print(result)

# Define the vectors X and Y
X <- c(5, 6, 8, 9)
Y <- c(8, 10, 12, 5)
# Outer product of X and Y
outer_product <- outer(X, Y)
# Inner product (dot product) of X and Y
inner_product <- sum(X * Y)
print(outer_product)
print(inner_product)
# Create a diagonal matrix using the vector X
A <- diag(X)
print(A)
# Print the diagonal elements of A
print(diag(A))

# Create a 6x6 identity matrix in one line
identity_matrix <- diag(6)
# Print the identity matrix
print(identity_matrix)

# Create the matrix A with the given elements
A <- matrix(c(3, 4, -2, 4, -5, 1, 10, -6, 5), nrow=3, ncol=3)
print(A)
# Create the 3x1 matrix B with the given elements
B <- matrix(c(5, -3, 13), nrow=3, ncol=1)
print(B)

# Define matrix A
A <- matrix(c(3, 4, -2, 4, -5, 1, 10, -6, 5), nrow=3, ncol=3)
# Define matrix B (3x1 vector)
B <- matrix(c(5, -3, 13), nrow=3, ncol=1)
# Solve the equation AX = B to find X
X <- solve(A, B)
print(X)
print(class(X)) #X is a matrix
print(typeof(X))

# Define matrix A
A <- matrix(c(3, 4, -2, 4, -5, 1, 10, -6, 5), nrow=3, ncol=3)
print(A)
# Find the inverse of A
Ainv <- solve(A)
print(Ainv)
# Check if Ainv * A is the identity matrix
identity_check <- Ainv %*% A
print(identity_check) #gives an identity matrix

# Find eigenvalues and eigenvectors of A
results <- eigen(A)
# Print the eigenvalues and eigenvectors
print(results$values)
print(results$vectors)

#Perform matrix-vector multiplication of A and the second eigenvector
second_eigenvector <- results$vectors[,2]  # second eigenvector
A_times_second_eigenvector <- A %*% second_eigenvector
print(A_times_second_eigenvector)
# Check if the result is approximately the eigenvalue times the second eigenvector
eigenvalue_times_second_eigenvector <- results$values[2] * second_eigenvector
print(eigenvalue_times_second_eigenvector)
#A×second eigenvector gives a result approximately equal to the second eigenvalue multiplied by the second eigenvector






















