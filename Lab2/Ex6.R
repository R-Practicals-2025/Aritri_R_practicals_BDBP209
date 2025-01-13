X=matrix(c(1:24),nrow=3) #creating a matrix with 3 rows
X
Y=(1:10*5) #matrix with 1 row
Y
colMeans(X) #to calculate the mean of each column
Z <- X[1:4] %o% Y[1:3] #calculate the outer product
Z
YoX <- Y[1:3] %o% X[1:4]#calculate the outer product
YoX
t(Z) #transpose of Z
t(t(Z)) #transpose of transpose results in original matrix
t(YoX)
X=matrix(c(1:24),nrow=3)
Y=(1:8*5)
X %*% Y #dot product
sum(X*Y) #another way to carry out dot product
crossprod(X[1:4],Z)
diag(4) #identity matrix
class(X)#matrix
attributes(X) #dimensions of the matrix
X=matrix(c(1:24),nrow=3)
Y=(1:8*5)
X[2,3]=NA #substituting a value with NA
X
X %*% Y #anything multiplied with NA is still NA