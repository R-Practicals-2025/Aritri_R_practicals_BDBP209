y=1:24
dim(y)=c(2,4,3) #forming a 3D array
y
X <- matrix (c(1,0,0,0,1,0,0,0,1),nrow=3) #forming a matrix with 3 rows
X
vector <- c(1,2,3,4,4,3,2,1)
V <- matrix(vector,byrow=T,nrow=2) #method 1 of converting vector to matrix by rows
V
vector <- c(1,2,3,4,4,3,2,1)
V <- matrix(vector,byrow=F,nrow=2) #method 1 of converting vector to matrix by columns
V
dim(vector) <- c(4,2) #method 2 of converting vector to matrix by columns
vector
is.matrix(vector) #true
#numeric and string datas combined
x=c(1,2,3,4,"AB","R")
dim(x)=c(3,2,1)
x
#4D array
vec=1:64
dim(vec)=c(2,2,4,4)
vec
vec[1,1,2,4]
vec[,,1,1]
