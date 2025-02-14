#Define a vector A with elements a,b,c,d,e. Also define another vector B with elements d,e,f,g. Print the vectors
A=c('a','b','c','d','e')
B=c('d','e','f')
print(A)
print(B)
#Perform a union operation between the two sets and print the result
union(A,B)
c(setdiff(A,B),intersect(A,B),setdiff(B,A)) #for union
# Perform an intersection operation between the two sets and print the result
intersect(A,B)
A[A%in%B] #another way
# Perform a difference operation between the two sets and print the result
setdiff(A,B)
setdiff(B,A)
#The function setequal() checks for the equality of two objects. Create an object with a sequence of A-B, A∩ B and B-A and use this function to check its contents with the result of the union operation above. Print the result.
setequal(c(setdiff(A,B),intersect(A,B),setdiff(B,A)),union(A,B))
#List the elements of B present in A using two different approaches
intersect(A,B)
B[B %in% A]
#Print the elements of A present in B
intersect(A,B)














