#Question-1
operation=function(k,n){ #defining a function
  print(paste("The sum is: ",k+n)) #computing and printing the sum of 2 integers
  print(paste("The difference is: ",k-n)) #computing and printing the difference of 2 integers
  print(paste("The product is: ",k*n)) #computing and printing the product of 2 integers
  print(paste("The quotient is: ",k%/%n)) #computing and printing the quotient of 2 integers
  print(paste("The remainder is: ",k%%n))#computing and printing the remainder of 2 integers 
  print(paste("The power is: ",k**n)) #computing and printing the power of 2 integers
}
operation(5,2) #calling of function

#Question-2
quadratic=function(a,b,c){ #defining a function
  r1=(-b+sqrt((b*b)-(4*a*c)))/(2*a) #computing the first root
  r2=(-b-sqrt((b*b)-(4*a*c)))/(2*a) #computing the first root
  print(paste("The roots of the equation are: ",r1,"and",r2)) #printing the roots of the equation
}
quadratic(3,-10,8) #calling of the function