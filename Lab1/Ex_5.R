x<-sqrt(2) #assigns the square root value of 2 to x
x*x==2 #returns FALSE because x*x is slightly less than 2 and not exactly 2
x*x-2 #returns the total rounding error
all.equal(x*x,2) #'all.equal' specifically used for compairing real numbers