x<-c(5,3,7,8)
#default datatype for number vector is numeric and not integer
is.integer(x)
is.numeric(x)
x<-integer(x)
print(x)
#converts the numeric datatype to integer datatype
x<-as.integer(x)
#checks if it is integer datatype
is.integer(x)