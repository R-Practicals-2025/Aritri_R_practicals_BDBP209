vec=c(90,5,8,9,10,45,55) #creating a vector
class(vec) #class of the vector which is "numeric
vec1=c(3,7,4,10,"Aritri") #creating another vector
class(vec1) #now the class is not numeric as there is a string element in the vector
max(vec) #to find the maximum value in the vector
min(vec) #to find the minimum value in the vector
vec2=scan() #to take a series of entries
vec[4] #to print the 4th element in the vector
ind=c(2,3,4) #to define a group of indices  
vec[ind] #to get the values of only the defined indices
vec[c(2,3,8)] #another way of writing the above
vec[length(vec)] #gives the length of the vector
vec[-1] #removes the first element in the vector vec
vec_red=vec[-1] #removes the first element from vector vec and assigned it to another variable
length(vec_red) #get the length of vec after removing first element
vec[-length(vec)] #removes the last element
vec[c(-2,-3)] #removes the second and the third element from vec
vec[-2:-4] #removes elements between indices 2-4
#function to largest two and smallest two elements from a vector
trim=function(x){
  vec_sort=sort(x)
  print(vec_sort)
  trimmed_x=vec_sort[3:(length(vec_sort)-2)]
  print(trimmed_x)
}
trim(vec)
#another way to do the above
trim1=function(x) sort(x) [c(-1,-2,-(length(x)-1),-length(x))]
trim1(vec)

vec[seq(2,length(vec),2)] #elements of even indices
vec[1:length(vec)%%2==0] #another way to do the above
x=1:10 #stores 1 to 10 in x
x
x[x<5] #prints only the elements less than 5
sum(x[x<5])
#function to add the 3 largest elements of any given vector
add=function(x){
  s=sort(x) 
  ss=x[c((length(x)-2),length(x)-1,length(x))]
  r=sum(ss)
  print(r)
} 
add(vec)
which.max(x) #gives the index of largest element
which.min(x) #gives the index of smallest element
cbind(1:10,10:1) #column bind
rbind(1:10,10:1) #row bind
X <- c(1:10) #stores values 1 to 10
X
Y <- c(1:10*5) #stores values of multiples of 5 upto 10
Y
X*Y
X+Y
X/Y
X^Y
log(X)
exp(Y)

  