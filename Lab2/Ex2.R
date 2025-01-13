0:10 #generates sequence from 0 to 10
15:5 #generates sequence from 15 to 5 in reverse order
seq(0,1.5,0.1)  #generates sequence from 0 to 1.5 by a step of 0.1
seq(6,4,-0.2) #generates sequence from 6 to 4 by a step of 0.2 in reverse order
N=c(55,76,92,103,84,88,121,91,65,77,99)#generates a sequence
seq(0.04,by=0.01,along=N)#generates sequence from 0.04 to the length of N by 0.01 steps
seq(from=0.04,to=0.14,along=N) #generates sequence from 0.04 to the length of N by 0.01 steps
sequence(c(4,3,4,4,4,5))#generates a sequence containing 1-4,1-3,1-4,1-4,1-4 and 1-5
rep(9,5) #replicates 9 only 5 times
rep(1:4,2) #replicates 1-4 only 2 times
rep(1:4,each=2) #replicates 1-4 in which each number is repeated 2 times
rep(1:4,each=2,times=3) #replicates 1-4 in which each number is repeated 2 times and whole is repeated 3 times
rep(1:4,c(4,1,4,2)) #repeats 1 4 times 2 1 time etc...
rep(c("cat","dog","goldfish","rat"), c(2,3,2,1)) #repeats each element in 1st list to numbers of times in the 2nd list
seq(-1,1,0.1) # generates sequence from -1 to 1 in a step of 0.1  
seq(-1,1,length=7) #generates sequence from -1 to 1 having length of 7
number<--1+0.1*(0:20)
print(number)# generates sequence from -1 to 1 in a step of 0.1  
