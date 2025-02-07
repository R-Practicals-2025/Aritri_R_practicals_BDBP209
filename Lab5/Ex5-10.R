#Ex-5 loading the dataset(BrainCancer)
df=read.csv("/home/ibab/Downloads/BrainCancer.csv")
print(df)
df$sex=factor(df$sex,levels=c('Male','Female'))
print(is.factor(df$sex)) #to check if it is a factor
df$diagnosis=factor(df$diagnosis,levels=c('Meningioma','HG glioma','LG glioma','other'))
print(is.factor(df$diagnosis)) #to check if it is a factor
df$sex=factor(df$loc,levels=c('Infratentorial','Supratentorial'))
print(is.factor(df$loc)) #to check if it is a factor
print(levels(df$sex)) #printing the levels
print(nlevels(df$sex)) #printing the number of levels
print(levels(df$diagnosis)) #printing the levels
print(nlevels(df$diagnosis)) #printing the number of levels


#Ex6
temp=gl(3,2,88,labels=c('Hot','Cold','Lukewarm')) #creating a new column with labels=hot,cold and lukewarm
print(temp)
newdataframe=data.frame(df,temp) #adding the new column to the dataframe
print(newdataframe)
new=cbind(df,temp) #2nd method to do the above
print(new)

#Ex7
tapply(df$gtv, df$ki, mean) #to take out the mean of the 2 columns
tapply(df$gtv, df$ki, mean,trim=0.1) #trim helps in removing the outliers i.e when trim=0.1, 10% high and low values are discarded

#Ex8
print(pmin(df$gtv,df$time,df$ki)) #gives the minimum value along 3 parallel columns
print(pmax(df$gtv,df$time,df$ki)) #gives the maximum value along 3 parallel columns

#Ex9
ranks=rank(df$gtv) #to get the rank of the column
sorted=sort(df$gtv) #to sort the column
ordered=order(df$gtv) #gives indices of sorted column
view=data.frame(df$gtv,ranks,sorted,ordered) #adding the sorted,ordered and rank columns to the dataframe
print(view)
output=data.frame(df$diagnosis,df$gtv,ordered) #adding the ordered column from previous along with diagnosis and gtv columns
print(output)
write.csv(output,file="/home/ibab/Downloads/lab4_ordered_data_BrainCancer.csv") #writing the subset to a new file

#Ex10
filter1=df[1:6,3:8] #extracting rows from 1-6 and columns from 3-8
filter1mat=as.matrix(filter1) #printing the above extracted rows and columns as a matrix
print(filter1mat)
print(class(filter1mat)) #to get the class
print(mode(filter1mat)) #to get the mode
print(attributes(filter1mat)) #to get the attributes
newcol=df$ki+df$gtv+df$time #creating a new column
newcoladded=data.frame(df,newcol) #adding the new column to the exsisting dataframe
print(newcoladded)
newcoladded2=cbind(df,newcol) #2nd method
print(newcoladded2)
filter4=df[c(26,35),] #creating a subset of 26th and 35th rows
newrowadded=rbind(df,filter4) #new row created using the above subset
print(newrowadded)
print(dim(newrowadded)) #printing the dimensions of the new dataframe
print(dim(df)) #the dimensions of original dataset











