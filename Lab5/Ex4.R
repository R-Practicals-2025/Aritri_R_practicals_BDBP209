#Ex4 (Braincancer dataset)
df=read.csv("/home/ibab/Downloads/BrainCancer.csv")
f1=subset(df,df$gtv>20) #gtv data with values greater than 20
print(f1)
f2=df[c(1,3,8,9,13,14, 18,21),] #all the mentioned rows are selected
print(f2)
f3_ind=which(df$sex=='Female') #indices of rows containing female
print(f3_ind)
f3=df[f3_ind,] #subset of the data containing female
print(f3)
#extracting the gtv column
gtv_col=df$gtv 
print(gtv_col)
#extracting ki column
ki_col=df$ki 
print(ki_col)
new_col=(gtv_col*ki_col)/234 #creating a new column with the formula:data$gtv*data$ki/234.
print(new_col)
#creating new dataframe with gtv,ki and new column values
new_df <- data.frame(GTV = df$gtv, KI = df$ki, New_Column = df$new_col)
print(new_df)
f=subset(df,df$sex=='Female') #subset containing only females
print(f)
write.csv(f,file="/home/ibab/Downloads/lab4_female_BrainCancer.csv") #writing the subset to a new file
