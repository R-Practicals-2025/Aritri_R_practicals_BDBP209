data=read.csv("/home/ibab/Downloads/BrainCancer.csv",header=TRUE)
print(colnames(data))
newcol=(data$gtv^2) + data$time
addcol=cbind(data,newcol)
print(addcol)
#Print the row and column names of the modified data
print(colnames(addcol))
print(rownames(addcol))
# Change row names using the paste() function
rownames(addcol) <- paste("Row-", 1:nrow(df), sep="")
print(addcol)
#Remove the column ’ki’ by assigning NULL value to this column. Print the modified data to make sure that the ‘ki’ column has indeed been removed
data$ki=NULL
print(colnames(data))
