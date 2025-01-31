#Ex-1 loading the dataset(BrainCancer)
df=read.csv("/home/ibab/Downloads/BrainCancer.csv")
#Ex-2
print(dim(df)) #dimensions of the data
print(colnames(df)) #column names of the data
print(rownames(df)) #row names of the data
print(head(df,30)) #first 10 lines of the dataset
#data representation as a frequency table
print(table(df$diagnosis))
print(table(df$gtv))
print(table(df$loc))
print(table(df$ki))
#count of categorical variables is:4 and they are listed below:
print(unique(df$sex))
print(unique(df$dignosis))
print(unique(df$loc))
print(unique(df$stereo))
#defining categorical variables:
df$sex=factor(df$sex,levels=c('Male','Female')) 
is.factor(df$sex)
print(levels(df$sex))
print(nlevels(df$sex)) #level=2

df$diagnosis=factor(df$diagnosis,levels=c( "Meningioma", "HG glioma",  "LG glioma" ,"Other" ))
is.factor(df$diagnosis)
print(levels(df$diagnosis))
print(nlevels(df$diagnosis)) #level=4

df$loc=factor(df$loc,levels=c( "Infratentorial", "Supratentorial"))
is.factor(df$loc)
print(levels(df$loc))
print(nlevels(df$loc)) #level=2

df$stereo=factor(df$stereo,levels=c(  "SRS" ,"SRT"))
is.factor(df$stereo)
print(levels(df$stereo))
print(nlevels(df$stereo)) #level=2

