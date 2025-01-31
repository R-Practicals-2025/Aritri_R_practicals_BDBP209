#performing all the exercises for heart.csv dataset
df=read.csv("C:/Users/Aritri Baidya/Downloads/Heart.csv")
print(dim(df))
print(colnames(df))
print(rownames(df))
print(head(df,30))
print(table(df$Age))
print(table(df$ChestPain))
print(table(df$Thal))
print(table(df$AHD))
#to know the categorical variable
print(unique(df$Sex))
print(unique(df$ChestPain))
print(unique(df$Thal))
print(unique(df$AHD))
#to know the levels of categorical variables
df$ChestPain=factor(df$ChestPain,levels=c("typical","asymptomatic", "nonanginal", "nontypical" ))
is.factor(df$ChestPain)
print(levels(df$ChestPain))
print(nlevels(df$ChestPain)) #levels=4
df$Thal=factor(df$Thal,levels=c('fixed','normal','reversable'))
is.factor(df$Thal)
print(levels(df$Thal))
print(nlevels(df$Thal)) #levels=3
df$AHD=factor(df$AHD,levels=c('Yes','No'))
is.factor(df$AHD)
print(levels(df$AHD))
print(nlevels(df$AHD)) #levels=2
#mean of RestBP
print(mean(df$RestBP))
#median of RestBP
print(median(df$RestBP))
#mode of RestBP
print(table(df$RestBP))
mode_val=names(sort(table(df$RestBP),decreasing=TRUE))[1]
print(mode_val)
#as mean>median>mode, so it is right-skewed and hence not symmetric
print(sd(df$RestBP)) #standard deviation of restbp column
print(summary(df$RestBP)) #statistical summary of restbp column
hist(df$RestBP) #histogram plot of restbp data and it is in accordance to previously calculated mean,median,mode
library(moments) #calling moments library to compute skewness and kurtosis
print(skewness(df$RestBP)) #skewness(right-skewed) of gtv column
print(kurtosis(df$RestBP)) #kurtosis(leptokurtic) of RestBP column

#for Chol column
boxplot(df$Chol) #simple box plot
boxplot(df$Chol, xlabel="Spread of Chol",ylabel="Cholestrol",range=0.1,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.1
boxplot(df$Chol, xlabel="Spread of Chol",ylabel="Cholestrol",range=0.2,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.2
boxplot(df$Chol, xlabel="Spread of Chol",ylabel="Cholestrol",range=0.01,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.01
#range = 0.1: The whiskers will be closer to the box, and only very extreme values will appear as outliers.
#range = 0.2: The whiskers will extend farther out, and more data points will be captured within the whiskers.
#range = 0.01: The whiskers will be very short, and you will see a plot with only the most extreme outliers plotted as points.

#for RestBp column
boxplot(df$RestBP) #simple box plot
boxplot(df$RestBP, xlabel="Spread of BP",ylabel="BP",range=0.1,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.1
boxplot(df$RestBP, xlabel="Spread of BP",ylabel="BP",range=0.2,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.2
boxplot(df$RestBP, xlabel="Spread of BP",ylabel="BP",range=0.01,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.01

#for MaxHR column
boxplot(df$MaxHR) #simple box plot
boxplot(df$MaxHR, xlabel="Spread of MaxHR",ylabel="MaxHR",range=0.1,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.1
boxplot(df$MaxHR, xlabel="Spread of MaxHR",ylabel="MaxHR",range=0.2,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.2
boxplot(df$MaxHR, xlabel="Spread of MaxHR",ylabel="MaxHR",range=0.01,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.01
#MaxHR column has the broadest distribution
print(table(df$MaxHR))


#Ex4 
f1=subset(df,df$MaxHR>150) #MaxHR data with values greater than 20
print(f1)
f2=df[c(1,3,8,9,13,14, 18,21),] #all the mentioned rows are selected
print(f2)
f3_ind=which(df$ChestPain=='typical') #indices of rows containing typical
print(f3_ind)
f3=df[f3_ind,] #subset of the data containing typical
print(f3)
#extracting the MaxHR column
MaxHR_col=df$MaxHR 
print(MaxHR_col)
#extracting chol column
chol_col=df$Chol 
print(chol_col)
new_col=(MaxHR_col*chol_col)/234 #creating a new column with the formula:data$MaxHR*data$chol/234.
print(new_col)
#creating new dataframe with MaxHR,chol and new column values
new_df <- data.frame(MaxHR = df$MaxHR, Chol = df$Chol, New_Column = df$new_col)
print(new_df)
f=subset(df,df$MaxHR>150) #subset containing only MaxHR
print(f)
write.csv(f,file="C:/Users/Aritri Baidya/Downloads/lab5_new_heart_dataset.csv")
