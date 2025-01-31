#Ex-3(Braincancer dataset)
df=read.csv("/home/ibab/Downloads/BrainCancer.csv")
print(mean(df$gtv)) #mean of column gtv
print(mean(df$time)) #mean of column time
print(median(df$gtv)) #median of column gtv
#the mean is greater than the median, the distribution of the data is likely right-skewed (positively skewed), meaning there are some higher values pulling the mean upwards.
print(table(df$gtv))
#mode of gtv column:
mode_value <- names(sort(table(df$gtv), decreasing = TRUE))[1] #forms a table ,then sorts the values in descending order and then returns the actual name of the field with max frequency
print(mode_value)
#as mean>median>mode, so it is right-skewed and hence not symmetric
print(sd(df$gtv)) #standard deviation of gtv column
print(summary(df$gtv)) #statistical summary of gtv column
hist(df$gtv) #histogram plot of gtv data and it is in accordance to previously calculated mean,median,mode
library(moments) #calling moments library to compute skewness and kurtosis
print(skewness(data$gtv)) #skewness(right-skewed) of gtv column
print(kurtosis(data$gtv)) #kurtosis(leptokurtic) of gtv column

#for gtv column
boxplot(data$gtv) #simple box plot
boxplot(data$gtv, xlabel="Spread of GTV",ylabel="GTV",range=0.1,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.1
boxplot(data$gtv, xlabel="Spread of GTV",ylabel="GTV",range=0.2,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.2
boxplot(data$gtv, xlabel="Spread of GTV",ylabel="GTV",range=0.01,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.01
#range = 0.1: The whiskers will be closer to the box, and only very extreme values will appear as outliers.
#range = 0.2: The whiskers will extend farther out, and more data points will be captured within the whiskers.
#range = 0.01: The whiskers will be very short, and you will see a plot with only the most extreme outliers plotted as points.

#for ki column
boxplot(data$ki) #simple box plot
boxplot(data$ki, xlabel="Spread of ki",ylabel="Ki",range=0.1,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.1
boxplot(data$ki, xlabel="Spread of ki",ylabel="Ki",range=0.2,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.2
boxplot(data$ki, xlabel="Spread of Ki",ylabel="Ki",range=0.01,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.01

#for time column
boxplot(data$time) #simple box plot
boxplot(data$time, xlabel="Spread of time",ylabel="time",range=0.1,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.1
boxplot(data$time, xlabel="Spread of time",ylabel="time",range=0.2,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.2
boxplot(data$time, xlabel="Spread of time",ylabel="time",range=0.01,horizontal=FALSE,border=c("purple"),col=c("red")) #range=0.01
#time column has the broadest distribution



