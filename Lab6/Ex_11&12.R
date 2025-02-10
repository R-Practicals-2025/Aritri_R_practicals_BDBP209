#Ex-11
data=read.csv("/home/ibab/Downloads/BrainCancer.csv")
X=matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0),nrow=4)
print(X)
print(rownames(X))
print(colnames(X))
rownames(X)=rownames(X,do.NULL=FALSE,prefix='Trial')
print(rownames((X)))
print(X)
drugs=c('aspirin','paracetamol','nurofen','palacebo','hedex')
colnames(X)=drugs
print(colnames(X))
print(X)
dimnames(X)=list(NULL,paste("drug",1:5,sep="")) #Changes in column names
print(X)

#Ex12
print(mean(X[,5]))
print(var(X[4,]))
print(rowSums(X))
print(colSums(X))
print(apply(X,1,sum))
print(apply(X,2,sum))
print(rowMeans(X))
print(colMeans(X))
print(apply(X,1,mean))
print(apply(X,2,mean))
print(apply(X,2,sqrt))
print(apply(X,2,function(X)X^2+X))
group=c("A","B","B","A")
print(rowsum(X,group))
print(row(X))
print(col(X))
print(tapply(X,list(group[row(X)], col(X)),sum))
print(aggregate(X,list(group),sum))
print(apply(X,2,sample))

X=rbind(X,apply(X,2,mean))
print(X)

X=cbind(X,apply(X,1,var))
print(X)
heading=c(paste('drug',1:5,sep=''),'var')
dimnames(X)=list(NULL,heading)
print(X)
rowhead=c(paste('Trial',1:4,sep=''),mean)
dimnames(X)=list(rowhead,heading)
print(X)

eg_sweep=data.frame(data$ki,data$gtv,data$time)
#method 1 to perform sweep action
cols=apply(eg_sweep,2,mean)
print(cols)
col.means=matrix(rep(cols,rep(dim(eg_sweep)[1],dim(eg_sweep)[2])),nrow=dim(eg_sweep)[1])
print(col.means)
eg_sweep_alt=eg_sweep-col.means
print(eg_sweep_alt)
eg_sweep_alt2=sweep(eg_sweep,2,cols)
print(eg_sweep_alt2)

#sapply for vectors
eg_sapply=sapply(3:7,seq)
print(attributes(eg_sapply))
print(class(eg_sapply))

pgdata=read.table("/home/ibab/Downloads/pgfull.txt")
print(pgdata)
print(names(pgdata))
species=pgdata[,1:54]
print(max.col(species))
print(names(species)[max.col(species)])
print(table(names(species)[max.col(species)]))
print(max.col(-species))


#lists
apples <- c(4,4.5,4.2, 5.1,3.9)
oranges <- c(TRUE,TRUE,FALSE)
chalk <- c("limestone","marl","oolite","CaCO3")
cheese <- c(3.2-4.5i,12.8+2.2i)
items <- list(apples, oranges, chalk, cheese)
print(items)
data.frame(apples,oranges,chalk) #different rows so data frame is not possible
#subscripts on lists have double square brackets
print(items[[3]])
print(items[[3]][3])
print(items[3])
print(items[[1]][2])
print(items[1][2])

items <- list(first=apples,second=oranges,third=chalk,fourth=cheese)
print(names(items))
print(items$fourth)
print(class(items))
print(lapply(items,length))
print(lapply(items,class))
print(lapply(items,mean)) #Predict the output first
print(summary((items)))
print(str(items))
#difference between class and mode
