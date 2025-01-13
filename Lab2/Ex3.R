3/0
exp(-Inf)
(0:3)**Inf
0/0
Inf - Inf
Inf/Inf
is.finite(10)
is.infinite(10)
is.infinite(Inf)
y<- c(4,NA,7)
y=="NA"
is.na(y)
y[!is.na(y)]
c1=c(1,2,3,NA)
c2=c(5,6,NA,8)
c3=c(9,NA,11,12)
c4=c(NA,14,15,16)
full.frame <- data.frame (c1,c2,c3,c4)
full.frame
full.frame[,]
full.frame[1,]
full.frame[c1,]
full.frame[c2,]
full.frame[c3,]
full.frame[c(1,3),]
full.frame[c(1,2,3)]
reduced.frame=full.frame[! is.na(full.frame$c1),]
reduced.frame
reduced.frame=full.frame[! is.na(full.frame$c1),2]
reduced.frame
full.frame[! is.na(full.frame)]
full.frame[! is.na(full.frame),]
p1=c(1,2,3,4)
p2=c(6,7,8,9)
p3=c(7,8,1,2)
p4=c(1,1,1,1)
full.frame=data.frame(p1,p2,p3,p4)
full.frame
full.frame[p2,2]
mean(x,na.rm=T)
v <- c(1:6,NA,NA,9:12)
print(v)
seq(along=v)[is.na(v)]
which(is.na(v))


