thedata <- read.csv("/home/karthik/r/jdt.csv" , header=T, sep=",")
mydata=scale(thedata[,-18])
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 9) # 4 cluster solution
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
library(cluster)
# library(fpc)
# plotcluster(mydata, fit$cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,labels=5, lines=0)
mydata1 <- data.frame(mydata, fit$cluster) 

finaldata = data.frame(mydata1,bugs=thedata[,18])

library(psych)
printdata=describeBy(finaldata,finaldata$bugs)