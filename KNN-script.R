data<-read.csv("formability.csv",FALSE,",")
str(data)
table(data$V1)
head(data)
data$V1<-factor(data$V1)
str(data)

table(data$V1)
str(data)

head(data)
set.seed(9850)
#generating (nrow) random numbers 
gp <- runif(nrow(data))
#randomizing the order of data
data<-data[order(gp),]
str(data)
head(data)
#rescale numerical fatures necessary for KNN
summary(data[,c(1,2,3)])
normalize <- function(x) {
  + return((x-min(x))/(max(x)-min(x)))
}
normalize(c(1,2,3))
data_n<-as.data.frame(lapply(data[,c(2,3)],normalize))
str(data_n)
summary(data_n)
# create a training data set
data_train<-data_n[1:200,]
data_test<-data_n[201:223,]
data_train_target<-data[1:200,1]
data_test_target<-data[201:223,1]
str(data_n)
str(data)
# KNN
require(class)
m1<-knn(train=data_train,test=data_test,cl=data_train_target, k= 15)
m1
tab1<-table(data_test_target,m1)
tab1
sum(diag(tab1))/sum(tab1)
