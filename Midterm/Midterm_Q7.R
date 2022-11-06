#################################################

#  University     : Stevens Institute of Technology
#  Project        : Midterm Q7
#  First Name     : Omkar
#  Last Name      : Sinha
#  CWId			      : 10468312
#  Date           : 11/02/2021

#################################################

# drop all pre-existing variables

rm(list = ls())

# importing "COVID19_v4.CSV" dataset into R

df = read.csv("C:/Users/Omkar_PC/Downloads/COVID19_v4.csv",header=TRUE, sep=",")
df

#Remove the rows with missing values 
df <- na.omit(df)
df

#Convert labels to factor class
df$Infected <- factor(df$Infected,levels = c("Yes","No"),labels = c("Infected","Not Infected"))
#df$Class <- factor(df$Class,labels = c("Yes","No"))

#Generating train and test data in the ratio of 70:30

size   <- sample(1:nrow(df),0.7*nrow(df)) 
norm1  <- function(x) {(x -min(x))/(max(x)-min(x))}


##Executing normalization on the required columns of the given data because they are the predictors
norm2 <- as.data.frame(lapply(df[,c(2,3,5,6)],norm1))
df2  = df['Infected']

#Train set
train_size <- norm2[size,] 
cl_train   <- df2[size,]

##Test set
test_size <- norm2[-size,] 
cl_test   <- df2[-size,]

#Loading the package class
library(class)

#Running KNN for k = 5
clf <- knn(train_size,test_size,cl=cl_train,k=5)

#Creating confusion matrix
conf_matrix <- table(clf,cl_test)
print(conf_matrix)

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)







