###################################################################

#  University : Stevens Institute of Technology
#  First Name : Omkar
#  Last Name  : Sinha
#  CWID       : 10468312
#  Course     : Knowledge Discovery and Data Mining(CS 513)
#  Purpose    : Midterm Q6
#  Date       : 11/02/2021

###################################################################

rm(list=ls())
#libraries
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("RColorBrewer")

#Q-6. Load the CANVAS "COVID19_v4.CSV" dataset into R/Python. 
coviddata<-read.csv("C:/Users/Omkar_PC/Downloads/COVID19_v4.csv", na.strings = c("?"))

#Remove the missing values.
#Answer:
coviddata <- na.omit(coviddata)

#Discretize the "MonthAtHospital" into "less than 6 months" and "6 or more months". 

#Answer:
coviddata$MonthAtHospitalDisc <- ifelse(coviddata$MonthAtHospital >= 6, c("6 or more months"), c("less than 6 months"))

# Also discretize the age into "less than 35", "35 to 50" and "51 or over".
#Answer:
attach(coviddata)
coviddata$ageDisc[Age >= 51] <- "51 or over"
coviddata$ageDisc[Age >= 35 & Age <= 50] <- "35 to 50"
coviddata$ageDisc[Age < 35] <- "less than 35"
detach(coviddata)

# Construct a CART model to classify infection ("infected') based on the other variables.
#Answer:
coviddata$MaritalStatus <- as.factor(coviddata$MaritalStatus)
coviddata$MonthAtHospitalDisc <- as.factor(coviddata$MonthAtHospitalDisc)
coviddata$ageDisc <- as.factor(coviddata$ageDisc)
coviddata$Infected <- as.factor(coviddata$Infected)
#setting the seed
set.seed(155)
train_index <- sample(1:nrow(coviddata), 0.7 * nrow(coviddata))
test_index <- setdiff(1:nrow(coviddata), train_index)
train <- coviddata[train_index, ]
test <- coviddata[test_index, ]

train<- train[, c(-1, -6)]
test <- test[, c(-1, -6)]
# idx<-sort(sample(nrow(data),as.integer(.70*nrow(data))))
# training<-data[idx,]
# test<-data[-idx,]

# PlotTree
View(data_train)
coviddata_model<-rpart(Infected~.,data = train)
rpart.plot(coviddata)

# Prediction
pred<-predict(coviddata_model, test[, -5], type="class")
# Frequency table
table(actual=test[, 5],pred)
# Calculate Error Rate 
wrong<-(test[,5]!=pred)
error_rate<-sum(wrong)/length(wrong)
error_rate

# Accuracy Percentage
print(paste0('Accuracy Percentage: ', (1-error_rate)*100))

