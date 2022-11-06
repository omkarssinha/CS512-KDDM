---
  output:
  pdf_document: default
html_document: default
---
  
  #################################################

#  University     : Stevens Institute of Technology
#  Project        : HW4 
#  Purpose        : Homework
#  First Name     : Omkar
#  Last Name      : Sinha
#  CWId			      : 10468312
#  Date           : 10/17/2021
#  Comments       : Kindly knit in PDF format


#################################################  


\    
\

```{r}

##Import Packages
# install.packages("e1071")
# install.packages(class)
library(e1071)

## Warning: package ’e1071’ was built under R version 4.0.4

library(class)

##Uploading the file, omitting the missing value rows and converting labels to factor Class

rm(list=ls())
BCW = read.csv("C:/breast-cancer-wisconsin.data.csv",header = TRUE, sep = ",",na.strings = c("?"))

#Remove the rows with missing values 
BCW <- na.omit(BCW)

#Convert labels to factor class
BCW$Class <- factor(BCW$Class,levels = c("2","4"),labels = c("Benign","Malignant"))
is.factor(BCW$Class)


SampleData <- BCW[2:11]

#Taking 70% of the sample size
SampleSize <- floor(0.70 * nrow(SampleData))

#Setting the seed
set.seed(123)
Train <- sample(seq_len(nrow(SampleData)), size = SampleSize)

#Loading 70% of data in training DataSet
Training_Data <- SampleData[Train, ]

#Loading 30% of data in test DataSet
Test_Data <- SampleData[-Train, ]

#Implementing NaiveBayes
model_naive <- naiveBayes(Class ~ ., data = Training_Data)

#Predicting target class 
predict_naive <- predict(model_naive, Test_Data)


conf_matrix <- table(predict_nb=predict_naive,class=Test_Data$Class)
print(conf_matrix)

#Output of Naive Bayes Classifier
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)
```

\ 
\ 

# End of Assignment 04 - NB