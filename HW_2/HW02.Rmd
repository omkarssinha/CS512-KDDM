---
  output:
  pdf_document: default
html_document: default
---
  
  
  
  #################################################

#  University     : Stevens Institute of Technology
#  Project        : HW_02 EDA 
#  Purpose        : Homework
#  First Name     : Omkar
#  Last Name      : Sinha
#  CWId			      : 10468312
#  Date           : 10/10/2021
#  Comments       : Kindly knit in PDF format


#################################################  

\    
\  


## Question 1: Load the "breast-cancer-wisconsin.data.csv" from canvas into R and perform the EDA analysis by:  

\  


### Subquestion I.	Summarizing each column (e.g. min, max, mean)

```{r}

rm(list=ls())

##Choosing the file

Breast_Cancer_Wisconsin <- read.csv("C:/breast-cancer-wisconsin.data.csv", header = TRUE, na.strings = c("?"))

##Summary
summary(Breast_Cancer_Wisconsin)

```

\  

### Subquestion II.	Identifying missing values

```{r}

##True/False format indicating if the values is missing
is.na(Breast_Cancer_Wisconsin)
View(Breast_Cancer_Wisconsin)


MissingDataBCW <- Breast_Cancer_Wisconsin[which(is.na(Breast_Cancer_Wisconsin$F6)),]
MissingDataBCW

```

\  

### Subquestion III.	Replacing the missing values with the "mean" of the column.
##As we can see only F6 has character values i.e. only missing values, hence calculating for F6 column individually

```{r}

##Replacing missing values with mean of the column

Mean_F6  <- mean(Breast_Cancer_Wisconsin$F6,na.rm = TRUE)
Mean_BCW <- Breast_Cancer_Wisconsin
Mean_BCW[is.na(Mean_BCW)] <- Mean_F6
Mean_BCW

```

\  

### Subquestion IV.	Displaying the frequency table of "Class" vs. F6

```{r}

BCR_Class <- Breast_Cancer_Wisconsin$Class
transform(table(Breast_Cancer_Wisconsin$F6,BCR_Class))

```

\  

### Subquestion V.	Displaying the scatter plot of F1 to F6, one pair at a time

```{r}

pairs(Breast_Cancer_Wisconsin[2:7],pch = 20, col = 'yellow')

```

\  

### Subquestion VI.	Show histogram box plot for columns F7 to F9

```{r}

F7 <- hist(Breast_Cancer_Wisconsin$F7)
F8 <- hist(Breast_Cancer_Wisconsin$F8)
F9 <- hist(Breast_Cancer_Wisconsin$F9)

#Range of X-axis

Xrange <- range(c(F7$breaks, F8$breaks, F9$breaks))

#Range of Y-axis

Yrange <- max(c(F7$count, F8$count, F9$count))

##Histogram


plot(F7, col = 'grey', main = 'Histogram of F7,F8 and F9', xlab = 'F7/F8/F9',xlim = c(Xrange), ylim = c(0,Yrange+100))
plot(F8, add = TRUE, col = 'red')
plot(F9, add = TRUE, col = 'blue')

boxplot(Breast_Cancer_Wisconsin[8:10])

```

\  
\  


## 2- Delete all the objects from your R- environment. Reload the "breast-cancer-wisconsin.data.csv" from canvas into R. Remove any row with a missing value in any of the columns.


```{r}

rm(list=ls())

BCW <- read.csv("C:/breast-cancer-wisconsin.data.csv", header = TRUE, na.strings = c("?"))
BCW <- na.omit(BCW)

BCW

```
