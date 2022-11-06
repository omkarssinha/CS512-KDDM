#################################################

#  University     : Stevens Institute of Technology
#  Project        : Midterm Q3
#  Purpose        : Homework
#  First Name     : Omkar
#  Last Name      : Sinha
#  CWId			      : 10468312
#  Date           : 11/01/2021

#################################################

# drop all pre-existing variables

rm(list = ls())

# importing "COVID19_v4.CSV" dataset into R

df = read.csv("C:/Users/Omkar_PC/Downloads/COVID19_v4.csv",header=TRUE, sep=",")
df

##Subquestion I : Summararizing each column
summary(df)

##Subquestion II :	Identifying missing values

##True/False format indicating if the values is missing
is.na(df)
View(df)

##Subquestion III.	Replacing the missing values with the "mode" of the column.

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for(i in 1:ncol(df)) {
  
  ##View(mode(df[,i]))
  df[is.na(df[,i]), i] <- getmode(df[,i])
  
}
View(df)
modee<-mean(df$Age)
modee

### Subquestion IV.	Displaying the scatter plot of Age, Exposure and MonthsAtHospital one pair at a time

pairs(df[c(2:3,6)], pch = 20, col = 'yellow')

### Subquestion V. Show box plot for columns Age and MonthAtHospital

boxplot(df[c(2,6)])

