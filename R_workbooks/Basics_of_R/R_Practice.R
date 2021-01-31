####################################################
# 												 ###
# R Training -- Data Types, Basic Operations,    ###
#                    Reading and Writing Data    ###
# Driven For: Antuit R Training                  ###
# Last Modified On:                              ###
# Reason for Modification:                       ###
####################################################

## Installing a package
install.packages("dplyr")
## Attaching the package into memory
library(dplyr)

## Working Directory

## Getting current working directory
getwd()
## Setting new working directory
setwd("C:/Users/paritosh/Downloads")

## Assigning variables 
x <- c(1:10)
y = c(1:10)

## overwriting 
x <- c(11:20)

## removing an object 
rm(x)
rm(y)
# rm(x,y) ## remove multiple objects at the same time 

## Importing Data
## Reading a tab seperated file 
mydata <- read.table("data/61860.txt",sep="\t",header = T)
## Reading a comma seperated value file
mydata1 <- read.csv("data/Bands.csv",sep=",",stringsAsFactors = F)
mydata2 <- read.delim("data/Bands2.csv",sep=",")

## Exporting Data
## Creating a text file
write.table(mydata,"data1.txt",sep="\t",row.names =F)
## creating a csv file
write.csv(mydata1,"data2.csv",row.names = F)

## removing datasets
rm(mydata,mydata1,mydata2)


## Data Types 
## Vectors 
a <- c(1,2,3,4) # Numeric vector
b <- c("one","two","three") #Character vector
c <- c(T,F,TRUE,FALSE) ## Logical Vector

## Matrix
matrix1 <- matrix(1:20,ncol = 5,byrow = T) ## generated 5X4 matrix
matrix2 <- matrix(1:25,ncol=5,byrow=F) ## generates 5X5 matrix


## Dataframes 
a <- c(1,2,3,4)
b <- c("Mumbai","pune","Bengaluru","Chennai")
c <- c("Maharashtra","Maharashtra","Karnataka","TamilNadu")
DataFrame1 <- data.frame(a,b,c)
## changing the columnnames
colnames(DataFrame1) <- c("ID","City","State")
rm(a,b,c) ## Removing objects

## Lists
n <- c(2, 3, 5) 
s <-  c("aa", "bb", "cc", "dd", "ee") 
b <- c(TRUE, FALSE, TRUE, FALSE, FALSE) 
x <-  list(n, s, b)  
# x is a list which has 3 components,a numeric vector,a character vector and a logical vector 
x ## print x 
rm(n,s,b) ## Remove intermediate objects

## Basic Operations on Vectors
x <- 1:10
y <-  sample(1:5,10,replace = T) ## generates a random sample
(z <- rep("erd",10)) ## generates a character vector
(a <- seq(from=12,to=34,by=3)) ## geneartes a sequence 
(b <- paste("c",1:5,sep="")) ## generates a character vector

## Arthimetic Operations
x+y; x*y ## Does operations element by element
x+c(10:19)

## Other Operations
sum(x); length(x); mean(x); summary(x) 
## change vector class
class(x) ## check the class of x 
x <- as.character(x) ## changing the class of x 
class(x) ## again check the class of x 

## Accessing Vector Elements
x <- 1:5
x[c(1,2)] ## returns the first two elements of x

(Idx<-which(x*x>10&x+2<=20)) ## Returns the index of x for which the condition is true

## Basic Operations on Matrix
matrix1 <- matrix(1:25,ncol=5,byrow=T)
matrix1[1,2] ## returns the 2nd element of the 1st row
matrix1[2,3] ## returns the 3rd element of the 2nd row
matrix1[3,]  ## returns all the elements of the 3rd row
matrix1[,4]  ## returns all the elements of the 4th column
## Adding a column 
y <- c(1:5)
matrix1 <- cbind(matrix1,y) ## a new column is added at the end 
## Transpose
matrix2 <- t(matrix1)
## sum
sum(matrix2) ## returns the sum of the matrix
colSums(matrix2) ## returns the sum of each column 

## Basic Operations on Lists
new_list <- list(n = c(2,3,5), s=c("aa", "bb", "cc", "dd", "ee"), b=c(TRUE, FALSE, TRUE, FALSE, FALSE))

new_list[1] ## returns all the elements of the first member of new_list along with the name of first member
new_list[["s"]] ## returns all the elements of the second member of new_list
new_list$b ##  returns all the elements of the third member of new_list
new_list[[2]][2] ## returns the second element of second member of new_list
new_list$b[1] ## returns the first element of third member of new_list

## lapply 
lapply(new_list,length) ## returns the length of each member of new_list
lapply(new_list,function(x){x[1]}) ## returns the first element of each member of new_list


## Basic Operations on Dataframes
a <- 1:10; b <- c(90:99); x <- data.frame(a,b)
x ## to see the dataframe 
x$a ## to get the first column of dataframe 'x', note 'a' is the first column here 
x[1,] ## to get the first row of dataframe 'x'
x[1,"b"] ## to get the first row of second column of dataframe 'x'

## subsetting
subset(x,a>4 & b>95) ## subsets the dataframe for all the rows where both conditions hold true
## subsets the dataframe for all the rows where both conditions hold true, alternative of above statement
x[which(x$a>4 & x$b>95),] 
 
x$c <- c(101:110) ## Adding a column
x[,c(2,3)]  ## taking the 2nd and 3rd of dataframe 'x'
x[,-c(2,3)] ## taking all the columns except 2rd and 3rd, which is here only the first column
## apply
apply(x,1,sum) ## returns the sum of each row of dataframe 'x'
apply(x,2,sum) ## returns the sum of each column of dataframe 'x'
x <- rbind(x,100:102) ## add a row at the end of dataframe x
x <- cbind(x,d=100:110) ## add a new column

## Other Data Operations
summary(x) ## the summary command will print out the min, max, mean, median, and quantiles:
str(x) ## displays the internal structure of an R object
mean(x$a); median(x$b) ; var(x$a) ## mean,median and varaince of specified columns
quantile(x$a) ## returns the different percentile values
min(x$a); max(x$b) ## min and max of specified columns
length(x$a) ## returns the length of a specified column
city <- c("Mumbai","Delhi","Mumbai","Bangalore","Mumbai")
unique(city) ## returns all the distinct values of character vector 'city'

## IfElse
## Example1 
 x <- 1:10
if(length(x)>10){
  print("The length of vector x is greater than 10")
}else if(length(x)<10){
  print("The length of vector x is less than  10")
}else{
  print("The length of x is equal to 10")
}

## Example2 
y <- 16
ifelse(sqrt(y)==4,"y is an integer","y is not an integer")

## Loops
## Example1
## Prints all the elements of vector 'x'
for(i in 1:length(x))
{
  print(x[i])
}
## Example2
## Squares of each element of vector x
A <- 0 ## defining an empty vector 'A'
for(i in 1:length(x))
{
  A[i] <- x[i]*x[i]
  print(A[i])
}
