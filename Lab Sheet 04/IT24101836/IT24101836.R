setwd("C://Users//it24101836//Desktop//IT24101836")
data<-read.table("DATA 4.txt",header=TRUE,sep = " ")
fix(data)
attach(data)

#Obtaining Box Plots
boxplot(X1,main="Box plot for Team Attendence",outline=TRUE,outpch=8,horizontal=TRUE) 
boxplot(X2,main="Box plot for Team Salary",outline=TRUE,outpch=8,horizontal=TRUE) 
boxplot(X3,main="Box plot for Years",outline=TRUE,outpch=8,horizontal=TRUE)

#obtaining Histogram
hist(X1,ylab="Frequency",Xlab="Team Attendence",main="Histogram for Team Attendence")
hist(X2,ylab="Frequency",Xlab="Team Salary",main="Histogram for Team Salary") 
hist(X3,ylab="Frequency",Xlab="Years",main="Histogram for Years")

#Stem & Leaf Plot
stem(X1)
stem(X2)
stem(X3)

#Mean
mean(X1)
mean(X2)
Mean(X3)

#Median
median(X1)
median(X2)
median(X3)

#Standard Deviation
sd(X1)
sd(X2)
sd(X3)

#Getting five number summary along with mean value
summary(X1)
summary(X2)
summary(X3)

#Getting only five number summary for X1 variable
quantile(X1)

#Calling first Quartile of X1 using index value
quantile(X1) [2]

#calling third Quartile of X1 using index value
quantile(X1) [4]

#Obtaining Inter Quartile Range (IQR) of each variable
IQR (X1)
IQR (X2)
IQR (X3)

#Part 3
#Function to get the mode of a data set
get.mode<-function(y){
  counts<-table(X3)
  names(counts[counts == max(counts)])
}

get.mode(X3)
table(X3)
max(counts)
counts == max(counts)
counts[counts == max(counts)]
names(counts[counts == max(counts)])

#part4
#Function to check the existence of outliers of a data set
get.outliers<-function (z) {
  q1<-quantile (z) [2]
  q3<-quantile (z) [4]
  iqr<-q3-q1
  
  ub<-q3 + 1.5*igr
  lb<-q1 - 1.5*igr
  
  print(paste("Upper Bound = ", ub)) 
  print(paste("Lower Bound = ", lb))
  print(paste("Outliers:", paste(sort(z[z<lb | z>ub]), collapse = ",")))
}

#Checking the outliers of a variable using the function defined above 
get.outliers(X1)
get.outliers(x2) 
get.outliers(X3)

#Following command is to calculate the interval for outlier
get.outliers<-function (z) {
  q1<-quantile (z) [2]
  q3<-quantile (z) [4]
  iqr<-q3-q1
  
  ub<-q3 + 1.5*igr
  lb<-q1 - 1.5*igr

  print(paste("Upper Bound = ", ub)) 
  print(paste("Lower Bound = ", lb))
}

print(paste("Outliers:",Paste(sort(z[z<lb | z>ub]), collapse = ","))) 



#Exercise
#Q1
setwd("C://Users//it24101836//Desktop//IT24101836")
branch_data <- read.table("Exercise.txt", header = TRUE, sep = ",")
#Q3
boxplot(branch_data$Sales,
        main = "Boxplot of Sales",
        ylab = "Sales",
        col = "lightblue",
        horizontal = TRUE)
#Q4
summary(branch_data$Advertising)
fivenum(branch_data$Advertising)
IQR(branch_data$Advertising)
#Q5
# Function to find outliers in a numeric vector
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- IQR(x)
  
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  
  outliers <- x[x < lower_bound | x > upper_bound]
  
  return(list(
    Q1 = Q1,
    Q3 = Q3,
    IQR = IQR_val,
    Lower_Bound = lower_bound,
    Upper_Bound = upper_bound,
    Outliers = outliers
  ))
}

# Import dataset
branch_data <- read.table("Exercise.txt", header = TRUE, sep = ",")

# Apply function to Years variable
find_outliers(branch_data$Years_X3)

