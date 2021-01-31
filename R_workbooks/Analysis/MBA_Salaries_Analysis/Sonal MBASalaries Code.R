# Analysis of MBA SALARIES
# NAME: Sonal Somani
# EMAIL: sonalm300@gmail.com
# COLLEGE / COMPANY: Antuit India Pvt. Ltd.

#Load packages

library(statsr)
library(dplyr)
library(ggplot2)
library(gplots)

#set working directory

setwd("C:/Users/Sonal Somani/Desktop/IIMInternship/R_code")

#load dataset into R

salary <- read.csv(paste("MBA_Salary_Data.csv",sep=""))

#View dataset
View(salary)

#Check how your data looks like with their datatypes
str(salary)

#To get summary statistics of your data, like mean, median, mode for numeric data etc.

summary(salary)

#Single varibale visualizations - Explorartory Data Analysis 

#Histogram of salary
library(lattice)
histogram(salary$salary, type="count", xlab="Salary",ylab = "Count",col=c("lightblue"))

#Histogram of salary with percentages

histogram(salary$salary, xlab="Salary",ylab = "Percent",col=c("lightblue"))

#Box plot of salary
bwplot(salary$salary, 
       horizontal=TRUE, xlab="Salary")

#Here, our results are right skewed as there are more than 60% people who are getting salary as 0 as they are not placed. 

#Filter people not placed i.e. people with salary less than 999.
placed <- subset(salary, salary > 999)

#Box plot of salary with data frame placed
bwplot(placed$salary, 
       horizontal=TRUE, xlab="Salary")

#Histogram of age
histogram(salary$age, type="count", xlab="Age",ylab = "Count",col=c("lightblue"))

#Histogram of age
histogram(salary$age, type="count", xlab="Age",ylab = "Count",col=c("lightblue"))

#Box plot of gmat total score of all students

bwplot(salary$gmat_tot, 
       horizontal=TRUE, xlab="Salary")

summary(salary$gmat_tpc)

#Histogram of gmat total percentiles

histogram(salary$gmat_tpc, type="count", xlab="Gmat_Percentiles",ylab = "Count",col=c("lightblue"))

#Box plot of work experience (years)

bwplot(salary$work_yrs, 
       horizontal=TRUE, xlab="Work Experience (years)")

table(salary$satis)

#Since many didn't fill out the survey for satisfaction levels, we will filter them out and see the distribution again

survey <- subset(salary, satis < 8)

#Bar chart of satisfaction level with the MBA Program

ggplot(survey, aes(x = satis, fill = satis)) + geom_bar()

# Histogram for Spring Average (s_avg)

histogram(salary$s_avg, type="count", xlab="Spring Average",ylab = "Count",col=c("lightblue"))

# Histogram for Fall Average (f_avg)

histogram(salary$f_avg, type="count", xlab="Fall Average",ylab = "Count",col=c("lightblue"))

#Fall average for a couple of students is 0, which may be beacuse those people dropped out of the course or some other reason. If we exclude those and re run our analysis, let's see whta do we get -

passed <- subset(salary, f_avg > 0 )

histogram(passed$f_avg, type="count", xlab="Fall Average",ylab = "Count",col=c("lightblue"))

table(salary$quarter)

#Bar chart of the quartile ranking of students
ggplot(salary, aes(x = quarter)) + geom_bar()

table(salary$frstlang)

#Bar chart of the first language of people enrolled in the course
ggplot(salary, aes(x = frstlang)) + geom_bar()

#Time for bivariate analysis - scatterplots

library(lattice)
#Let's do this only for students who were placed.

#Scatter plot for Salary and spring average (Since both are numeric in nature)

plot(placed$s_avg,placed$salary,
     col="blue",
     main="Salary vs spring average",
     xlab="Spring avg. GPA", ylab="Starting Salary")

# Add the sample means to the Scatterplot

abline(h=mean(placed$salary), col="dark blue", lty="dotted")
abline(v=mean(placed$s_avg), col="dark blue", lty="dotted")


# Add a regression line

abline(lm(placed$salary ~ placed$s_avg))

# With this information, it is hard to say more spring GPA avg. is correlated with more starting salary.

#Let's do a correlation test to confirm this observation of ours.

cor.test(placed$salary,placed$s_avg)

#Scatter plot for Salary and fall average (Since both are numeric in nature)

plot(placed$f_avg,placed$salary,
     col="blue",
     main="Salary vs fall average",
     xlab="Fall avg. GPA", ylab="Starting Salary")

# Add the sample means to the Scatterplot

abline(h=mean(placed$salary), col="dark blue", lty="dotted")
abline(v=mean(placed$f_avg), col="dark blue", lty="dotted")


# Add a regression line

abline(lm(placed$salary ~ placed$f_avg))

# With this information, it is hard to say more fall GPA avg. is correlated with more starting salary.

#Scatter plot matrix between work experience and salary

library(car)
scatterplotMatrix(formula = ~work_yrs + salary, cex = 0.6, data = placed, diagonal = "histogram")

#Scatter plot for Salary and work experience (Since both are numeric in nature)

plot(placed$work_yrs,placed$salary,
     col="blue",
     main="Salary vs Work experience",
     xlab="Work experience (years)", ylab="Starting Salary")

# Add the sample means to the Scatterplot

abline(h=mean(placed$salary), col="dark blue", lty="dotted")
abline(v=mean(placed$work_yrs), col="dark blue", lty="dotted")


# Add a regression line

abline(lm(placed$salary ~ placed$work_yrs))

# The regression line shows an upward trend in the starting salaries and work experience (yrs), we can confirm this by doing a correlation test.

cor.test(placed$salary, placed$work_yrs)

#Scatter plot for Salary and Gmat Score (Since both are numeric in nature)

plot(placed$gmat_tot,placed$salary,
     col="blue",
     main="Salary vs Gmat scores",
     xlab="Gmat Scores", ylab="Starting Salary")

# Add the sample means to the Scatterplot

abline(h=mean(placed$salary), col="dark blue", lty="dotted")
abline(v=mean(placed$gmat_tot), col="dark blue", lty="dotted")


# Add a regression line

abline(lm(placed$salary ~ placed$gmat_tot))

# With the above information, it is hard to say if more GMAT scores are correlated with starting salary, we can confirm this by doing a correlation test.

cor.test(placed$salary,placed$gmat_tot)

#Scatter plot for Salary and Gmat Total Percentile (Since both are numeric in nature)

plot(placed$gmat_tpc,placed$salary,
     col="blue",
     main="Salary vs Gmat Total Percentile",
     xlab="Gmat Total Percentile", ylab="Starting Salary")

# Add the sample means to the Scatterplot

abline(h=mean(placed$salary), col="dark blue", lty="dotted")
abline(v=mean(placed$gmat_tpc), col="dark blue", lty="dotted")


# Add a regression line

abline(lm(placed$salary ~ placed$gmat_tpc))

# With the given plot, it is hard to say if more GMAT scores are correlated with starting salary.

#Box plots for salaries by quartiles

library(lattice)

bwplot(quarter ~ salary, data=placed, horizontal=TRUE, 
       xlab = "Starting Salary")

#The plot shows that quartiles does not affect much the distribution of salaries.

# Box plot for salary by sex

bwplot(sex ~ salary, data=placed, horizontal=TRUE, 
       xlab = "Starting Salary")

# The plot shows that there seems to be some gender disparity in place when comparing salaries by gender. Median salary for females seems to be comparitively lower than that of males.

#Box plot for salary by first language

bwplot(frstlang ~ salary, data=placed, horizontal=TRUE, 
       xlab = "Starting Salary")

#While english speakers salary follows a normal distribution with some outliers on both end, none of the students whose first language is not english and are placed got a salary below 90000.

#Let's do a correlation test -
cor.test(placed$salary, placed$frstlang)

library(corrplot)

par(mfrow=c(1, 1))
corrplot.mixed(corr=cor(placed[ , c(1:13)], use="complete.obs"),
               upper="ellipse", tl.pos="lt",
               col = colorpanel(50, "red", "gray", "blue"))

#Contingency table for salary,sex and first language

#Since this would only work on factors, I would be converting variables like sex and firstlang into factors.

placed <- placed %>% mutate(sex1=factor(sex),firstlang1=factor(frstlang),quarter1=factor(quarter))

barchart(salary ~ firstlang1 , data=placed, 
         groups=sex1, auto.key=TRUE,
         par.settings = simpleTheme(col=c("gray95", "gray50")),xlab="First Language",ylab="Salary" )

#Contingency tables between sex,first language and quartiles

prop.table(table(placed$sex1))

prop.table(table(placed$sex1,placed$firstlang1))

mytable <- table(placed$sex1,placed$quarter1)
prop.table(table(placed$sex1,placed$quarter1))

#Chi sq test of independence between quartiles and sex

chisq.test(mytable)

#Since the p-value is not below 0.05, we fail to reject our null hypothesis that quartiles and sex are independent of each other.

mytable1 <- table(placed$quarter1,placed$firstlang1)
prop.table(table(placed$quarter1,placed$firstlang1))

#Chi sq test of independence between quartiles and first language

chisq.test(mytable1)

#Since the p-value is not below 0.05, our null hypothesis that quartiles and first language are independent of each other is not rejected.

#t-tests

#Articulating hypothesis as -
#H1 = Work experience does have an effect on salary
#Running t-test to test our hypothesis (H0,H1) -
#Here, the null hypothesis (H0) is that work experience does not have an effect on salary.
#Let's see -

boxplot(placed$salary~placed$work_yrs,main = "Work Exp. and Salary",col = (c("green","blue")), ylab = "Salary")

t.test(placed$salary,placed$work_yrs)

#Articulating hypothesis as -
#H1 = Males have a higher mean starting salary than females
#Running t-test to test our hypothesis (H0,H1) -
#Here, the null hypothesis (H0) is that Males and females have equal mean starting salaries.

#Let's see -

boxplot(placed$salary~placed$sex1,main = "Work Exp. and Sex",col = (c("green","blue")), ylab = "Salary")

t.test(placed$salary~placed$sex1,alternative="greater")

#Linear Regression Models

#Model_1
model_1 <- lm(salary ~ work_yrs + s_avg + sex, data = placed)
summary(model_1)

#Multiple R-squared:  0.2239, Adjusted R-squared:  0.2004
#p-value: 1.388e-05

model_2 <- lm(salary ~ work_yrs + s_avg + sex + gmat_tot, data = placed)
summary(model_2)
#Multiple R-squared:  0.2262,	Adjusted R-squared:  0.1947
#p-value: 4.208e-05

model_3 <- lm(salary ~ work_yrs + s_avg + sex + gmat_tpc, data = placed)
summary(model_3)

#Multiple R-squared:  0.2317,	Adjusted R-squared:  0.2004 
#p-value: 3.036e-05

model_4 <- lm(salary ~ work_yrs + f_avg + sex , data = placed)
summary(model_4)

#Multiple R-squared:  0.2224,	Adjusted R-squared:  0.1989 
#p-value: 1.523e-05


model_5 <- lm(salary ~ work_yrs + f_avg + sex + quarter, data = placed)
summary(model_5)

#Multiple R-squared:  0.2287,	Adjusted R-squared:  0.1972 
#p-value: 3.644e-05

#Comparing those who didn't get a job whith those who did

#Removing not answered survey responses records 
salary1 <- subset(salary, salary!=998 & salary!=999)
View(salary1)

#Subset of salary dataframe for people who weren't placed
notplaced <- subset(salary, salary == 0)
View(notplaced)
summary(notplaced)

#Let's keep 1 in place of salaries which are greater than 1, as we are only concerned with whether the person got placed or not.

salary1$salary = ifelse(salary1$salary > 1, 1,0)
View(salary1)
summary(salary1)


#Subset of salary1 dataframe for people who were placed
placed1 <- subset(salary1, salary == 1)
View(placed1)
summary(placed1)

#Contingency table between salary and s_avg

mytable2 <- table(salary1$salary,salary1$s_avg)

#Stacked barplot for placed or not depending on s_avg

barplot(mytable2, main="Avg Spring GPA and placements",
        xlab="Avg. Spring GPA", col=c("red","darkblue"),
        legend = rownames(mytable2))

chisq.test(salary1$salary,salary1$s_avg)

#Contingency tables between salary and gmat_tot

mytable3 <- table(salary1$salary,salary1$gmat_tot)

#Stacked barplot for placed or not depending on gmat total scores

barplot(mytable3, main="GMAT total scores and placements",
        xlab="GMAT total scores", col=c("red","darkblue"),
        legend = rownames(mytable3))

chisq.test(salary1$salary,salary1$gmat_tot)

#Contingency tables between salary and gmat_tot

mytable4 <- table(salary1$salary,salary1$sex)

#Stacked barplot for placed or not depending on gmat total scores

barplot(mytable4, main="Gender and placements",
        xlab="Gender", col=c("red","darkblue"),
        legend = rownames(mytable4))

chisq.test(salary1$salary,salary1$sex)

#Contingency tables between salary and work experience

mytable5 <- table(salary1$salary,salary1$work_yrs)

#Stacked barplot for placed or not depending on gmat total scores

barplot(mytable5, main="work Experience and Placements",
        xlab="Work Exp (yrs)", col=c("red","darkblue"),
        legend = rownames(mytable5))

chisq.test(salary1$salary,salary1$work_yrs)

#We ran a couple of chi sq tests above but none of them suggests that we reject the null hypothesis as for all the tests, the obtained p-value is not statistically significant.

#Corrgram
corrplot.mixed(corr=cor(salary1[ , c(1:13)], use="complete.obs"),
               upper="ellipse", tl.pos="lt",
               col = colorpanel(50, "red", "gray60", "blue4"))

#No strong correlations are found between the salary varibale and other variables.

#Before proceeding ahead, I would want to convert the categorical variables from int to factors.

salary1 <- salary1 %>% mutate(sex1=factor(sex),firstlang1=factor(frstlang),quarter1=factor(quarter),satis1=factor(satis))

str(salary1)

salary2 <- subset(salary1,select=c(1,3,4,5,6,7,8,10,12,14,15,16,17))

str(salary2)

#Here, we would run this regression to predict if a student will get placed or not.

#Splitting the observations into training and test datasets

train <- salary2[1:154,]
test <- salary2[155:193,]

names(train)

#Logistic regression model implemented

logit_model <- glm(salary ~.,family=binomial(link='logit'),data=train)
summary(logit_model)

#Taking all variables may not have helped much as none of them are significant acc. to the model. AIC: 224.31 

logit_model_1 <- glm(salary ~ sex1 + work_yrs + gmat_tot + s_avg + firstlang1,family=binomial(link='logit'),data=train)
summary(logit_model_1)

#Also, we can do ANOVA test on the models, the greater the deviation between null and residuals, the better.

anova(logit_model, test="Chisq")
anova(logit_model_1, test="Chisq")

#The drop in the devaince upon dropping a couple variables suggests that logit_model_1 is better among the two.

#Assessing predictabilty of the model

fitted.results <- predict(logit_model_1,newdata=subset(test,select=c(1,2,3,4,5,6,7,8,10,11,12,13)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$salary)
print(paste('Accuracy',1-misClasificError))

#ROC and AUC curve for performance measures : As a rule of thumb, a model with good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5.

library(ROCR)
p <- predict(logit_model_1, newdata=subset(test,select=c(1,2,3,4,5,6,7,8,10,11,12,13)), type="response")
pr <- prediction(p, test$salary)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc










