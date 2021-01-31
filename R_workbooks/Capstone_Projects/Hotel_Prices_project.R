#Load packages

library(statsr)
library(dplyr)
library(ggplot2)
library(gplots)
library(car)
library(lattice)
library(psych)
library(corrgram)

#set working directory

setwd("C:/Users/Sonal Somani/Desktop/IIMInternship/R_code")

#load dataset into R

hotel <- read.csv(paste("Collated_Hotel_Data.csv",sep=""))

#View dataset
View(hotel)
dim(hotel)

#Check your dataset's observations and datatypes
str(hotel)

#Summarising dataset's mean,std dev , median etc.

describe(hotel)[,c(2,3,4,5,8,9)]

#The dependent variable , Y would be RoomRent as it varies based on other factors like star rating, amenties, location etc.

##To determine which 3 independent variables we will be choosing for our analysis, let's Look for the predictor variable that is associated with the greatest increase in R-squared.

#Let us do stepwise regression -

model <- lm(RoomRent ~ StarRating,data=hotel)
summary(model)
#Multiple R-squared:  0.1906,	Adjusted R-squared:  0.1905

model_1 <- lm(RoomRent ~ StarRating+IsMetroCity,data=hotel)
summary(model_1)
#Multiple R-squared:  0.2023,	Adjusted R-squared:  0.2022

model_1 <- lm(RoomRent ~ StarRating+IsTouristDestination,data=hotel)
summary(model_1)
#Multiple R-squared:  0.2177,	Adjusted R-squared:  0.2175


model_1 <- lm(RoomRent ~ StarRating+IsWeekend,data=hotel)
summary(model_1)
#Multiple R-squared:  0.1906,	Adjusted R-squared:  0.1905

model_1 <- lm(RoomRent ~ StarRating+CityRank,data=hotel)
summary(model_1)
#Multiple R-squared:  0.2228,	Adjusted R-squared:  0.2226 ****

model_1 <- lm(RoomRent ~ StarRating+Airport,data=hotel)
summary(model_1)
#Multiple R-squared:  0.2001,	Adjusted R-squared:    0.2

model_1 <- lm(RoomRent ~ StarRating+HotelCapacity,data=hotel)
summary(model_1)
#Multiple R-squared:  0.2031,	Adjusted R-squared:  0.203

model_1 <- lm(RoomRent ~ StarRating+HasSwimmingPool,data=hotel)
summary(model_1)
#Multiple R-squared:  0.2089,	Adjusted R-squared:  0.2087

model_1 <- lm(RoomRent ~ StarRating+FreeWifi,data=hotel)
summary(model_1)
#Multiple R-squared:  0.1906,	Adjusted R-squared:  0.1905 

model_1 <- lm(RoomRent ~ StarRating+FreeBreakfast,data=hotel)
summary(model_1)
#Multiple R-squared:  0.191,	Adjusted R-squared:  0.1908

model_1 <- lm(RoomRent ~ StarRating+IsNewYearEve,data=hotel)
summary(model_1)
#Multiple R-squared:  0.1927,	Adjusted R-squared:  0.1925

model_1 <- lm(RoomRent ~ StarRating+Population,data=hotel)
summary(model_1)
#Multiple R-squared:  0.2183,	Adjusted R-squared:  0.2182

model_2 <- lm(RoomRent ~ StarRating+CityRank+IsMetroCity,data=hotel)
summary(model_2)
#Multiple R-squared:  0.2229,	Adjusted R-squared:  0.2227

model_2 <- lm(RoomRent ~ StarRating+CityRank+IsTouristDestination,data=hotel)
summary(model_2)
#Multiple R-squared:  0.2367,	Adjusted R-squared:  0.2365

model_2 <- lm(RoomRent ~ StarRating+CityRank+IsWeekend,data=hotel)
summary(model_2)
#Multiple R-squared:  0.2228,	Adjusted R-squared:  0.2226

model_2 <- lm(RoomRent ~ StarRating+CityRank+IsNewYearEve,data=hotel)
summary(model_2)
#Multiple R-squared:  0.2249,	Adjusted R-squared:  0.2247

model_2 <- lm(RoomRent ~ StarRating+CityRank+Population,data=hotel)
summary(model_2)
#Multiple R-squared:  0.2237,	Adjusted R-squared:  0.2235

model_2 <- lm(RoomRent ~ StarRating+CityRank+Airport,data=hotel)
summary(model_2)
#Multiple R-squared:  0.2228,	Adjusted R-squared:  0.2227

model_2 <- lm(RoomRent ~ StarRating+CityRank+HotelCapacity,data=hotel)
summary(model_2)
#Multiple R-squared:  0.2285,	Adjusted R-squared:  0.2283

model_2 <- lm(RoomRent ~ StarRating+CityRank+HasSwimmingPool,data=hotel)
summary(model_2)
#Multiple R-squared:  0.2422,	Adjusted R-squared:  0.242 ****

model_2 <- lm(RoomRent ~ StarRating+CityRank+FreeWifi,data=hotel)
summary(model_2)
#Multiple R-squared:  0.223,	Adjusted R-squared:  0.2228

model_2 <- lm(RoomRent ~ StarRating+CityRank+FreeBreakfast,data=hotel)
summary(model_2)
#Multiple R-squared:  0.2233,	Adjusted R-squared:  0.2231 

#So, according to the above adjusted R square values, we see that the best results are from model with explanatory variables as StarRating, CityRank ,HasSwimmingPool. Hence, we pick these three variables for further examination.

#Lets visualize the above mentioned variables Y,x1,x2,x3 where Y is Room rent and X1,X2,X3 are StarRating, CityRank, and HasSwimmingPool respectively.

#Since star rating is categorical in nature with ordinal values, we draw a table and barchart for StarRating.

table(hotel$StarRating)
Rating <- prop.table(table(hotel$StarRating))
barplot(Rating,main = "Star Ratings of Hotels",xlab="Star Rating",ylab="Count",col="lightpink")

#We see that 3 star rated hotels are the most common with being close to 6000 in number.

#Table for CityRank
table(hotel$CityRank)
Ranks<-prop.table(table(hotel$CityRank))
barplot(Ranks,main="Bookings by City Ranks",xlab="City Ranks",ylab="Count",col="lightpink")

#More than 15% bookings are there in City with Rank 1 which is Delhi (Capital City)

#Table for HasSwimmingPool
table(hotel$HasSwimmingPool)
Pool<-prop.table(table(hotel$HasSwimmingPool))
barplot(Pool,main="Bookings by Swimming Pool",xlab="Has Swimming Pool",ylab="Count",col="lightpink")


# We have more than 60% of the bookings in hotels without swimming pools.

str(hotel$HotelCapacity)  
#Since hotel capacity is numeric in nature, we would make a histogram and box plot fot it. 

summary(hotel$HotelCapacity)

#The median capacity of hotels is 62 while max may even go upto 600.

#Histogram for Hotel Capacity

hist(hotel$HotelCapacity, main="Histogram for Hotel Capacity",xlab="Hotel Capacity",col="lightpink")

#This looks like a right skewed distribution as most of the hotels have capacity less than 50 with several outliers at the right end of the chart. 

#Excluding outliers or hotels with capacity > 300 and less than 1 or better results.

hotel <- subset(hotel,hotel$HotelCapacity <= 300 & hotel$HotelCapacity > 0)
hist(hotel$HotelCapacity, main="Histogram for Hotel Capacity",xlab="Hotel Capacity",col="lightpink")

#BoxPlot for Hotel Capacity
boxplot(hotel$HotelCapacity, main="Boxplot for Hotel Capacity",horizontal = TRUE,xlab="Hotel Capacity",col="lightpink")

#The box plot clearly shows that there are a lot of outliers in the distribution.

#Scatter Plots to understand how are the variables correlated pair-wise

#StarRating Vs RoomRent

scatterplot(hotel$StarRating,hotel$RoomRent,main="Hotel Room Prices vs Star Rating",ylab = "Hotel Room Prices (INR)", xlab="Star Rating (0-5)")

#The scatter plot shows a positive trend in the room prices with increase in star ratings.

#Let's confirm with a correlation test.

cor.test(hotel$StarRating,hotel$RoomRent)

#CityRank Vs RoomRent

scatterplot(hotel$CityRank,hotel$RoomRent,main="Hotel Room Prices vs City Rank",ylab = "Hotel Room Prices (INR)", xlab="City Rank")

#The scatter plot does not show a clear trend in the room prices with better city ranks.

#Let's do a correlation test.

cor.test(hotel$CityRank,hotel$RoomRent)

#HasSwimmingPool Vs RoomRent

scatterplot(hotel$HasSwimmingPool,hotel$RoomRent,main="Hotel Room Prices vs City Rank",ylab = "Hotel Room Prices (INR)", xlab="Has Swimming Pool")

#The scatter plot shows a clear trend in the room prices with swimming pools with the rates being on the higher end.

#Let's do a correlation test to confirm this.

cor.test(hotel$HasSwimmingPool,hotel$RoomRent)

#RoomRent Vs HotelCapacity

scatterplot(hotel$RoomRent,hotel$HotelCapacity,main="Hotel Room Prices vs Hotel Capacity",ylab = "Hotel Capacity", xlab="Hotel Room Prices (INR)",col="darkblue")

#There is a lot of variabity in the hotel room prices and hotel capacity.

#Let's confirm with a correlation test.

cor.test(hotel$HotelCapacity,hotel$RoomRent)

#RoomRent Vs IsTouristDestination

#Excluding outlier with Room rents more than 100000 -

hotel <- subset(hotel,hotel$RoomRent <= 100000)

#Boxplot of Tourist Destination vs Room Rent

bwplot(IsTouristDestination~RoomRent, data = hotel,main="Prices of Hotel Rooms vs Tourist destination",ylab = "Is Tourist destination ", xlab="Hotel room Price (INR)" )

#Jitter Plot of Tourist Destination vs Room Rent

plot(hotel$RoomRent,jitter(hotel$IsTouristDestination),main="Prices of Hotels vs Tourist destination",ylab = "Is Tourist destination", xlab="Hotel room Price (INR)",col="darkblue")

#Both the plots clearly confirms that being a tourist destination shows a positive trend in increase of hotel room prices.

#Let's confirm with a correlation test.

cor.test(hotel$IsTouristDestination,hotel$RoomRent)

#Scatterplot matrix between Y and x1,x2,x3

scatterplotMatrix(
  hotel[
    ,c("RoomRent","StarRating","CityRank","HasSwimmingPool" )], 
  spread=FALSE, smoother.args=list(lty=2),
  main="Scatter Plot Matrix", diagonal = "histogram")

#Corrgram of Y, x1, x2, x3
subset <- data.frame(hotel$RoomRent, hotel$HasSwimmingPool, hotel$CityRank, hotel$StarRating)

corrgram(subset, lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt,
         main="Corrgram of Room Rent (Y) vs Explanatory variables")

#Correlation Matrix

library(Hmisc)
colroom <- c("RoomRent", "HasSwimmingPool", "CityRank", "StarRating")
corMatrix <- rcorr(as.matrix(hotel[,colroom]))
corMatrix

#Another way to do this -

#Variance-Covariance Matrix

x<-hotel[,c("HasSwimmingPool","StarRating", "CityRank")]
y<-hotel[,c("RoomRent")]
cor(x,y)
cov(x,y)

#A. Hypothesis -
#  H0: The variables ISWeekend,IsTouristDestination,HasSwimmingPool,StarRating,CityRank,HotelCapacity,Airport,IsNewYear,IsMetroCity collectively have no effect on RoomRent.
#H1: ISWeekend,IsTouristDestination,HasSwimmingPool,StarRating,CityRank,HotelCapacity,Airport,IsNewYear,IsMetroCity together affect RoomRent

#B. Hypothesis -
#  H0 : The variables IsTouristDestination,HasSwimmingPool,StarRating,CityRank,Airport ,IsNewYearEve collectively have no effect on RoomRent.
#H1 : IsTouristDestination,HasSwimmingPool,StarRating,CityRank,Airport ,IsNewYearEve together affect RoomRent

#Hypothesis 1 
#H1 = On weekends, we have higher room prices
#H0= Room prices do not vary with being a weekend/weekday

#Let's test this using a t-test:
#We first subset the booking made on a weekday and those on a weekend.

weekday <- subset(hotel,IsWeekend==0)
weekend <- subset(hotel,IsWeekend==1)
par(mfrow=c(1,2))
boxplot(weekday$RoomRent,main="Weekday Room Prices")
boxplot(weekend$RoomRent,main="Weekend Room Prices")

var.test(weekday$RoomRent,weekend$RoomRent)

t.test(weekday$RoomRent,weekend$RoomRent,var.equal = TRUE,alternative="less")

#Since the p-value is 0.2135 which is above the significance level of 0.05, we fail to reject the null hypothesis of independence between weekend and room prices.

#Hypothesis 2
#H1 = With Swimming Pool, the room prices are higher
#H0= Room prices do not vary with having a Swimming pool or not 

#Let's test this using a t-test:
#We first subset the bookings with and without swimming pool.

no_pool <- subset(hotel,HasSwimmingPool==0)
pool <- subset(hotel,HasSwimmingPool==1)

par(mfrow=c(1,2))
boxplot(no_pool$RoomRent,main="Without Pool Room Prices")
boxplot(pool$RoomRent,main="With Pool Room Prices")

var.test(no_pool$RoomRent,pool$RoomRent)

t.test(no_pool$RoomRent,pool$RoomRent,alternative="less",var.equal=FALSE)

#Hypothesis 3
#H1 = When cities are tourist destinations, the room prices are higher
#H0= Room prices do not vary with being a tourist destination or not

#Let's test this using a t-test:
#We first subset the booking made in tourist destinations & those which aren't.

not_touristy <- subset(hotel,IsTouristDestination==0)
touristy <- subset(hotel,IsTouristDestination==1)

par(mfrow=c(1,2))
boxplot(not_touristy$RoomRent,main="Not Tourist Destination vs Room Prices")
boxplot(touristy$RoomRent,main="Tourist Destinations vs Room Prices")

t.test(not_touristy$RoomRent,touristy$RoomRent,alternative="less")

#Hypothesis 4
#H1 = Are better star rated hotels better priced?
#H0 = Prices do not vary with Star Rating

#Let's test this using a t-test:
#We create two buckets, one with bookings made in less than 4 star rated rotels & another bookings with more than 4 star rated hotels.

below_4_star <- subset(hotel,StarRating<4)
above_4_star <- subset(hotel,StarRating>=4)

par(mfrow=c(1,2))
boxplot(below_4_star$RoomRent,main="Below 4 star rated hotels vs Room Prices")
boxplot(above_4_star$RoomRent,main="Above 4 star rated hotels vs Room Prices")

t.test(below_4_star$RoomRent,above_4_star$RoomRent,alternative="less")

#Hypothesis 5
#H1 = Room prices with lesser city ranks are higher
#H0 = Room prices with lesser city ranks are not higher

#Let's test this using a t-test:
#We create two buckets, one with bookings made in cities ranked below 22 & another with bookings made in cities ranked more than 22.

below_10 <- subset(hotel,CityRank<=22)
above_10 <- subset(hotel,CityRank>22)

par(mfrow=c(1,2))
boxplot(below_10$RoomRent,main="Bookings in cities ranked below 22 vs Room Prices")
boxplot(above_10$RoomRent,main="Bookings in cities ranked above 22 vs Room Prices")

#Our one tailed t-test is as follows - 

t.test(below_10$RoomRent,above_10$RoomRent,alternative="greater")

#Since this gives a p-value of 1, which although is very unlikely and also lies way above our significance levels, hence we fail to reject our null hypothesis.

#Although, if we do a two tailed test with hypothesis being :
#Hypothesis 6
#H1 = Room prices vary with city ranks
#H0 = Room prices do not vary with city ranks

t.test(below_10$RoomRent,above_10$RoomRent)

#Hypothesis 6
#H1 = Hotels with higher capacities have higher room prices
#H0 = Room prices do not vary with Hotel Capacity

summary(hotel$HotelCapacity)
boxplot(hotel$HotelCapacity)

#Let's test this using a t-test:
#We create two buckets, one with bookings made in hotels with capacity below 60 & another with capacity more than 60.

below_60 <- subset(hotel,HotelCapacity<=60)
above_60 <- subset(hotel,HotelCapacity>60)

par(mfrow=c(1,2))
boxplot(below_60$RoomRent,main="Bookings in cities with hotel capacity below 60 vs Room Prices")
boxplot(above_60$RoomRent,main="Bookings in cities with hotel capacity above 60 vs Room Prices")

t.test(below_60$RoomRent,above_60$RoomRent,alternative="less")

#Hypothesis 7
#H1 = Hotels with higher distances to airport have higher prices (Probably because airports are outside the city)
#H0 = Room prices do not vary with distance to Airport

summary(hotel$Airport)
boxplot(hotel$Airport)

#Let's test this using a t-test:
#We create two buckets, one with bookings made in hotels with distance to airport less than 15 kms & another with distance to airport more than 15 kms.

below_15 <- subset(hotel,Airport<=15)
above_15 <- subset(hotel,Airport>15)

par(mfrow=c(1,2))
boxplot(below_15$RoomRent,main="Bookings in cities with distance to airport below 15 vs Room Prices")
boxplot(above_15$RoomRent,main="Bookings in cities with distance to airport above 15 vs Room Prices")

t.test(below_15$RoomRent,above_15$RoomRent,alternative="less")

#We will now make several models and then pick the best of them based on AIC,BIC and R2 values.

#Let's make a regression model for hypothesis A.

model <- lm(log(RoomRent) ~ IsWeekend+IsTouristDestination+HasSwimmingPool+StarRating+CityRank+HotelCapacity+Airport +IsNewYearEve+IsMetroCity,data=hotel)

summary(model)

exp(model$coefficients)

library(leaps)
leap1 <- regsubsets(log(RoomRent) ~ IsWeekend+IsTouristDestination+HasSwimmingPool+StarRating+CityRank+HotelCapacity+Airport +IsNewYearEve+IsMetroCity, data = hotel, nbest=1)

# summary(leap1)

plot(leap1, scale="adjr2")

#The best fit model excludes IsMetrocCity,HotelCapacity,IsWeekend. Therefore, in our next model, we rerun the regression, excluding these variables.

#Let's make a regression model for hypothesis B.

model_1 <- lm(log(RoomRent) ~ IsTouristDestination+HasSwimmingPool+StarRating+CityRank+Airport +IsNewYearEve,data=hotel)
summary(model_1)

exp(model_1$coefficients)

#Let's choose the better model out of these two -

AIC(model) 
BIC(model)
#AIC = 20761.41 , BIC = 20843.56, Multiple R-squared:  0.4214,	Adjusted R-squared:  0.421

AIC(model_1) 
BIC(model_1)
#AIC = 20758.84 , BIC = 20818.59, Multiple R-squared:  0.4212,	Adjusted R-squared:  0.421 

#Visualize the beta coefficients and their confidence intervals from model 1 -

library(coefplot)

coefplot(model_1, intercept=FALSE,outerCI=1.96,coefficients=c("IsTouristDestination","HasSwimmingPool","StarRating","CityRank","Airport","IsNewYearEve"))

#For inferences, testing a subset of variables using partial F-Test.
#The following code performs the partial F-test:

# Reduced model
reduced = lm(log(RoomRent) ~ IsTouristDestination+HasSwimmingPool+StarRating+CityRank, data=hotel) 
# Full Model
full = lm(log(RoomRent) ~ IsTouristDestination+HasSwimmingPool+StarRating+CityRank+Airport+IsNewYearEve,data=hotel) 

anova(reduced, full)

#Another Partial F-Test -

# Reduced model
reduced = lm(log(RoomRent) ~ IsTouristDestination+Airport+HasSwimmingPool+StarRating+CityRank, data=hotel) 
# Full Model
full = lm(log(RoomRent) ~ IsTouristDestination+HasSwimmingPool+StarRating+CityRank+Airport+IsNewYearEve,data=hotel) 

anova(reduced, full) 

#Confidence and prediction Intervals

dim(hotel)
#Let's split the data set with some observations for test dataset
train <- hotel[1:12900,]
test <- hotel[12901:12941,]
test

#Taking, log of the price as it should not come to be negative.
#Log Linear model
results = lm(log(RoomRent) ~ IsTouristDestination+HasSwimmingPool+StarRating+CityRank+Airport+IsNewYearEve,data=train)

predict(results,data.frame(IsTouristDestination=0,HasSwimmingPool=0,StarRating=2,CityRank=3,Airport=9.6,IsNewYearEve=0),interval="confidence")

#Exponentiating back the results
exp(7.278233) #1448.426
exp(7.332348) #1528.968

#A 95% confidence interval is given by (1448.426, 1528.968)
# With a 95% confidence interval, the results after exponentitaion that we get are really close to the actual room price of 1468. Thus, our model seems to be doing good.

predict(results,data.frame(IsTouristDestination=0,HasSwimmingPool=0,StarRating=2,CityRank=3,Airport=9.6,IsNewYearEve=0),interval="prediction")

exp(6.25586) #521.0573
exp(8.376676) #4344.544

#A 95% prediction interval is given by (521.0573, 4344.544).
#The results obtained for prediction have a wider range than confidence interval indicating that the variation about the mean is fairly large.

#Applyting this model in a generic manner to full testing data which is split in a a 70:30 ratio -

# For reproducibility; 123 has no particular meaning
set.seed(123) 

# randomly pick 70% of the number of observations (365)
index <- sample(1:nrow(hotel),size = 0.7*nrow(hotel)) 

# subset weather to include only the elements in the index
train <- hotel[index,] 

# subset weather to include all but the elements in the index
test <- hotel[-index,] 

nrow(train)
nrow(test)

#Evaluation metrics

#Baseline model
best.guess <- mean(train$RoomRent)
RMSE.baseline <- sqrt(mean((best.guess-test$RoomRent)^2))
RMSE.baseline #5288.912

MAE.baseline <- mean(abs(best.guess-test$RoomRent))
MAE.baseline #3063.866

results = lm(log(RoomRent+1) ~ IsTouristDestination+HasSwimmingPool+StarRating+CityRank+IsNewYearEve+Airport,data=train)

summary(results)

#Multiple R-squared:  0.4213,	Adjusted R-squared:  0.421

test.pred <- exp(predict(results,test))-1

#Evaluating the accuracy -
RMSE <- sqrt(mean((test.pred-test$RoomRent)^2))
RMSE #1982.59

MAE.lin.reg <- mean(abs(test.pred-test$RoomRent))
MAE.lin.reg #1472.424

library(coefplot)

coefplot(results, intercept=FALSE,outerCI=1.96,coefficients=c("IsTouristDestination","HasSwimmingPool","StarRating","CityRank","Airport","IsNewYearEve"))



