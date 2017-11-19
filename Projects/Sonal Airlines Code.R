# Analysis of Airline Ticket Pricing
# NAME: <Sonal Somani>
# EMAIL: <sonalm300@gmail.com>
# COLLEGE / COMPANY: <Antuit>

#Load packages

library(statsr)
library(dplyr)
library(ggplot2)

#set working directory

setwd("C:/Users/Sonal Somani/Desktop/IIMInternship/R_code")

#load dataset into R

airlines <- read.csv(paste("SixAirlines.csv",sep=""))

#View dataset
View(airlines)

#Check how your data looks like with their datatypes
str(airlines)

#To get summary statistics of your data, like mean, median, mode for numeric data etc.

summary(airlines)

#Since Airline column contains nomimal categorical data, we can draw a bar chart to represent its distribution.
#Plotting barchart for the airline frequency distribution
ggplot(airlines, aes(x = AIRLINE, fill = AIRLINE)) + geom_bar()

#Let's check which company makes the most aircrafts according to our data.
ggplot(airlines, aes(x = AIRCRAFT, fill = AIRCRAFT)) + geom_bar()

#Let's check how the airlines relates to these aircarfts.
#Distribution showing aircrafts used by the airlines
table(airlines$AIRLINE, airlines$AIRCRAFT)

#Since flight duration holds numeric data, let's make a box plot for it and see its distribution.
library(lattice)
summary(airlines$FLIGHT_DURATION)

#Box plot for flight duration to understand its distribution

bwplot(airlines$FLIGHT_DURATION, 
       horizontal=TRUE, xlab="Flight_Duration (hours)")

#Box plot showing the distribution for flight duration of different airlines side by side

bwplot(airlines$AIRLINE ~ airlines$FLIGHT_DURATION, data=airlines, 
       horizontal=TRUE, xlab="Flight_Duration (hours)")

#Box plot showing the distribution for flight duration of different airlines based on whether the flight is domestic or international side by side

bwplot(airlines$AIRLINE ~ airlines$FLIGHT_DURATION | airlines$INTERNATIONAL, data=airlines, 
       horizontal=TRUE, xlab="Flight_Duration (hours)")

#Plot to see in which months are the flights most frequent i.e. from July, Aug, Sept and Oct.
ggplot(airlines, aes(x = MONTH, fill = MONTH)) + geom_bar()

#Plot to see how many flights are international and how many aren't.
#Bar plot to see number of International and domestic flights
ggplot(airlines, aes(x = INTERNATIONAL))+ geom_bar()

#Distribution of economy and premium seats
#Since the variables seats_economy and seats_premium are numberic in nature, we will draw box plots for these.

par(mfrow=c(1,2))

bwplot(airlines$SEATS_ECONOMY, 
       horizontal=TRUE, xlab="Economy Seats")


bwplot(airlines$SEATS_PREMIUM, 
       horizontal=TRUE, xlab="Premium Seats")

#Similarly we will draw bar plots for the variables pitch_economy, pitch_premium, width economy and width premium.
par(mfrow=c(1,4))
ggplot(airlines, aes(x = PITCH_ECONOMY))+ geom_bar()

ggplot(airlines, aes(x = PITCH_PREMIUM))+ geom_bar()

ggplot(airlines, aes(x = WIDTH_ECONOMY))+ geom_bar()

ggplot(airlines, aes(x = WIDTH_PREMIUM))+ geom_bar()


par(mfrow=c(1,2))
bwplot(airlines$PRICE_PREMIUM, 
       horizontal=TRUE, xlab="Premium Seats Price")

bwplot(airlines$SEATS_ECONOMY, 
       horizontal=TRUE, xlab="Economy Seats Price")

#Distribution showing airlines and premium seats distribution

table(airlines$AIRLINE, airlines$SEATS_PREMIUM)
barchart(airlines$AIRLINE ~ airlines$SEATS_PREMIUM, data=airlines,xlab="Premium Seats",ylab="Airlines")

#Let's do a box plot for the premium seat price based on the airlines.
bwplot(airlines$AIRLINE ~ airlines$PRICE_PREMIUM, data=airlines, 
       horizontal=TRUE, xlab="Premium Price (USD)")

#Let's see the distribution of premium width and pitch based on the diff. airlines.
table(airlines$AIRLINE, airlines$PITCH_PREMIUM)
barchart(airlines$AIRLINE ~ airlines$PITCH_PREMIUM, data=airlines,xlab="Premium Pitch (inches)",ylab="Airlines")

table(airlines$AIRLINE, airlines$WIDTH_PREMIUM)
barchart(airlines$AIRLINE ~ airlines$WIDTH_PREMIUM, data=airlines,xlab="Width Pitch (inches)",ylab="Airlines")

#Let's do a scatterplot of price economy vs flight duration -
#Scatterplot of Price vs Flight duration

plot(airlines$FLIGHT_DURATION,airlines$PRICE_ECONOMY, 
     col="blue",
     main="Price Economy vs Flight Duration",
     xlab="Flight Duration (hours)", ylab="Price (USD)")

# Add the sample means to the Scatterplot

abline(h=mean(airlines$PRICE_ECONOMY), col="dark blue", lty="dotted")
abline(v=mean(airlines$FLIGHT_DURATION), col="dark blue", lty="dotted")


# Add a regression line

abline(lm(airlines$PRICE_ECONOMY ~ airlines$FLIGHT_DURATION))

#Similarly, let's do this same exrcise for price premium and flight duration - 
# A Scatterplot of price premium vs flight duration

plot(airlines$FLIGHT_DURATION,airlines$PRICE_PREMIUM, 
     col="blue",
     main="Price Premium vs Flight duration",
     xlab="Flight duration (Hours)", ylab="Price (USD)")

# Add the sample means to the Scatterplot
# ==========
abline(h=mean(airlines$PRICE_PREMIUM), col="dark blue", lty="dotted")
abline(v=mean(airlines$FLIGHT_DURATION), col="dark blue", lty="dotted")

# Add a regression line
# ==========
abline(lm(airlines$PRICE_PREMIUM ~ airlines$FLIGHT_DURATION))

#Let's do a similar exercise for seats_premium and price_premium.
plot(airlines$SEATS_PREMIUM,airlines$PRICE_PREMIUM, 
col="blue",
main="Price Premium vs Seats Premium",
xlab="No. of premium seats", ylab="Price (USD)")

# Add the sample means to the Scatterplot
# ==========
abline(h=mean(airlines$PRICE_PREMIUM), col="dark blue", lty="dotted")
abline(v=mean(airlines$SEATS_PREMIUM), col="dark blue", lty="dotted")

# Add a regression line
# ==========
abline(lm(airlines$PRICE_PREMIUM ~ airlines$SEATS_PREMIUM))

#Now let's do this for economy seats and economy prices -

plot(airlines$SEATS_ECONOMY,airlines$PRICE_ECONOMY, 
col="blue",
main="Price Economy vs Seats Economy",
xlab="No. of economy seats", ylab="Price (USD)")

# Add the sample means to the Scatterplot
# ==========
abline(h=mean(airlines$PRICE_ECONOMY), col="dark blue", lty="dotted")
abline(v=mean(airlines$SEATS_ECONOMY), col="dark blue", lty="dotted")

# Add a regression line
# ==========
abline(lm(airlines$PRICE_ECONOMY ~ airlines$SEATS_ECONOMY))

#Similarly, we would do this for pitch premium as well as width premium.

plot(airlines$PITCH_PREMIUM,airlines$PRICE_PREMIUM, 
     col="blue",
     main="Price PREMIUM vs Pitch PREMIUM",
     xlab="Pitch for premium (inches)", ylab="Price (USD)")

# Add the sample means to the Scatterplot
# ==========
abline(h=mean(airlines$PRICE_PREMIUM), col="dark blue", lty="dotted")
abline(v=mean(airlines$PITCH_PREMIUM), col="dark blue", lty="dotted")

# Add a regression line
# ==========
abline(lm(airlines$PRICE_PREMIUM ~ airlines$PITCH_PREMIUM))

plot(airlines$WIDTH_PREMIUM,airlines$PRICE_PREMIUM, 
     col="blue",
     main="Price PREMIUM vs Width PREMIUM",
     xlab="Width for premium (inches)", ylab="Price (USD)")

# Add the sample means to the Scatterplot
# ==========
abline(h=mean(airlines$PRICE_PREMIUM), col="dark blue", lty="dotted")
abline(v=mean(airlines$WIDTH_PREMIUM), col="dark blue", lty="dotted")

# Add a regression line
# ==========
abline(lm(airlines$PRICE_PREMIUM ~ airlines$WIDTH_PREMIUM))

#Let's create a correlation matrix for price economy

#Correlation and Correlation Matrix for Price Economy

library(corrplot)
library(gplots)      # for color interpolation
par(mfrow=c(1, 1))
corrplot.mixed(corr=cor(airlines[ , c(2:6, 8,10,12,15,17)], use="complete.obs"), 
upper="ellipse", tl.pos="lt", 
col = colorpanel(50, "red", "gray60", "blue4"))

#Correlation and Correlation Matrix for Price Premium

par(mfrow=c(1, 1))
corrplot.mixed(corr=cor(airlines[ , c(2:6, 7,9,11,13,15,17)], use="complete.obs"), 
               upper="ellipse", tl.pos="lt", 
               col = colorpanel(50, "red", "gray60", "blue4"))

#Scatter Plot Matrix for Price Economy and Price Premium
library(car)
scatterplotMatrix(formula = ~ SEATS_ECONOMY + PITCH_ECONOMY + WIDTH_ECONOMY + PRICE_ECONOMY, cex=0.6,
                  data=airlines, diagonal="histogram")

scatterplotMatrix(formula = ~ SEATS_PREMIUM + PITCH_PREMIUM + WIDTH_PREMIUM + PRICE_PREMIUM, cex=0.6,
                  data=airlines, diagonal="histogram")

#Calculating correlations between Prices of Economy and Premium in correlation to other factors
cor.test(airlines$PRICE_ECONOMY, airlines$PITCH_ECONOMY)
cor.test(airlines$PRICE_ECONOMY, airlines$WIDTH_ECONOMY)
cor.test(airlines$PRICE_PREMIUM, airlines$PITCH_PREMIUM)
cor.test(airlines$PRICE_PREMIUM, airlines$WIDTH_PREMIUM)

#Articulating two hypothesis -
#H1 = Type of aircrafts does have an effect on premium pricing
#Running t-test to test our hypothesis (H0,H1) -
#Here, the null hypothesis (H0) is that type of Aircarft does not have an effect on premium prices.
#Let's see -

boxplot(airlines$PRICE_PREMIUM~airlines$AIRCRAFT,main = "Price premium based on aircrafts",col = (c("green","blue")), ylab = "Price (USD)")

t.test(airlines$PRICE_PREMIUM~airlines$AIRCRAFT,var.equal = TRUE)

#Since the p-value is > 0.05, we fail to reject the null hypothesis. Thus, we can say that type of aircraft does not affect the premium prices.

# Formulating a regression model -
#Hypothesis - The premium seat prices are affected by flight duration and no. of premium seats offered in a flight as well as width and pitch.
#y = premium prices
#x1 = flight duration
#x2 = width premium
#x3 = pitch premium
#x4 = seats_premium

model <- lm(PRICE_PREMIUM ~ FLIGHT_DURATION + WIDTH_PREMIUM + PITCH_PREMIUM + SEATS_PREMIUM, data=airlines)
summary(model)

model_1 <- lm(PRICE_PREMIUM ~ AIRLINE + FLIGHT_DURATION + WIDTH_PREMIUM + PITCH_PREMIUM + SEATS_PREMIUM, data=airlines)
summary(model_1)

model2<- lm(PRICE_PREMIUM~ FLIGHT_DURATION + SEATS_PREMIUM + QUALITY + PITCH_PREMIUM + WIDTH_PREMIUM, data = airlines)
summary(model2)

