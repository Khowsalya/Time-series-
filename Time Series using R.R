################################ TIME SERIES ########################################



################## Case study on Australia tourist data ############################

#Install some commonly required package and load the library

#install.packages("forecast")
#install.packages("tseries")

library(forecast)

library(tseries)


#Loading data austourists from the package fpp

#install.packages("fpp")
library(fpp)
data()
data("austourists")


#Quick View of data using some functions in R to know some basic details

View(austourists)

#Checking datatype
class(austourists)


plot(austourists)
#Increasing trens and seasonality is there

#Quick view of start and end year data,frequency to know some basic details
start(austourists)

end(austourists)

dim(austourists)

frequency(austourists)


#Explore time series

#Aggregating the time series

aggregate(austourists)
plot(aggregate(austourists))
aggregate(austourists, FUN = mean)

plot(aggregate(austourists, FUN = mean))

#Boxplot creates one boxplot for each quarter  of all years

b1 <- boxplot(austourists~cycle(austourists))

#Expenditure o vistior is high in first quarter than other 3 quarter

#subsetting the time series window function
aus <- window(austourists, start = c(1999,1), end =c(2006,4), frequency = 4)
dim(aus)

aus1 <- window(austourists, start = c(2007,1), end = c(2010, 4), frequency = 4)
dim(aus1)


plot(aus)
plot(aus1)


#we shall create a time series model on (1999 to 2006)
#and forecast for the time period(2007 to 2010)
##we shall then compare the forecasts with the actual time series


#Data has both trend and seasonality
#Decomposing the time series  into seasonal,trend and irregular
#components with additive seasonality

dec1 <- decompose(aus)
plot(dec1)


#Top panel contains the original time series
#second panel contain the trends
#Third panel contain the seasonality component 
#Last panel contain random flucuation


#To forecast the quarterly expenditure of vistior on eating out 
#using  ets model alogrithm 

auto <- ets(aus1)
summary(auto)

#Finding MAPE value
#step 1:
#Getting original value (means actual value)
auto$x  #orignal values


#step 2
#Getting forecast value 
auto$fitted

#step 3
#getting resdiual value (Difference between actual and forecast value)
auto$residuals

#step 4
#MAPE(Absolute resdiual value /actual value)*100
mean(abs(auto$residuals/auto$x)*100)#MAPE

#The MAPE is less than 7 and its seems like a good MAPE

foc <- forecast(auto, h= 8)
foc
plot(foc)


#Validating the model
checkresiduals(foc)



