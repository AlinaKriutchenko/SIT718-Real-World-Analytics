#Assign the data to a matrix
the.data<-as.matrix(read.table("Energy20.txt"))

#To investigate Y, generate a subset of 350 data
my.data<-the.data[sample(1:671,350),c(1:6)]
View(my.data)

#Using scatter plots and histograms, 
#report on the general relationship between each of the variables 
#X1, X2, X3, X4, X5 and the variable of interest Y

#Scatter plot 1
plot(my.data[,1],my.data[,6], xlab = "Temperature in kitchen area, in Celsius", ylab = "Energy, Wh",  main = "Connection between Energy and Temperature", col = "#FDD542")

#Scatter plot 2
plot(my.data[,2],my.data[,6], xlab = "Humidity in kitchen area, %", ylab = "Energy, Wh", main = "Connection between Energy and Humidity in kitchen area")

#Scatter plot 3
plot(my.data[,3],my.data[,6], xlab = "Temperature outside (from weather station), in Celsius", ylab = "Energy, Wh", main = "Connection between Energy and Temperature outside (from weather station)", col = "#433666")

#Scatter plot 4
plot(my.data[,4],my.data[,6], col = "#45b6d9", xlab = "Humidity outside (from weather station), given as a percentage", ylab = "Energy, Wh", main = "Connection between Energy and Humidity outside (from weather station)")

#Scatter plot 5
plot(my.data[,5],my.data[,6], col = "#b467c9", xlab = "Visibility (from weather station), in km", ylab = "Energy, Wh", main = "Connection between Energy and Visibility (from weather station)")




#Histogram 1
hist(my.data[,1], col = "#CCEF5B", main = "Histogram for Temperature in kitchen area", xlab = "Temperature in kitchen area, in Celsius", ylab = "Frequency of Temperature in kitchen area")

#Histogram 2
hist(my.data[,2], xlab = "Humidity in kitchen area, %", ylab = "Frequency of Humidity in kitchen area", col = "#fcc66f", main = "Histogram for Humidity in kitchen area")

#Histogram 3
hist(my.data[,3], xlab = "Temperature outside (from weather station), in Celsius", ylab = "Frequency of Temperature outside (from weather station)", col = "#fcf368", main = "Histogram for Temperature outside (from weather station)")

#Histogram 4
hist(my.data[,4],breaks = 10, xlab = "Humidity outside (from weather station), given as a percentage", ylab = "Frequency of Humidity outside (from weather station)", col = "#9fe5ed", main = "Histogram for Humidity outside (from weather station)")

#Histogram 5
hist(my.data[,5], xlab = "Visibility (from weather station), in km", ylab = "Frequency of Visibility (from weather station)", col = "#dfbaf7", main = "Histogram for Visibility (from weather station)")

#Histogram 6
hist(my.data[,6],  xlab = "Energy use of appliances, Wh", ylab = "Frequency of Energy use of appliances", col = "#d1b88a", main = "Histogram for Energy use of appliances")




#Task2
#Data transformation

#Polynomial Transformation and Scaling
my.data[,1]<-(my.data[,1])^(1/6)
my.data[,1]<-1-(my.data[,1]-min(my.data[,1]))/(max(my.data[,1])-min(my.data[,1]))
hist(my.data[,1], col = "#CCEF5B", main = "Histogram for Temperature in kitchen area", xlab = "Temperature in kitchen area, in Celsius", ylab = "Frequency of Temperature in kitchen area")

#Polynomial Transformation and Scaling
my.data[,3]<-(my.data[,3])^(1/1.9)
my.data[,3]<-1-(my.data[,3]-min(my.data[,3]))/(max(my.data[,3])-min(my.data[,3]))
hist(my.data[,3],xlab = "Temperature outside (from weather station), in Celsius", ylab = "Frequency of Temperature outside (from weather station)", col = "#fcf368", main = "Histogram for Temperature outside (from weather station)")

#Linear Feature Scaling
my.data[,2]<-(my.data[,2]-min(my.data[,2]))/(max(my.data[,2])-min(my.data[,2]))
hist(my.data[,2], xlab = "Humidity in kitchen area, %", ylab = "Frequency of Humidity in kitchen area", col = "#fcc66f", main = "Histogram for Humidity in kitchen area")   

#Log Transformation and Scaling
my.data[,5]<-log(my.data[,5])
my.data[,5]<-1-(my.data[,5]-min(my.data[,5]))/(max(my.data[,5])-min(my.data[,5]))
hist(my.data[,5], xlab = "Visibility (from weather station), in km", ylab = "Frequency of Visibility (from weather station)", col = "#dfbaf7", main = "Histogram for Visibility (from weather station)")

#Polynomial Transformation and Scaling
my.data[,6]<-(my.data[,6])^(1/7)
my.data[,6]<-1-(my.data[,6]-min(my.data[,6]))/(max(my.data[,6])-min(my.data[,6]))
hist(my.data[,6], xlab = "Energy use of appliances, Wh", ylab = "Frequency of Energy use of appliances", col = "#d1b88a", main = "Histogram for Energy use of appliances")

your.data<-cbind(my.data[,1], my.data[,2], my.data[,3],  my.data[,5], my.data[,6])

write.table(your.data,"name-transformed.txt")




#Download the AggWaFit718.R file to your working directory and
#load into the R workspace

source("AggWaFit718.R")


#Use the fitting functions to learn the parameters for:

#Weighted arithmetic mean
fit.QAM(your.data[,c(1:4,5)],"WAMoutput.txt", "WAMstats.txt")

#Weighted power mean
#WPM, p=0.5
#Weighted power mean
#WPM, p=5
fit.QAM(your.data[,c(1:4,5)],"PM05output.txt", "PM05stats.txt", g=PM05, g.inv= invPM05)
fit.QAM(your.data[,c(1:4,5)],"PM5output.txt", "PM5stats.txt", g=PM5, g.inv= invPM5)

#Ordered weighted averaging function
fit.OWA(your.data[,c(1:4,5)],"OWAoutput.txt", "OWAstats.txt")

#Choquet integral
fit.choquet(your.data[,c(1:4,5)], "Choquetoutput2.txt", "Choquetstats2.txt")

#PREDICTIONS

#Choquet integral
choquet(c(0.264, 0.761),c(0.265, 0.031, 0,439, 0.265))
choquet




#Linear regression model

your.regression <- lm(your.data[,5] ~ your.data[,3])
summary(your.regression)

plot(your.data[,3],my.data[,5])
abline(your.regression, col='purple')
