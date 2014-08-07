# Exploratory Data Analysis - Assignment 1 - plot1 function
# by Cristian Popescu (August 6th, 2014)
#
#

library(datasets)
createPlot1Data <- function(fileName, date){
  myData <- read.table(fileName, sep = ";", dec=".")
  myFilteredData <- subset(myData,myData[,1] %in% date)
  myFilteredData
}

createPlot1Graph <- function(fileName, date){  
 myData <- createPlot1Data(fileName,date)
 myVector <- as.numeric(myData[,3])
 hist(myVector,col="red", xlab="Global Active Power (kilowatts)", main="Global Active Power")
 dev.copy(png,'plot1.png')
 dev.off()
}

createPlot1Graph("household_power_consumption.txt",c("1/2/2007","2/2/2007"))