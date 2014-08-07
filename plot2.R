# Exploratory Data Analysis - Assignment 1 - plot2 function
# by Cristian Popescu (August 6th, 2014)
#
#

library(datasets)
createPlot2Data <- function(fileName, date){
  myData <- read.table(fileName, sep = ";", dec=".")
  myFilteredData <- subset(myData,myData[,1] %in% date)
  myFilteredData
}

createPlot2Graph <- function(fileName, date){
 myData <- createPlot2Data(fileName,date)
 myMatrix <- as.data.frame.matrix(myData)
 myMatrix$V1 <- strptime(paste(myMatrix$V1,myMatrix$V2), "%d/%m/%Y %H:%M:%S")
 myXVector <- myMatrix$V1
 myYVector <- as.numeric(myMatrix$V3)
 plot(myXVector,myYVector,xlab=" ", ylab="Global Active Power (kilowatts)", type="l", lty=1)
 dev.copy(png,'plot2.png')
 dev.off()
}


createPlot2Graph("household_power_consumption.txt",c("1/2/2007","2/2/2007"))