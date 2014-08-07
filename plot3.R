# Exploratory Data Analysis - Assignment 1 - plot3 function
# by Cristian Popescu (August 6th, 2014)
#
#

library(datasets)

createPlot3Data <- function(fileName, date){
  myData <- read.table(fileName, sep = ";", dec=".")
  myFilteredData <- subset(myData,myData[,1] %in% date)
  myFilteredData
}

createPlot3Graph <- function(fileName, date){
  myData <- createPlot3Data(fileName, date)
  myMatrix <- as.data.frame.matrix(myData)
  
  #V1=Date and V2=Time
  myMatrix$V1 <- strptime(paste(myMatrix$V1,myMatrix$V2), "%d/%m/%Y %H:%M:%S")
  myXVector <- myMatrix$V1
  
  #V7=Sub_metering_1
  myYVectorSubMetering1 <- as.numeric(myMatrix$V7)
  
  #V7=Sub_metering_2
  myYVectorSubMetering2 <- as.numeric(myMatrix$V8)
  
  #V7=Sub_metering_3
  myYVectorSubMetering3 <- as.numeric(myMatrix$V9)
  
  par(mfrow=c(1,1))
  
  plot(myXVector,myYVectorSubMetering1,xlab=" ", ylab="Energy sub metering", type="l", lty=1, col="black")
  lines(myXVector,myYVectorSubMetering2,xlab=" ", ylab="Energy sub metering", type="l", lty=1, col="red")
  lines(myXVector,myYVectorSubMetering3,xlab=" ", ylab="Energy sub metering", type="l", lty=1, col="blue")
  
  #add the legend
  legend("topright", inset=.05, 
         c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), horiz=FALSE, lty=1, col=c("black","red","blue"), cex=0.8)
  
  #save to the file
  dev.copy(png,'plot3.png')
  dev.off()  
}


createPlot3Graph("household_power_consumption.txt",c("1/2/2007","2/2/2007"))