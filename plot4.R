# Exploratory Data Analysis - Assignment 1 - plot4 function
# by Cristian Popescu (August 6th, 2014)
#
#

library(datasets)

createPlot4Data <- function(fileName, date){
  myData <- read.table(fileName, sep = ";", dec=".")
  myFilteredData <- subset(myData,myData[,1] %in% date)
  myFilteredData
}

createPlot4Graph <- function(fileName, date){
  myData <- createPlot4Data(fileName, date)
  myMatrix <- as.data.frame.matrix(myData)
  
  #V1=Date and V2=Time
  myMatrix$V1 <- strptime(paste(myMatrix$V1,myMatrix$V2), "%d/%m/%Y %H:%M:%S")
  myXVector <- myMatrix$V1

  #V3=Global_Active_Power
  myYVectorGlobalActivePower <- as.numeric(myMatrix$V3)
  
  #V4=Global_Reactive_Power
  myYVectorGlobalReactivePower <- as.numeric(myMatrix$V4)  
  
  #V5=Voltage
  myYVectorVoltage <- as.numeric(myMatrix$V5)  
  
  #V7=Sub_metering_1
  myYVectorSubMetering1 <- as.numeric(myMatrix$V7)
  
  #V7=Sub_metering_2
  myYVectorSubMetering2 <- as.numeric(myMatrix$V8)
  
  #V7=Sub_metering_3
  myYVectorSubMetering3 <- as.numeric(myMatrix$V9)

  
  par(mfrow=c(2,2))
  
  #1st graph
  plot(myXVector,myYVectorGlobalActivePower,xlab=" ", ylab="Global Active Power", type="l", lty=1, col="black")
  
  #2nd graph
  plot(myXVector,myYVectorVoltage,xlab="datetime", ylab="Voltage", type="l", lty=1, col="black")
  
  #3rd graph
  plot(myXVector,myYVectorSubMetering1,xlab=" ", ylab="Energy sub metering", type="l", lty=1, col="black")
  lines(myXVector,myYVectorSubMetering2,xlab=" ", ylab="Energy sub metering", type="l", lty=1, col="red")
  lines(myXVector,myYVectorSubMetering3,xlab=" ", ylab="Energy sub metering", type="l", lty=1, col="blue")
  #add the legend
  legend("topright", inset=.01, 
         c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), horiz=FALSE, lty=1, col=c("black","red","blue"),cex=0.4, bty="n")
  
  #4th graph
  plot(myXVector,myYVectorGlobalReactivePower,xlab="datetime", ylab="Global_Reactive_Power", type="l", lty=1, col="black")
 
  #save to the file
  dev.copy(png,'plot4.png')
  dev.off()  
}


createPlot4Graph("household_power_consumption.txt",c("1/2/2007","2/2/2007"))