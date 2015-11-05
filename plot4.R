
# Exploratory Data Analysis course
# Coursera
# Project 1

# Due: 8 November 2015
# Author: Burt Rosenberg

getdata <- function(df, day1, day2 ){
	days <- strptime(df$Date,"%d/%m/%Y")
	df[((days>=day1) & (days<=day2)),]
}

cleandata <- function(df) {
	
	# columns are:
	#
	#  3 $Global_active_power 
	#  4 $Global_reactive_power
	#  5 $Voltage
	#  6 $Global_intensity
	#  7 $Sub_metering_1
	#  8 $Sub_metering_2
	#  9 $Sub_metering_3
	
	df[,3:9] <- lapply(df[,3:9],as.numeric)
	df$t <- strptime(paste(df$Date,df$Time,sep=" "), "%d/%m/%Y %H:%M:%S")
	df
}

readdata <- function() {
	d <- getdata(read.csv2("household_power_consumption.txt",as.is=c(TRUE),na.strings=c("?")),
			strptime("2007-02-01","%Y-%m-%d"),
			strptime("2007-02-02","%Y-%m-%d")
	)
	cleandata(d)			
}

plot4 <- function(dc) {
	png("plot4.png", width = 480, height = 480, units = "px")
	par(mfcol = c(2,2), cex=0.75)
	
	# first plot
	plot(dc$t,dc$Global_active_power,type="l", ylab="Global Active Power",xlab="")
	
	# second plot
	plot(dc$t,dc$Sub_metering_1,type="l", ylab="Energy sub metering",xlab="")
	points(dc$t,dc$Sub_metering_2,type="l",col="red")
	points(dc$t,dc$Sub_metering_3,type="l",col="blue")
	legend("topright", bty="n", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
		col=c("black","red","blue"),lwd=1)
		
	# third plot
	plot(dc$t,dc$Voltage,type="l",xlab="datetime",ylab="Voltage")
	
	# fourth plot
	plot(dc$t,dc$Global_reactive_power,type="l",xlab="datetime",ylab="Global_reactive_power")
	dev.off()
}

print("reading data and creating plot4 ...") 
plot4(readdata())

