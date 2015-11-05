
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


plot2 <-function(dc) {
	png("plot2.png", width = 480, height = 480, units = "px")
	plot(dc$t,dc$Global_active_power,type="l", ylab="Global Active Power (kilowatts)",xlab="")
	dev.off()
}


print("reading data and creating plot2 ...") 
plot2(readdata())

