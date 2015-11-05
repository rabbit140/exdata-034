plot4 <- function() {
      
      if(!file.exists("household_power_consumption.txt")){
            url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
            download.file(url, "household_power_consumption.zip")
            unzip("household_power_consumption.zip")
      }
      
      power_data <- read.table("household_power_consumption.txt", header = TRUE, 
                               comment.char = "", nrows = 70000, sep = ";", na.strings = "?")
      
      #Subset data
      data <- power_data[66637:dim(power_data)[1],]
      row.names(data) <- seq(along.with = data$Date)
      data <- data[1:2880,]
      
      attach(data)
      
      #Format dates
      Date <- as.Date(Date, format("%d/%m/%Y"))
      DateTime <- as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S")
      Sub_metering_1 <- as.numeric(as.character(Sub_metering_1))
      Sub_metering_2 <- as.numeric(as.character(Sub_metering_2))
      Sub_metering_3 <- as.numeric(as.character(Sub_metering_3))
      
      #Convert "Voltage" and "Global_reactive_power" to numeric data type
      Voltage <- as.numeric(as.character(Voltage))
      Global_reactive_power <- as.numeric(as.character(Global_reactive_power))
      
      #Convert "Global_active_power" to numeric
      Global_active_power <- as.numeric(Global_active_power)
      
      #Make a histogram and save it to "plot4.png" file
      png("plot4.png", width = 480, height = 480)
      par(mfrow = c(2,2))
      plot(DateTime, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
      plot(DateTime, Voltage, type = "l", xlab = "datetime")
      plot(DateTime, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub")
      lines(DateTime, Sub_metering_2, col = "red")
      lines(DateTime, Sub_metering_3, col = "blue")
      legend("topright", legend = colnames(data)[7:9], lwd = 1, 
             col = c("black", "red", "blue"), bty = "n")
      plot(DateTime, Global_reactive_power, type = "l", xlab = "datetime")
      dev.off()
      
      
}