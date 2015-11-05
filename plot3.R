plot3 <- function() {
      
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
      
      Global_active_power <- as.numeric(Global_active_power)
      
      #Make a histogram and save it to "plot3.png" file
      png("plot3.png", width = 480, height = 480)
      plot(DateTime, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
      lines(DateTime, Sub_metering_2, col = "red")
      lines(DateTime, Sub_metering_3, col = "blue")
      legend("topright", legend = colnames(data)[7:9], lwd = 1, 
             col = c("black", "red", "blue"))
      dev.off()
      
      
}