plot2 <- function() {
      
      power_data <- read.table("household_power_consumption.txt", header = TRUE, 
                               comment.char = "", nrows = 70000, sep = ";", na.strings = "?")
      
      #Subset data
      power_data_subset <- power_data[66637:dim(power_data)[1],]
      row.names(power_data_subset) <- seq(along.with = power_data_subset$Date)
      power_data_subset <- power_data_subset[1:2880,]
      
      attach(power_data_subset)
      
      #Format dates
      Date <- as.Date(Date, format("%d/%m/%Y"))
      DateTime <- as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S")
      
      
      #Change "?" to "NA"
      Global_active_power <- gsub("\\?", "NA", Global_active_power)
      Global_active_power <- as.numeric(Global_active_power)
      
      #Make a histogram and save it to "plot2.png" file
      png("plot2.png", width = 480, height = 480)
      plot(DateTime, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
      dev.off()
      
      
}