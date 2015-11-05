plot1 <- function() {
      
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
      
      
      #Change "?" to "NA"
      Global_active_power <- gsub("\\?", "NA", Global_active_power)
      Global_active_power <- as.numeric(Global_active_power)
      
      #Make a histogram and save it to "plot1.png" file
      png("plot1.png", width = 480, height = 480)
      hist(Global_active_power, col = "red", main = "Global Active Power", 
           xlab = "Global Active Power (kilowatts)")
      dev.off()
      
      
}