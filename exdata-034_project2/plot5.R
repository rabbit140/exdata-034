plot5 <- function(){
      
      #Install "dplyr" package if not present
      if (!all(c("dplyr", "ggplot2") %in% installed.packages())){
            install.packages("dplyr")
      }
      
      #Load "dplyr" and "ggplot2"
      library(dplyr)
      library(ggplot2)
      
      #Check whether necessary data exists and if not - download and unzip it
      if (!all(file.exists(c("Source_Classification_Code.rds", "summarySCC_PM25.rds")))){
            download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                          destfile = "peer_assessment_data.zip")
            unzip("peer_assessment_data.zip")
      }
      
      #Load the data
      NEI <- readRDS("summarySCC_PM25.rds")
      SCC <- readRDS("Source_Classification_Code.rds")
      
      #Subset and transform appropriate data
      NEI_group <- group_by(NEI, year)
      baltimore_group <- filter(NEI_group, fips == "24510")
      motor_rows <- SCC[grep("Motor", SCC$Short.Name),]
      #Remove first row, since the source is not a motor vehicle
      motor_rows <- motor_rows[-1,]
      motor_SCC <- as.character(motor_rows$SCC)
      baltimore_motor <- filter(baltimore_group, SCC %in% motor_SCC) 
      summary <- summarise(baltimore_motor, emissions = sum(Emissions))
      
      #Plot the data and save to "plot1.png" file
      png("plot5.png", width = 600)
      g <- ggplot(summary, aes(year, emissions))
      print({
            g + geom_point() + 
                  geom_smooth(method = "lm") + 
                  xlab("Year") + 
                  ylab("Total PM2.5 emissions (tonnes)") + 
                  ggtitle("Total PM2.5 emissions from motor vehicle sources for Baltimore City from 1999 to 2008")
      })
      dev.off()
      
}