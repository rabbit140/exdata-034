plot6 <- function(){
      
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
      la_group <- filter(NEI_group, fips == "06037")
      motor_rows <- SCC[grep("Motor", SCC$Short.Name),]
      #Remove first row, since the source is not a motor vehicle
      motor_rows <- motor_rows[-1,]
      motor_SCC <- as.character(motor_rows$SCC)
      baltimore_motor <- filter(baltimore_group, SCC %in% motor_SCC) 
      la_motor <- filter(la_group, SCC %in% motor_SCC)
      summary_la <- summarise(la_motor, emissions = sum(Emissions))
      summary_baltimore <- summarise(baltimore_motor, emissions = sum(Emissions))
      summary_both <- rbind(summary_baltimore, summary_la)
      city <- rep(c("Baltimore City", "Los Angeles County"), each = 4)
      summary_both <- cbind(summary_both, city)
      
      #Plot the data and save to "plot1.png" file
      png("plot6.png", width = 850)
      g <- ggplot(summary_both, aes(year, emissions))
      print({
            g + geom_point() + 
                  geom_smooth(method = "lm") + 
                  xlab("Year") + 
                  ylab("Total PM2.5 emissions (tonnes)") + 
                  ggtitle("Total PM2.5 emissions from motor vehicle sources for Baltimore City and Los Angeles County from 1999 to 2008") + 
                  facet_grid(.~city)
      })
      dev.off()
      
}