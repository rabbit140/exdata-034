plot2 <- function(){
      
      #Install "dplyr" package if not present
      if (!("dplyr" %in% installed.packages())){
            install.packages("dplyr")
      }
      
      #Load "dplyr"
      library(dplyr)
      
      #Check whether necessary data exists and if not - download and unzip it
      if (!all(file.exists(c("Source_Classification_Code.rds", "summarySCC_PM25.rds")))){
            download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                          destfile = "peer_assessment_data.zip")
            unzip("peer_assessment_data.zip")
      }
      
      #Load the data
      NEI <- readRDS("summarySCC_PM25.rds")
      SCC <- readRDS("Source_Classification_Code.rds")
      
      #Transform NEI data
      NEI_group <- group_by(NEI, year)
      baltimore_emission <- filter(NEI_group, fips == "24510")
      baltimore_summary <- summarise(baltimore_emission, emissions = sum(Emissions))
      
      #Plot the data and save to "plot1.png" file
      png("plot2.png")
      plot(baltimore_summary$year, baltimore_summary$emissions, 
           main = "Total PM2.5 emissions from 1999 to 2008 for Baltimore City", 
           xlab = "Year", 
           ylab = "Total PM2.5 emissions (tonnes)")
      abline(lm(baltimore_summary$emissions ~ baltimore_summary$year), lwd = 2, col = "red")
      dev.off()
      
}