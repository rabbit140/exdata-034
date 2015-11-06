plot1 <- function(){
      
      #Install "dplyr" package if not present
      if (!("dplyr" %in% installed.packages())){
            install.packages("dplyr")
      }
      
      #Load "dplyr"
      library(dplyr)
      
      #Check whether necessary data exists and if not - download and unzip it
      if (!all(file.exists(c("Source_Classification_Code.rds", "summarySCC_PM25.rds")))){
            download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                          destfile = "peer_assessment_data")
            unzip("peer_assessment_data")
      }
      
      #Load the data
      NEI <- readRDS("summarySCC_PM25.rds")
      SCC <- readRDS("Source_Classification_Code.rds")
      
      #Group NEI data by year
      NEI_group <- group_by(NEI, year)
      year_emission <- summarise(NEI_group, sum(Emissions))
      colnames(year_emission) <- c("year", "emissions")
      
      #Plot the data and save to "plot1.png" file
      png("plot1.png")
      plot(year_emission$year, year_emission$emissions, 
           main = "Total PM2.5 emissions from 1999 to 2008", 
           xlab = "Year", 
           ylab = "Total PM2.5 emissions (tonnes)")
      abline(lm(year_emission$emissions ~ year_emission$year), col = "red", lwd = 2)
      dev.off()
      
}