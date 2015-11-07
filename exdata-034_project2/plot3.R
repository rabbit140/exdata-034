plot3 <- function(){
      
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
      
      #Transform NEI data
      NEI_group <- group_by(NEI, type, year)
      baltimore_group <- filter(NEI_group, fips == "24510")
      year_type_sum <- summarise(baltimore_group, emissions = sum(Emissions))
      
      
      #Plot the data and save to "plot1.png" file
      png("plot3.png", width = 850)
      print({
            qplot(data = year_type_sum, x = year, y = emissions, facets = . ~ type, geom = c("point")) +
                  geom_smooth(method = "lm") + 
                  xlab("Year") + ylab("Total PM2.5 emissions (tonnes)") + 
                  ggtitle("Total PM2.5 emissions for Baltimore City by type of source of emission from 1999 to 2008")
            
      })
      

      dev.off()
      
}