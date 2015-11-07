plot4 <- function(){
      
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
      NEI$SCC <- as.character(NEI$SCC)
      SCC_comb_subset <- SCC[grep("Comb", SCC$Short.Name),]
      SCC_comb_coal <- SCC_comb_subset[grep("Coal", SCC_comb_subset$Short.Name),]
      SCC_comb_coal_list <- as.character(SCC_comb_coal$SCC)
      NEI_SCC_subset <- filter(NEI, SCC %in% SCC_comb_coal_list)
      NEI_subset_group <- group_by(NEI_SCC_subset, year)
      coal_comb_emission <- summarise(NEI_subset_group, emissions = sum(Emissions))
      
      
      
      #Plot the data and save to "plot1.png" file
      png("plot4.png", width = 600)
      print({
            
                  qplot(year, emissions, data = coal_comb_emission, 
                        xlab = "Year", 
                        ylab = "Total PM2.5 emission (tonnes)", 
                        main = "Total PM2.5 emissions from coal combustion-related sources from 1999 to 2008") +
                        geom_smooth(method = "lm", lwd = 1)    
            
      })
      
      
      dev.off()
      
}