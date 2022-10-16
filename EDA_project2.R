# unzip("databases/EDA_database.zip")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("databases/summarySCC_PM25.rds")
SCC <- readRDS("databases/Source_Classification_Code.rds")

library(tibble)
library(dplyr)

SCC <- as_tibble(subset(SCC, select = c(1,4,7,8,9,10)))
NEI <- as_tibble(merge(NEI, SCC, by="SCC"))


################################## First plot #######################################
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? #
# Using the base plotting system, make a plot showing the total PM2.5 emission      #
# from all sources for each of the years 1999, 2002, 2005, and 2008.                #
#####################################################################################

NEI <- readRDS("databases/summarySCC_PM25.rds")
SCC <- readRDS("databases/Source_Classification_Code.rds")

library(tibble)
library(dplyr)

SCC <- as_tibble(subset(SCC, select = c(1,4,7,8,9,10)))
NEI <- as_tibble(merge(NEI, SCC, by="SCC"))

NEI <- group_by(NEI, year)
emissions_per_year <- summarise(NEI, total_emissions=sum(Emissions)/1e6)

png("plot1.png", height=480, width=600)
par(mar=c(5.1,5.1,4.1,2.1))
plot(emissions_per_year, 
     xlab="Year",
     ylab="Total emissions (millions of tons)",
     main="PM2.5 emissions per year in the USA", 
     pch=20,
     cex=3,
     cex.axis=1.5,
     cex.lab=1.5,
     cex.main=2)
abline(lm(emissions_per_year$total_emissions ~ emissions_per_year$year), lty=2, col="#777777", lwd=4)
dev.off()

################################# Second plot #######################################
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland         #
# from 1999 to 2008?                                                                #
# Use the base plotting system to make a plot answering this question.              #
# Baltimore City, Maryland: (fips == "24510")                                       #
#####################################################################################

NEI <- readRDS("databases/summarySCC_PM25.rds")
SCC <- readRDS("databases/Source_Classification_Code.rds")

library(tibble)
library(dplyr)

SCC <- as_tibble(subset(SCC, select = c(1,4,7,8,9,10)))
NEI <- as_tibble(merge(NEI, SCC, by="SCC"))

NEI_Baltimore <- NEI[NEI$fips == "24510",]
emissions_per_year_Baltimore <- summarise(NEI_Baltimore, total_emissions=sum(Emissions)/1e3)

png("plot2.png", height=480, width=600)
par(mar=c(5.1,5.1,4.1,2.1))
plot(emissions_per_year_Baltimore, 
     xlab="Year",
     ylab="Total emissions (thousands of tons)",
     main="PM2.5 emissions per year in Baltimore City, Maryland", 
     pch=20,
     cex=3,
     cex.axis=1.5,
     cex.lab=1.5,
     cex.main=1.6)
abline(lm(emissions_per_year_Baltimore$total_emissions ~ emissions_per_year_Baltimore$year), lty=2, col="#777777", lwd=4)
dev.off()

################################## Third plot ###########################################
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) #
# variable, which of these four sources have seen decreases in emissions from 1999–2008 #
# for Baltimore City? Which have seen increases in emissions from 1999–2008?            #
# Use the ggplot2 plotting system to make a plot answer this question.                  #
#########################################################################################

NEI <- readRDS("databases/summarySCC_PM25.rds")
SCC <- readRDS("databases/Source_Classification_Code.rds")

library(tibble)
library(dplyr)

SCC <- as_tibble(subset(SCC, select = c(1,4,7,8,9,10)))
NEI <- as_tibble(merge(NEI, SCC, by="SCC"))

library(ggplot2)
emissions_per_year_type_Baltimore <- group_by(NEI_Baltimore, year, type) %>% summarize(total_emissions=sum(Emissions)/1e3)

png("plot3.png", height=480, width=800)
g <- ggplot(data=emissions_per_year_type_Baltimore, aes(year, total_emissions))
g <- g + geom_point(size=4) +
         geom_smooth(method = "lm", se=FALSE, color = "#777777", size=1.5, linetype=2) +
         theme_bw(base_size = 20) +
         theme(axis.text.x = element_text(angle=-70, hjust=-0.5)) +
         facet_grid(. ~ type) +
         labs(title = "PM2.5 emissions in Baltimore City, Maryland",
              subtitle = "Arranged by type of emission source",
              x = "Year",
              y = "Total emissions (thousands of tons)")
print(g)
dev.off()

################# Fourth plot ############################
# Across the United States, how have emissions from coal #
# combustion-related sources changed from 1999–2008?     #
##########################################################

NEI <- readRDS("databases/summarySCC_PM25.rds")
SCC <- readRDS("databases/Source_Classification_Code.rds")

library(tibble)
library(dplyr)

SCC <- as_tibble(subset(SCC, select = c(1,4,7,8,9,10)))
NEI <- as_tibble(merge(NEI, SCC, by="SCC"))

NEI_coal <- NEI[grep("Coal", NEI$EI.Sector),]
emissions_per_year_coal <- summarize(NEI_coal, total_emissions=sum(Emissions)/1e3)

png("plot4.png", height=480, width=600)
g <- ggplot(data=emissions_per_year_coal, aes(year, total_emissions))
g <- g + geom_point(size=4) +
  geom_smooth(method = "lm", se=FALSE, color = "#777777", size=1.5, linetype=2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle=-70, hjust=-0.5)) +
  labs(title = "PM2.5 emissions in the USA",
       subtitle = "Coal combustion-related sources",
       x = "Year",
       y = "Total emissions (thousands of tons)")
print(g)
dev.off()

################## Fifth plot ############################
# How have emissions from motor vehicle sources changed  #
# from 1999–2008 in Baltimore City?                      #
##########################################################

NEI <- readRDS("databases/summarySCC_PM25.rds")
SCC <- readRDS("databases/Source_Classification_Code.rds")

library(tibble)
library(dplyr)

SCC <- as_tibble(subset(SCC, select = c(1,4,7,8,9,10)))
NEI <- as_tibble(merge(NEI, SCC, by="SCC"))

NEI_Baltimore_vehicle <- NEI_Baltimore[grep("Vehicles", NEI_Baltimore$EI.Sector),]
emissions_per_year_Baltimore_vehicle <- summarize(NEI_Baltimore_vehicle, total_emissions=sum(Emissions))

png("plot5.png", height=480, width=600)
g <- ggplot(data=emissions_per_year_Baltimore_vehicle, aes(year, total_emissions))
g <- g + geom_point(size=4) +
  geom_smooth(method = "lm", se=FALSE, color = "#777777", size=1.5, linetype=2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle=-70, hjust=-0.5)) +
  labs(title = "PM2.5 emissions in Baltimore City, Maryland",
       subtitle = "Motor vehicle sources",
       x = "Year",
       y = "Total emissions (tons)")
print(g)
dev.off()

################## Sixth plot ############################
# How have emissions from motor vehicle sources changed  #
# from 1999–2008 in Baltimore City?                      #
##########################################################

NEI <- readRDS("databases/summarySCC_PM25.rds")
SCC <- readRDS("databases/Source_Classification_Code.rds")

library(tibble)
library(dplyr)

SCC <- as_tibble(subset(SCC, select = c(1,4,7,8,9,10)))
NEI <- as_tibble(merge(NEI, SCC, by="SCC"))

NEI_Baltimore_LA <- NEI[NEI$fips == "24510" | NEI$fips == "06037",]
emissions_per_year_Baltimore_LA_vehicle <- NEI_Baltimore_LA[grep("Vehicles", NEI_Baltimore_LA$EI.Sector),] %>% 
                group_by(year, fips) %>%
                summarize(total_emissions=log10(sum(Emissions))) %>%
                mutate(city=mapply(function(x){
                                if(x=="24510"){
                                  return("Baltimore City, Maryland")
                                } else {
                                  return("Los Angeles County, California")
                                }}, fips),
                       .keep="unused")

png("plot6.png", height=480, width=800)
g <- ggplot(data=emissions_per_year_Baltimore_LA_vehicle, aes(year, total_emissions))
g <- g + geom_point(size=4) +
  geom_smooth(method = "lm", se=FALSE, color = "#777777", size=1.5, linetype=2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle=-70, hjust=-0.5)) +
  facet_grid(. ~ city) +
  labs(title = "Comparison of PM2.5 emissions from motor vehicle sources",
       subtitle = "Baltimore City, Maryland vs Los Angeles County, California",
       x = "Year",
       y = "Total emissions (orders of magnitude)")
print(g)
dev.off()

