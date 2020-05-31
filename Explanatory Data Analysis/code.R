setwd("C:/Users/Siddharth Goel/Desktop/srishti/R files/exdata_data_NEI_data")
list.files()

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


#q1
library(dplyr)
nei1= NEI %>%
  group_by(year)%>%
  summarize(total_emission= sum(Emissions))
png("plot1.png")
plot(nei1$year, nei1$total_emission, xlab="Year" ,ylab= "Total PM2.5 Emissions",
     main= "Total Emissions from PM2.5 in the United States from 1999 to 2008" )
lines(nei1$year, nei1$total_emission)
dev.off()


#q2
nei2= NEI%>%
  filter(fips == "24510")%>%
  group_by(year)%>%
  summarize(total_emission_baltimore= sum(Emissions))

png("plot2.png")
plot(nei2$year, nei2$total_emission_baltimore, xlab="Year" ,ylab= "Total PM2.5 Emissions",
     main= "Total Emissions from PM2.5 in Baltimore City from 1999 to 2008" )
lines(nei2$year, nei2$total_emission_baltimore)
dev.off()


#q3
if ("ggplot2" %in% installed.packages()){
  library(ggplot2)
} else {
  install.packages("ggplot2")
  library(ggplot2)
}

nei3= NEI%>%
  filter(fips == "24510")%>%
  group_by(year, type)%>%
  summarize(total_emission_baltimore= sum(Emissions))
png("plot3.png")
qplot(year, total_emission_baltimore, data=nei3, geom=c("point", "line"), color= type, 
      xlab="Year" ,ylab= "Total PM2.5 Emissions",
      main= "PM2.5 Emissions in Baltimore City from 1999 to 2008 across type" )
dev.off()

#q4
coal= SCC$SCC[ grep("Coal", SCC$EI.Sector)]

coalscc= NEI%>%
  filter(SCC ==coal)

nei4= coalscc %>%
  group_by(year) %>%
  summarize(total_emission_coal= sum(Emissions) )

png("plot4.png")
qplot(year, total_emission_coal, data= nei4, geom= c("point", "line"), xlab="Year" ,
      ylab= "Total PM2.5 Emissions",
      main ="PM2.5 Emissions from Coal combustion-related Sources in US" )
dev.off()


#q5 
#searched removing v to ignore upper or lower case thing
motor= SCC[grep("ehicle", SCC$EI.Sector ),]
m = motor$SCC
nei5= NEI%>%
  filter(fips== "24510")%>%
  filter ( SCC %in% m) %>%
  select(year, Emissions)%>%
  group_by(year)%>%
  summarize(motor_emission_baltimore= sum(Emissions))

png("plot5.png")
qplot(year,motor_emission_baltimore, data= nei5, geom= c("point", "line"), xlab="Year" ,
      ylab= "Total PM2.5 Emissions",
      main="PM2.5 Emissions from Motor Vehicle Sources in Baltimore City")
dev.off()

#q6
nei6= NEI%>%
  filter(fips== c("24510","06037"))%>%
  filter ( SCC %in% m) %>%
  select(year, Emissions,fips)%>%
  group_by(year, fips)%>%
  summarize(motor_emission_baltimore= sum(Emissions))

png("plot6.png")
qplot(year,motor_emission_baltimore, data= nei6, geom= c("point", "line"), color= fips,
      xlab="Year" ,ylab= "Total PM2.5 Emissions",
      main= "Emissions from Motor Vehicle sources in Baltimore City and Los Angeles")+ 
  scale_color_manual(labels = c("Los Angeles", "Baltimore City"),values = c("blue", "red"))

dev.off()
