# question 1
library(dplyr)

if (!exists("NEI"))
  NEI <- readRDS("summarySCC_PM25.rds")

if (!exists("SCC"))
  SCC <- readRDS("Source_Classification_Code.rds")

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from 
# all sources for each of the years 1999, 2002, 2005, and 2008.

# note that the years 1999, 2002, 2005, and 2008 are the only years in the dataset
# note that the Pollutant is PM25-PRI through the entire dataset
df <- group_by(NEI, year)
df <- summarise(df, total_emission = sum(Emissions))
png("plot1.png")
with(df, plot(x = year, y = total_emission / (10^6), xlab="year", ylab = "emissions (millions)", main = "PM2.5 emissions", type = "l"))
dev.off()

# looking at the plot, we see that the answer is yes

# question 2
library(dplyr)
if (!exists("NEI"))
  NEI <- readRDS("summarySCC_PM25.rds")

if (!exists("SCC"))
  SCC <- readRDS("Source_Classification_Code.rds")

# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make
# a plot answering this question.

# note that the years 1999, 2002, 2005, and 2008 are the only years in the dataset
# note that the Pollutant is PM25-PRI through the entire dataset

df <- filter(NEI, fips == "24510")
df <- group_by(df, year)
df <- summarise(df, total_emission = sum(Emissions))
png("plot2.png")
with(df, plot(x = year, y = total_emission, xlab="year", ylab = "emissions", main = "PM2.5 emissions for Baltimore City", type = "l"))
dev.off()

# looking at the plot, we see that the answer is yes

# question 3
library(dplyr)
library(ggplot2)
## This first line will likely take a few seconds. Be patient!
if (!exists("NEI"))
  NEI <- readRDS("summarySCC_PM25.rds")

if (!exists("SCC"))
  SCC <- readRDS("Source_Classification_Code.rds")

# Of the four types of sources indicated by the type 
# (point, nonpoint, onroad, nonroad) variable, which
# of these four sources have seen decreases in emissions from 1999-2008
# for Baltimore City? Which have seen increases in emissions from 1999-2008?
# Use the ggplot2 plotting system to make a plot answer this question.
df <- filter(NEI, fips == "24510")
df <- group_by(df, year, type)
df <- summarise(df, total_emission = sum(Emissions))
png("plot3.png")
qplot(x = year, y = total_emission, xlab="year", ylab = "emissions", type = "l", data = df, main = "PM2.5 emissions for Baltimore City by type", facets = type ~ .) + geom_line()
dev.off()

# question 4
library(dplyr)
library(ggplot2)
## This first line will likely take a few seconds. Be patient!
if (!exists("NEI"))
  NEI <- readRDS("summarySCC_PM25.rds")

if (!exists("SCC"))
  SCC <- readRDS("Source_Classification_Code.rds")

# Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999-2008?
coal_related <- grep(pattern = "Coal|coal", SCC$Short.Name)
coal_related_source <- SCC$SCC[coal_related]
df <- NEI[NEI$SCC %in% coal_related_source, ]
df <- group_by(df, year)
df <- summarise(df, total_emission = sum(Emissions))
png("plot4.png")
qplot(x = year, y = total_emission, xlab="year", ylab = "emissions", type = "l", main = "Emissions from coal-related sources", data = df) + geom_line()
dev.off()

# question 5
# How have emissions from motor vehicle sources changed from 1999-2008
# in Baltimore City?

library(dplyr)
library(ggplot2)
## This first line will likely take a few seconds. Be patient!
if (!exists("NEI"))
  NEI <- readRDS("summarySCC_PM25.rds")

if (!exists("SCC"))
  SCC <- readRDS("Source_Classification_Code.rds")

# an inspection of the data reveals that motor vehicles have Short.Name fields 
# which contain, as a separate embedded word, Veh
motor_related <- grep(pattern = " Veh ", SCC$Short.Name)
motor_related_source <- SCC$SCC[motor_related]
df <- NEI[NEI$SCC %in% motor_related_source, ]
df <- filter(df, fips == "24510")
df <- group_by(df, year)
df <- summarise(df, total_emission = sum(Emissions))
png("plot5.png")
qplot(x = year, y = total_emission, xlab="year", ylab = "emissions", type = "l", main = "Emissions from motor vehicle sources in Baltimore City", data = df) + geom_line()
dev.off()

# question 6

library(dplyr)
library(ggplot2)
## This first line will likely take a few seconds. Be patient!
if (!exists("NEI"))
  NEI <- readRDS("summarySCC_PM25.rds")

if (!exists("SCC"))
  SCC <- readRDS("Source_Classification_Code.rds")

# Compare emissions from motor vehicle sources in Baltimore City
# with emissions from motor vehicle sources in Los Angeles County, 
# California (fips == "06037"). Which city has seen greater changes
# over time in motor vehicle emissions?

# an inspection of the data reveals that motor vehicles have Short.Name fields 
# which contain, as a separate embedded word, Veh
motor_related <- grep(pattern = " Veh ", SCC$Short.Name)
motor_related_source <- SCC$SCC[motor_related]
df <- NEI[NEI$SCC %in% motor_related_source, ]
df <- filter(df, fips == "24510" | fips == "06037")
df <- group_by(df, year, fips)
df <- summarise(df, total_emission = sum(Emissions))
df$city <- ifelse (df$fips == "24510", "Baltimore City", "Los Angeles County")
png("plot6.png")
qplot(x = year, y = total_emission, xlab="year", ylab = "emissions", type = "l", main = "Emissions from motor vehicle sources in Baltimore City", data = df, col = city) + geom_line()
dev.off()