
library(data.table)
library(lubridate)
library(dplyr)
library(statsr)

##Load data 
dt <- read.table("household_power_consumption.txt",header = T, sep = ";")
dt$Date <- dmy(dt$Date)
dt$Time <- hms(dt$Time)
plot_data <- filter(dt, Date %in% c(ymd("2007-02-01"),ymd("2007-02-02")))

##First Plot
plot_data$Global_active_power <- as.numeric(as.character(plot_data$Global_active_power))
hist(plot_data$Global_active_power, col="red", main = "Global Active Power",
     xlab = "Global Active Power (kilowatts)", ylab = "Frequency")

## PNG file
dev.copy(png,"plot1.png")
dev.off()
