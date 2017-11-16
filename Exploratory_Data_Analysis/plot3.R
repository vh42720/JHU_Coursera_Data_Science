library(data.table)
library(lubridate)
library(dplyr)
library(statsr)

##Load data 
dt <- read.table("household_power_consumption.txt",header = T, sep = ";")
dt$Date <- dmy(dt$Date)
plot_data <- dt %>%
        filter(Date %in% c(ymd("2007-02-01"),ymd("2007-02-02"))) %>%
        mutate(datetime = (paste(Date, Time)))
plot_data$datetime <- lubridate::ymd_hms(plot_data$datetime, truncated = 3)

#Filter only required data
plot3_data <- plot_data %>%
        select(Sub_metering_1,Sub_metering_2,Sub_metering_3, datetime)
plot3_data$Sub_metering_1 <- as.numeric(as.character(plot3_data$Sub_metering_1))
plot3_data$Sub_metering_2 <- as.numeric(as.character(plot3_data$Sub_metering_2))

#Plot
with(plot3_data, plot(datetime, Sub_metering_1,
                      type = "n", xlab = "", ylab = "Energy sub metering"))
with(plot3_data, lines(datetime, Sub_metering_1, col = "black"))
with(plot3_data, lines(datetime, Sub_metering_2, col = "red"))
with(plot3_data, lines(datetime, Sub_metering_3, col = "blue"))

#Legends
legend("topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       col = c("black","red","blue"), lty = 1)

##Export
dev.copy(png,"plot3.png")
dev.off()
