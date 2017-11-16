if(!file.exists(household_power_consumption.txt)){
        stop("File is not founded!")
}

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

##Set parameters
par(mfcol = c(2,2))

##First Row + First Collumn (copy from plot2)
plot2_data <- plot_data %>%
        select(Global_active_power, datetime)
with(plot2_data, plot(datetime, Global_active_power, type = "n", xlab = "", ylab = "Global active power (kilowatts)"))
with(plot2_data, lines(datetime, Global_active_power))

##Second Row + First Collumn(copy from plot3)
plot3_data <- plot_data %>%
        select(Sub_metering_1,Sub_metering_2,Sub_metering_3, datetime)
plot3_data$Sub_metering_1 <- as.numeric(as.character(plot3_data$Sub_metering_1))
plot3_data$Sub_metering_2 <- as.numeric(as.character(plot3_data$Sub_metering_2))
with(plot3_data, plot(datetime, Sub_metering_1,
                      type = "n", xlab = "", ylab = "Energy sub metering"))
with(plot3_data, lines(datetime, Sub_metering_1, col = "black"))
with(plot3_data, lines(datetime, Sub_metering_2, col = "red"))
with(plot3_data, lines(datetime, Sub_metering_3, col = "blue"))
legend("topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       col = c("black","red","blue"), lty = 1, cex=0.75, bty = "n")

##First Row + Second Collumn
plot4_data <- plot_data %>%
        select(Voltage, datetime)
plot4_data$Voltage <- as.numeric(as.character(plot4_data$Voltage))
with(plot4_data, plot(datetime, Voltage, type = "n", xlab = "datetime", ylab = "Voltage"))
with(plot4_data, lines(datetime, Voltage))

##Second Row + Second Collumn
plot5_data <- plot_data %>%
        select(Global_reactive_power, datetime)
plot5_data$Global_reactive_power <- as.numeric(as.character(plot5_data$Global_reactive_power))
with(plot5_data, plot(datetime, Global_reactive_power,type = "n", xlab = "datetime"))
with(plot5_data, lines(datetime, Global_reactive_power))

##Export
dev.copy(png,"plot4.png")
dev.off()