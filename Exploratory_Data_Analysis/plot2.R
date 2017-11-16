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

#filter required data
plot2_data <- plot_data %>%
        select(Global_active_power, datetime)
with(plot2_data, plot(datetime, Global_active_power, type = "n", xlab = "", ylab = "Global active power (kilowatts)"))
with(plot2_data, lines(datetime, Global_active_power))

#Save png file
dev.copy(png,"plot2.png")
dev.off()
