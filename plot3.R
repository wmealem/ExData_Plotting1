# plot3.R
#
# Author:  M. Ealem
#
# Date:    Sun Sep  7 07:12:33 PDT 2014
#
# Purpose: Produces a line plot of energy sub metering in
#          kilowatts versus day of the week (Thurs, Friday, Saturday)
#          and saves it as "plot3.png." Assumes datafile
#          is in the current working directory, otherwise it is
#          downloaded and unzipped
#
# Usage:   Rscript plot3.R
#

# options
options(warn=1) # we want to see warnings as they occur, not after R
                # gets around to flushing the buffer...

# Required libraries

library(dplyr)
library(lubridate)

# Parameter values - makes it easier/quicker to find and change later
# Note: left off the 's' in "https' for the URL since download.file()
# can't use SSH
url <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
zipfile_name <- "data.zip"
datafile_name <- "household_power_consumption.txt"

print("plot3.png generator running...")

# Has the data file been downloaded and unzipped yet? If not, do so...
if (!file.exists(datafile_name)) {
    if (!file.exists(zipfile_name)) {
        warning("Datafile not found! Downloading...")
        download.file(url=url, destfile=zipfile_name, method="curl")
    }
    unzip(zipfile_name)
}

# Note: because there are embedded '?'s in the data, fread() kept spitting
# out long, detailed warnings, so it is easier just to have it read *everything* as a character
library(data.table) # for fast read, etc.
GAP_data <- fread(datafile_name, colClasses=c("character", "character", "character", "character", "character", "character", "character", "character", "character"), header=TRUE, stringsAsFactors=FALSE, sep=";")


# columns are Date, Time, Global_active_power, Global_reactive_power, Global_intensity, Sub_metering_1, Sub_metering_2, Sub_metering_3
# Chain our transforms...
print("cleaning data...")
cleaned_data <- GAP_data %>%
    select(Date, Time, SB1=Sub_metering_1, SB2=Sub_metering_2, SB3=Sub_metering_3) %>%
    mutate(DateTime= ymd_hms(paste(dmy(Date), Time))) %>%
    select(DateTime, SB1, SB2, SB3) %>%
    filter((DateTime >= ymd("2007-02-01")) & (DateTime <= ymd("2007-02-03"))) %>% 
    mutate(SB1=as.numeric(SB1), SB2=as.numeric(SB2), SB3=as.numeric(SB3)) %>%
    group_by(Day_of_week = lubridate::wday(DateTime, label=TRUE, abbr=FALSE))

print("creating plot...")
png(filename="plot3.png", width=480, height=480)
with(cleaned_data, {
    plot(DateTime, SB1, type="n", xlab="", ylab = "Energy sub metering")
    lines(DateTime, SB1)
    lines(DateTime, SB2, col="red")
    lines(DateTime, SB3, col="blue")
    legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1), col=c("black", "red", "blue"))
})


dev.null <- dev.off()
print("Done!")
