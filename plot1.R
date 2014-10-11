# plot1.R
#
# Author:  M. Ealem
#
# Date:    Sat Oct 11 08:48:36 PDT 2014
#
# Purpose: Produces a histogram plot of Global Active Power in
#          kilowatts and saves it as "plot1.png." Assumes datafile
#          is in the current working directory, otherwise it is
#          downloaded and unzipped
#
# Usage:   Rscript plot1.R
#

# options
options(warn=1) # we want to see warnings as they occur, not after R
                # gets around to flushing the buffer...

# Required libraries
library(data.table) # for fast read, etc.
library(dplyr)
library(lubridate)

# Parameter values - makes it easier/quicker to find and change later
# Note: left off the 's' in "https' for the URL since download.file()
# can't use SSH
url <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
zipfile_name <- "data.zip"
datafile_name <- "household_power_consumption.txt"

print("plot1.png generator running...")

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
GAP_data <- fread(datafile_name, colClasses=c("character", "character", "character", "character", "character", "character", "character", "character", "character"), header=TRUE, stringsAsFactors=FALSE, sep=";")

# columns are Date, Time, Global_active_power, Global_reactive_power, Global_intensity, Sub_metering_1, Sub_metering_2, Sub_metering_3
# Chain our transforms...
print("cleaning data...")
cleaned_data <- GAP_data %>%
    mutate(Date=dmy(Date)) %>%
    filter((Date >= ymd("2007-02-01")) & (Date <= ymd("2007-02-02"))) %>%
    mutate(Global_active_power = as.numeric(Global_active_power),
           Global_reactive_power = as.numeric(Global_reactive_power),
           Voltage=as.numeric(Voltage),
           Sub_metering_1 = as.numeric(Sub_metering_1),
           Sub_metering_2 = as.numeric(Sub_metering_2),
           Sub_metering_3 = as.numeric(Sub_metering_3))

rm(GAP_data) #don't need it any more
print("saving plot...")
png(filename="plot1.png", width=480, height=480)
with(cleaned_data,
     hist(Global_active_power,
          main="Global Active Power",
          xlab="Global Active Power (kilowatts)",
          # color obtained from sampling web image
          # and converting to local color profile
          col=rgb(255,37,0, maxColorValue=255))) 
dev.off()
print("Done!")
