# plot2.R
#
# Author:  M. Ealem
#
# Date:    Sat Sep  6 18:03:19 PDT 2014
#
# Purpose: Produces a line plot of Global Active Power in
#          kilowatts versus day of the week (Thurs, Friday, Saturday)
#          and saves it as "plot2.png." Assumes datafile
#          is in the current working directory, otherwise it is
#          downloaded and unzipped
#
# Usage:   Rscript plot2.R
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
library(data.table) # for fast read, etc.
GAP_data <- fread(datafile_name, colClasses=c("character", "character", "character", "character", "character", "character", "character", "character", "character"), header=TRUE, stringsAsFactors=FALSE, sep=";")


# columns are Date, Time, Global_active_power, Global_reactive_power, Global_intensity, Sub_metering_1, Sub_metering_2, Sub_metering_3
# Chain our transforms...
print("cleaning data...")
cleaned_data <- GAP_data %>%
    select(Date, Time, GAP=Global_active_power) %>%
    mutate(DateTime= ymd_hms(paste(dmy(Date), Time))) %>%
    select(DateTime, GAP) %>%
    filter((DateTime >= ymd("2007-02-01")) & (DateTime <= ymd("2007-02-03"))) %>% 
    mutate(GAP = as.numeric(GAP)) %>%
    group_by(Day_of_week = lubridate::wday(DateTime, label=TRUE, abbr=FALSE))
print("creating plot...")
png(filename="plot2.png", width=480, height=480)
with(cleaned_data,
    plot(DateTime, GAP, type="l", ylab = "Global Active Power (kilowatts)", xlab=""))


dev.null <- dev.off()
print("Done!")
