## Function for making the plot1 from Course projects of the Exploratory Data Analysis Course on Coursera
## The function reads the data, formats and combines the time and date columns 
## Checks for ? values that represent NAs in the data

plot1 <- function() {
        ## Read the data 
        data<-read.csv("household_power_consumption.txt", sep = ";")
        
        ## Format the date and time and combine them to a single column
        data$DateTime <- strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M")
        head(data)
        
        ## Make the subset for actual plotting according to given start and end dates
        startDate <- as.POSIXlt("2007-02-01")
        endDate <- as.POSIXlt("2007-02-02 23:59:59")
        plotData <- subset(data, DateTime >= startDate & DateTime <= endDate)
        
        ## Check for ? values that represent NAs in the data. Check is done after subsetting to make it faster
        na<-plotData$Global_active_power == "?"
        
        ## Plot a histogram of the Global Active Power
        hist(as.numeric(as.character(plotData$Global_active_power[!na])), col="red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
        
        ## Save the plot as a PNG-file
        dev.copy(png, file="plot1.png")
        dev.off()
}
