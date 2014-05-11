## Function for making the plot2 from Course projects of the Exploratory Data Analysis Course on Coursera
## The function reads the data, formats and combines the time and date columns 
## Checks for ? values that represent NAs in the data

plot2 <- function() {
        ##Override the localization settings to print out the weekdays in english
        Sys.setlocale(category="LC_ALL", "C")
        ## Read the data 
        data<-read.csv("household_power_consumption.txt", sep = ";")
        
        ## Format the date and time and combine them to a single column
        data$DateTime <- strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M")
        head(data)
        
        ## Make the subset for actual plotting according to given start and end dates
        startDate <- as.POSIXlt("2007-02-01")
        endDate <- as.POSIXlt("2007-02-02 23:59:59") ## time added to include the whole day, pure date would translate to time 0:0:0
        plotData <- subset(data, DateTime >= startDate & DateTime <= endDate)
        
        ## Check for ? values that represent NAs in the data. Check is done after subsetting to make it faster
        na<-plotData$Global_active_power == "?"
        
        ## Plot a line-graph of the Global Active Power as a function of time with x-label as weekdays
        plot(plotData$DateTime[!na], as.numeric(as.character(plotData$Global_active_power[!na])), type ="l", ylab = "Global Active Power (kilowatts)", xlab ="")
        
        ##old version, axis-formatting experiments
        ##axis(1,at = NULL,labels = as.Date(plotData$DateTime[!na], format = "%a"))        
        
        ## Save the plot as a PNG-file
        dev.copy(png, file="plot2.png")
        dev.off()
}
