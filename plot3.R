## Function for making the plot3 from Course projects of the Exploratory Data Analysis Course on Coursera
## The function reads the data, formats and combines the time and date columns 
## Checks for ? values that represent NAs in the data

plot3 <- function() {
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
        
        ## Find the extreme values to set the y-scale
        ## the data is as factors and therefore the as.numeric(as.character()) conversion is needed
        ymax <- max(c(as.numeric(as.character(plotData$Sub_metering_1)), as.numeric(as.character(plotData$Sub_metering_2)), as.numeric(as.character(plotData$Sub_metering_3))))
        ymin <- min(c(as.numeric(as.character(plotData$Sub_metering_1)), as.numeric(as.character(plotData$Sub_metering_2)), as.numeric(as.character(plotData$Sub_metering_3))))
        
        ## Create the plot template
        plot(plotData$DateTime, as.numeric(as.character(plotData$Sub_metering_1)), ylim = c(ymin,ymax), type ="n", ylab = "Energy sub metering", xlab ="")
        
        ## Plot submeter 1
        ## Check for ? values that represent NAs in the data. Check is done after subsetting to make it faster
        na<-plotData$Sub_metering_1 == "?"
        
        ## Plot a line-graph of the Global Active Power as a function of time with x-label as weekdays
        lines(plotData$DateTime[!na], as.numeric(as.character(plotData$Sub_metering_1[!na])))
        
        ## Plot submeter 2
        ## Check for ? values that represent NAs in the data. Check is done after subsetting to make it faster
        na<-plotData$Sub_metering_2 == "?"
        
        ## Plot a line-graph of the Global Active Power as a function of time with x-label as weekdays
        lines(plotData$DateTime[!na], as.numeric(as.character(plotData$Sub_metering_2[!na])), col= "red")
        
        ## Plot submeter 3
        ## Check for ? values that represent NAs in the data. Check is done after subsetting to make it faster
        na<-plotData$Sub_metering_3 == "?"
        
        ## Plot a line-graph of the Global Active Power as a function of time with x-label as weekdays
        lines(plotData$DateTime[!na], as.numeric(as.character(plotData$Sub_metering_3[!na])), col = "blue")
        
        
        ## Add legend
        par(xpd=TRUE)
        legend("topright", text.width = strwidth("Sub_metering_3"), lty = c(1,1,1), col = c("black", "red", "blue"),cex = 0.7, legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        
        ## Save the plot as a PNG-file
        dev.copy(png, file="plot3.png")
        dev.off()
        par(xpd=FALSE)
}
