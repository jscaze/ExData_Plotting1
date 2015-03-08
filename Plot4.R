library(datasets)
Sys.setlocale(category = "LC_ALL", locale = "English")

create_plot4 <- function(directory) {
  
  #set the directory in parameter as the working folder
  setwd(directory)
  #check if the file exists in the folder data
  if(!file.exists("./data/household_power_consumption.txt")){
    print("Directory not correct, please input the path where the file <household_power_consumption.txt> is.")
  } 
  else{
    
    #load the household_power_consumption.txt file
    dfHousePower <- read.table("./data/household_power_consumption.txt", sep=";", 
                               header =TRUE, na.strings = "?")
    
    #subsetting on the 2007-02-01 and 2007-02-02
    dfSubHousePower <- dfHousePower[
      as.Date(dfHousePower$Date,"%d/%m/%Y") == 
        as.Date("2007-02-01", "%Y-%m-%d")
      |
        as.Date(dfHousePower$Date,"%d/%m/%Y") == 
        as.Date("2007-02-02", "%Y-%m-%d")  
      ,]
    #add a column in the DF to get the DATE + TIME in one column
    dfSubHousePower$Timestamp <- paste (as.Date(dfSubHousePower$Date,"%d/%m/%Y"), 
                                        dfSubHousePower$Time, 
                                        sep = " ", collapse = NULL)
    #create the graph in PNG file
    png("plot4.png", width = 480, height = 480, units = "px", bg = "white")
    
    #to get the graph on 2 columns and 2 lines
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
    #all graphs
    with(dfSubHousePower,
        { #first graph
          plot( 
            x=strptime(dfSubHousePower$Timestamp,"%Y-%m-%d %H:%M:%S"),
            y=dfSubHousePower$Global_active_power,
            type="l",
            main="",
            xlab=as.Date(dfSubHousePower$Date, format = "%a"),
            ylab="Global Active Power (killowatts)",
            #xaxt="n"
          )
          #second graph
          plot( 
            x=strptime(dfSubHousePower$Timestamp,"%Y-%m-%d %H:%M:%S"),
            y=dfSubHousePower$Voltage,
            type="l",
            main="",
            xlab=as.Date(dfSubHousePower$Date, format = "%a"),
            ylab="Voltage",
            #xaxt="n"
          )
          #third graph
          {
                plot( 
                    x=strptime(dfSubHousePower$Timestamp,"%Y-%m-%d %H:%M:%S"),
                    y=dfSubHousePower$Sub_metering_1,
                    type="l",
                    col="black", 
                    main="",
                    xlab=as.Date(dfSubHousePower$Date, format = "%a"),
                    ylab="Energy sub metering",
                  )
                lines( 
                    x=strptime(dfSubHousePower$Timestamp,"%Y-%m-%d %H:%M:%S"),
                    y=dfSubHousePower$Sub_metering_2,
                    col = "red")
                lines( 
                    x=strptime(dfSubHousePower$Timestamp,"%Y-%m-%d %H:%M:%S"),
                    y=dfSubHousePower$Sub_metering_3,
                    col = "blue")
                legend("topright", pch = "_", 
                    col = c("black", "red", "blue"), 
                    legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3")
                    )
          }
          #last graph
          plot( 
            x=strptime(dfSubHousePower$Timestamp,"%Y-%m-%d %H:%M:%S"),
            y=dfSubHousePower$Global_reactive_power,
            type="l",
            main="",
            xlab=as.Date(dfSubHousePower$Date, format = "%a"),
            ylab="Global ReActive Power",
            #xaxt="n"
          )
        })
    dev.off()
  }
}
