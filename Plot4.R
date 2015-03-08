library(datasets)
Sys.setlocale(category = "LC_ALL", locale = "English")

create_plot4 <- function(directory) {
  
  #set the directory in parameter as the working folder
  setwd(directory)
  
  if(!file.exists("./data/household_power_consumption.txt")){
    print("Directory not correct, please input the path where the file <household_power_consumption.txt> is.")
  } 
  else{
    
    #load the activity and name the column
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
    
    dfSubHousePower$Timestamp <- paste (as.Date(dfSubHousePower$Date,"%d/%m/%Y"), 
                                        dfSubHousePower$Time, 
                                        sep = " ", collapse = NULL)
    
    png("plot4.png", width = 480, height = 480, units = "px", bg = "white")
    
    #par(mfrow = c(1, 4), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
    
    with(dfSubHousePower,
        {
          plot( 
            x=strptime(dfSubHousePower$Timestamp,"%Y-%m-%d %H:%M:%S"),
            y=dfSubHousePower$Global_active_power,
            type="l",
            main="",
            xlab=as.Date(dfSubHousePower$Date, format = "%a"),
            ylab="Global Active Power (killowatts)",
            #xaxt="n"
          )
          
          plot( 
            x=strptime(dfSubHousePower$Timestamp,"%Y-%m-%d %H:%M:%S"),
            y=dfSubHousePower$Voltage,
            type="l",
            main="",
            xlab=as.Date(dfSubHousePower$Date, format = "%a"),
            ylab="Voltage",
            #xaxt="n"
          )
          
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