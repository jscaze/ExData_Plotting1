library(datasets)
Sys.setlocale(category = "LC_ALL", locale = "English")

create_plot2 <- function(directory) {
  
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
    
    #png(filename = "plot1.png",
    #    width = 480, height = 480, units = "px", pointsize = 12,
    #    bg = "white", res = NA, family = "", restoreConsole = TRUE,
    #    type = c("windows", "cairo", "cairo-png"))
    
    png("plot2.png", width = 480, height = 480, units = "px", bg = "white")
    
    par(mar= c(4, 4, 2, 1))
    
    plot( 
          x=strptime(dfSubHousePower$Timestamp,"%Y-%m-%d %H:%M:%S"),
          y=dfSubHousePower$Global_active_power,
          type="l",
          main="",
          xlab=as.Date(dfSubHousePower$Date, format = "%a"),
          ylab="Global Active Power (killowatts)",
          #xaxt="n"
    )
     
    dev.off()
  }
}