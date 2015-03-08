library(datasets)
Sys.setlocale(category = "LC_ALL", locale = "English")

create_plot1 <- function(directory) {
  
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

    png("plot1.png", width = 480, height = 480, 
                      units = "px", 
                      #bg = "white" 
                      bg = "transparent"
        )
    
    par(mar= c(4, 4, 2, 1))
    
    hist(dfSubHousePower$Global_active_power,
                                  col="red", 
                                  main="Global Active Power",
                                  xlab="Global Active Power (killowatts)"
        )

    dev.off()
  }
}
