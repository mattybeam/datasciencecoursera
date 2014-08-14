# Programming assignment 1
# Pollutant mean function
pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## INPUT: directory = character, location of CSV file
    ##        pollutant = character, name of pollutant = c("sulfate","nitrate")
    ##        id = integer, monitor id numbers to be used
    
    ## OUTPUT: mean of the pollutant across all monitors in the id vector, ignoring NA values
    setwd("~/datasciencecoursera")
    
    # Initialize monitors data frame
    monitors=data.frame(Date=NULL,sulfate=NULL,nitrate=NULL,ID=NULL) 
    for (i in id){
        if (nchar(i) == 1){
            i <- paste0("00",i)
        } else if (nchar(i) == 2){
            i <- paste0("0",i)
        } 
        monitors <- rbind(monitors,read.csv(paste0(directory,"/",i,".csv")))
    }
    # Get column of pollutant (2 for sulfate, 3 for nitrate)
    if (pollutant == "sulfate"){
        col <- 2
    } else if (pollutant == "nitrate"){
        col <- 3
    }
    mean(monitors[,col],na.rm=T)
}