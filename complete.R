# Function to read a directory full of lines and report the nuber of completely observed cases in each data file.
complete <- function(directory,id=1:332){
    ## INPUT: directory = character, location of the CSV files
    ##        id = integer, monitor id numbers to be used
    
    ## OUTPUT: data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    setwd("~/datasciencecoursera")
    
    # Initialize data frame for output
    comp <- data.frame(id=id,nobs=rep(0,length(id)))
    
    # Import csv files from directory and find the number of complete lines
    for (j in 1:length(id)){
        if (nchar(id[j]) == 1){
            k <- paste0("00",id[j])
        } else if (nchar(id[j]) == 2){
            k <- paste0("0",id[j])
        } else k <- id[j]
        data <- read.csv(paste0(directory,"/",k,".csv"))
        comp$nobs[j] <- sum(complete.cases(data))
    }
    comp
}