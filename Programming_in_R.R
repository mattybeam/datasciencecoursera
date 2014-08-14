# Coursera: Programming in R course

setwd("~/datasciencecoursera")

# Week 1
x <- list(foo=1:4,bar=0.6)
x[1]
x[[1]]
x$bar
x[["bar"]]
x["bar"]
x <- c(4,"a",TRUE)
class(x)
x <-list(2,"a","b",TRUE)
x[[1]]
x <- 1:4
y <- 2
x+y

setwd("~/Downloads")
ex <- read.csv("hw1_data.csv")
head(ex)
ex[1:2,]
nrow(ex)
tail(ex,2)
ex$Ozone[47]
bad <- is.na(ex$Ozone)
sum(bad)
mean(ex$Ozone,na.rm=T)
exs <- subset(ex,Ozone>31 & Temp>90)
mean(exs$Solar.R,na.rm=T)
mo6 <- subset(ex,Month==6)
mean(mo6$Temp,na.rm=T)
mo5 <- subset(ex,Month==5)
max(mo5$Ozone,na.rm=T)


x <- c(17, 14, 4, 5, 13, 12, 10)
x[x>10]==4


install.packages("swirl")
library(swirl)
swirl()


# Week 2
above10 <- function(x){
  use <- x > 10
    x[use]
}

columnmean <- function(y){
  nc <- ncol(y)
  means <- numeric(nc)
  for (i in nc){
    means[i]=mean(y[,i])
  }
  means
}
x <- Sys.time()
x
p <- as.POSIXlt(x)
p
names(unclass(p))

f <- function(x) {
    g <- function(y) {
        y + z
    }
    z <- 4
    x + g(x)
}


# Programming assignment 1
# Pollutant mean function
pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## INPUT: directory = character, location of CSV file
    ##        pollutant = character, name of pollutant = c("sulfate","nitrate")
    ##        id = integer, monitor id numbers to be used
   
    ## OUTPUT: mean of the pollutant across all monitors in the id vector, ignoring NA values
   
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
   round(mean(monitors[,col],na.rm=T),3)
}



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
        comp$id[j] <- id[j]
        comp$nobs[j] <- sum(complete.cases(data))
    }
    comp
}


# A function to calculate the correlation between sulfate and nitrate for monitor locations where the number of completely
# observed cases is greater than a threshold
corr <- function(directory, threshold = 0) {
    ## INPUT:  directory = character, location of the CSV files
    ##         threshold = numeric, number of completely observed observations (on all
    ##                     variables) required to compute the correlation between
    ##                     nitrate and sulfate; the default is 0
    
    ## OUPUT: numeric vector of correlations
    
    # Initialize vector of correlations
    cr <- numeric()
    
    # Use complete to find monitor locations with complete cases above threshold
    comp <- complete("specdata")
    above <- which(comp$nobs > threshold)
    if (length(above) == 0){
        return(cr)
    } else {
        # Initialize vector of correlations
        cr <- numeric(length = length(above))
        
        for (i in 1:length(above)){
            if (nchar(comp$id[above[i]]) == 1){
                k <- paste0("00",comp$id[above[i]])
            } else if (nchar(comp$id[above[i]]) == 2){
                k <- paste0("0",comp$id[above[i]])
            } else k <- comp$id[above[i]]
            data <- read.csv(paste0(directory,"/",k,".csv"))
            cr[i] <- round(cor(data[,2],data[,3],use="complete.obs"),5)
        }
        return(cr)
    }
}