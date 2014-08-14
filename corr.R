# A function to calculate the correlation between sulfate and nitrate for monitor locations where the number of completely
# observed cases is greater than a threshold
corr <- function(directory, threshold = 0) {
    ## INPUT:  directory = character, location of the CSV files
    ##         threshold = numeric, number of completely observed observations (on all
    ##                     variables) required to compute the correlation between
    ##                     nitrate and sulfate; the default is 0
    
    ## OUPUT: numeric vector of correlations
    setwd("~/datasciencecoursera")
    
    # Initialize vector of correlations
    cr <- numeric()
    
    # Use complete to find monitor locations with complete cases above threshold
    comp <- complete("specdata")
    above <- which(comp$nobs > max(threshold,0))
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
            cr[i] <- cor(data[,2],data[,3],use="complete.obs")
        }
        return(cr)
    }
}