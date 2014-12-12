corr <- function(directory, threshold = 0) {
    require(plyr)
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    totalRows = 0
    firstFile <- TRUE
    for (i in 1:332) {
        filepath = file.path(directory, paste(sprintf("%03d", i), '.csv', sep=""))
        filedata = read.csv(filepath)
        completeCasesFile <- na.omit(filedata)
        
        completeCasesNumber <- nrow(completeCasesFile)
        if (completeCasesNumber > threshold) {
            totalRows =  totalRows + completeCasesNumber
            if (firstFile) {            
                total <- completeCasesFile
                firstFile <- FALSE
            }
            else {
                total <- rbind(total, completeCasesFile)
            }
        }
    }
    
   if (totalRows > 0) {
       result <- daply(total, .(ID), function(y) cor(y$sulfate, y$nitrate)) 
   }
   else {
       result <- numeric()
   }
    result
}