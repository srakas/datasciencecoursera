complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    

    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    vectorSize <- length(id)
    firstFile <- TRUE
    for (i in 1:vectorSize) {
        filepath = file.path(directory, paste(sprintf("%03d", id[i]), '.csv', sep=""))
        filedata = read.csv(filepath)

        completeCasesFile <- nrow(na.omit(filedata))
        
        if (firstFile) {            
            firstFile <- FALSE
            result <- data.frame(id=rep(NA, vectorSize), nobs=rep(NA, vectorSize))
        }
        result[i, ] <- c(id[i], completeCasesFile)
    }
    
    result   

}