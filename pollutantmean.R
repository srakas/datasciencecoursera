pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    firstFile <- TRUE
    for (i in id) {
        filepath = file.path(directory, paste(sprintf("%03d", i), '.csv', sep=""))
        filedata = read.csv(filepath)
        if (firstFile) {
            total <- filedata
            firstFile <- FALSE
        }
        else {
            total <- rbind(total, filedata)
        }

    }
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    round(mean(total[!(is.na(total[, c(pollutant)])), c(pollutant)]), digits = 3)

}
    