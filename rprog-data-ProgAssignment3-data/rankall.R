rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    outcomeCareMeasures = read.csv("outcome-of-care-measures.csv", colClasses = "character")
    validStates = unique(outcomeCareMeasures$State)
    validOutcomes = c("heart attack", "heart failure", "pneumonia")
    
      if (!(outcome %in% validOutcomes)) {
        stop("invalid outcome")
    }
    
    relevantColumns <- c("Hospital.Name", "State")
    if (outcome == "heart attack") {
        relevantColumns = append(relevantColumns, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")
    }
    else if (outcome == "heart failure") {
        relevantColumns = append(relevantColumns, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
    }
    else {
        relevantColumns = append(relevantColumns, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    }
    
    
    finalData <- data.frame("hospital"=character(0), "state"=character(0), stringsAsFactors=FALSE)
    
    for (state in validStates) {
        stateHospitalsMeasures <- outcomeCareMeasures[outcomeCareMeasures$State == state, ][relevantColumns]
        #if (i > 1) {
        #    next
        #}
            
        if (is.numeric(num) && num > nrow(stateHospitalsMeasures)) {
            finalData[state, ] <- c(NA, state)
            next
        }
        
        completestateHospitalsMeasures <- stateHospitalsMeasures[stateHospitalsMeasures[, 3] != "Not Available", ]
        
        orderBy = as.numeric(completestateHospitalsMeasures[, 3])
        if (num == "worst") {
            orderBy = -orderBy
        }
    
        sorted <- completestateHospitalsMeasures[order(orderBy, completestateHospitalsMeasures[, 1]), ]
        displayRow = 1
        if (is.numeric(num)) {
            displayRow = num
        }

        finalData[state, ] <- c(sorted[displayRow, 1], state)        
    }
    
    finalData[order(finalData[, 2]), ]       
}