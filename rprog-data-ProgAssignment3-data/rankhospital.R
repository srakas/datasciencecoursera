rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    outcomeCareMeasures = read.csv("outcome-of-care-measures.csv", colClasses = "character")
    validStates = unique(outcomeCareMeasures$State)
    validOutcomes = c("heart attack", "heart failure", "pneumonia")
    
    if (!(state %in% validStates)) {
        stop("invalid state")
    }
    
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
    
    stateHospitalsMeasures <- outcomeCareMeasures[outcomeCareMeasures$State == state, ][relevantColumns]
    
    if (is.numeric(num) && num > nrow(stateHospitalsMeasures)) {
        return(NA)
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
    sorted[displayRow, 1]
}
