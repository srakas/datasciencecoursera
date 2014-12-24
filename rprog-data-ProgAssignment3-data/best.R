best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    outcomeCareMeasures = read.csv("outcome-of-care-measures.csv", colClasses = "character")
    validStates = unique(outcomeCareMeasures$State)
    validOutcomes = c("heart attack", "heart failure", "pneumonia")
    
    if (!(state %in% validStates)) {
        stop("invalid state")
    }
    
    if (!(outcome %in% validOutcomes)) {
        stop("invalid outcome")
    }
    
    #relevantColumns <- names(outcomeCareMeasures) %in% c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
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
    
    completestateHospitalsMeasures <- stateHospitalsMeasures[stateHospitalsMeasures[, 3] != "Not Available", ]
    sorted <- completestateHospitalsMeasures[order(as.numeric(completestateHospitalsMeasures[, 3]), completestateHospitalsMeasures[, 1]), ]
    sorted[1, 1]
}