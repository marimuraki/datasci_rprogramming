# JHU DS: R Programming 
# https://class.coursera.org/rprog-003

setwd("~/Dropbox/Mari/courses/Coursera/DS Track/R Programming/rprog_data_asgn3")

# Find the best hospital in a state for a given outcome
#
# Arguments:
#   state: 2-character abbreviated name of state
#   outcome (30-day mortality rate): 
#     "heart attack"
#     "heart failure"
#     "pneumonia"
#
# Returns:
#   a character vector with the name of the hospital 
#   with the best (i.e. lowest) 30-day mortality rate
#   for given (state, outcome)
#
# Example:
# > source("best.R")
# > best("TX", "heart attack")
#   [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("BB", "heart attack") 
#   Error in best("BB", "heart attack"): invalid state
# > best("NY", "hrt attack")
#   Error in best("NY", "hrt attack") : invalid outcome

best <- function(state, outcome) {
    
    # Read outcome data
    
    data.hospital <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    
    # Check (state, outcome) arguments valid
    
    if (!any(state == data.hospital$State)) {
        stop("invalid state")
    }
    
    if (outcome == "heart attack") {
        outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure") {
        outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome == "pneumonia") {
        outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else {
        stop("invalid outcome")
    }
    
    # Coerce column data to numeric
    
    data.hospital[, outcome] <- suppressWarnings(as.numeric(data.hospital[, outcome]))
    
    # Subset data
    
    data.state        <- subset(data.hospital, State == state)
    
    data.stateoutcome <- subset(data.state, 
                                select = c("Hospital.Name", outcome))
    
    data.stateoutcome <- data.stateoutcome[complete.cases(data.stateoutcome), ]  
    
    data.stateoutcome <- data.stateoutcome[order(data.stateoutcome[outcome], 
                                                 data.stateoutcome["Hospital.Name"]), ]
    
    # Return hospital name in state with lowest 30-day mortality rate
    
    return.best <- data.stateoutcome[data.stateoutcome[outcome] == min(data.stateoutcome[outcome]), "Hospital.Name"]
    
    return.best
    
}

