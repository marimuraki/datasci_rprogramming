# JHU DS: R Programming 
# https://class.coursera.org/rprog-003

setwd("~/Dropbox/Mari/courses/Coursera/DS Track/R Programming/rprog_data_asgn3")

# Rank hospitals in a state for a given outcome
#
# Arguments:
#   state: 2-character abbreviated name of state
#   outcome (30-day mortality rate): 
#     "heart attack"
#     "heart failure"
#     "pneumonia"
#   rank: ranking of a hospital in state for given outcome 
#
# Returns:
#   a character vector with the name of the hospital 
#   for the given (state, outcome, rank)
#
# Example:
# > source("rankhospital.R")
# > rankhospital("TX", "heart failure", 4)
#   [1] "DETAR HOSPITAL NAVARRO"
# > rankhospital("MD", "heart attack", "worst") 
#   [1] "HARFORD MEMORIAL HOSPITAL"
# > rankhospital("MN", "heart attack", 5000)
#   [1] NA

rankhospital <- function(state, outcome, num = "best") {
    
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
    
    data.state <- subset(data.hospital, State == state)
    
    data.stateoutcome <- subset(data.state, 
                                select = c("Hospital.Name", outcome))
    
    data.stateoutcome <- data.stateoutcome[complete.cases(data.stateoutcome), ]  
    
    # Sort data such that same rank sorted by hospital name (alphabetical tie break)
    
    data.stateoutcome <- data.stateoutcome[order(data.stateoutcome[outcome],
                                                 data.stateoutcome["Hospital.Name"]), ]
    
    # Return hospital name in state with given rank for specified outcome
    
    if (num == "best") {
        rank = 1
    } else if (num == "worst") {
        rank = nrow(data.stateoutcome)
    } else if (is.numeric(num)) {
        rank = as.integer(num)      
        if (rank > nrow(data.stateoutcome) || rank < 1) {
            return(NA)
        }
    } else {
        stop("invalid number")
    }
    
    rank.hospital <- data.stateoutcome[rank, "Hospital.Name"]
    
    rank.hospital
    
}

