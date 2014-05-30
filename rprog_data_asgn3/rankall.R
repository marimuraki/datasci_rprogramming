# JHU DS: R Programming 
# https://class.coursera.org/rprog-003

setwd("~/Dropbox/Mari/courses/Coursera/DS Track/R Programming/rprog_data_asgn3")

# Rank hospitals in each state for a given outcome
#
# Arguments:
#   outcome (30-day mortality rate): 
#     "heart attack"
#     "heart failure"
#     "pneumonia"
#   rank: ranking of a hospital in state for given outcome 
#
# Returns:
#   a character vector with the name of the hospital 
#   for the given (outcome, rank)
#
# Example:
# > source("rankall.R")
# > head(rankall("heart attack", 20), 10)
# hospital state
# 1   D W MCMILLAN MEMORIAL HOSPITAL          AL
# 2   <NA>                                    AK
# 3   JOHN C LINCOLN DEER VALLEY HOSPITAL     AZ
# 4   ARKANSAS METHODIST MEDICAL CENTER       AR
# 5   SHERMAN OAKS HOSPITAL                   CA
# 6   SKY RIDGE MEDICAL CENTER                CO
# 7   MIDSTATE MEDICAL CENTER                 CT
# 8   <NA>                                    DE
# 9   <NA>                                    DC
# 10  SOUTH FLORIDA BAPTIST HOSPITAL          FL

rankall <- function(outcome, num = "best") {
    
    # Read outcome data
    
    data.hospital <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    
    state.names <- unique(data.hospital$State)
    states <- character()
    hospitals <- character()
    
    for(state in state.names) {
        hospital <- rankhospital(state, outcome, data.hospital, num)
        states <- append(states, state)
        hospitals <- append(hospitals, hospital)
    }
    data.frame(hospital=hospitals, state=states)
}

rankhospital <- function(state, outcome, data.hospital, num = "best") {
    
    # Check (state, outcome) arguments valid
    
    if (!any(state == data.hospital$State)) {
        stop("invalid state")
    }
    
    if (outcome == "heart attack") {
        outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        index = 11
    } else if (outcome == "heart failure") {
        outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        index = 17
    } else if (outcome == "pneumonia") {
        outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        index = 23
    } else {
        stop("invalid outcome")
    }
    
    # Coerce column data to numeric
    
    data.hospital[, index] <- suppressWarnings(as.numeric(data.hospital[, index]))
    
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

