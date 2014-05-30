best <- function(state, outcome) {
        ## Read outcome data
        #outcome <- readOutcomeData("outcome-of-care-measures.csv")
        
        ## Check that state and outcome are valid
        if (!isValidState(state)){
                stop("invalid state")
        }
        else if(!isValidOutcome(outcome)){
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
}

readOutcomeData <- function(outcomeDataFile){
        read.csv(outcomeDataFile, colClasses = "character")
}

isValidState <- function(inputStateCode){
        validStateCodes <- getListofStateCodes()
        any(validStateCodes==inputStateCode)
}

getListofStateCodes <- function (){
        stateData <- read.csv("states.csv")
        stateData$Abbreviation
        }

isValidOutcome <- function(inputOutcome){
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        any(validOutcomes==inputOutcome)     
}