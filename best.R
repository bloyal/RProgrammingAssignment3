best <- function(state, outcome) {
               
        ## Check that state and outcome are valid
        if (!isValidState(state)){
                stop("invalid state")
        }
        else if(!isValidOutcome(outcome)){
                stop("invalid outcome")
        }
        
        ## Read outcome data
        outcomeData <- readOutcomeData("outcome-of-care-measures.csv")
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        stateData <- getStateData(state, outcomeData)
        stateData <- stateData[order(stateData[outcome],stateData["name"]) , ]
        stateData[1,1]
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

getStateData <- function(state, outcomeData){
        stateList<-split(outcomeData,outcomeData$State)
        stateData<-stateList[[state]]
        stateData<-stateData[,c("Hospital.Name",
                     "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                     "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                     "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
        names(stateData)<-c("name","heart attack", "heart failure", "pneumonia")
        stateData[,c(2,3,4)] <- sapply(stateData[,c(2,3,4)], as.numeric)
        stateData
}