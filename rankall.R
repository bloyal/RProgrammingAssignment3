rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcomeData <- readOutcomeData("outcome-of-care-measures.csv")
        stateList <- unique(outcomeData$State)
        
        ## Check that state and outcome are valid
        if(!isValidOutcome(outcome)){
                stop("invalid outcome")
        }
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        results <- data.frame(hospital=character(),state=character())
        
        for(stateItem in stateList){
                hospitalResult <- newrankhospital(stateItem, outcome, outcomeData, num)
                results <- rbind(results,data.frame(hospital=hospitalResult,state=stateItem))
        }
        results <- results[order(results["state"]) , ]
}

newrankhospital <- function(state, outcome, outcomeData, num = "best") {
        
              
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        stateData <- getStateData(state, outcomeData)
        stateData <- stateData[order(stateData[outcome],stateData["name"]) , ]
        stateData <- stateData[complete.cases(stateData[outcome]),]
        
        num <- translateNum(num,nrow(stateData))
        
        if(num>nrow(stateData)){
                return(NA)
        }
        
        stateData[num,1]
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

translateNum <- function(num, dfLength){
        if (num == "best"){
                1                
        }
        else if (num == "worst"){
                dfLength
        }
        else {
                num
        }
}