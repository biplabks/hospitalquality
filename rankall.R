rankall <- function(outcome, num = "best") { 
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    suppressWarnings(data[, 11] <- as.numeric(data[, 11]))
    suppressWarnings(data[, 17] <- as.numeric(data[, 17]))
    suppressWarnings(data[, 23] <- as.numeric(data[, 23]))
    
    ## Check that state and outcome are valid
    allOutcome <- c("heart attack", "heart failure", "pneumonia")
    if(!outcome %in% allOutcome)
    {
        stop("Invalid outcome")
    }
    
    if(outcome == "heart attack")
    {
        colTrack = 11
    }
    else if(outcome == "heart failure")
    {
        colTrack = 17
    }
    else
    {
        colTrack = 23
    }
    
    ## For each state, find the hospital of the given rank
    tableDfr <- data.frame(State = names(tapply(data$State, data$State, length)), Freq = tapply(data$State, data$State, length))
    rownames(tableDfr) <- NULL
    
    nameChr <- character(0)
    #distinctStates = unique(data[,7])
    
    for(state in tableDfr$State)
    {
        stateDfr <- data[data$State == state, ]
        stateDfr <- stateDfr[complete.cases(stateDfr[, colTrack]), ]
        stateDfr <- stateDfr[order(stateDfr[, colTrack], stateDfr$Hospital.Name), ]
        totRows <- nrow(stateDfr)
        
        if(num == "best")
        {
            numTrack = 1
        }
        else if(num == "worst")
        {
            numTrack = totRows
        }
        else
        {
            suppressWarnings(numTrack <- as.numeric(num))
        }
        
        nameChr <- c(nameChr, stateDfr[numTrack, ]$Hospital.Name)
    }
    ## Return a data frame with the hospital names and the ## (abbreviated) state name
    return(data.frame(hospital = nameChr, state = tableDfr$State))
}
