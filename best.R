best <- function(state, outcome) { 
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    distinctStates = unique(data[,7])
    allOutcome <- c("heart attack", "heart failure", "pneumonia")
    
    if(!state %in% distinctStates)
    {
        stop("Invalid state")
    }
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
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    filteredDataset = data[data$State == state, c(2,colTrack)]
    suppressWarnings(filteredDataset[which.min(filteredDataset[,2]), 1])
}
