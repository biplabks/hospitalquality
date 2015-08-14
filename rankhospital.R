rankhospital <- function(state, outcome, num = "best") 
{   
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
    
    ## Return hospital name in that state with the given rank ## 30-day death rate
    suppressWarnings(data[, colTrack] <- as.numeric(data[, colTrack]))
    filteredDatasetbyState = data[data$State == state, c(2,colTrack)] ##filter data by state
    filteredDatasetbyState = na.omit(filteredDatasetbyState) ##omit NA values 
    totRows = nrow(filteredDatasetbyState) ##calculating number of rows in the dataset
    if(num == "best")
    {
        numTrack = 1
    }
    else if(num == "worst")
    {
        numTrack = totRows
    }
    else if(num > totRows)
    {
        return (NA)
    }
    else
    {
        numTrack = num
    }
    
    orderData = order(filteredDatasetbyState[,2], filteredDatasetbyState[,1])
    filteredDatasetbyState[orderData, ][numTrack, 1]
}
