## R programming assignment 3 Q2

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if(!(state %in% data[,"State"])){
        stop("invalid state")
    }
    
    disease <- c("heart attack","heart failure","pneumonia")
    
    if(!(outcome %in% disease)){
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank 30-day death rate
    # extract rows of the state and disease
    temp <- if(outcome == "heart attack"){
        #print("a")
        data[data[["State"]]==state, c(2,11)]
    }
    else if(outcome == "heart failure"){
        #print("b")
        data[data[["State"]]==state, c(2,17)]
    }
    else{
        #print("c")
        data[data[["State"]]==state, c(2,23)]
    }
    
    
    # convert to numeric and exclude NAs
    temp[,2] <- as.numeric(temp[,2])
    tem <- temp[complete.cases(temp),]
    
    # get the ranking
    if(num == "best"){
        tem[tem[,2] == min(tem[,2]),c(1)]
    }
    else if(num == "worst"){
        tem[tem[,2] == max(tem[,2]),c(1)]
    }
    else{
        if(as.numeric(num) > nrow(temp)){
            # invalid num
            return(NA)
        }
        ranking <- order(tem[,2], tem[,1])
        tem[ranking[num], c(1)]
    }
    
}