## R programming assignment 3 Q1

best <- function(state, outcome) {
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
    
    ## Return hospital name in that state with lowest 30-day death rate
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
    
    # do the ranking
    tem[tem[,2] == min(tem[,2]),c(1)]
}