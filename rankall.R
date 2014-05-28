## R programming assignment 3 Q3

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    disease <- c("heart attack","heart failure","pneumonia")
    
    if(!(outcome %in% disease)){
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    # split based on state & get relevant column
    seg <<- if(outcome == "heart attack"){
        #print("a")
        split(data[, c(2,7,11)], data$State)
    }
    else if(outcome == "heart failure"){
        #print("b")
        split(data[, c(2,7,17)], data$State)
    }
    else{
        #print("c")
        split(data[, c(2,7,23)], data$State)
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    # construct the data frame
    rankResult <- data.frame()
    d <- character(length = 0)
    test <- character(length = 0)
    
    # loop through every state
    for(i in 1 : length(seg)){
        temp <- seg[[i]]
        
        # convert to numeric and exclude NAs
        temp[,3] <- as.numeric(temp[,3])
        tem <<- temp[complete.cases(temp[,3]),]
        
        ret<- if(num == "best"){
            #print("a")
            ranking <- order(tem[,3], tem[,1])
            tem[ranking[1], c(1,2)]
        }
        else if(num == "worst"){
            #print("b")
            ranking <- order(tem[,3], tem[,1], decreasing = TRUE)
            tem[ranking[1], c(1,2)]
        }
        else{
            #print("c")
            if(as.numeric(num) > nrow(tem)){
                #print(unique(tem$State))
                # invalid num
                data.frame(Hospital.Name = NA, State = unique(tem$State))
                #c(Hospital.Name = "gaga", State = unique(tem$State))
            }
            else{
                ranking <- order(tem[,3], tem[,1])
                tem[ranking[num], c(1,2)]
                #print(class(tem[ranking[num], c(1,2)]))
            }
        }
        d <- rbind(d, ret)
        #print(i)
        test <- rbind(test,seg[[i]]$State[1])
    }
    #     class(d)
    #     names(d)
    #     head(d)
    #   length(unique(test))
    
    #d
    #d[[1]]
    
    #print(nrow(d[2]))
    #print(nrow(test))
    rankResult <- data.frame(hospital = d[1], state = d[2], row.names = d[[2]])
    names(rankResult) <- c("hospital","state")
    rankResult
}