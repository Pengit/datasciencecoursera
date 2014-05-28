## R programming assignment 1

complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    # read in the list of spreadsheets
    files <- list.files(directory)
    myFiles <- files[id]
    
    nobs <- numeric(length = 0)
    for (file in myFiles){
        curr <- read.csv(paste(c(directory,file), collapse = "/"))
        nobs <- if(length(nobs) == 0){
            sum(complete.cases(curr["sulfate"]) & complete.cases(curr["nitrate"]))
        } else{
            c(nobs,sum(complete.cases(curr["sulfate"]) & complete.cases(curr["nitrate"])))
        }
    }
    ret <- data.frame(id = id, nobs = nobs)
    print(ret)
}