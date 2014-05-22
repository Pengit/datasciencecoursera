pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    
    # read in the list of spreadsheets
    files <- list.files(directory)
    
    # determine the demanded files based on IDs
    myFiles <- files[id]
        
    # retrieve raw data from spreadsheets
    data <- numeric(length = 0)
    for (file in myFiles)
    {
        curr <- read.csv(paste(c(directory,file), collapse = "/"))
        #print(curr)
        data <- if(length(data) == 0){
            curr
        } else{
            rbind(data, curr)
        }
    }

    # calculate the mean ignoring NAs in the column
    print(mean(data[complete.cases(data[pollutant]),pollutant]))
}