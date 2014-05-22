corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    files <- list.files(directory)
    
    ret <- numeric(length = 0)
    for (file in files){
        curr <- read.csv(paste(c(directory,file), collapse = "/"))
        if(sum(complete.cases(curr["sulfate"]) & complete.cases(curr["nitrate"])) > threshold){
            ret <- if(length(ret) == 0){
                ret <- cor(curr["sulfate"], curr["nitrate"], use = "pairwise.complete.obs")
            } else{
                ret <- c(ret, cor(curr["sulfate"], curr["nitrate"], use = "pairwise.complete.obs"))
            }
        }
    }
    print(ret)
}