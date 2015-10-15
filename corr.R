corr <- function(directory, threshold = 0){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Returns a numeric vector of correlations
    
    complete_cases <- complete(directory)
    complete_cases_over_threshold <- as.data.frame(complete_cases[complete_cases$nobs > threshold,])
    count <- nrow(complete_cases_over_threshold)
    if(count > 0){
        corrs <- vector("numeric", length = count)
        for(i in 1:count){
            id <- complete_cases_over_threshold$id[[i]]
            corr_data <- read.csv(paste(directory, "/", sprintf("%03d", complete_cases_over_threshold[i,"id"]), ".csv", sep=""))
            corr_data <- corr_data[complete.cases(corr_data),]
            corrs[i] = cor(corr_data$sulfate, corr_data$nitrate)
        }
    }
    else{
        corrs <- vector("numeric", length = count)
    }
    corrs
}