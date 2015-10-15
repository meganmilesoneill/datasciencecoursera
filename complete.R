complete <- function(directory, id = 1:332){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    filelist <- sapply(id, function(x) filename <- paste(directory, "/", sprintf("%03d", x), ".csv", sep=""))
    data = lapply(filelist, read.csv)
    mcomplete <- matrix(nrow=length(id), ncol=2)
    for(i in 1:length(id)){
        complete_data_frame <- as.data.frame(data[i])
        mcomplete[i, 1] <- complete_data_frame[1, "ID"]
        mcomplete[i, 2] <- nrow(complete_data_frame[complete.cases(complete_data_frame),])
    }
    colnames(mcomplete) <- c("id", "nobs")
    rownames(mcomplete) <- 1:length(id)
    as.data.frame(mcomplete)
}