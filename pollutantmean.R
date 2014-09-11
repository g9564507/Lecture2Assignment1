
pollutantmean <- function(directory, pollutant, id = 1:332) {      
     
    library(data.table)  # load the package to use fread() 
    tempSum<-0
    tempCount<-0

	for(i in 1:length(id)){
		
		     files <- list.files( directory )
		     fileName  <- paste(".", directory, files[id[i]],sep="/")
		     tempFile  <- fread(fileName) 
		     tempColumn<- tempFile[ ][[pollutant]]
		     tempSum   <- sum(tempColumn[!is.na(tempColumn)]) + tempSum 
             tempCount <- length(tempColumn[!is.na(tempColumn)]) +  tempCount

	}

    return( round(tempSum/tempCount, digit=3) ) 




}

