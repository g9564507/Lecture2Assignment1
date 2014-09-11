
complete <- function(directory, id = 1:332) {      
     
     
    library(data.table)  # load the package to use fread() 
 
    
    tempDf <- data.frame(id=rep(0,length(id)), nobs=rep(0, length(id)) )
	
	for(i in 1:length(id)){
		     
		     files <- list.files( directory )
		     fileName  <- paste(".", directory, files[id[i]],sep="/")
		     tempFile  <- fread(fileName) 
             
             good<-complete.cases(tempFile$sulfate,tempFile$nitrate)
		     ## complete cases
		     tempColumn<- tempFile$sulfate[good]

             tempDf$id[i]<- id[i]
             tempDf$nobs[i]<- length(tempColumn)
		              
	}

    return( tempDf ) 



}