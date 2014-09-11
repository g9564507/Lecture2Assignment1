corr <- function(directory, threshold = 0) {

 	library(data.table)  # load the package to use fread() 
    
  
    tempCount<-1

    corrVector<- rep(0,332) 
    corrTemp <- 0

	for(i in 1:332){
		
		     files <- list.files( directory )
		     fileName  <- paste(".", directory, files[i],sep="/")
		     tempFile  <- fread(fileName) 
		    
		     good<-complete.cases(tempFile$sulfate,tempFile$nitrate)
		     
		     if(length(tempFile$sulfate[good]) <= threshold){
		     	 corrTemp <- NA
		     }
		     else{
             	corrTemp<-cor(tempFile$sulfate[good],tempFile$nitrate[good])
		     }
		     if(is.na(corrTemp)){
		     	;		     
		     }
		     else{
		     	corrVector[tempCount] <- corrTemp
		     	tempCount<- tempCount +1

		     }

	}
    
    if(tempCount==1){
    	
    	return( numeric() ) 

    }
    
    else{
    	return( corrVector[1:(tempCount-1)] )
    }

}