#' @title Transform values (events and no events) and add 0.5 in the no zero cell.
#' @description The function creates events and failure type data and add 0.5 for data including only 2arm studies.
#' @param tmp is the data frame with number of events and number of total sample size
#' @return a matrix with events and failure data with extra 0.5 if needed.
#' @keywords internal

add.half.2arm <- function(tmp){
if (dim(tmp)[2]==4){

  tmp[,3:4] <- tmp[,3:4] - tmp[,1:2]

  for (i in 1:nrow(tmp)){
    if(sum(tmp[i,1:4]==0, na.rm=TRUE)>0 & !is.na(tmp[i,1]) & !is.na(tmp[i,2])){    # If there is any zero cell, ~
      tmp[i,1:4]<- tmp[i,1:4]+0.5
    }
  }
  return(tmp)
}else{
  stop("Data with 2 arm study has to have 4 columns")
}
}
