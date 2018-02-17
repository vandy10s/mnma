#' @title Transform values (events and no events) and add 0.5 in the no zero cell.
#' @description The function creates events and failure type data and add 0.5.
#' @param tmp is the data frame with number of events and number of total sample size
#' @return a matrix with events and failure data with extra 0.5 if needed.
#' @keywords internal

add.half.3arm <- function(tmp){
if (dim(tmp)[2]==6){

  tmp[,4:6] <- tmp[,4:6] - tmp[,1:3]

  for (i in 1:nrow(tmp)){
    if(sum(tmp[i,1:6]==0, na.rm=TRUE)>0 & is.na(tmp[i,3])){    # If there is any zero cell, ~
      tmp[i,c(1,2,4,5)]<- tmp[i,c(1,2,4,5)]+0.5
    }
    if(sum(tmp[i,1:6]==0, na.rm=TRUE)>0 & !is.na(tmp[i,3])){    # If there is any zero cell, ~
      tmp[i,1:6]<- tmp[i,1:6]+0.5
    }
  }
  return(tmp)
}else{
  stop("Data with 3 arm study has to have 6 columns")
}
}
