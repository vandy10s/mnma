#' @title Check the treatment data
#' @description Check the any missing numbering in the treatments and shows the warning sign if needed
#' @param TX is the data frame contains treatment 1,2, or 3 and # of columns have to be half of that of data1.
#' @return It returns the # of treatment.
#' @keywords internal


# Checking TX =============================================================================
tx.check <- function(TX){
  tlist <- TX
  max <- max(tlist, na.rm=TRUE)
  min <- min(tlist, na.rm=TRUE)
  prsnt <- min:max
  
  if (!is.numeric(TX[,1]) | !is.numeric(TX[,2])){
    stop("Treatment numbers need to be numeric")
  }
  
  if (sum(prsnt %in% as.vector(as.matrix(tlist)))!=length(prsnt)){
    # This compares the number of treatments coinciding with the list of treatments.
    # Will check if there are any missing treatment numbers in total treatments.
    stop("Treatment numbers are not supposed to be missing from the beginning to the end")

  }else{
    NT <- (max-min+1)
    return(NT)
  }
}
