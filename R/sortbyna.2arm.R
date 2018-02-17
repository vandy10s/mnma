#' @title Sort data by number of arms for trial data only with 2 arm studies.
#' @description The function sort existing data and create a sorted dataset depending on the number of arms
#' @param out1 is the dataframe for outcome 1
#' @param out2 is the dataframe for outcome 2
#' @param TX is the dataframe for treatment
#' @return combined sorted data.frame with additinonal column for number of arms and number of outcome
#' @keywords internal


# Sorting data ============================================================================

sortbyna.2arm <- function(out1,out2,TX){
  temp <- cbind(out1,out2,TX)
  na  <- apply(TX,1, function(y) sum(!is.na(y)))                # row sums of number of NA cells
  noo <- apply(temp[,c(1,5)], 1, function(y) sum(!is.na(y)))     
  temp$na <- with(temp, na)
  temp$noo<- with(temp, noo)
  temp <- temp[order(temp$na),]
  data <- temp
  return(data)
}

