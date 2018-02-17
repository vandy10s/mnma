#' @title Generate y for bugs input
#' @description The function generates y for datasets only with 2 arm studies.
#' @param yr for outcome 1
#' @param yd for outcome 2
#' @param NS is total number of studies
#' @return A vector of combined y as a bugs input
#' @keywords internal

# Value combination ========================================================================
# combine values from yr and yd  -----------------------------------------------------------

gen.y.2arm <- function(yr,yd, NS=NS){

  y<-numeric(2*NS)

  for (i in 1:length(yr)){
    y[2*i-1] <-yr[i]
    y[2*i]   <-yd[i]
  }
  return(y)
}



