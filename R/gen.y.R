#' @title Generate y for bugs input
#' @description The function generates y for bivariate anlaysis based on NS and N2h
#' @param yr for outcome 1
#' @param yd for outcome 2
#' @param NS is total number of studies
#' @param N2h is number of studies with only 2 arms.
#' @return A vector of combined y as a bugs input
#' @keywords internal

# Value combination ========================================================================
# combine values from yr and yd  -----------------------------------------------------------

gen.y <- function(yr,yd, NS=NS, N2h=N2h){

  y<-numeric(2*(N2h+2*(NS-N2h)))

  for (i in 1:length(yr)){
    y[2*i-1] <-yr[i]
    y[2*i]   <-yd[i]
  }
  return(y)
}



