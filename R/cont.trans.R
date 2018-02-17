#' @title Transform values and add 0.5
#' @description This function create events and failure type data
#' @param tmp is the data file
#' @param NS is total number of studies
#' @param N2h is number of studies with only 2 arms
#' @return a matrix with log odds ratios
#' @keywords internal


cont.trans <- function(tmp, NS=NS, N2h=N2h){

  NS <- nrow(tmp)
  X<-matrix(NA, nrow = nrow(tmp), ncol = 4)
  colnames(X) <- c("y21", "y31", "se21", "se31")

  for (i in 1:NS){

    med<-c(0,1)

    if (!is.na(tmp[i,2])){   #r2
      m1<-matrix(c(tmp[i,1],tmp[i,4],tmp[i,2],tmp[i,5]),ncol=2, byrow=TRUE)
      res <- glm(m1~med, family=binomial())
      X[i,1]<-summary(res)$coefficients[2,1]  # For y21
      X[i,3]<-summary(res)$coefficients[2,2]  # For se21
    }

    if (!is.na(tmp[i,3])){   #r3
      m2<-matrix(c(tmp[i,1],tmp[i,4],tmp[i,3],tmp[i,6]),ncol=2, byrow=TRUE)
      res2 <- glm(m2~med, family=binomial())
      X[i,2]<-summary(res2)$coefficients[2,1]
      X[i,4]<-summary(res2)$coefficients[2,2]
    }
  }

  X<-as.data.frame(X)

  # ---------------------------------------------------------------------------
  # Creating varr

  varr <- yr <- numeric(N2h+2*(NS-N2h))

  varr[1:N2h] <- X$se21[1:N2h] ## plug in

  if (NS > N2h) {
    for (i in 1:(NS-N2h)){
      varr[N2h+2*i-1] <-X$se21[N2h+i]
      varr[N2h+2*i]   <-X$se31[N2h+i]
    }
  }

  varr[is.na(varr)] <- 100000; varr<-varr^2

  # Creating yr
  yr[1:N2h] <- X$y21[1:N2h] ## plug in

  if (NS > N2h) {
    for (i in 1:(NS-N2h)){
      yr[N2h+2*i-1] <- X$y21[N2h+i]
      yr[N2h+2*i]   <- X$y31[N2h+i]
    }
  }

  return(list(y=yr, var= varr))
}
