#' @title Transform values into LOR values
#' @description This function creates events and failure type data
#' @param tmp is the data file
#' @param NS is total number of studies
#' @return a matrix with log odds ratios
#' @keywords internal


cont.trans.2arm <- function(tmp, NS=NS){

  NS <- nrow(tmp)
  X<-matrix(NA, nrow = nrow(tmp), ncol = 2)
  colnames(X) <- c("y21", "se21")

  for (i in 1:NS){

    med<-c(0,1)

    if (!is.na(tmp[i,2])){   #r2
      m1<-matrix(c(tmp[i,1],tmp[i,3],tmp[i,2],tmp[i,4]),ncol=2, byrow=TRUE)
      res <- glm(m1~med, family=binomial())
      X[i,1]<-summary(res)$coefficients[2,1]  # For y21
      X[i,2]<-summary(res)$coefficients[2,2]  # For se21
    }
  }

  X<-as.data.frame(X)

  # ---------------------------------------------------------------------------
  # Creating varr

  varr <- yr <- numeric(NS)

  varr[1:NS] <- X$se21[1:NS] ## plug in

  varr[is.na(varr)] <- 100000; varr<-varr^2

  # Creating yr
  yr[1:NS] <- X$y21[1:NS] ## plug in

  return(list(y=yr, var= varr))
}
