#' @title Create a data object for multivariate network meta-analysis (MNMA)
#' @description The function creates a data frame for bugs operation, using outcome 1, outcome 2, and treatments inputs
#' @param data1 A data frame for outcome 1. First 2 (or 3) columns contain the information about number of events for each arm and the rest 2 (or 3) colums contain the sample size for each arm. Number of columns is from 2 to 3, depending on the number of maximum arms in the data.
#' @param data2 A data frame for outcome 2, which has the same number of rows and columns.
#' @param TX A data frame for tratments, which has maximum 3 treatments in 3 columns. The number of rows should coincide with data1 and data2.
#' @param noa Maximum number of arms in the trial studies.
#' @param ref reference treatment numbers in the analysis
#' @param parameters 'simple' or 'details' list of parameters.
#' @export
#' @examples
#' data(acute)
#' # Split the dataset
#' data1 <- acute[,c(1:6)]     # outcome1
#' data2 <- acute[,c(7:12)]    # outcome2
#' TX    <- acute[,13:15]
#' # Convert them into log odds ratio inputs
#' data <- uni.model(data1,data2,TX, ref=1, noa="3arm", parameters="simple")
#' data
#'
#' @return mnma.model object for mnma.bugs operation
#' @keywords internal


uni.model <- function(data1, data2, TX, noa = "3arm", ref = 1, parameters = "simple"){
  
  if (noa=="3arm"){
    
    # warnings about the dimensions
    
    colnames(TX) <- c("T1", "T2", "T3")
    
    # transfomrm as events and failures then Add 0.5 for 0 cells -----------------------------
    out1 <- mnma:::add.half.3arm(data1)
    out2 <- mnma:::add.half.3arm(data2)
    #-----------------------------------------------------------------------------------------
    # sorting by na information
    data <- mnma:::sortbyna(out1,out2,TX)
    # ----------------------------------------------------------------------------------------
    # check the number of treatments
    NT <- mnma:::tx.check(TX)
    #-----------------------------------------------------------------------------------------
    NS  <- nrow(data)
    N2h <- sum(is.na(data$T3))
    N1O <- sum(data$noo==1)  # number of missing studies
    # ----------------------------------------------------------------------------------------
    # Split the dataset
    dat1 <- data[,c(1:6)]     # outcome1
    dat2 <- data[,c(7:12)]    # outcome2
    T1<-data$T1
    T2<-data$T2
    T3<-data$T3
    #--------------------------------------------------------------------------------------------
    # Data transformation
    #--------------------------------------------------------------------------------------------
    ref  =  ref
    out1 <- mnma:::cont.trans(dat1, NS=NS, N2h=N2h)
    out2 <- mnma:::cont.trans(dat2, NS=NS, N2h=N2h)
    y1   <- out1$y
    y2   <- out2$y
    var1 <- out1$var
    var2 <- out2$var
    input1 <- list(NS=NS,N2h=N2h,NT=NT,ref=ref,T1=T1,T2=T2,T3=T3,y=y1,var=var1)
    input2 <- list(NS=NS,N2h=N2h,NT=NT,ref=ref,T1=T1,T2=T2,T3=T3,y=y2,var=var2)
  }
  
  #====================================================================================================
  
  if (noa=="2arm"){
    
    # warnings about the dimensions
    
    colnames(TX) <- c("T1", "T2")
    
    # transfomrm as events and failures then Add 0.5 for 0 cells -----------------------------
    out1 <- mnma:::add.half.2arm(data1)
    out2 <- mnma:::add.half.2arm(data2)
    #-----------------------------------------------------------------------------------------
    # sorting by na information
    data <- mnma:::sortbyna.2arm(out1,out2,TX)
    # ----------------------------------------------------------------------------------------
    # check the number of treatments
    NT <- mnma:::tx.check(TX)
    #-----------------------------------------------------------------------------------------
    NS  <- nrow(data)
    N2h <- nrow(data)
    N1O <- sum(data$noo==1)  # number of missing studies
    # ----------------------------------------------------------------------------------------
    # Split the dataset
    dat1 <- data[,c(1:4)]     # outcome1
    dat2 <- data[,c(5:8)]     # outcome2
    T1<-data$T1
    T2<-data$T2
    # T3<-data$T3
    #--------------------------------------------------------------------------------------------
    # Data transformation
    #--------------------------------------------------------------------------------------------
    ref  =  ref
    out1 <- mnma:::cont.trans.2arm(dat1, NS=NS)
    y1   <- out1$y
    var1 <- out1$var
    input1 <- list(NS=NS,N2h=N2h,NT=NT,ref=ref,T1=T1,T2=T2,y=y1,var=var1)
    #----------------
    out2 <- mnma:::cont.trans.2arm(dat2, NS=NS)
    y2   <- out2$y
    var2 <- out2$var
    input2 <- list(NS=NS,N2h=N2h,NT=NT,ref=ref,T1=T1,T2=T2,y=y2,var=var2)
  }
  
  if (parameters=="simple"){
    parameters <- c( "dOUT1", "psi1")
  } else if (parameters=="detail"){
    parameters <- c( "ddOUT1", "Eff.O1", "psi1")
  } else if (parameters!="detail" & parameters!="simple"){
    stop("Choose either 'simple' or 'detail' for the list of parameters")
  }
  
  source <- list(data1 = input1, data2 = input2, parameters = parameters, noa=noa)
  class(source) <- "mnma.model"
  return(source)
}


