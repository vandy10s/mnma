#' @title Run univariate NMA anlaysis
#' @description The function conducts univariate NMA using an mnma.model object as an input
#' @param input mnma.model class data, which can be generated using mnma.model function
#' @param inits a list with n.chains element (See \code{\link{bugs}})
#' @param n.iter number of total iterations per chain (default: 5000)
#' @param n.burnin length of burnin per chain (default: 2000)
#' @param n.chains number of Markov chains (default: 1)
#' @param n.thin  Thining (default: 1)
#' @param debug Openbugs is closed automatically when finishing running the bugs script with the FALSE setting (default).
#' @param codaPkg if FALSE (default) a bugs object is returned. (See and check \code{\link{bugs}})
#' @import R2OpenBUGS
#' @export
#' @seealso \code{\link{bugs}}
#' @return mnma.result class bugs summary result
#' @examples \dontrun{
#' # Run after data transformation using mnma.model function.
#' res <- unma.run(data, n.iter=3000, n.burnin=1000)
#' res$outcome1$summary
#' res$outcome2$summary
#' }
#'
#'


unma.run <- function(input, inits=NULL, n.iter=5000, n.burnin=2000, n.chains=1, n.thin=1, debug=FALSE, codaPkg=FALSE){
  
  input <- input$univariate
  
  if (input$noa=="3arm"){
    models = model.file = system.file("unma_3arm.txt",package="mnma")  # Fix the location
  } else if (input$noa=="2arm"){
    models = model.file = system.file("unma_2arm.txt",package="mnma")  # Fix the location
  } 
  
  if ( class(input) == "mnma.model"){
    #-------------------------------------------------------------------------------------------------
    outcome1 <- as.list(input$data1)
    
    out.uni1 <- bugs(data=outcome1,
                     inits=inits,
                     parameters.to.save= input$parameters,
                     n.iter=n.iter,
                     n.burnin=n.burnin,
                     n.chains=n.chains,
                     model.file = models,
                     debug=debug,
                     codaPkg=codaPkg,
                     DIC = FALSE  # To have the results, we need to set this off.
    )
    
    #-------------------------------------------------------------------------------------------------  
    outcome2 <- as.list(input$data2)  
    
    out.uni2 <- bugs(data=outcome2,
                     inits=inits,
                     parameters.to.save= input$parameters,
                     n.iter=n.iter,
                     n.burnin=n.burnin,
                     n.chains=n.chains,
                     model.file = models,
                     debug=debug,
                     codaPkg=codaPkg,
                     DIC = FALSE  # To have the results, we need to set this off.
    )
    
  }else{
    stop("Input data have to be 'mnma.model' class ")
  }
  
  output <- list(outcome1=out.uni1, outcome2=out.uni2)
  class(output) <- "mnma.result"
  return(output)
}



