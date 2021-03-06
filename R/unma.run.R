#' @title Run univariate network meta-analysis (NMA) anlaysis
#' @description The function conducts univariate NMA using a \code{mnma.model} object as an input. It uses \bold{OpenBUGS} software (through the \code{R2OpenBUGS} package).
#' @param input \code{mnma.model} class data, which can be generated using \code{mnma.model} function
#' @param inits a list of initial values for \bold{OpenBUGS} model with \code{n.chains} elements; If \code{inits=NULL} (default), initial values are randomly generated by \bold{OpenBUGS}.
#' @param n.iter number of total iterations per chain (default: 5000)
#' @param n.burnin length of burnin per chain (default: 2000)
#' @param n.chains number of Markov chains (default: 1)
#' @param n.thin  thining (default: 1)
#' @param debug if FALSE (default), \strong{OpenBUGS} is closed automatically after running the \code{unma.run} script. If TRUE, \strong{OpenBUGS} remains open with additional information.
#' @param codaPkg if FALSE (default), a \code{bugs} object is returned. Otherwise, file names of \strong{OpenBUGS} output are returned with TRUE.
#' @import R2OpenBUGS
#' @export
#' @references Lu and Ades (2006) Journal of the American Statistical Association 101(474): 447-459
#' (\href{https://www.tandfonline.com/doi/abs/10.1198/016214505000001302}{doi: 10.1198/016214505000001302})
#'
#' Dias et al. (2010) Statistics in Medicine 29(7-8): 932-944
#' (\href{http://onlinelibrary.wiley.com/doi/10.1002/sim.3767/full}{doi: 10.1002/sim.3767})
#' @seealso \code{\link[R2OpenBUGS]{bugs}}
#' @return \code{mnma.result} class \code{bugs} result. See \code{\link[R2OpenBUGS]{bugs}} for details.
#' @examples \dontrun{
#' # Run after data transformation using mnma.model function.
#' res <- unma.run(data, n.iter=3000, n.burnin=1000)
#' names(res$outcome1)
#' # provides mean, standard deviation, and percentile information about log odds ratio
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



