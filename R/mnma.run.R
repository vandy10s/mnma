#' @title Run multivariate NMA anlaysis
#' @description The function conducts bivariate NMA using a mnma.model object as an input
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
#' @references Efthimiou et al. (2015) Biostatistics 16(1):84-97
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/24992934}{PubMed})
#' @seealso \code{\link{bugs}}
#' @return mnma.result class bugs result. See \code{\link{bugs}} for details.
#' @examples \dontrun{
#' # Run after data transformation using mnma.model function.
#' res <- mnma.run(data, n.iter=3000, n.burnin=1000)
#' res$summary
#' }
#'
#'


mnma.run <- function(input, inits=NULL, n.iter=5000, n.burnin=2000, n.chains=1, n.thin=1, debug=FALSE, codaPkg=FALSE){

  input <- input$multivariate

  if (input$noa=="3arm"){
    models = model.file = system.file("bnma_3arm.txt",package="mnma")  # Fix the location
  } else if (input$noa=="2arm"){
    models = model.file = system.file("bnma_2arm.txt",package="mnma")  # Fix the location
  }

  if ( class(input) == "mnma.model"){

    data <- as.list(input$data)

    out.mul <- bugs(data=data,
                    inits=inits,
                    parameters.to.save= input$parameters,
                    n.iter=n.iter,
                    n.burnin=n.burnin,
                    n.thin=n.thin,
                    n.chains=n.chains,
                    model.file = models,
                    debug=debug,
                    codaPkg=codaPkg,
                    DIC = FALSE  # To have the results, we need to set this off.
    )

    class(out.mul) <- "mnma.result"
  }else{
    stop("Input data have to be 'mnma.model' class ")
  }
  return(out.mul)
}



