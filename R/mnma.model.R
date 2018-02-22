#' @title Create a data object for multivariate and univariate network meta-analysis (NMA)
#' @description The function generates a data object for bugs operation, using outcome 1, outcome 2, and treatments inputs
#' @param outcome1 A data frame for outcome 1. First 2 (or 3) columns contain the information about number of events for each arm and the rest 2 (or 3) columns contain the sample size for each arm. Number of columns ranges from 2 to 3, depending on the number of maximum arms in the trial data.
#' @param outcome2 A data frame for outcome 2, which has the same dimensions as outcome1.
#' @param TX A data frame for tratments, which has maximum 3 treatment arms in 3 columns. The number of rows should coincide with outcome1 and outcome2. Each element needs to be numeric.
#' @param noa Maximum number of arms in the trial studies, which is either '2arm' or '3arm'. Note that number of arm is different from number of total treatments in the network.
#' @param ref Reference treatment in the analysis in a numeric class.
#' @param parameters List of parameters to infer in the analysis. They are either 'simple' or 'details' depending on comparisons.
#' @export
#' @examples
#' data(acute)
#' # Split the dataset
#' outcome1 <- acute[,c(1:6)]     # outcome1
#' outcome2 <- acute[,c(7:12)]    # outcome2
#' TX    <- acute[,13:15]
#' # Convert them into log odds ratio inputs
#' data <- mnma.model(outcome1,outcome2,TX, ref=1, parameters="simple")
#' data
#'
#' @details A 'simple' list for parameters includes log odds ratio (comparison) parameters between reference and all other treatments. A 'detail' list for parameters includes log odds ratio parameters between all treatments. Both cases include a correlation parameter and heterogeneity parameters for two outcomes.
#' @return An object of class 'mnma.model' that contains data information and parameter lists to estimate for multivariate and univariate NMA.


mnma.model <- function(outcome1, outcome2, TX ,noa="3arm", ref=1, parameters="simple"){

  mulv <- mnma:::mul.model(data1=outcome1, data2=outcome2, TX=TX, ref=ref, noa=noa, parameters=parameters)
  univ <- mnma:::uni.model(data1=outcome1, data2=outcome2, TX=TX, ref=ref, noa=noa, parameters=parameters)

  source <- list(multivariate=mulv, univariate=univ)
  class(source) <- "mnma.model"
  return(source)

}


