#' @title Acute mania treatment dataset for efficacy and toleratiblity outcomes
#'
#' @description A dataset of 65 trials comparing 14 treatments for acute mania treatment. The dataset contains response and dropout rate information, both of which are binary outcome, along with treatments information.
#'
#' @format A multivarite network-meta analysis dataset containig 65 rows with 18 columns. First 12 columns includes information about events and sample size of each arm for both outcomes. Last 6 columns contains treatments information with treatment name and corresponding number. For example, \code{r1} and \code{rt1} stand for number of events and sample size of \code{T1} (arm 1) in response outcome, respectively. So do \code{d} and \code{dt} in dropout outcome.
#'
#' @usage data(acute)
#'
#' @source Efthimiou, O., Mavridis, D., Riley, R.D., Cipriani, A. and Salanti, G., 2014. \emph{Joint synthesis of multiple correlated outcomes in networks of interventions}. Biostatistics, 16(1), pp.84-97.
"acute"
