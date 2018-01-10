#' assessInitialModel
#'
#' Out of a series of possible model choices, which is the best one for a random
#' member of the group? Use this as a starting point.
#' Plan sample randomly a number from each cluster, estimate
#' in all the ways contended, compare the MAPSE for each and decided on the best
#' in each group.
#' Current choices: automated arima or nnetar
#' Possible future choices: arch + garch
#'
#' @param numberGroupTests how many times are we going to test in each group?
#' So far, it only keeps one choice!
#' @param dataGroup the data for the group we're testing
#'
#' @return choiceModel, the character choice of the model we've found best options for.
#' @export
#'
#' @examples
#'
#' modelChoice <- assessInitialModel(numberTests, data)
#'
assessInitialModel <- function(numberGroupTests, dataGroup){

  for (k in 1: numberGroupTests){
    if (is.null(ncol(dataGroup))){
      pick <- 1
      pick <- dataGroup
    } else {
      pick <- as.integer(runif(1, 1, ncol(dataGroup) + 1))
      pick <- dataGroup[,pick]
    }

    fitARIMA.pick <- accuracy(auto.arima(pick)) # this should be a functional of some kind
    fitANN.pick <- accuracy(nnetar(pick))

    if (fitARIMA.pick[4] <= fitANN.pick[4]){
      choiceModel <- "arima"
    } else {
      choiceModel <- "ann"
    }

  }

  return(choiceModel)
}
