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
#'  @param dataGroup the data for the group we're testing
#'
#' @return choiceModel, the character choice of the model we've found best options for.
#' @export
#'
#' @examples
#'
#' modelChoice <- assessInitialModel(data)
#'
assessInitialModel <- function(dataGroup, modelList, h){

     if (is.null(ncol(dataGroup))){
      pick <- 1
      pick <- dataGroup
    } else {
      pick <- as.integer(runif(1, 1, ncol(dataGroup) + 1))
      pick <- dataGroup[,pick]
    }

    pick <- pick[complete.cases(pick)]

    n = length(modelList)
    initialFit <- vector(mode = "numeric", length = n)

    for (i  in 1:n){
      x <- estModel(modelList, names(modelList)[i], pick,h)
      initialFit[i] <- x[4]
    }

    minFit <- which.min(initialFit)
    choiceModel <- names(modelList)[minFit]

    # fitARIMA.pick <- estModel("arima", pick) # this should be a functional of some kind
    # fitANN.pick <- estModel("nnetar", pick)
    # fitMean.pick <- estModel("meanforecast", pick)
    #
    #
    #
    # if (fitARIMA.pick[4] <= fitANN.pick[4]){
    #   choiceModel <- "arima"
    # } else {
    #   choiceModel <- "nnetar"
    # }

  return(choiceModel)
}
