#' assessWeakLinks
#'
#' Which of the time series needs work?
#'
#' @param bestFit the best fit obtained so far for each time series
#' @param weakLinkPercent  the % of the time series we are interested in with
#' the lowest fits to be examined
#'
#' @return weakLinks, the group of time series that need reassessment
#' @export
#'
#' @examples
#'
#' weakLinks <- assessWeakLinks(bestFit, weakLinkPercent)
#'
assessWeakLinks <- function(bestFit, weakLinkPercent){
  weakLinks <- quantile(bestFit, weakLinkPercent, na.rm = TRUE)
  weakLinks <-which(bestFit[,4] <= weakLinks)
  return(weakLinks)
}
