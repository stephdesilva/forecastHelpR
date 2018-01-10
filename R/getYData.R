#' getYData
#'
#' A function that retrieves each object from the MComp super object
#'
#' @param item the number of the TS you wish to retrieve from the MComp super object
#'
#' @return list of both the data and future data
#' @export
#'
#' @examples
#' while(i <= nObjects){
#'      returnData <- getYData(y[[i]])
#' }
#'
getYData <- function(item){
  # a function that retrieves each object from the MComp super object
  data <- (item$x)
  forecast <-(item$xx)
  return(list("d" = data,"fc" = forecast))
}
