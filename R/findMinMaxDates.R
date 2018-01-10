#' findMinMaxDates
#'
#' Find the minimum and maximum dates for each time series object in an M1 comp object
#'
#' @param y the MComp object
#' @param freqList the frequency list you're dealing with a 2 column matrix with
#' frequency number code (1 = annual, 4 = quarterly, 12 = monthly) in column 1 and
#' name of the frequencies in column 2.
#' @param nObjects the number of objects in the MComp objects you want to check in
#' sequence
#' @param skipList a vector incidcating which objects should be skipped
#'
#' @return a list with minimum and maximum dates for each frequency type
#' @export
#'
#' @examples
#'
#'
#'
findMinMaxDates <- function(y, freqList, nObjects, skipList){
  nFreqs <- nrow(freqList)
  minDate <- matrix(1, nFreqs, 2) # A 3 x 2 matrix. First row: annual series
  # second row: quarterly
  # third row: monthly
  # First column year
  # second column: quarter or month
  minDate[,1] <- 2018       # Earliest date to start with 2018, 1 -> intent to update
  maxDate <- matrix(1, nFreqs, 2) # Same as above
  maxDate[,1] <- 0        # Latest date to begin with 0 AD, 1
  i <- 1
  while(i <= nObjects){

    if (i %in% skipList){ # don't worry about those items that don't have a stated date right now
      i = i + 1
      next
    }
    returnData <- getYData(y[[i]])
    dates <- tsp(returnData$d)

    x <- as.numeric(unlist(strsplit(as.character(dates[1]), "\\.")))


    if (x[1] == 1){
      skipList <- append(skipList, i)
      i = i + 1
      next
    }

    freqObserved <- FALSE
    for (j in 1: nrow(freqList)){
      if(as.integer(dates[3]) == freqList[j,1]){
        freqObserved <- TRUE
        freq <- freqList[j,2]

        dateMinMax <- checkDates(returnData$d, minDate, maxDate, j)
        minDate <- dateMinMax$minDate

        maxDate <- dateMinMax$maxDate
      }
    }
    if (freqObserved <- FALSE){
      cat("Object number: ")
      cat(i, "/n")
      cat("dates: ", dates)
      cat("I'm sorry I don't have a good option for frequency here./n")
      cat("Breaking.")
      break
    }


    i = i + 1
  }

  return(list("minDate" = minDate, "maxDate" = maxDate, "skipList" = skipList))
}
