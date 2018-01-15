#' buildTSArray
#'
#' Build an array of time series objects from Mcomp objects,
#' sorted by their frequency.
#'
#' @param y the MComp object with the data
#' @param nObjects the number of objects, a scalar
#' @param endDates an object with components of minDate, maxDate two matrices with
#' minimum and maximum dates for each type of frequency as described by freqList
#' As well as the skipList, the list of objects which did not get used to
#' build this list.
#' @param freqList a tibble with frequency codes and names. First column frequency codes,
#' e.g. 1 = annual, 4 = quarterly, 12 = monthly, second the names as strings.
#' @param skipList a vector incidcating which objects should be skipped

#' @return data, a matrix with all the data arrayed out in rows that match their dates and periods
#' @export
#'
#' @examples
#' nObjects = 1001
#' proposedMaxLength = 10000
#' freqList <- tibble(
#'       freq = c(1, 4, 12),
#'       named = c("annual", "quarterly", "monthly")
#' )
#' arrayData <- buildTSArray(M1, nObjects, dates, freqList))
#'
#'
buildTSArray <- function(y,nObjects, minDate, maxDate, freqList, skipList){
  require(abind)

  nFreqs <- nrow(freqList)
  proposedMaxLength <-findMaxLength(minDate, maxDate, freqList)


  maxLength <- max(as.numeric(proposedMaxLength))
  data <- array(data = NA, dim = c(nObjects, nFreqs, maxLength)) # Three columns, one for each frequency type
  # intent to put the data into the corresponding column as a list
  # first col: annual frequency, second: quarterly, third: monthly
  # Three columns, one for each frequency type
  i <- 1


  while(i <= nObjects){

    if (i %in% skipList){ # don't worry about those items that don't have a stated date right now
      i = i + 1
      next
    }

    returnData <- getYData(y[[i]])

    dates <- tsp(returnData$d)

    x <- as.numeric(unlist(strsplit(as.character(dates[1]), "\\.")))
    skipList2 <- skipList # I'm not even using this but if we don't call skiplist, R complains

    if (x[1] == 1){
      skipList <- append(skipList, i)
      i = i + 1
      next
    }

    freqObserved <- FALSE
    for (j in 1: nrow(freqList)){
      if(dates[3] == freqList[j,1]){
        freqObserved <- TRUE
        freq <- freqList[j,2]
        # find # of periods since earliest start date in this frequency group
        firstItem <- unlist(strsplit(as.character(dates[1]), "\\."))
        secondItem <- unlist(strsplit(as.character(dates[2]), "\\."))
        startYear <- as.numeric(firstItem[1])
        endYear <- as.numeric(secondItem[1])
        if (length(firstItem) == 1){
          startPeriod <- 0
        } else {
          item <- as.numeric(paste("0.",firstItem[2], sep = ""))
          startPeriod <- item[1]
        }
        if (length(secondItem) == 1){
          endPeriod <- 0
        } else {
          item <- as.numeric(paste("0.",secondItem[2], sep = ""))
          endPeriod <- item[1]
        }

        earliestDate <- (c(as.integer(startYear), startPeriod))
        if (earliestDate[1] == minDate[j,1] & earliestDate[2] == minDate[j,2]){
          timeSinceStart <- 1
        } else {
          timeSinceStart <- findMaxLength(minDate[j,], earliestDate, freqList) ##
          timeSinceStart <- as.numeric(timeSinceStart)
        }
        periodsCompleted <- length(returnData$d)
        data[i,j,timeSinceStart:(timeSinceStart + periodsCompleted - 1)] <- (returnData$d)
        break # we've found our frequency
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



  return(data)
}
