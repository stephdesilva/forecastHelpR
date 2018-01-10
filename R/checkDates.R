#' checkDates
#'
#' @param data the data you're checking for dates, should be TS objects
#' @param minDate proposed minimum date to start with
#' @param maxDate proposed maximum date to start with
#' @param freqN the frequency of the data you're dealing with
#'
#' @return list with matrix of minimum and maximum dates according to each frequency
#' @export
#'
#' @examples
#'
#' findDates <- checkDates(data, minDate, maxDate, freqN)
#'
#'
checkDates <- function(data, minDate, maxDate, freqN){
  # a function that checks whether a time series object has coverage before current
  # minimum date or after current maximum date
  # data, the time series object
  # minDate, current minimum date
  # maxDate, current max date
  # freqN, number indicating slot of freq of current TS object
  dates <- tsp(data)

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


  # start dates
  if (startYear < minDate[freqN,1]){
    minDate[freqN,1] <- startYear
    minDate[freqN,2] <- startPeriod
  } else if (startYear == minDate[freqN,1]){
    if (startPeriod < minDate[freqN,2])
      minDate[freqN,2] = startPeriod
  }

  # finish dates
  if (endYear > maxDate[freqN,1]){
    maxDate[freqN,1] <- endYear
    maxDate[freqN,2] <- endPeriod
  } else if (endYear == maxDate[freqN,1]){
    if (endPeriod > maxDate[freqN,2])
      maxDate[freqN,2] = endPeriod
  }

  return(list("minDate" = minDate, "maxDate" = maxDate))

}
