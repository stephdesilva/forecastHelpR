retrieveDates <- function(source, parameters){


  endDates <- findMinMaxDates(M1, parameters$freqList, parameters$nObjects, parameters$skipList)
  # skipList has objects that don't have a formalised start date


  minDate <- endDates[[1]]

  maxDate <- endDates[[2]]

  returnList <- list("endDates" = endDates,
                     "minDate" = minDate,
                     "maxDate" = maxDate,
                     "dataRetrieved" = source)

  return(returnList)
}
