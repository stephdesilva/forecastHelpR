#' findMaxLength
#'
#' find the maximum length of a group of dates of different frequencies
#'
#' @param endDates a list with two matrices in particular: with the year in column 1
#' and the fractional part of the year in column 2, minDate and maxDate
#' @param freqList a tibble with frequency codes and names. First column frequency codes,
#' e.g. 1 = annual, 4 = quarterly, 12 = monthly, second the names as strings.
#'
#'
#' @return maxLength, a vector with the same number of frequencies tested
#' @export
#'
#' @examples
#'
#' maxLength <- findMaxLength(endDates, freqList)
#'
#'
findMaxLength <- function(minDate, maxDate){
  n <- nrow(minDate)
  if (is.null(n)){
    n = 1
  }
  maxLength <- vector(mode = "numeric", n)

  if (n == 1){
    minDate <- rbind(minDate)
    maxDate <- rbind(maxDate)
  }
  for (j in 1:n){
      if (maxDate[j,2] < minDate[j,2]){
        years = maxDate[j,1] - minDate[j,1] - 1
        fracYears = maxDate[j,2] - minDate[j,2] + 1
      } else {
        years = maxDate[j,1] - minDate[j,1]
        fracYears = maxDate[j,2] - minDate[j,2]
      }
    maxLength[j] <- years*freqList[j,1] + as.integer(fracYears*freqList[j,1])
  }

return(maxLength)
}
