#' assess Panels
#'
#' Is it possible we can treat at least part of any time series group as a panel,
#' at least to begin with? Correlation between columns calculated on detrended,
#' deseasonalised data.
#'
#' @param index index describing which of the columns in y should be tested
#' @param y the matrix of time series to estimate
#' @param corThreshold, scalar, the threshold at which we decide something can be
#' in a psuedo panel or not
#'
#' @return groupPanel, excessPanel - which columns in index should be included
#' in the pseudo panel, which should be excluded.
#' @export
#'
#' @examples
#' output <- assessPanels(index, y, corThreshold)
#'
assessPanels <- function(index,y, corThreshold){
  dataDetrend <- {}
  n <- ncol(y)
  notThis <- matrix(NA, nrow = nrow(y), ncol = n)
  assessThis <- matrix(NA, nrow = nrow(y), ncol = n)
  for (i in 1:ncol(y)){
    if (length(unique(y[,i])) < 12 | !(i %in% index)){
      notThis[,i] <- y[,i]
    } else {
      assessThis[,i] <- y[,i]
    }
  }


  for (j in 1:length(index)){
    if (length(unique(assessThis[,i])) < 2){
      dataDetrend <- cbind(dataDetrend, rep(NA, length(assessThis[,i])))
    } else {
      dataDetrend <- cbind(dataDetrend,decomp(assessThis[,j])$remainder)
    }
  }

  groupCorrelation <- cor(dataDetrend, use = "everything")
  groupCorrelation <- diag(groupCorrelation)
  panelMembership <- which(groupCorrelation >= corThreshold)

  groupPanel <- dataGroup[,panelMembership]
  excessPanel <- dataGroup[,-panelMembership]


  return(list("GP" = groupPanel, "EP"= excessPanel, "TS" = tooShort))

}
