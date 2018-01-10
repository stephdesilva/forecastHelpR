#' Decomp
#'
#' ## code as per Wang, Smith and Hyndman (2006) Characteristic-based clustering for time series data. Data Mining and Knowledge Discovery, 13(3), 335-364.
# code published here: https://robjhyndman.com/hyndsight/tscharacteristics/
# accessed 13/11/17
#'
#' @param x time series
#' @param transform whether to transform
#'
#' @return transformed series
#' @export
#'
#' @examples
decomp <- function(x,transform=TRUE)
{
  require(forecast)
  # Transform series
  if(transform & min(x,na.rm=TRUE) >= 0)
  {
    lambda <- BoxCox.lambda(na.contiguous(x))
    x <- BoxCox(x,lambda)
  }
  else
  {
    lambda <- NULL
    transform <- FALSE
  }
  # Seasonal data
  if(frequency(x)>1)
  {
    x.stl <- stl(x,s.window="periodic",na.action=na.contiguous)
    trend <- x.stl[["time.series"]][,2]
    season <- x.stl[["time.series"]][,1]
    remainder <- x - trend - season
  }
  else #Nonseasonal data
  {
    require(mgcv)
    tt <- 1:length(x)
    trend <- rep(NA,length(x))
    trend[!is.na(x)] <- fitted(gam(x ~ s(tt)))
    season <- NULL
    remainder <- x - trend
  }
  return(list(x=x,trend=trend,season=season,remainder=remainder,
              transform=transform,lambda=lambda))
}
