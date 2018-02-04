setParameters <- function(){

  # parameters
  skipList <- c(51, 217, 218, 493, 494, 223, 442, 548, 549, 550, 551,
                552, 553, 554, 555, 556, 557, 558, 559)
  nObjects <- 1001
  numGen <- 100
  maxCluster <- 3
  numFitSources<- 7
  weakLinkPercent <- 0.005
  permuteParam <- c(0.9, 0.3) # two paramters, first for model switching, second for Box Cox switching

  freqList <- tibble(
    freq = c(1, 4, 12),
    named = c("annual", "quarterly", "monthly")
  )

  # list of things in the metadata for each series

  IC <- c("AIC", "AICc", "BIC", "HQ", "RSS")
  measures <- c("MAPE", "MASE", "MdAPE", "MdASE")
  features <- c("frequency", "trend", "seasonal", "autocorrelation",
                "non-linear", "skewness",
                "kurtosis", "Hurst", "Lyapunov",
                "dc autocorrelation", "dc non-linear", "dc skewness",
                "dc kurtosis")
  meta <- c("clusterNumber", "rank", "gensSinceImprovement",
            "modelChoice")

  features <- c(IC, measures, features, meta)
  modelList <- list(
    arima = function(x) auto.arima(x, stepwise=FALSE, approximation=FALSE),
    nnetar = function(x) nnetar(x),
    meanforecast = function(x) meanf(x, h),
    naive = function(x) rwf(x, h),
    seasonalNaive = function(x) snaive(x, h),
    rwDrift = function(x) rwf(x, h, drift=TRUE),
    simpleExpSmoothing = function(x) ses(x, h),
    holtMethod = function(x) holt(x, h),
    holtDamped = function(x) holt(x, damped = TRUE, phi = 0.9, h),
    hwSeasonalAdditive = function(x) hw(x, seasonal = "additive"),
    hwSeasonalMultiplicative = function(x) hw(x, seasonal = "multiplicative"),
    hwDSeasonalAdditive = function(x) hw(x, seasonal = "additive", damped = TRUE),
    hwDSeasonalMultiplicative = function(x) hw(x, seasonal = "multiplicative", damped = TRUE),
    bagETS = function(x) baggedETS(x)
  )
  h = 5

  lastSlot <- length(features)


  returnList <- list("skipList" = skipList,
                     "nObjects" = nObjects,
                     "numGen" = numGen,
                     "maxCluster" = maxCluster,
                     "numFitSources" = numFitSources,
                     "weakLinkPercent" = weakLinkPercent,
                     "permuteParam" = permuteParam,
                     "features" = features,
                     "freqList" = freqList,
                     "modelList" = modelList,
                     "h" = h,
                     "lastSlot" = lastSlot)
  return(returnList)
}
