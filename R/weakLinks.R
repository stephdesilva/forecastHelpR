weakLinks <- function(wlPcnt, metaY, bestFit){
    fit <- cbind(metaY, bestFit)
    fit <- group_by(fit, frequency, group)
    freqTypes <- unique(fit$frequency)

    groups <- unique(fit$group)

    weakLinks <- list()
    for (f in 1:length(freqTypes)){
      for (g in 1:length(groups)){
        freq <- freqTypes[f]
        indvidualGroup <- groups[g]

        fitFG <- filter(fit, group == indvidualGroup & frequency == freq)
        weakLink <- quantile(fitFG$bestFit, wlPcnt)
        weakObjects <- filter(fitFG, bestFit <= weakLink)
        weakObjects <- as.matrix(weakObjects[,1])
        weakLinks <- append(weakLinks, weakObjects)
      }
    }

    return(weakLinks)
}
