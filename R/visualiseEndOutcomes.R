visualiseEndOutcomes <- function(parameters, endOutcomes){
  gens <- as.vector(seq(1,parameters$numGen, 1))
  object <- rep(1, parameters$numGen)
  objData <- as.data.frame(cbind(object, endOutcomes$Fi[1,4,], gens, endOutcomes$MY[1,3]))
  chartData <- objData



  for (i in 2:parameters$nObjects){
    object <- rep(i, parameters$numGen)
    objData <- as.data.frame(cbind(object, endOutcomes$Fi[i,4,],gens, endOutcomes$MY[i,3]))
    chartData <- rbind(chartData, objData)

  }

  colnames(chartData) <- c("Object #", "Fit", "Generation", "Group")


  ggplot(chartData)+
    geom_jitter(aes(x = Generation, y = Fit, colour= Group), alpha = 0.2)+
    theme_light()+
    facet_wrap(~Group)

}
