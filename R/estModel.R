estModel <- function(modelList, choiceModel, estY,h){


  accFit <- tryCatch({
    accuracy(modelList[[choiceModel]](estY))
    }, error = function(estY){
      rep(10000,7) # revert to stupidly high fit function
    })


  # if (choiceModel %in% c("arima", "nnetar")){
  #   accFit <- accuracy(modelList[[choiceModel]](estY))
  # } else {
  #   accFit <- accuracy(modelList[[choiceModel]](estY))
  # }





  # if (choiceModel == "arima"){
  #   accFit <- accuracy(auto.arima(estY))
  # } else if (choiceModel == "ann") {
  #   accFit <- tryCatch({
  #     accuracy(nnetar(estY))
  #   }, error = function(err){
  #     print("Couldn't manage nnetar.")
  #     print("Switching to ARIMA.")
  #     accuracy(auto.arima(estY))
  #     choiceModel <- "arima"
  #   })
  #
  # } else {
  #   print ("Arrgggh don't have that model")
  #   break
  # }

  return(accFit)

}
