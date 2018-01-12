estModel <- function(choiceModel, estY){
  if (choiceModel == "arima"){
    accFit <- accuracy(auto.arima(estY))
  } else if (choiceModel == "ann") {
    accFit <- tryCatch({
      accuracy(nnetar(estY))
    }, error = function(err){
      print("Couldn't manage nnetar.")
      print("Switching to ARIMA.")
      accuracy(auto.arima(estY))
      choiceModel <- "arima"
    })

  } else {
    print ("Arrgggh don't have that model")
    break
  }

  return(accFit)

}
