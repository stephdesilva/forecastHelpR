estModel <- function(modelList, choiceModel, estY,h){


  accFit <- tryCatch({
    accuracy(modelList[[choiceModel]](estY))
    }, error = function(estY){
      rep(10000,7) # revert to stupidly high fit function
    })




  return(accFit)

}
