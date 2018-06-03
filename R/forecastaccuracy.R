#' Input predictions and observations
#'
#' A function to calculate accuracy of timeseries forecasting models
#' Created by Sulekha Aloorravi
#'
#' @param infile Predictions and Observations
#' @return Forecasting Model Accuarcy in percentage
#' @export
forecastaccuracy <- function(pred,obs){
  acc <- c()
  n = length(obs)
  for (i in 1:n){
    singleacc <- ((1-abs((obs[i] - pred[i]))/obs[i])*100)
    acc[[i]] <- singleacc
  }
  return (sum(acc)/n)
}
