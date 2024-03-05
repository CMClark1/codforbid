#'@title DO_RATIOS Landings Multiplier (Gavaris et al. 2007)
#'@description Calculate the ratio of cod:had on observed vs. unobserved trips
#'@param df A dataframe of trips with numeric variables for cod landings and  haddock landings (called COD and HAD), and a character (Y/N) variable for whether the trip was observed (called OBS).
#'@param cod.field The field in the dataframe with the cod landings weight values. The default is "COD."
#'@param had.field The field in the dataframe with the haddock landings weight values. The default is "HAD."
#'@return a data.frame with landings multiplier
#'@examples
#'example1 <- DO_RATIOS(df)
#'example2 <- DO_RATIOS(df=data,cod.field="SPECIES10", had.field="SPECIES11")
#'@export

DO_RATIOS <- function(df=NULL,cod.field="COD", had.field="HAD"){
  OBS_RATIO <- sum(data$COD[data$OBS=="Y"])/sum(data$HAD[data$OBS=="Y"])
  UNOBS_RATIO <- sum(data$COD[data$OBS=="N"])/sum(data$HAD[data$OBS=="N"])
  MULTIPLIER <- OBS_RATIO/UNOBS_RATIO
  output <- data.frame(MULTIPLIER)
  print(output)
}
