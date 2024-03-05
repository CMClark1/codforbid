#'@title DO_RATIOS
#'@description Calculate the landings ratio of cod:haddock on observed vs. unobserved trips according to the method from Gavaris et al. (2007).
#'@param df A dataframe of trips with numeric variables for cod landings and  haddock landings (called COD and HAD), and a character (Y/N) variable for whether the trip was observed (called OBS).
#'@param cod.field The field in the dataframe with the cod landings weight values. The default is "COD."
#'@param had.field The field in the dataframe with the haddock landings weight values. The default is "HAD."
#'@return a data.frame with landings multiplier
#'@examples
#'example1 <- DO_RATIOS(df)
#'example2 <- DO_RATIOS(df=data,cod.field="SPECIES10", had.field="SPECIES11")
#'@export

DO_RATIOS <- function(df=NULL,cod.field="COD", had.field="HAD"){
  OBS_RATIO <- sum(df$COD[df$OBS=="Y"])/sum(df$HAD[df$OBS=="Y"])
  UNOBS_RATIO <- sum(df$COD[df$OBS=="N"])/sum(df$HAD[df$OBS=="N"])
  MULTIPLIER <- OBS_RATIO/UNOBS_RATIO
  output <- data.frame(MULTIPLIER)
  print(output)
}
