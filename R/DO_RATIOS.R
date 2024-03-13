#'@title DO_RATIOS
#'@description Calculate the landings ratio of cod:haddock on observed vs. unobserved trips according to the method from Gavaris et al. (2007).
#'@param df A dataframe of trips with numeric variables for cod landings and  haddock landings (called COD and HAD), and a character (Y/N) variable for whether the trip was observed (called OBS).
#'@return a data.frame with landings multiplier
#'@examples
#'COD=runif(4)
#'HAD=runif(4)
#'OBS=c("N","N","Y","Y")
#'df<-data.frame(COD,HAD,OBS)
#'example1 <- DO_RATIOS(df)
#'@export
#'

DO_RATIOS <- function(df=NULL){
  stopifnot(is.data.frame(df),length(df)>1)
  OBS_RATIO <- sum(df$COD[df$OBS=="Y"])/sum(df$HAD[df$OBS=="Y"])
  UNOBS_RATIO <- sum(df$COD[df$OBS=="N"])/sum(df$HAD[df$OBS=="N"])
  MULTIPLIER <- OBS_RATIO/UNOBS_RATIO
  output <- data.frame(MULTIPLIER)
  print(output)
}
