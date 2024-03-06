#'@title noncommercial
#'@description Remove trips that were not commercial.
#'@param marfis.df A dataframe of MARFIS data with TRIP codes.
#'@param isdb.df A dataframe of ISDB data with TRIP codes and SETCD_ID. Default is "isdb" as supplied by the function isdbpull.R
#'@return a list of two dataframes: a data frame of commercial trips to keep; another with non-commercial trips to remove
#'@examples
#'example1 <- noncommercial(marfis.df=marfis,isdb.df=NULL)
#'@export

noncommercial <- function(marfis.df=NULL,isdb.df=NULL){
  marfis <- marfis.df %>% dplyr::rename(TRIP=TRIP.marfis) %>%
                dplyr::left_join(isdb.df %>%
                      dplyr::filter(SETCD_ID == 7) %>%
                      dplyr::select(TRIP, SETCD_ID), by="TRIP") %>%
                dplyr::filter(is.na(SETCD_ID) | SETCD_ID != 7)
  noncomtrips <- marfis.df %>% dplyr::rename(TRIP=TRIP.marfis) %>%
                dplyr::left_join(isdb.df %>%
                      dplyr::filter(SETCD_ID == 7) %>%
                      dplyr::select(TRIP, SETCD_ID), by="TRIP") %>%
                dplyr::filter(SETCD_ID == 7) %>%
                dplyr::mutate(COMMENT="NON COMMERCIAL TRIP") %>%
                dplyr::distinct()

  df.list <- list(marfis,noncomtrips)
  print(df.list)

}
