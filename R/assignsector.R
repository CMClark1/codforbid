#'@title assignsector
#'@description Assign fleet sectors to data.
#'@param df A dataframe with a fleet code column (SFLT_DESC_ID)
#'@return a data.frame with a new column for fleet/sector
#'@examples
#'example1 <- isdbpull(year=2023)
#'@export

assignsector <- function(df=NULL) {
  df %>%
    dplyr::mutate(
      SECTOR = dplyr::case_when(
        SFLT_DESC_ID %in% c(90:95, 8976, 8977, 9318, 9319, 10865, 10866, 12305, 12195) ~ 90 #New Fleet Desc 90 (FG<45’)
        , SFLT_DESC_ID %in% c(89, 9317) ~ 89 #New Fleet Desc 89 (HL) (Gear = 41 so not handline)
        , SFLT_DESC_ID %in% c(881) ~ 881 #New Fleet Desc 881 (MG<65’ – SF ITQ)
        , SFLT_DESC_ID %in% c(884, 6875) ~ 884 #New Fleet Desc 884 (FG 45’ to 64’)
        , SFLT_DESC_ID %in% c(885, 1472) ~ 885 #New Fleet Desc 885 (MG>100’)
        , SFLT_DESC_ID %in% c(886, 1817) ~ 886 #New Fleet Desc 886 (FG 65’ to 100’)
        , SFLT_DESC_ID %in% c(311, 1523) ~ 311 #New Fleet Desc 311(MG 65’ to 100’)
        , SFLT_DESC_ID %in% c(3328) ~ 3328) ) #New Fleet Desc 3328 (First Nations)

}
