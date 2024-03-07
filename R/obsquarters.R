#'@title obsquarters
#'@description Identify quarters with 100% observer coverage, and then remove
#'@param marfis.df A dataframe of MARFIS data with observer TRIP ID codes.
#'@return a list of two data frames: data that are part of quarters with 100% observer coverage (by fleet/sector and zone) to remove; another that are part of quarters with <100% observer coverage (by fleet/sector and zone)
#'@examples
#'example1 <- obsquarters(marfis.df=marfis)
#'@export

#Remove quarters with 100% observer coverage

obsquarters <- function(marfis.df=NULL) {

part1 <- marfis.df %>%
  dplyr::mutate(OBS=dplyr::case_when(is.na(TRIP) ~ "N",
                                     !is.na(TRIP) ~ "Y")) %>%
  dplyr::group_by(SECTOR,ZONE,Q,OBS) %>%
  dplyr::tally() %>%
  tidyr::pivot_wider(id_cols=c(SECTOR,ZONE,Q),names_from=OBS,values_from=n) %>%
  dplyr::mutate(N=tidyr::replace_na(N,0),
         Y=tidyr::replace_na(Y,0)) %>%
  dplyr::mutate(test=dplyr::case_when(N==0 & Y>=1 ~ 1)) %>%
  dplyr::filter(test==1) %>%
  dplyr::select(SECTOR,ZONE,Q)

marfis <- dplyr::anti_join(marfis.df, part1, by = c("SECTOR","ZONE","Q"))

allobserved <- dplyr::semi_join(marfis.df,part1,by=c("SECTOR","ZONE","Q")) %>%
  dplyr::mutate(COMMENT="QUARTER WITH 100% OBSERVER COVERAGE")

df.list <- list(marfis,allobserved)
print(df.list)

}

