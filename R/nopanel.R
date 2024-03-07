#'@title noncommercial
#'@description Remove trips that were not commercial.
#'@param marfis.df A dataframe of MARFIS data with TRIP codes.
#'@param y Year
#'@param username Oracle username. Default is the value oracle.username stored in .Rprofile.
#'@param password Oracle password. Default is the value oracle.password stored in .Rprofile.
#'@param dsn Oracle dsn. Default is the value oracle.dsn stored in .Rprofile.
#'@return a list of two dataframes: one with trips that used a separator panel to keep; another with trips that did not use a separator panel to remove
#'@examples
#'example1 <- nopanel(marfis.df=marfis)
#'@export


nopanel <- function(marfis.df=NULL, username=oracle.username,password=oracle.password,dsn=oracle.dsn, y=year) {
  channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=username, password=password, dsn)
  separator <- ROracle::dbGetQuery(channel, paste("select a.CFV, a.VESSEL_NAME, b.TRIP, b.LANDING_DATE, c.GEARCD_ID, d.GEARFCD_ID, Count(e.SET_NO)
from isdb.isvessels a, isdb.istrips b, isdb.isgears c, isdb.isgearfeatures d, isdb.isfishsets e, isdb.issetprofile f
where b.VESS_ID = a.VESS_ID
and f.FISHSET_ID = e.FISHSET_ID
and b.TRIP_ID = e.TRIP_ID
and e.GEAR_ID = c.GEAR_ID
and c.GEAR_ID = d.GEAR_ID
and TO_CHAR(b.LANDING_DATE,'yyyy')=",y,"
and e.NAFAREA_ID like '5Z%'
and f.PNTCD_ID=2
and c.GEARCD_ID=12
and d.GEARFCD_ID in (89, 90)
group by a.CFV, a.VESSEL_NAME, b.TRIP, b.LANDING_DATE, c.GEARCD_ID, d.GEARFCD_ID
",sep=" "))

  marfis <- dplyr::left_join(marfis.df, separator %>%
                               dplyr::select(TRIP, GEARFCD_ID)) %>%
            dplyr::filter(is.na(GEARFCD_ID) | GEARFCD_ID != 89)

  nopaneltrips <- dplyr::left_join(marfis.df, separator %>%
                                     dplyr::select(TRIP, GEARFCD_ID)) %>%
            dplyr::filter(GEARFCD_ID == 89) %>%
    dplyr::mutate(COMMENT="DID NOT USE SEPARATOR PANEL")

   df.list <- list(marfis,nopaneltrips)
   print(df.list)
}
