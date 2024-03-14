#'@title speciessought
#'@description Remove trips that sought species other than haddock, and then remove
#'@param marfis.df A dataframe of MARFIS data with TRIP codes.
#'@param y Year
#'@param username Oracle username. Default is the value oracle.username stored in .Rprofile.
#'@param password Oracle password. Default is the value oracle.password stored in .Rprofile.
#'@param dsn Oracle dsn. Default is the value oracle.dsn stored in .Rprofile.
#'@return a list of two data.frames: one with trips that sought haddock to keep; another with trips that sought other species to remove.
#'@examples
#'example1 <- speciessought(marfis.df=marfis)
#'@export

speciessought <- function(marfis.df=NULL, username=oracle.username,password=oracle.password,dsn=oracle.dsn, y=year) {

#Identify trips that recorded that they sought species other than cod or haddock
  channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=username, password=password, dsn)
  sought <- ROracle::dbGetQuery(channel, paste("select a.CFV, a.VESSEL_NAME, b.TRIP, c.GEARCD_ID, b.LANDING_DATE, d.SPECSCD_ID, d.SET_NO, e.SETDATE
from isdb.isvessels a, isdb.istrips b, isdb.isgears c, isdb.isfishsets d, isdb.issetprofile e
where b.VESS_ID = a.VESS_ID
and e.FISHSET_ID = d.FISHSET_ID
and b.TRIP_ID = d.TRIP_ID
and d.GEAR_ID = c.GEAR_ID
and TO_CHAR(b.LANDING_DATE,'yyyy')=",y,"
and d.NAFAREA_ID like '5Z%'
and e.PNTCD_ID in (1,2)
and d.SETCD_ID=1
group by a.CFV, a.VESSEL_NAME, b.TRIP, c.GEARCD_ID, b.LANDING_DATE, d.SPECSCD_ID, d.SET_NO, e.SETDATE",sep=" "))

noncodhad <- dplyr::left_join(marfis.df, sought %>%
                                dplyr::group_by(TRIP) %>%
                             dplyr::select(TRIP, SPECSCD_ID) %>%
                             dplyr::distinct(TRIP, SPECSCD_ID), relationship="many-to-many") %>%
                             dplyr::filter(!SPECSCD_ID %in% c(10, 11) & !is.na(SPECSCD_ID))

#Identify trips where cod:had ratio for all sets in the trip are greater than 0.8 (i.e. sought cod for the whole trip)

test1 <- marfis.df %>% dplyr::mutate(`100` = tidyr::replace_na(`100`, 0),
                            `110` = tidyr::replace_na(`110`, 0),
                            `170` = tidyr::replace_na(`170`, 0),
                            codhad_ratio=as.numeric(`100`)/as.numeric(`110`),
                            test=ifelse(codhad_ratio>0.8,1,0)) %>%
  dplyr::group_by(TRIP_ID) %>%
  dplyr::summarise(test=mean(test)) %>%
  dplyr::filter(test==1) #if mean is 1, then all sets have a ratio > 0.8

coddirected1 <- marfis.df %>% dplyr::filter(TRIP_ID%in%test1$TRIP_ID)

#Identify trips where sets at the end of the trip had a cod:had ratio greater than 0.8 (i.e. sought cod at the end of the trip). Sets with ratio >0.8 are cod directed on these trips.

coddirected2 <-marfis.df %>% dplyr::mutate(`100` = tidyr::replace_na(`100`, 0),
                            `110` = tidyr::replace_na(`110`, 0),
                            `170` = tidyr::replace_na(`170`, 0),
                            codhad_ratio=as.numeric(`100`)/as.numeric(`110`),
                            test=ifelse(codhad_ratio>0.8,1,0)) %>%
              dplyr::filter(codhad_ratio!=Inf,
                            codhad_ratio!=-Inf,#filter out Inf values (either cod or haddock = 0)
                            !is.na(codhad_ratio),
                            !is.nan(codhad_ratio)) %>%
  dplyr::group_by(TRIP_ID) %>%
  dplyr::arrange(LOG_EFRT_STD_INFO_ID) %>%
  dplyr::mutate(test2=dplyr::case_when(dplyr::first(test)==0 & dplyr::last(test)==1 ~ 1)) %>%
  dplyr::filter(test2==1) %>% #include only trips with last set >0.8 greater than first set <0.8
  dplyr::select(-c(test,test2))%>%
  dplyr::filter(codhad_ratio>0.8)

#Identify sets where pollock landings are greater than combined cod and haddock landings (i.e. sought pollock)

pollockdirected <- marfis.df %>%
  dplyr::mutate(`100` = tidyr::replace_na(`100`, 0),
                `110` = tidyr::replace_na(`110`, 0),
                `170` = tidyr::replace_na(`170`, 0),
                pol_ratio=as.numeric(`170`)>(as.numeric(`100`)+as.numeric(`110`))) %>%
  dplyr::filter(pol_ratio==TRUE) #These sets sought pollock

directed <- rbind(noncodhad%>%
                    dplyr::select(1:23) %>%
                    dplyr::mutate(COMMENT="SPECIES OTHER THAN COD OR HADDOCK SOUGHT"),
                  coddirected1%>%
                    dplyr::select(1:23) %>%
                    dplyr::mutate(COMMENT="ALL SETS COD DIRECTED"),
                  coddirected2%>%
                    dplyr::select(1:23) %>%
                    dplyr::mutate(COMMENT="LAST SETS COD DIRECTED"),
                  pollockdirected%>%
                    dplyr::select(1:23) %>%
                    dplyr::mutate(COMMENT="POLLOCK DIRECTED"))

marfis <- dplyr::anti_join(marfis.df, directed%>%dplyr::distinct(), by = c("VR_NUMBER_FISHING","TRIP_ID","LANDED_DATE","DATE_FISHED","100","110","170"))

df.list <- list(marfis,directed)
print(df.list)

}
