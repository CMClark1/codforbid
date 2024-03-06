#'@title assignzone
#'@description Assign discard zones to data.
#'@param marfis.df A dataframe of MARFIS data with latitude and longitude columns.
#'@param isdb.df A dataframe of ISDB data for the relevant year. Default is "isdb" as supplied by the function isdbpull.R
#'@param y Year
#'@param lat.field Latitude field, in decimal degrees. Default is "LAT".
#'@param lon.field Longitude field, in decimal degrees. Default in "LON".
#'@param dir Directory with locally stored data, which can be generated with the function localcopies.R. Default is "C:/LocalDataDump"
#'@return a list of two dataframes: one with an additional column for Zone, with all Zones assigned; another with an additional column for Zone, with no Zones assigned.
#'@examples
#'example1 <- assignzone(marfis)
#'@export

assignzone <- function(marfis.df=NULL, isdb.df=NULL, y=year, lat.field="LAT",lon.field="LON", dir="C:/LocalDataDump"){

 marfis.df1 <- Mar.utils::identify_area(df=marfis.df%>%as.data.frame(), lat.field = lat.field, lon.field = lon.field, agg.poly.shp = Mar.data::GeorgesBankDiscardZones_sf ,agg.poly.field = "Id")
 marfis.df2 <- marfis.df1 %>% dplyr::rename("ZONE"="Id")

 ###Correct zone if missing or 0-------------------
 ##Create two dataframes, one that is correct and one that needs corrections
 zone_correct <- marfis.df2 %>% dplyr::filter(ZONE>=1)
 zone_corrections <- marfis.df2 %>% dplyr::filter(!ZONE>=1)

 ##Correct observed trips with missing zones
 observed <- marfis.df2 %>%
   dplyr::filter(!ZONE>=1) %>%
   dplyr::filter(nchar(TRIP.marfis)<1) %>%
   dplyr::rename(TRIP = TRIP.marfis) %>%
   dplyr::left_join(isdb.df%>%
                      dplyr::select(TRIP, LAT, LON), by="TRIP")

 if(nrow(observed)>1){
   observed <- Mar.utils::identify_area(df=observed%>%as.data.frame(), lat.field = "LAT.y", lon.field = "LON.y", agg.poly.shp = Mar.data::GeorgesBankDiscardZones_sf,agg.poly.field = "Id")
   observed <- observed %>% dplyr::rename("ZONE"="Id","LAT"="LAT.x","LON"="LON.x")
 } else {
   observed <- observed
 }

 ##Correct unobserved trips with missing zones

mobileVMSRaw <- readRDS(paste(dir,"/mobile_5Z_VMS_",y,".rds",sep=""))
fixedVMSRaw <- readRDS(paste(dir,"/fixed_5Z_VMS_",y,".rds",sep=""))
chpVMS <- rbind(mobileVMSRaw, fixedVMSRaw)

unobserved <- marfis.df2 %>%
  dplyr::filter(!ZONE>=1) %>%
  dplyr::filter(nchar(TRIP.marfis)<1 | is.na(TRIP.marfis)) %>%
  dplyr::mutate(LANDED_DATE=lubridate::as_date(LANDED_DATE),DATE_FISHED=lubridate::dmy(DATE_FISHED))

#Filter existing VMS data using VR_NUMBER and LANDED_DATE from unobserved trips missing zone numbers
chpVMS_filter <- chpVMS %>%
  dplyr::mutate(DATE=as.Date(format(as.POSIXct(POSITION_UTC_DATE,format='%Y/%m/%d %H:%M:%S'),format='%Y/%m/%d'))) %>%
  dplyr::filter(VR_NUMBER %in% unobserved$VR_NUMBER_FISHING & DATE %in% unobserved$DATE_FISHED) %>%
  dplyr::group_by(VR_NUMBER, DATE) %>%
  dplyr::summarize(MeanLat = mean(LATITUDE, na.rm=TRUE), MeanLon = mean(LONGITUDE, na.rm=TRUE)) #Available VMS data for unobserved trips

if(nrow(unobserved)>1){
  unobserved <- dplyr::left_join(unobserved, as.data.frame(chpVMS_filter), by = (c("VR_NUMBER_FISHING" = "VR_NUMBER", "DATE_FISHED" = "DATE")))
  unobserved <- Mar.utils::identify_area(df=unobserved, lat.field = "MeanLat", lon.field = "MeanLon",
                                         agg.poly.shp = Mar.data::GeorgesBankDiscardZones_sf,agg.poly.field = "Id")
  unobserved <- unobserved %>% dplyr::mutate(ZONE=Id,LON=MeanLat,LAT=MeanLon) %>% dplyr::select(-MeanLat, -MeanLon)
} else {
  unobserved <- unobserved }

  #Bind observed and unobserved dataframes together and remove trips missing Zone

temp <- rbind(zone_correct, setNames(observed%>%dplyr::select(c(1:23)), names(zone_correct)), setNames(unobserved%>%dplyr::select(1:23), names(zone_correct)))

marfis <- temp %>% dplyr::filter(ZONE>=1)
noZone <- temp %>% dplyr::filter(!ZONE>=1) %>% dplyr::mutate(COMMENT="NO ZONE")

df.list <- list(marfis,noZone)
print(df.list)

}


