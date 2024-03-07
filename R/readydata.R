#'@title readydata
#'@description A function to pull raw data, clean it, and QAQC it. After, it is ready for analysis.
#'@param year The year required.
#'@param username Oracle username. Default is the value oracle.username stored in .Rprofile.
#'@param password Oracle password. Default is the value oracle.password stored in .Rprofile.
#'@param dsn Oracle dsn. Default is the value oracle.dsn stored in .Rprofile.
#'@param directory Directory where output files will be saved. Default is "~/" which is usually Documents.
#'@return a data frame of clean data ready for analysis
#'@examples
#'example1 <- isdbpull(year=2023)
#'@export

readydata <- function(year=NULL,directory="~/",username=oracle.username,password=oracle.password,dsn=oracle.dsn) {
 marfis <- marfispull(year=year)
 isdb <- isdbpull(year=year)

 join_match <- marfis %>%
   dplyr::rename(TRIP.marfis=TRIP) %>%
   dplyr::left_join(isdb %>%
               dplyr::select(!c(LAT,LON,VESSEL_NAME)) %>%
               dplyr::distinct(VR_NUMBER_FISHING, LANDED_DATE, .keep_all=TRUE) %>%
               dplyr::rename(TRIP.isdb=TRIP)) %>%
   dplyr::filter(!is.na(TRIPCD_ID))

 #Create a dataframe of records that do not match
 join_nomatch <- marfis %>%
   dplyr::rename(TRIP.marfis=TRIP) %>%
   dplyr::left_join(isdb%>%
                      dplyr::select(!c(LAT,LON,VESSEL_NAME)) %>%
                      dplyr::distinct(VR_NUMBER_FISHING, LANDED_DATE, .keep_all=TRUE) %>%
                      dplyr::rename(TRIP.isdb=TRIP)) %>%
   dplyr::filter(is.na(TRIPCD_ID))

 #For MARFIS data that did match ISDB based on VRN and landed date, match trip numbers in joined MARFIS and ISDB records, and replace missing/mistake MARFIS trip numbers with ISDB trip numbers

 join_good1 <- join_match %>%
   dplyr::filter(TRIP.marfis == TRIP.isdb) %>%
   dplyr::full_join(join_match %>%
                      dplyr::filter(is.na(TRIP.marfis) & is.na(TRIP.isdb))) %>% #MARFIS and ISDB trip numbers match
   dplyr::select(c(1:21)) %>%
   dplyr::mutate(LANDED_DATE=lubridate::dmy(LANDED_DATE))

 join_good2 <- join_match %>%
   dplyr::filter(TRIP.marfis != TRIP.isdb) %>%
   dplyr::mutate(TRIP.marfis=dplyr::case_when(TRIP.marfis=="" ~ TRIP.isdb,
                                TRIP.marfis !="" ~ TRIP.marfis)) %>% #If MARFIS trip number is missing, replace it with the ISDB trip number
   dplyr::select(c(1:21)) %>% dplyr::mutate(LANDED_DATE=lubridate::dmy(LANDED_DATE))

 #For MARFIS data that did not match ISDB based on VRN and landed date, match MARFIS data to ISDB data +/- 2 days. Do not replace MARFIS dates with ISDB dates.

 join2 <- fuzzyjoin:: fuzzy_left_join(
   join_nomatch %>%
     dplyr::select(c(1:21)) %>%
     dplyr::mutate(LANDED_DATE=lubridate::dmy(LANDED_DATE)), isdb %>%
     dplyr::mutate(start=lubridate::dmy(LANDED_DATE)-2,end=lubridate::dmy(LANDED_DATE)+2)%>%dplyr::rename(TRIP.isdb=TRIP),
   by = c(
     "VR_NUMBER_FISHING" = "VR_NUMBER_FISHING",
     "LANDED_DATE" = "start",
     "LANDED_DATE" = "end"
   ),
   match_fun = list(`==`, `>=`, `<=`)
 )

 join2_match <- join2 %>% dplyr::filter(!is.na(TRIPCD_ID)) #Records that match
 join2_nomatch <- join2 %>% dplyr::filter(is.na(TRIPCD_ID)) #Records that do not match

 #For matched dataframe, match trip numbers in joined MARFIS and ISDB records, and replace missing/mistake MARFIS trip numbers with ISDB trip numbers
 join_good3 <- join2_match %>% dplyr::filter(TRIP.marfis == TRIP.isdb) %>% dplyr::select(c(1:21))

 join_good4 <- join2_match %>% dplyr::filter(TRIP.marfis != TRIP.isdb) %>% dplyr::mutate(TRIP.marfis=dplyr::case_when(TRIP.marfis=="" ~ TRIP.isdb, TRIP.marfis!="" ~ TRIP.marfis)) %>% dplyr::select(c(1:21))

 #MARFIS records with no match to ISDB based on VRN and date, fall into two categories: observer records in MARFIS that have not yet been entered into the ISDB (will have a TRIP#) and non-observed trips (no TRIP#)

 join_good5 <- join2_nomatch %>% dplyr::filter(is.na(TRIP.marfis)) %>% dplyr::select(c(1:21)) #These are the unobserved trips
 join_good6 <- join2_nomatch %>% dplyr::filter(!is.na(TRIP.marfis)) %>% dplyr::select(c(1:21)) #These are the observed trips not entered in the ISDB (aka ISDB errors)

 #Select MARFIS columns and bind the QAQC'd parts of the MARFIS dataframe back together

 marfis_qaqc <- rbind(join_good1, join_good2,
                      setNames(join_good3, names(join_good2)),
                      setNames(join_good4, names(join_good2)),
                      setNames(join_good5, names(join_good2)),
                      setNames(join_good6, names(join_good2)))

 #Write files to check with CDD and observer companies

 marfis_error1 <- join_match %>%
   dplyr::filter(TRIP.marfis != TRIP.isdb) %>%
   dplyr::mutate(COMMENT=dplyr::case_when(TRIP.marfis=="" ~ "MARFIS trip number missing", TRIP.marfis!="" ~ "MARFIS trip number incorrect as compared to ISDB")) %>%
   dplyr::select(c(1:18), TRIP.isdb) %>%
   dplyr::mutate(LANDED_DATE=lubridate::dmy(LANDED_DATE))

 marfis_error2 <- join2_match %>%
   dplyr::filter(TRIP.marfis != TRIP.isdb) %>%
   dplyr::mutate(COMMENT=dplyr::case_when(TRIP.marfis=="" ~ "MARFIS trip missing",TRIP.marfis!="" ~ "MARFIS trip incorrect as compared to ISDB")) %>%
   dplyr::select(c(1:18), TRIP.isdb)

 marfis_errors <- rbind(marfis_error1, setNames(marfis_error2, names(marfis_error1))) #Observed trips entered in MARFIS that do not match the ISDB.

 marfis_missing <- marfis_errors %>%
   dplyr::select(VR_NUMBER_FISHING, TRIP_ID, LANDED_DATE, TRIP.marfis, TRIP.isdb) %>%
   dplyr::filter(TRIP.marfis == "") %>% dplyr::distinct(VR_NUMBER_FISHING, TRIP_ID, LANDED_DATE, TRIP.marfis, TRIP.isdb, .keep_all = TRUE) #File with missing trip numbers sent to CDD to add trip numbers from ISDB d

 marfis_incorrect <- marfis_errors %>%
   dplyr::select(VR_NUMBER_FISHING, TRIP_ID, LANDED_DATE, TRIP.marfis, TRIP.isdb) %>%
   dplyr::filter(TRIP.marfis!= "") %>% dplyr::distinct()

 isdb_check <- isdb %>%
   dplyr::filter(TRIP %in% marfis_incorrect$TRIP.isdb) %>%
   dplyr::select(VR_NUMBER_FISHING, TRIP, LANDED_DATE) %>%
   dplyr::distinct() %>%
   dplyr::mutate(TRIP.marfis="match",COMMENT="Check if VRN, date landed, and trip number are correct")

 isdb_errors <- join_good6 %>%
   dplyr::rename(VR_NUMBER_FISHING=VR_NUMBER_FISHING.x,
                 LANDED_DATE=LANDED_DATE.x,
                 TRIP=TRIP_ID) %>%
   dplyr::distinct(VR_NUMBER_FISHING, TRIP, LANDED_DATE, TRIP.marfis) %>%
   dplyr::mutate(TRIP=as.character(TRIP), LANDED_DATE=as.character(LANDED_DATE), COMMENT = "Trip number entered in MARFIS but not ISDB") %>%
   dplyr::ungroup() %>%
   dplyr::select(VR_NUMBER_FISHING,TRIP,LANDED_DATE,TRIP.marfis)#Observed trip number entered in MARFIS not entered equal to that in the ISDB - this may nto be an ISDB error, but these should also be checked.

isdb_check <- dplyr::full_join(isdb_check,isdb_errors)

write.csv(marfis_missing, paste(directory,"marfis_missing.csv",sep="")) #File sent to CDD to add missing trip numbers from ISDB database
write.csv(isdb_check, paste(directory,"isdb_check.csv",sep="")) #File sent to observer program to check if VRN, date landed, and trip number are correct -- or if trips entered in MARFIS are missing from the ISDB for a reason.

marfis1 <- assignsector(marfis_qaqc)

marfis2 <- assignzone(marfis.df=marfis1, isdb.df=isdb, y=year) #list of two dataframes - data with zones to keep, and data without zones to remove

marfis3 <- noncommercial(marfis.df=marfis2[[1]],isdb.df=isdb) #list of two dataframes - data that are commercial trips to keep, and data that are not commercial trips to remove

marfis4 <- nopanel(marfis.df=marfis3[[1]], y=year) #list of two dataframes - data that use a separator panel to keep, and data that do not use a separator panel to remove

marfis5 <- speciessought(marfis.df=marfis4[[1]], y=year)

marfis6 <- obsquarters(marfis.df=marfis5[[1]])

nozone <- marfis2[[2]]%>%dplyr::select(1:23,COMMENT)
noncom <- marfis3[[2]]%>%dplyr::select(1:23,COMMENT)
nopan <- marfis4[[2]]%>%dplyr::select(1:23,COMMENT)
directed <- marfis5[[2]]%>%dplyr::select(1:23,COMMENT)
allobserved <- marfis6[[2]]%>%dplyr::select(1:23,COMMENT)

removed <- rbind(nozone,
      setNames(noncom, names(nozone)),
      setNames(nopan, names(nozone)),
      setNames(directed, names(nozone)),
      setNames(allobserved, names(nozone)))

write.csv(removed, paste(directory,"removedrecords.csv",sep=""))

print(marfis6)

}


