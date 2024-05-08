#'@title readydata
#'@description A function to pull raw data, clean it, and QAQC it. After, it is ready for analysis.
#'@param year The year required. Default is the previous year (current system year minus one).
#'@param username Oracle username. Default is the value oracle.username stored in .Rprofile.
#'@param password Oracle password. Default is the value oracle.password stored in .Rprofile.
#'@param dsn Oracle dsn. Default is the value oracle.dsn stored in .Rprofile.
#'@param directory Directory where output files will be saved. Default is the working directory.
#'@param savedoutput Specify whether or not you could like the five Excel or .csv files for review or action (see below for the descriptions of these files). Default is "T" (true).
#'@returns Returns a data frame of clean, aggregated data ready for analysis, and five Excel or .csv files for review or action: Discards_ISDB_check to send to the observer companies, Discards_MARFIS_missing to send to CDD, an observer coverage summary (Discards_ObsCoverageSummary), removed records (Discards_RemovedRecords), aggregated data (Discards_MARFISXtab_Aggregated) and data for grouping (Discards_DataforGrouping).
#'@examples
#'example1 <- isdbpull(year=2023)
#'@export

readydata2 <- function(year=as.numeric(substr(Sys.Date(),1,4))-1,directory=getwd(),username=oracle.username,password=oracle.password,dsn=oracle.dsn,savedoutput="T") {
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

  #Identify data to check with CDD and observer companies

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

  #Assign sector to MARFIS data
  marfis1 <- assignsector(marfis_qaqc)

  #Assign zone to MARFIS data. Creates a list of two dataframes - data with zones to keep, and data without zones to remove
  marfis2 <- assignzone(marfis.df=marfis1, isdb.df=isdb, y=year)

  #Identify non-commercial trips. Creates a list of two dataframes - data that are commercial trips to keep, and data that are not commercial trips to remove
  marfis3 <- noncommercial(marfis.df=marfis2[[1]],isdb.df=isdb)

  #Identify trips that did not use a separator panel. Creates a list of two dataframes - data that use a separator panel to keep, and data that do not use a separator panel to remove
  marfis4 <- nopanel(marfis.df=marfis3[[1]], y=year)

  #Identify trips that did not seek haddock. Creates a list of two dataframes - data from trips that did seek haddock to keep, and data that did not seek haddock to remove
  marfis5 <- speciessought(marfis.df=marfis4[[1]], y=year)

  #Create a coverage summary document, saved to working directory
  coveragesummary(marfis5[[1]],y=year)

  #Identify quarters/zone/fleet combinations that were 100% observed. Creates a list of two dataframes - data from quarters/zone/fleet combinations that were not 100% observed to keep, and data from quarters/zone/fleet combinations that were 100% observed to remove
  marfis6 <- obsquarters(marfis.df=marfis5[[1]])

  #Create dataframe file with all removed records
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

  #Aggregate data
  aggregated <- marfis5[[1]] %>%
    dplyr::group_by(VR_NUMBER_FISHING, VESSEL_NAME, LICENCE_ID, TC, LC, TRIP_ID, LANDED_DATE, GEAR_CODE, Q, TRIP, ZONE, SECTOR) %>%
    dplyr::summarize(COD = sum(as.numeric(`100`), na.rm=TRUE), HAD = sum(as.numeric(`110`), na.rm=TRUE), POL = sum(as.numeric(`170`), na.rm=TRUE)) %>%
    dplyr::filter(!is.na(SECTOR) | !is.nan(SECTOR) | SECTOR!=Inf | SECTOR!=-Inf) %>%
    dplyr::mutate(OBS=ifelse(is.na(TRIP), "N", "Y"),
                  Q=as.integer(Q),
                  ZONE=as.integer(ZONE))

  #Step 12c. Group aggregated data for analysis based on conditions
  #This part of the script groups by sector (fishery), zone, and quarter.

  forgroups <- aggregated %>%
    dplyr::group_by (SECTOR, ZONE, Q) %>%
    dplyr::mutate(OBS = 1) %>%
    dplyr::summarise(UNOBS = sum(OBS[is.na(TRIP)]), OBS = sum(OBS[!is.na(TRIP)])) %>%
    dplyr::mutate(COVERAGE =OBS/(UNOBS + OBS)) %>%
    dplyr::filter(COVERAGE!=1) %>% #Remove fully observed
    dplyr::mutate(DGROUP=dplyr::case_when(COVERAGE>0.2 & COVERAGE<1 ~ 1, #good to go
                                          COVERAGE<0.2 ~ 2)) #needs a friend

  if(savedoutput=="T"){
    write.csv(marfis_missing, paste(directory,"/Discards_MARFIS_missing_",year,"_",Sys.Date(),".csv",sep="")) #File sent to CDD to add missing trip numbers from ISDB database
    write.csv(isdb_check, paste(directory,"/Discards_ISDB_check_",year,"_",Sys.Date(),".csv",sep=""))#File sent to observer program to check if VRN, date landed, and trip number are correct -- or if trips entered in MARFIS are missing from the ISDB for a reason.
    write.csv(removed, paste(directory,"/Discards_RemovedRecords_",year,"_",Sys.Date(),".csv",sep=""))#Write .csv with all removed records
    write.csv(aggregated, paste(directory, "/Discards_MARFISXtab_Aggregated_",year,"_",Sys.Date(),".csv", sep="")) #Aggregated data
    write.csv(forgroups, paste(directory, "/Discards_DataforGrouping_",year,"_",Sys.Date(),".csv", sep="")) #file to help figure out the different groupings. Turned into xlsx to colour code and make the initial groupings
  }

  print(aggregated)

}


