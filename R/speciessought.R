#'@title speciessought
#'@description Remove trips that sought species other than haddock, and then remove
#'@param marfis.df A dataframe of MARFIS data with TRIP codes.
#'@param y Year
#'@param username Oracle username. Default is the value oracle.username stored in .Rprofile.
#'@param password Oracle password. Default is the value oracle.password stored in .Rprofile.
#'@param dsn Oracle dsn. Default is the value oracle.dsn stored in .Rprofile.
#'@return a list of four data.frames: one with trips that sought cod or haddock or keep; another with trips that directly sought species other than cod or haddock; a third that, based on the cod:haddock ratio, sought cod; a fourth that, based on pollock landings being greater than cod and haddock combined, sought pollock.
#'@examples
#'example1 <- speciessought(marfis.df=marfis)
#'@export

speciessought <- function(marfis.df=NULL, username=oracle.username,password=oracle.password,dsn=oracle.dsn, y=year) {
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

marfis <- dplyr::left_join(marfis.df, sought %>% group_by(TRIP) %>%
                      dplyr::filter(!SPECSCD_ID %in% c(10,11)) %>%
                      dplyr::select(TRIP, SPECSCD_ID) %>%
                      dplyr::distinct(TRIP, SPECSCD_ID, .keep_all = TRUE), by=c("TRIP" = "TRIP")) %>%
                      dplyr::filter(SPECSCD_ID %in% c(10, 11) | is.na(SPECSCD_ID))

noncodhad <- dplyr::left_join(marfis.df, sought %>% group_by(TRIP) %>%
                             dplyr::filter(!SPECSCD_ID %in% c(10,11)) %>%
                             dplyr::select(TRIP, SPECSCD_ID) %>%
                             dplyr::distinct(TRIP, SPECSCD_ID, .keep_all = TRUE), by=c("TRIP" = "TRIP")) %>%
                             dplyr::filter(!SPECSCD_ID %in% c(10, 11) & !is.na(SPECSCD_ID)) %>%
                             dplyr::mutate(COMMENT="SOUGHT SPECIES OTHER THAN COD OR HADDOCK")

df.list <- list(marfis,noncodhad)
print(df.list)

#Identify trips where cod:had ratio for all sets in the trip are greater than 0.8

test1 <- marfis.df %>% dplyr::mutate(`100` = tidyr::replace_na(`100`, 0),
                            `110` = tidyr::replace_na(`110`, 0),
                            `170` = tidyr::replace_na(`170`, 0),
                            codhad_ratio=as.numeric(`100`)/as.numeric(`110`),
                            test=ifelse(codhad_ratio>0.8,1,0)) %>%
  dplyr::group_by(TRIP_ID) %>%
  dplyr::summarise(test=mean(test)) %>%
  dplyr::filter(test==1) #if mean is 1, then all sets have a ratio > 0.8

coddirected1 <- marfis.df %>% dplyr::filter(TRIP_ID%in%test1$TRIP_ID)

#Identify trips where sets at the end of the trip had a cod:had ratio greater than 0.8

test2 <- marfis.df %>% dplyr::mutate(`100` = tidyr::replace_na(`100`, 0),
                                     `110` = tidyr::replace_na(`110`, 0),
                                     `170` = tidyr::replace_na(`170`, 0),
                                     codhad_ratio=as.numeric(`100`)/as.numeric(`110`),
                                     test=ifelse(codhad_ratio>0.8,1,0)) %>%
  dplyr::group_by(TRIP_ID) %>%
  dplyr::summarise(test=mean(test)) %>%
  dplyr::filter(test>0 & test<1) #if mean is >0 and <1, at east one set had a ratio of >0.8

coddirected2 <-marfis.df %>% dplyr::mutate(`100` = tidyr::replace_na(`100`, 0),
                            `110` = tidyr::replace_na(`110`, 0),
                            `170` = tidyr::replace_na(`170`, 0),
                            codhad_ratio=as.numeric(`100`)/as.numeric(`110`),
                            test=ifelse(codhad_ratio>0.8,1,0)) %>%
              dplyr::filter(codhad_ratio!=Inf,
                            codhad_ratio!=-Inf,#filter out Inf values (either cod or haddock = 0)
                            !is.na(codhad_ratio),
                            !is.nan(codhad_ratio),
                            TRIP_ID%in%test2$TRIP_ID) %>% #include trips with at least one value >0.8
  dplyr::filter(TRIP_ID%in%test1$TRIP_ID) %>%
  dplyr::group_by(TRIP_ID) %>%
  dplyr::arrange(LOG_EFRT_STD_INFO_ID) %>%
  dplyr::mutate(last=case_when(last(test)>0 ~ 1)) %>%
  dplyr::filter(last==1) %>% #include only trips with last set >0.8
  dplyr::mutate(firstlast=case_when(first(codhad_ratio)<last(codhad_ratio) ~ 1)) %>%
  dplyr::filter(firstlast==1) %>% #keep only sets where final set had value greater than first set
  dplyr::select(-c(test,last,firstlast))

#Identify sets where pollock landings are greater than combined cod and haddock landings

pollockdirected <- marfis.df %>%
  dplyr::mutate(`100` = tidyr::replace_na(`100`, 0),
                `110` = tidyr::replace_na(`110`, 0),
                `170` = tidyr::replace_na(`170`, 0),
                pol_ratio=as.numeric(df$`170`)>(as.numeric(df$`100`)+as.numeric(df$`110`))) %>%
  dplyr::filter(pol_ratio==TRUE) #These sets sought pollock

marfis <- marfis.df %>% dplyr::filter(!TRIP_ID%in%coddirected1$TRIP_ID,
                               !TRIP_ID%in%coddirected2$TRIP_ID) %>%
  dplyr::full_join(marfis.df %>%
  dplyr::mutate(`100` = tidyr::replace_na(`100`, 0),
                `110` = tidyr::replace_na(`110`, 0),
                `170` = tidyr::replace_na(`170`, 0),
                pol_ratio=as.numeric(df$`170`)>(as.numeric(df$`100`)+as.numeric(df$`110`))) %>%
  dplyr::filter(pol_ratio==FALSE)) %>%
  dplyr::select(-pol_ratio)

directed <- rbind(coddirected1%>%dplyr::select(1:23)%>%dplyr::mutate(COMMENT="ALL SETS COD DIRECTED"),coddirected2%>%dplyr::select(1:23)%>%dplyr::mutate(COMMENT="LAST SETS COD DIRECTED"),pollockdirected%>%dplyr::select(1:23)%>%dplyr::mutate(COMMENT="POLLOCK DIRECTED"))

df.list <- list(marfis,directed)
print(df.list)


}


#Potential to add that sets are eliminated when cod or haddock landings = 0
#Remove trips where cod:had ratio for all sets in the trip are greater than 0.8

df <- marfis.df %>%
  dplyr::mutate(`100` = tidyr::replace_na(`100`, 0),
                `110` = tidyr::replace_na(`110`, 0),
                `170` = tidyr::replace_na(`170`, 0),
                codhad_ratio=as.numeric(`100`)/as.numeric(`110`),
                test1 = ifelse(codhad_ratio<0.8, "met","not met")) #Basic ifelse; evaluation of a value for each record/set.

temp2<-df %>%
  select(TRIP_ID, test1) %>% #Selects trip ID and test result
  distinct() %>% #Removes replicates
  group_by(TRIP_ID) %>%
  dplyr::summarise(n = n()) %>%
  filter(n==1) #select trips where sets all met or all did not meet conditions
temp3 <- df %>%
  select(TRIP_ID, test1) %>% #Selects trip ID and test result
  distinct() %>%
  filter(TRIP_ID %in% temp2$TRIP_ID & test1 == "not met") %>% #select trips where all sets did not meet the condition
  rename("test2"="test1") #Renames the test field to facilitate merging with df
df2<-full_join(df, temp3) %>% #merges the two, identifying groups which did not meet the conditions for at least one set
  mutate(test2=ifelse(is.na(test2), "met", test2)) %>% #Replaces 'NAs' in the test column (created NAs becaused join in only groups which DID NOT meet conditions at least once)
  select(-test1)

cod_directed1 <- df2 %>%filter(test2 == "not met")


df <- df2 %>% filter(test2 == "met") #keep only trips that were not 100% met or not met
rm(df2)

#Remove sets at the end of the trip with a ratio greater than 0.8

temp1 <- df %>% select(-test2) %>% mutate(test1=ifelse(codhad_ratio<0.8,"met","not met")) %>%
  filter(codhad_ratio > 0.8) %>%
  select(TRIP_ID) %>%
  distinct(TRIP_ID)  #select any remaining trips with a set with a codhad ratio greater than 0.8

temp2 <- df %>% select(-test2) %>% mutate(test1=ifelse(codhad_ratio<0.8,"met","not met")) %>%
  filter(TRIP_ID %in% temp1$TRIP_ID) %>%
  select(LOG_EFRT_STD_INFO_ID, TRIP_ID, codhad_ratio, test1) %>%
  group_by(TRIP_ID) %>%
  slice(which.max(LOG_EFRT_STD_INFO_ID)) %>%
  filter(test1 == "not met") #select any trips where the last set has a codhad ratio greater than 0.8

temp3 <- df %>% select(-test2) %>% mutate(test1=ifelse(codhad_ratio<0.8,"met","not met")) %>%
  filter(TRIP_ID %in% temp2$TRIP_ID) %>%
  select(LOG_EFRT_STD_INFO_ID, TRIP_ID, codhad_ratio, test1) %>%
  arrange(LOG_EFRT_STD_INFO_ID) %>%
  group_split(TRIP_ID) #trip with high cod ratio at the end of the trip in list. check to make sure 'not met' occurs only in the last set of the trip

cod_directed2 <- df %>%
  filter(LOG_EFRT_STD_INFO_ID %in% temp2$LOG_EFRT_STD_INFO_ID) #cod directed sets at the end of trips
cod_directed <- rbind(cod_directed1, setNames(cod_directed2, names(cod_directed1)))

df <- df %>%
  filter(!LOG_EFRT_STD_INFO_ID %in% temp2$LOG_EFRT_STD_INFO_ID) %>%
  select(-test1) #dataframe of remaining sets identified as not cod directed

rm(temp1, temp2, temp3, cod_directed1, cod_directed2)

#Remove sets where pollock landings are greater than combined cod and haddock landings

df <- df %>%
  replace_na(list(`100` = 0, `110` = 0, `170` = 0)) %>%
  mutate(pol_ratio=as.numeric(df$`170`)>(as.numeric(df$`100`)+as.numeric(df$`110`)))

pol_directed <- df %>% filter(pol_ratio == "TRUE") #dataframe with pollock directed sets
haddock_directed <- df %>% filter(pol_ratio == "FALSE") #dataframe with haddock directed sets

rm(df, df2, cod_directed1, cod_directed2)
