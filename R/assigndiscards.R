#' Assign Calculated Discards to Vessels
#'
#' Assign calculated discards to vessels based on their proportion of landings.
#'@param data1 The output from the readydata function. Default is ready.
#'@param data2 The output from the DO_BS function. Default is results.
#'@param file The location of the file with the fleet/zone/quarter combinations. Default is "Discards_GroupedData.csv." This is the readydata output Discards_DataforGrouping_year.csv with an additional column for group.
#'@return A dataframe of discards by vessel.
#'@examples
#'example1 <- assigndiscards()
#'@import dplyr
#'@export
#'

assigndiscards <- function(data1=ready,data2=results,file="Discards_GroupedData.csv"){

  grouped<-read.csv(file)
  data<-dplyr::full_join(data1,grouped%>%dplyr::select(-X,-DGROUP,-COVERAGE,-OBS,-UNOBS))

  byvessel=data%>%
    dplyr::filter(!is.na(GROUP), GROUP!="excluded") %>%
    dplyr::group_by(GROUP,SECTOR,ZONE,Q,VR_NUMBER_FISHING,VESSEL_NAME,LICENCE_ID)%>%
    dplyr::summarise(COD=sum(COD),HAD=sum(HAD))%>%
    dplyr::left_join(data2%>%dplyr::select(GROUP,SIGNIFICANT,SIGDISC))%>%
    dplyr::filter(SIGDISC>0)%>%
    dplyr::group_by(GROUP)%>%
    dplyr::mutate(DISCARDS=HAD/sum(HAD)*SIGDISC)%>%
    dplyr::ungroup()%>%
    dplyr::select(SECTOR,ZONE,Q,VR_NUMBER_FISHING,VESSEL_NAME,LICENCE_ID,DISCARDS)

  print(byvessel)

}
