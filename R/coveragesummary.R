#'@title coveragesummary
#'@description Export an observer coverage summary Excel document.
#'@param marfis.df A dataframe of MARFIS data with observer TRIP ID codes.
#'@param y Year
#'@return An Excel document (Coverage_Summary_year.xlsx) of observer coverage in the working directory.
#'@examples
#'example1 <- coveragesummary(marfis.df=marfis,y=2023)
#'@export


coveragesummary <- function(marfis.df=NULL,y=year){

#Write a coverage summary and export it as an Excel document

fleet_summary <- marfis.df %>%
  dplyr::select(SECTOR,ZONE,Q,VR_NUMBER_FISHING,TRIP_ID,TRIP) %>%
  dplyr::mutate(OBS=dplyr::case_when(is.na(TRIP) ~ "UNOBS",
                                     !is.na(TRIP) ~ "OBS")) %>%
  dplyr::group_by(SECTOR,OBS) %>%
  dplyr::tally() %>%
  tidyr::pivot_wider(id_cols=c(SECTOR),names_from=OBS,values_from=n) %>%
  dplyr::mutate(UNOBS=tidyr::replace_na(UNOBS,0),
                OBS=tidyr::replace_na(OBS,0)) %>%
  dplyr::mutate(COVERAGE=OBS/sum(UNOBS+OBS))

mean_coverage <- data.frame(desc="MEAN COVERAGE",value=mean(fleet_summary$COVERAGE))

fleet_allobs <- fleet_summary %>% dplyr::filter(COVERAGE==1)

fleetqz_summary <- marfis.df %>%
  dplyr::select(SECTOR,ZONE,Q,VR_NUMBER_FISHING,TRIP_ID,TRIP) %>%
  dplyr::mutate(OBS=dplyr::case_when(is.na(TRIP) ~ "UNOBS",
                                     !is.na(TRIP) ~ "OBS")) %>%
  dplyr::group_by(SECTOR, ZONE, Q, OBS) %>%
  dplyr::tally() %>%
  tidyr::pivot_wider(id_cols=c(SECTOR,ZONE,Q),names_from=OBS,values_from=n) %>%
  dplyr::mutate(UNOBS=tidyr::replace_na(UNOBS,0),
                OBS=tidyr::replace_na(OBS,0)) %>%
  dplyr::mutate(COVERAGE=OBS/sum(UNOBS+OBS))

fleetqz_allobs <- fleetqz_summary %>% dplyr::filter(COVERAGE==1)

#Export coverage summary as an excel document

wbName <- paste0("Discards_ObsCoverageSummary_",Sys.Date(),".xlsx",sep="")

wb<-openxlsx::createWorkbook(title="Observer Coverage Summary")
sheet1 <- openxlsx::addWorksheet(wb, sheetName = "FLEET SUMMARY")
openxlsx::writeDataTable(wb, x=fleet_summary, rowNames = FALSE, sheet = sheet1)

sheet2 <- openxlsx::addWorksheet(wb, sheetName = "MEAN COVERAGE")
openxlsx::writeDataTable(wb, x=mean_coverage, rowNames = FALSE, sheet = sheet2)

sheet3 <- openxlsx::addWorksheet(wb, sheetName = "FLEETS ALL OBS")
openxlsx::writeDataTable(wb, x=fleet_allobs, rowNames = FALSE, sheet = sheet3)

sheet4 <- openxlsx::addWorksheet(wb, sheetName = "FLEET-ZONE-Q SUMMARY")
openxlsx::writeDataTable(wb, x=fleetqz_summary, rowNames = FALSE, sheet = sheet4)

sheet5 <- openxlsx::addWorksheet(wb, sheetName = "FLEET-ZONE-Q ALL OBS")
openxlsx::writeDataTable(wb, x=fleetqz_allobs, rowNames = FALSE, sheet = sheet5)

openxlsx::saveWorkbook(wb, file = wbName, overwrite=TRUE)

}

