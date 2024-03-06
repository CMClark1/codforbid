#'@title localcopies
#'@description A function to pull local copies of the MARFIS and ISDB databases, as well as relevant fleet and VMS data.
#'@param year The year required for VMS data (all years are pulled for MARFIS and ISDB)
#'@param dir The local directory where the data will be stored. Default is "C:/LocalDataDump"
#'@param username Oracle username. Default is the value oracle.username stored in .Rprofile.
#'@param password Oracle password. Default is the value oracle.password stored in .Rprofile.
#'@param dsn Oracle dsn. Default is the value oracle.dsn stored in .Rprofile.
#'@return .Rdata copies of MARFIS and ISDB databases, and .rds copies of relevant fleet and VMS data (mobile and fixed)
#'@export

localcopies <- function(year=NULL,dir="C:/LocalDataDump", username=oracle.username, password=oracle.password,dsn=oracle.dsn,pkg="roracle"){

  #Local copies of MARFIS and ISDB databases
  Mar.fleets::enable_local(data.dir = dir, oracle.username=username, oracle.password=password, oracle.dsn=dsn, usepkg=pkg)

  #Local copies of relevant fleet data
  mobile_5Z<- Mar.fleets::fishin_CHPs(type="MOBILE", stock = "5Z", dateStart = paste(year,"-01-01",sep=""), dateEnd= paste(year,"-12-31",sep=""), useLocal = T, data.dir='C:/LocalDataDump', socks=T, usepkg = "roracle")
  saveRDS(mobile_5Z, paste(dir,"/mobile_5Z_",year,".rds",sep=""))

  fixed_5Z<- fishin_CHPs(type="FIXED", stock = "5Z", dateStart = paste(year,"-01-01",sep=""), dateEnd= paste(year,"-12-31",sep=""), useLocal = T, data.dir='C:/LocalDataDump', socks=T,usepkg = "roracle")
  saveRDS(fixed_5Z, paste(dir,"/fixed_5Z_",year,".rds",sep=""))

  #Local copies of relevant VMS data
  mobileVMSRaw <- Mar.utils::VMS_from_MARFIS(df=mobile_5Z$marf$MARF_TRIPS, VR_field = "VR_NUMBER_FISHING", usepkg =pkg, make_segments = F, LANDED_field = "T_DATE2" )
  mobileVMSRaw <- mobileVMSRaw[["marf_VMS"]]
  saveRDS(mobileVMSRaw, paste(dir,"/mobile_5Z_VMS_",year,".rds",sep=""))

  fixedVMSRaw <- Mar.utils::VMS_from_MARFIS(df=fixed_5Z$marf$MARF_TRIPS, VR_field = "VR_NUMBER_FISHING", usepkg = "roracle", make_segments = F, LANDED_field = "T_DATE2" ) #Depending on the license conditions, there may only be VMS data for unobserved trips of mobile gear
  fixedVMSRaw <- fixedVMSRaw[["marf_VMS"]]
  saveRDS(fixedVMSRaw, paste(dir,"/fixed_5Z_VMS_",year,".rds",sep=""))

}

