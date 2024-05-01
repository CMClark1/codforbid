#' Bootstrapped Landings Multiplier
#'
#' Bootstrap the landings multiplier and calculate variance and bias
#'@param data The output from the readydata function. Default is ready.
#'@param file The location of the file with the fleet/zone/quarter combinations. Default is "Discards_GroupedData.csv." This is the readydata output Discards_DataforGrouping_year.csv with an additional column for group.
#'@param nboot The number of bootstrap iterations required. Default is 1000.
#'@return A dataframe with the bootstrapped multiplier, variance, and bias, percentile confidence distribution at 0.05, bias corrected percentile confidence distribution at 0.05, and whether or not the discards are significant for a given group (>0.05).
#'@examples
#'example1 <- DO_BS(data=ready)
#'@import dplyr
#'@export
#'

DO_BS <- function(data=ready,file="Discards_GroupedData2022.csv",nboot=1000){

  grouped<-read.csv(file)
  data<-dplyr::left_join(data,grouped%>%dplyr::select(-X,-DGROUP,-COVERAGE,-OBS,-UNOBS))

  data=data%>%
    dplyr::filter(!is.na(GROUP), GROUP!="excluded") %>%
    dplyr::ungroup()%>%
    dplyr::mutate(COD=ifelse(COD==0,0.001,COD),
                  HAD=ifelse(HAD==0,0.001,HAD)) %>%
    dplyr::select(GROUP,COD,HAD,OBS)

  allmults <- data.frame()
  percout<-data.frame()

  df<-data.frame()

  for (g in unique(data$GROUP)) { #bootstrap the multiplier and calculate percentiles for each group, and append all groups together

    ready <- data %>% dplyr::filter(GROUP==g)

    bootstrap.mults <- rep(NA,nboot)
    set.seed(10)

    for(i in 1:nboot) {
      bootstrap.mults[i]<-DO_RATIOS(ready[sample(nrow(ready),size=nrow(ready),replace=TRUE),])
    }

    mult_g <- do.call(rbind, bootstrap.mults)
    mult_g_df <- data.frame(GROUP=g, MULT=mult_g)
    allmults <- rbind(allmults,mult_g_df)

    dat.samp<-mult_g_df%>%arrange(MULT)
    dat.samp$ID<-seq.int(nrow(dat.samp))
    dat.samp<-dat.samp%>%mutate(alpha=dat.samp$ID/nboot,
                                zalpha=qnorm(alpha,mean=0,sd=1))
    dat.samp$ID<-seq.int(nrow(dat.samp))
    dat.samp<-dat.samp%>%mutate(alpha=dat.samp$ID/nboot,
                                zalpha=qnorm(alpha,mean=0,sd=1))

    output<-data.frame(MULT=NA, omega=NA) #Create structure for loop output

    for (j in unique((dat.samp$MULT))){ #Loop
      temp<-subset(dat.samp, MULT<=j)
      omg<-nrow(temp)/nboot
      temp2<-data.frame(MULT=j, omega=omg)
      output<-rbind(output, temp2)
      output<-subset(output, !is.na(MULT))
    }

    dat.samp<-merge(dat.samp, output, all.x=TRUE) #Merge loop output into original

    dat.samp$z0 <- qnorm(dat.samp$omega,mean=0,sd=1)
    dat.samp$phibias <- pnorm((2*dat.samp$z0)+dat.samp$zalpha,0,1)
    dat.samp$phibias2 <- qnorm(dat.samp$phibias,0,1)
    dat.samp$phinobias <- pnorm((dat.samp$zalpha),0,1)
    dat.samp$phinobias2 <- qnorm(dat.samp$phinobias,0,1)

    output2<-data.frame(group=g,prob=seq(0.001,0.99,by=0.001))
    output2$perc <- quantile(dat.samp$phinobias2,seq(0.001,0.99,by=0.001),na.rm=TRUE)
    output2$bcperc <- quantile(dat.samp$phibias2,seq(0.001, 0.99, by=0.001),na.rm=TRUE)
      df<-rbind(df, output2)
  }

  result1 <- allmults %>%
    dplyr::filter(MULT>=0)%>%
    dplyr::group_by(GROUP) %>%
    dplyr::mutate(actual=MULT,predicted=mean(MULT)) %>%
    dplyr::summarise(MEANMULT=mean(MULT),
                     VAR=var(MULT),
                     BIAS=Metrics::bias(MULT,predicted))

  df2<-df%>%
    dplyr::left_join(result1%>%select(GROUP,MEANMULT)%>%
                       dplyr::rename(group=GROUP))%>%
    dplyr::mutate(perc2=perc*MEANMULT, bcperc2=bcperc*MEANMULT)

  print(df2)

}

