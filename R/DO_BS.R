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

DO_BS <- function(data=ready,file="Discards_GroupedData.csv",nboot=1000){

  grouped<-read.csv(file)
  data<-dplyr::full_join(data,grouped%>%dplyr::select(-X,-DGROUP,-COVERAGE,-OBS,-UNOBS))

  data=data%>%
    dplyr::filter(!is.na(GROUP), GROUP!="excluded") %>%
    dplyr::ungroup()%>%
    dplyr::mutate(COD=ifelse(COD==0,0.001,COD),
                  HAD=ifelse(HAD==0,0.001,HAD)) %>%
    dplyr::select(GROUP,COD,HAD,OBS)

  allmults <- data.frame()

  for (g in unique(data$GROUP)) {

    ready <- data %>% dplyr::filter(GROUP==g)

    bootstrap.mults <- rep(NA,nboot)
    set.seed(10)

    for(i in 1:nboot) {
      bootstrap.mults[i]<-DO_RATIOS(ready[sample(nrow(ready),size=nrow(ready),replace=TRUE),])
    }

    mult_g <- do.call(rbind, bootstrap.mults)
    mult_g_df <- data.frame(GROUP=g, MULT=mult_g)
    allmults <- rbind(allmults,mult_g_df)

  }

  result1 <- allmults %>%
    dplyr::filter(!is.nan(MULT) & !is.na(MULT))%>%
    dplyr::group_by(GROUP) %>%
    dplyr::mutate(actual=MULT,predicted=mean(MULT)) %>%
    dplyr::summarise(MULT2=mean(MULT),
                     VAR=var(MULT),
                     BIAS=Metrics::bias(MULT,predicted))

  GROUP <- unique(allmults$GROUP)
  PROB <- seq(0.001,1,0.001)
  GROUP2 <- data.frame(GROUP2=sort(rep(GROUP,length(PROB))),PROB2=rep(PROB,length(GROUP)))

df_total = data.frame()
for (i in PROB){
  dat <- allmults %>%
      dplyr::group_by(GROUP) %>%
      dplyr::filter(!is.na(MULT),!is.nan(MULT),MULT!=Inf,MULT!=-Inf)%>%
      dplyr::filter(MULT<i) %>%
      dplyr::tally()%>%
      dplyr::mutate(prob=i,
                    perc=(n/nboot)*100)%>%
      dplyr::select(-n)
  df <- data.frame(dat)
  df_total <- rbind(df_total,df)
}

ggplot2::ggplot(data=df_total, ggplot2::aes(x=perc,y=prob))+ggplot2::geom_path()+ggplot2::facet_wrap(GROUP~., scales="free")+ggplot2::geom_vline(ggplot2::aes(xintercept=1))

GROUP <- data.frame(GROUP=GROUP)
significance <- GROUP %>%
  dplyr::left_join(df_total%>%dplyr::filter(prob==0.05)%>%dplyr::select(-prob)) %>% dplyr::mutate(PERC=ifelse(is.na(perc),0,perc),BCPERC=NA,SIGNIFICANT=ifelse(PERC>1,"YES","NO")) %>% dplyr::select(-perc)

landings <- data %>%group_by(GROUP,OBS)%>%summarise(COD=sum(COD))%>%tidyr::pivot_wider(id_cols=GROUP, names_from=OBS, values_from=COD) %>% rename(OBS=Y,UNOBS=N)

results <- dplyr::full_join(result1,significance) %>% dplyr::left_join(landings) %>%
  dplyr::mutate(SIGDISC=ifelse(SIGNIFICANT=="YES" & MULT2>1,(MULT2*UNOBS)-UNOBS,0))

  print(results)

}
