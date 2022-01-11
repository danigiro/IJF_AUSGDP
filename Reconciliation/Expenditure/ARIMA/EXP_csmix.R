#' -----------------------------------------------------------------------------
#' EXP_csmix.R
#'
#' Averaging hts using the bts base forecasts from both models (SRW and ETS)
#' for GDP Exp
#'
#' Base forecasts: ARIMA
#'
#' Reconcile forecasts (+ nn):
#'       - mix-wls (wls and m-wls)
#'       - mix-ols (ols and m-ols)
#'       - mix-shr (shr and m-shr)
#'
#' Input files: EXP_htsrec.RData EXP_htsrec_mean.RData EXP_arima_bf.RData
#' Output files: EXP_csmix.RData
#'
#' This code is written by Daniele Girolimetto
#' Department of Statistics, University of Padua (Italy)
#' -----------------------------------------------------------------------------
rm(list = ls(all = TRUE))
library(tidyverse)

load("./Reconciliation/Expenditure/ARIMA/EXP_htsrec.RData")
DFb <- DFhts %>% filter(`R-comb` %in% c("shr", "wls", "ols"))
rm(list=setdiff(ls(), c("DFb")))
load("./Reconciliation/Expenditure/ARIMA/EXP_htsrec_mean.RData")
DFm <- DFhts %>% filter(`R-comb` %in% c("m-shr", "m-wls", "m-ols"))
rm(list=setdiff(ls(), c("DFb","DFm")))
load("./BaseForecasts/Expenditure/EXP_arima_bf.RData")

DFb$Series <- factor(DFb$Series, colnames(Exp), ordered = TRUE)
DFm$Series <- factor(DFm$Series, colnames(Exp), ordered = TRUE)
test_length <- as.numeric(max(DFb$Replication))
DF <- NULL
ty <- unique(DFb$`R-comb`)
names(ty) <- paste0("m-", ty)
for(j in 1:test_length){
  for(i in 1:length(ty)){
    brecf <- DFb %>% 
      filter(Replication==j, `R-comb` == ty[i], FoReco == "direct") %>% 
      arrange(Series) %>%
      select(Series, Forecasts, `Forecast Horizon`) %>% 
      pivot_wider(names_from = Series, values_from = Forecasts) %>%
      arrange(`Forecast Horizon`) %>% 
      select(-`Forecast Horizon`) %>% as.matrix() %>% t()
    
    mrecf <- DFm %>% 
      filter(Replication==j, `R-comb` == names(ty[i]), FoReco == "direct") %>% 
      arrange(Series) %>%
      select(Series, Forecasts, `Forecast Horizon`) %>% 
      pivot_wider(names_from = Series, values_from = Forecasts) %>%
      arrange(`Forecast Horizon`) %>% 
      select(-`Forecast Horizon`) %>% as.matrix() %>% t()
    
    Fltr <- DFb %>% 
      filter(Replication==j, `R-comb` == ty[i], FoReco == "direct") %>% 
      dplyr::select(-"Forecasts", -"R-method", -"R-comb", -"FoReco", -"nn") %>% 
      arrange(Series, `Forecast Horizon`)
    
    Recon_PointF <- (brecf+mrecf)/2
    nn_val <- all(Recon_PointF>=0)
    Df1 <- cbind(Fltr, "Forecasts" = as.vector(t(Recon_PointF)),
                 "R-method" = "cs", "R-comb" = paste0("mix-", ty[i]), 
                 nn = nn_val, 
                 FoReco = "direct")
    Df1 <- Df1[names(DFb)]
    DF <- rbind(DF, Df1)
  }
  cat(j, " ")
}
DFmix <- DF
save(DFmix, 
     file="./Reconciliation/Expenditure/ARIMA/EXP_csmix.RData")
