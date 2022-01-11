#' -----------------------------------------------------------------------------
#' INC_CCCmix.R
#'
#' Averaging CCC using the bts base forecasts from both models (SA and ETS)
#'
#' Base forecasts: ARIMA
#'
#' Reconcile forecasts (+ nn):
#'       - mix-CCC (mCCC and bCCC)
#'
#' Input files: INC_cslccd.RData INC_cslccd_mean.RData INC_arima_bf.RData
#' Output files: INC_CCCmix.RData
#'
#' This code is written by Daniele Girolimetto
#' Department of Statistics, University of Padua (Italy)
#' -----------------------------------------------------------------------------
rm(list = ls(all = TRUE))
library(tidyverse)

load("./Reconciliation/Income/ARIMA/INC_cslccd.RData")
DFb <- DFcslev %>% filter(`R-comb` == "bCCCexod")
rm(list=setdiff(ls(), c("DFb")))
load("./Reconciliation/Income/ARIMA/INC_cslccd_mean.RData")
DFm <- DFcslcc %>% filter(`R-comb` == "mCCCexod")
rm(list=setdiff(ls(), c("DFb","DFm")))
load("./BaseForecasts/Income/INC_arima_bf.RData")
rm(list=setdiff(ls(), c("DFb","DFm", "Inc", "DFbase")))

DFb$Series <- factor(DFb$Series, colnames(Inc), ordered = TRUE)
DFm$Series <- factor(DFm$Series, colnames(Inc), ordered = TRUE)
DFbase$Series <- factor(DFbase$Series, colnames(Inc), ordered = TRUE)

test_length <- as.numeric(max(DFb$Replication))
DF <- NULL

for(j in 1:test_length){
  brecf <- DFb %>% 
    filter(Replication==j, FoReco == "direct") %>% 
    arrange(Series) %>%
    select(Series, Forecasts, `Forecast Horizon`) %>% 
    pivot_wider(names_from = Series, values_from = Forecasts) %>%
    arrange(`Forecast Horizon`) %>% 
    select(-`Forecast Horizon`) %>% as.matrix() %>% t()
  
  mrecf <- DFm %>% 
    filter(Replication==j, FoReco == "direct") %>% 
    arrange(Series) %>%
    select(Series, Forecasts, `Forecast Horizon`) %>% 
    pivot_wider(names_from = Series, values_from = Forecasts) %>%
    arrange(`Forecast Horizon`) %>% 
    select(-`Forecast Horizon`) %>% as.matrix() %>% t()
  
  Fltr <- DFbase  %>% 
    filter(Replication==j) %>% 
    dplyr::select(-"Forecasts", -"R-method") %>% 
    arrange(Series, `Forecast Horizon`)
  
  Recon_PointF <- (brecf+mrecf)/2
  nn_val <- all(Recon_PointF>=0)
  Df1 <- cbind(Fltr, "Forecasts" = as.vector(t(Recon_PointF)),
               "R-method" = "cs", "R-comb" = "mix-CCC", 
               nn = nn_val, 
               FoReco = "direct")
  Df1 <- Df1[names(DFb)]
  DF <- rbind(DF, Df1)
  
  cat(j, " ")
}
DFmix <- DF
save(DFmix, 
     file="./Reconciliation/Income/ARIMA/INC_CCCmix.RData")
