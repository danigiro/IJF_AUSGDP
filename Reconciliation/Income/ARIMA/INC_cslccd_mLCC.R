#' -----------------------------------------------------------------------------
#' INC_cslccd_mLCC.R
#'
#' Creating an RData file of conditional levels reconciled forecasts for GDP 
#' (cross-sectional framework) - diagonal covariance matrix 
#' 
#' Base forecasts: ARIMA
#' 
#' Reconcile forecasts:
#'       - mLCC (means + exogenous + diagonal cov)
#'
#' Input files: VN555_base.RData
#' Output files: VN555_cslccd_mLCC.RData
#'
#' This code is written by Daniele Girolimetto
#' Department of Statistics, University of Padua (Italy)
#' -----------------------------------------------------------------------------
rm(list = ls(all = TRUE))
library(FoReco)
library(tidyverse)

load("./BaseForecasts/Income/INC_arima_bf.RData")
load("./BaseForecasts/Income/INC_means.RData")
source("./Reconciliation/C_balanced.R")
DF <- NULL
obj_bal <- C_balanced(C = C, nl = c(1,1,2,1,1))
C <- obj_bal$Cb
nl <- c(1, 3, 5, 7, 8)
id_unbal <- which(obj_bal$id_bal %in% c(1:6))
id_unbal <- id_unbal[!duplicated(obj_bal$id_bal[id_unbal])]

test_length <- as.numeric(max(DFbase$Replication))

mvdf$Series <- factor(mvdf$Series, colnames(Inc), ordered = TRUE)
DFbase$Series <- factor(DFbase$Series, colnames(Inc), ordered = TRUE)
resmat_all <- resmat_ARIMA
time_cslev <- array(NA, dim = c(test_length, 1, 1),
                    dimnames = list(NULL, NULL, c("bCCCexod")))

for (j in 1:test_length) {
    basef <- DFbase %>% filter(Replication==j) %>% 
      arrange(Series) %>%
      select(Series, Forecasts, `Forecast Horizon`) %>% 
      pivot_wider(names_from = Series, values_from = Forecasts) %>%
      arrange(`Forecast Horizon`) %>% 
      select(-`Forecast Horizon`) %>% as.matrix()
    
    means <- mvdf %>% filter(Replication==j) %>% 
      arrange(Series) %>%
      select(Series, Mean, `Forecast Horizon`) %>% 
      pivot_wider(names_from = Series, values_from = Mean) %>%
      arrange(`Forecast Horizon`) %>% 
      select(-`Forecast Horizon`) %>% as.matrix()
    
    fixv <- mvdf %>% filter(Replication==j) %>% 
      arrange(Series) %>%
      select(Series, MeanVar, `Forecast Horizon`) %>% 
      pivot_wider(names_from = Series, values_from = MeanVar) %>%
      arrange(`Forecast Horizon`) %>% 
      select(-`Forecast Horizon`) %>% as.matrix()
    fixv <- fixv[1,]
    
    basef[, colnames(Inc)[-c(1:27)]] <- means[, colnames(Inc)[-c(1:27)]]
    
    fixv <- c(fixv[obj_bal$id_bal], fixv[-c(1:6)])
    
    basef <- cbind(basef[, obj_bal$id_bal, drop = FALSE], basef[,-c(1:6), drop = FALSE])
    
    # Allocate the matrix for the reconcile forecasts with origin j 
    Fltr <- DFbase %>% filter(Replication==j) %>% 
      arrange(Series) %>%
      dplyr::select(-"Forecasts", -"R-method") %>% 
      arrange(Series, `Forecast Horizon`)
    
    ## Reconciliation ----
    Start <- Sys.time()
    objH <- suppressWarnings(lccrec(basef = basef, C = C, nl = nl, 
                                    CCC = TRUE, weights = fixv[-c(1:NROW(C))]))
    End <- Sys.time()
    time_cslev[j,1,1] <- as.numeric(difftime(End, Start, units = "secs"))
    
    nameLCC <- paste0("mLCC", sapply(names(objH$levrecf), 
                                     function(x) strsplit(x, "_")[[1]][3]))
    for(lev in 1:length(objH$levrecf)){
      objH$levrecf[[lev]] <- cbind(objH$levrecf[[lev]][, id_unbal, drop = FALSE], objH$levrecf[[lev]][, -c(1:NROW(C)), drop = FALSE])
      Df1 <- cbind(Fltr, "Forecasts" = as.vector(objH$levrecf[[lev]]),
                   "R-method" = "cslcc", "R-comb" = nameLCC[lev], 
                   nn = all(objH$levrecf[[lev]]>=0), 
                   FoReco = "direct")
      Df1 <- Df1[c(names(DFbase), "R-comb", "nn", "FoReco")]
      DF <- rbind(DF, Df1)
    }
    
    cat("Forecast origin number ", j, " (out of ", test_length, ", ",j/test_length*100,"%)\n", sep = "")
}

DFcslev <- DF
save(DFcslev, time_cslev, 
     file="./Reconciliation/Income/ARIMA/INC_cslccd_mLCC.RData")
