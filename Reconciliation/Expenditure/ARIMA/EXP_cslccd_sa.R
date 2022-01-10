#' -----------------------------------------------------------------------------
#' EXP_cslccd_sa.R
#'
#' Creating an RData file of conditional levels reconciled forecasts for GDPe 
#' (cross-sectional framework, balance version) - diagonal covariance matrix 
#' 
#' Base forecasts: ARIMA
#' 
#' Reconcile forecasts:
#'       - HCCCexod_sa (Hollyman + CCC + exogenous + diagonal cov)
#'
#' Input files: EXP_arima_bf.RData, EXP_means_sa.RData
#' Output files: EXP_cslccd_sa.RData
#'
#' This code is written by Daniele Girolimetto
#' Department of Statistics, University of Padua (Italy)
#' -----------------------------------------------------------------------------
rm(list = ls(all = TRUE))
libs <- c("tidyverse")
invisible(lapply(libs, library, character.only = TRUE))
rm(libs)

library(FoReco)
load("./BaseForecasts/Expenditure/EXP_arima_bf.RData")
load("./BaseForecasts/Expenditure/EXP_means_sa.RData")
source("./Reconciliation/C_balanced.R")
DF <- NULL
obj_bal <- C_balanced(C = C, nl = c(1,1,2,3,4,12,4))
C <- obj_bal$Cb
nl <- obj_bal$nl
id_unbal <- obj_bal$id_unbal

test_length <- as.numeric(max(DFbase$Replication))

mvdf$Series <- factor(mvdf$Series, colnames(Exp), ordered = TRUE)
DFbase$Series <- factor(DFbase$Series, colnames(Exp), ordered = TRUE)
resmat_all <- resmat_ARIMA
time_cslev <- array(NA, dim = c(test_length, 1, 1),
                    dimnames = list(NULL, NULL, c("HCCCexod_sa")))

for (j in 1:test_length) { #test_length
  resmat <- resmat_all[[j]]
  mse <- diag(crossprod(resmat)/NROW(resmat))
  
  basef <- DFbase %>% filter(Replication==j) %>% 
    arrange(Series) %>%
    select(Series, Forecasts, `Forecast Horizon`) %>% 
    pivot_wider(names_from = Series, values_from = Forecasts) %>%
    arrange(`Forecast Horizon`) %>% 
    select(-`Forecast Horizon`) %>% as.matrix()
  
  bnaive <- mvdf %>% filter(Replication==j) %>% 
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
  
  
  fixv <- c(fixv[obj_bal$id_bal], fixv[-c(1:27)])
  mse <- c(mse[obj_bal$id_bal], mse[-c(1:27)])
  
  basef <- cbind(basef[, obj_bal$id_bal, drop = FALSE], basef[,-c(1:27), drop = FALSE])
  bnaive <- cbind(bnaive[, obj_bal$id_bal, drop = FALSE], bnaive[,-c(1:27), drop = FALSE])
  
  # Allocate the matrix for the reconcile forecasts with origin j 
  Fltr <- DFbase %>% filter(Replication==j) %>% 
    arrange(Series) %>%
    dplyr::select(-"Forecasts", -"R-method") %>% 
    arrange(Series, `Forecast Horizon`)
  
  ## Reconciliation ----
  # HCCCexod (Hollyman + CCC + exogenous + diagonal cov) ----
  Start <- Sys.time()
  objH <- suppressWarnings(lccrec(basef = basef, C = C, nl = nl, 
                                  bnaive = bnaive[,-c(1:NROW(C))],
                                  CCC = TRUE, weights = fixv[-c(1:NROW(C))]))
  End <- Sys.time()
  time_cslev[j,1,1] <- as.numeric(difftime(End, Start, units = "secs"))
  
  Recon_PointF <- objH$recf
  Recon_PointF <- cbind(Recon_PointF[, id_unbal, drop = FALSE], Recon_PointF[, -c(1:NROW(C)), drop = FALSE])
  
  Df1 <- cbind(Fltr, "Forecasts" = as.vector(Recon_PointF),
               "R-method" = "cslcc", "R-comb" = "HCCCexod_sa", 
               nn = all(Recon_PointF>=0), 
               FoReco = "direct")
  Df1 <- Df1[c(names(DFbase), "R-comb", "nn", "FoReco")]
  DF <- rbind(DF, Df1)
  
  cat("Forecast origin number ", j, " (out of ", test_length, ", ",j/test_length*100,"%)\n", sep = "")
}

DFcslev <- DF
save(DFcslev, time_cslev, 
     file = "./Reconciliation/Expenditure/ARIMA/EXP_cslccd_sa.RData")
