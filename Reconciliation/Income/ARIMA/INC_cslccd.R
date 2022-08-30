#' -----------------------------------------------------------------------------
#' INC_cslccd.R
#'
#' Creating an RData file of conditional levels reconciled forecasts for GDP
#' (cross-sectional framework, balance version) - diagonal covariance matrix 
#' 
#' Base forecasts: ARIMA
#' 
#' Reconcile forecasts:
#'       - HCCCexod (Hollyman + CCC + exogenous + diagonal cov)
#'       - bCCCexod (basef + CCC + exogenous + diagonal cov)
#'       - HCCCendod (Hollyman + CCC + endogenous + diagonal cov)
#'       - bCCCendod (basef + CCC + endogenous + diagonal cov)
#'
#' Input files: INC_arima_bf.RData, INC_means.RData
#' Output files: INC_cslccd.RData
#'
#' This code is written by Daniele Girolimetto
#' Department of Statistics, University of Padua (Italy)
#' -----------------------------------------------------------------------------
rm(list = ls(all = TRUE))
libs <- c("tidyverse")
invisible(lapply(libs, library, character.only = TRUE))
rm(libs)

library(FoReco)
load("./BaseForecasts/Income/INC_arima_bf.RData")
load("./BaseForecasts/Income/INC_means.RData")
load("./Inc_bal.RData")
#source("./Reconciliation/C_balanced.R")
DF <- NULL
#obj_bal <- C_balanced(C = C, nl = c(1,1,2,1,1))
C <- obj_bal$Cb
nl <- c(1, 3, 5, 7, 8)
id_unbal <- which(obj_bal$id_bal %in% c(1:6))
id_unbal <- id_unbal[!duplicated(obj_bal$id_bal[id_unbal])]

test_length <- as.numeric(max(DFbase$Replication))

mvdf$Series <- factor(mvdf$Series, colnames(Inc), ordered = TRUE)
DFbase$Series <- factor(DFbase$Series, colnames(Inc), ordered = TRUE)
resmat_all <- resmat_ARIMA
time_cslev <- array(NA, dim = c(test_length, 1, 4),
                    dimnames = list(NULL, NULL, c("HCCCexod", "bCCCexod", 
                                                  "HCCCendod", "bCCCendod")))

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
  
  
  fixv <- c(fixv[obj_bal$id_bal], fixv[-c(1:6)])
  mse <- c(mse[obj_bal$id_bal], mse[-c(1:6)])
  
  basef <- cbind(basef[, obj_bal$id_bal, drop = FALSE], basef[,-c(1:6), drop = FALSE])
  bnaive <- cbind(bnaive[, obj_bal$id_bal, drop = FALSE], bnaive[,-c(1:6), drop = FALSE])
  
  # Allocate the matrix for the reconcile forecasts with origin j 
  Fltr <- DFbase %>% filter(Replication==j) %>% 
    arrange(Series) %>%
    dplyr::select(-"Forecasts", -"R-method") %>% 
    arrange(Series, `Forecast Horizon`)
  
  ## Reconciliation ----
  # HCCCexod (Hollyman + CCC + exogenous + diagonal cov) ----
  Start <- Sys.time()
  objH <- suppressMessages(lccrec(basef = basef, C = C, nl = nl, 
                                  bnaive = bnaive[,-c(1:NROW(C))],
                                  CCC = TRUE, weights = fixv[-c(1:NROW(C))]))
  End <- Sys.time()
  time_cslev[j,1,1] <- as.numeric(difftime(End, Start, units = "secs"))
  
  Recon_PointF <- cbind(objH$recf[, id_unbal, drop = FALSE], objH$recf[, -c(1:NROW(C)), drop = FALSE])
  
  Df1 <- cbind(Fltr, "Forecasts" = as.vector(Recon_PointF),
               "R-method" = "cslcc", "R-comb" = "HCCCexod", 
               nn = all(Recon_PointF>=0), 
               FoReco = "direct")
  Df1 <- Df1[c(names(DFbase), "R-comb", "nn", "FoReco")]
  DF <- rbind(DF, Df1)
  
  # bCCCexod (basef + CCC + exogenous + diagonal cov) ----
  Start <- Sys.time()
  objb <- suppressMessages(lccrec(basef = basef, C = C, nl = nl, 
                                  CCC = TRUE, weights = fixv[-c(1:NROW(C))]))
  End <- Sys.time()
  time_cslev[j,1,2] <- as.numeric(difftime(End, Start, units = "secs"))
  
  Recon_PointF <- cbind(objb$recf[, id_unbal, drop = FALSE], objb$recf[, -c(1:NROW(C)), drop = FALSE])
  
  Df1 <- cbind(Fltr, "Forecasts" = as.vector(Recon_PointF),
               "R-method" = "cslcc", "R-comb" = "bCCCexod", 
               nn = all(Recon_PointF>=0), 
               FoReco = "direct")
  Df1 <- Df1[c(names(DFbase), "R-comb", "nn", "FoReco")]
  DF <- rbind(DF, Df1)
  
  # HCCCendod (Hollyman + CCC + endogenous + diagonal cov) ----
  Start <- Sys.time()
  objHe <- suppressMessages(lccrec(basef = basef, C = C, nl = nl, 
                                   bnaive = bnaive[,-c(1:NROW(C))], const = "endo",
                                   CCC = TRUE, weights = fixv))
  End <- Sys.time()
  time_cslev[j,1,3] <- as.numeric(difftime(End, Start, units = "secs"))
  
  Recon_PointF <- cbind(objHe$recf[, id_unbal, drop = FALSE], objHe$recf[, -c(1:NROW(C)), drop = FALSE])
  
  Df1 <- cbind(Fltr, "Forecasts" = as.vector(Recon_PointF),
               "R-method" = "cslcc", "R-comb" = "HCCCendod", 
               nn = all(Recon_PointF>=0), 
               FoReco = "direct")
  Df1 <- Df1[c(names(DFbase), "R-comb", "nn", "FoReco")]
  DF <- rbind(DF, Df1)
  
  # bCCCendod (basef + CCC + endogenous + diagonal cov) ----
  Start <- Sys.time()
  objbe <- suppressMessages(lccrec(basef = basef, C = C, nl = nl, const = "endo",
                                   CCC = TRUE, weights = fixv))
  End <- Sys.time()
  time_cslev[j,1,4] <- as.numeric(difftime(End, Start, units = "secs"))
  
  Recon_PointF <- cbind(objbe$recf[, id_unbal, drop = FALSE], objbe$recf[, -c(1:NROW(C)), drop = FALSE])
  
  Df1 <- cbind(Fltr, "Forecasts" = as.vector(Recon_PointF),
               "R-method" = "cslcc", "R-comb" = "bCCCendod", 
               nn = all(Recon_PointF>=0), 
               FoReco = "direct")
  Df1 <- Df1[c(names(DFbase), "R-comb", "nn", "FoReco")]
  DF <- rbind(DF, Df1)
  
  cat("Forecast origin number ", j, " (out of ", test_length, ", ",j/test_length*100,"%)\n", sep = "")
}

DFcslev <- DF
save(DFcslev, time_cslev, 
     file = "./Reconciliation/Income/ARIMA/INC_cslccd.RData")
