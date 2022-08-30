rm(list = ls(all = TRUE))
library(FoReco)
library(tidyverse)

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
time_cslev <- array(NA, dim = c(test_length, 1, 1),
                    dimnames = list(NULL, NULL, c("bCCCendod")))

for (j in 1:test_length) {
    basef <- DFbase %>% filter(Replication==j) %>% 
      arrange(Series) %>%
      select(Series, Forecasts, `Forecast Horizon`) %>% 
      pivot_wider(names_from = Series, values_from = Forecasts) %>%
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
    
    basef <- cbind(basef[, obj_bal$id_bal, drop = FALSE], basef[,-c(1:6), drop = FALSE])
    
    # Allocate the matrix for the reconcile forecasts with origin j 
    Fltr <- DFbase %>% filter(Replication==j) %>% 
      arrange(Series) %>%
      dplyr::select(-"Forecasts", -"R-method") %>% 
      arrange(Series, `Forecast Horizon`)
    
    ## Reconciliation ----
    # HCCCendod (Hollyman + CCC + endogenous + diagonal cov) ----
    Start <- Sys.time()
    objH <- suppressWarnings(lccrec(basef = basef, C = C, nl = nl, const = "endo",
                                    CCC = TRUE, weights = fixv))
    End <- Sys.time()
    time_cslev[j,1,1] <- as.numeric(difftime(End, Start, units = "secs"))
    
    bCCCred <- Reduce("+", objH$levrecf[-length(objH$levrecf)])/(length(objH$levrecf)-1)
    
    bCCCred <- cbind(bCCCred[, id_unbal, drop = FALSE], bCCCred[, -c(1:NROW(C)), drop = FALSE])
    
    Df1 <- cbind(Fltr, "Forecasts" = as.vector(bCCCred),
                 "R-method" = "cslcc", "R-comb" = "bCCCred_endo", 
                 nn = all(bCCCred>=0), 
                 FoReco = "direct")
    Df1 <- Df1[c(names(DFbase), "R-comb", "nn", "FoReco")]
    DF <- rbind(DF, Df1)
    
    cat("Forecast origin number ", j, " (out of ", test_length, ", ",j/test_length*100,"%)\n", sep = "")
}

DFcslcc <- DF
save(DFcslcc, time_cslev, 
     file="./Reconciliation/Income/ARIMA/INC_cslccd_bCCCred_endo.RData")