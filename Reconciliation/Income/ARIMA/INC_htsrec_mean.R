#' -----------------------------------------------------------------------------
#' INC_htsrec_mean.R
#'
#' Creating an RData file of cross sectional reconciled forecasts for VN525
#' 
#' Base forecasts: ARIMA lev
#' 
#' Reconcile forecasts (+ nn):
#'       - shr (Shrunk covariance matrix) + mean bts
#'       - wls + mean bts
#'       - ols + mean bts
#'
#' Input files: INC_arima_bf.RData, INC_means.RData
#' Output files: INC_htsrec_mean.RData
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
DF <- NULL
resmat_all <- resmat_ARIMA

DFbase$Series <- factor(DFbase$Series, colnames(Inc), ordered = TRUE)
mvdf$Series <- factor(mvdf$Series, colnames(Inc), ordered = TRUE)
test_length <- as.numeric(max(DFbase$Replication))

ty_hts <- c("shr", "wls", "ols")

bts <- colnames(Inc)[-c(1:NROW(C))]
time_hts <- array(0, dim=c(test_length, 1, NROW(ty_hts)))
for (j in 1:test_length) {
    resmat <- resmat_all[[j]]
    colnames(resmat) <- colnames(Inc)
    resmat_mean <- as.matrix(residuals_mean[[j]])
    
    basef <- DFbase %>% filter(Replication==j) %>% 
      select(Series, Forecasts, `Forecast Horizon`) %>% 
      arrange(Series)%>%
      pivot_wider(names_from = Series, values_from = Forecasts) %>%
      arrange(`Forecast Horizon`) %>% 
      select(-`Forecast Horizon`) %>% as.matrix()
    
    means <- mvdf %>% filter(Replication==j) %>% 
      select(Series, Mean, `Forecast Horizon`) %>% 
      arrange(Series)%>%
      pivot_wider(names_from = Series, values_from = Mean) %>%
      arrange(`Forecast Horizon`) %>% 
      select(-`Forecast Horizon`) %>% as.matrix()
    
    basef[, bts] <- means[, bts]
    resmat[, bts] <- resmat_mean[, bts]
    
    Fltr <- DFbase %>% filter(Replication==j) %>% 
      dplyr::select(-"Forecasts", -"R-method") %>% 
      arrange(Series, `Forecast Horizon`)

    ## Reconciliation ----
    for(l in 1:NROW(ty_hts)){
      Start <- Sys.time()
      Recon_PointF <- htsrec(basef = basef, C = C, comb = ty_hts[l], res = resmat, 
                             type = "M", keep = "recf")
      End <- Sys.time()
      time_hts[j,1,l] <- as.numeric(difftime(End, Start, units = "secs"))
      
      # Add rows to DF
      Df1 <- cbind(Fltr, "Forecasts" = as.vector(Recon_PointF),
                   "R-method" = "cs", "R-comb" = ty_hts[l], 
                   nn = all(Recon_PointF>=0), 
                   FoReco = "direct")
      Df1 <- Df1[c(names(DFbase), "R-comb", "nn", "FoReco")]
      DF <- rbind(DF, Df1)
    }
  cat("Forecast origin number ", j, " (out of ", test_length, ", ",j/test_length*100,"%)\n", sep = "")
}

DF$`R-comb` <- paste0("m-",DF$`R-comb`)
DFhts <- DF
save(DFhts, time_hts,
     file="./Reconciliation/Income/ARIMA/INC_htsrec_mean.RData")
