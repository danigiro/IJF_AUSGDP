#' -----------------------------------------------------------------------------
#' EXP_htsrec.R
#'
#' Creating an RData file of cross sectional reconciled forecasts for GDP Exp
#' 
#' Base forecasts: ARIMA
#' 
#' Reconcile forecasts:
#'       - bu (Bottom-up)
#'       - ols (Identity)
#'       - struc (Structural variances)
#'       - wls (Series variances) 
#'       - shr (Shrunk covariance matrix)
#'
#' Input files: EXP_arima_bf.RData
#' Output files: EXP_htsrec.RData
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

DF <- NULL
resmat_all <- resmat_ARIMA

DFbase$Series <- factor(DFbase$Series, colnames(Exp), ordered = TRUE)
test_length <- as.numeric(max(DFbase$Replication))
ty_hts <- c("bu", "ols", "struc" ,"wls","shr")

time_hts <- array(0, dim=c(test_length, 1, NROW(ty_hts)))
for (j in 1:test_length) { #test_length
    resmat <- resmat_all[[j]]
    basef <- DFbase %>% filter(Replication==j) %>% 
      select(Series, Forecasts, `Forecast Horizon`) %>% 
      pivot_wider(names_from = Series, values_from = Forecasts) %>%
      arrange(`Forecast Horizon`) %>% 
      select(-`Forecast Horizon`) %>% as.matrix()

    Fltr <- DFbase %>% filter(Replication==j) %>% 
      dplyr::select(-"Forecasts", -"R-method") %>% arrange(Series, `Forecast Horizon`)

    ## Reconciliation ----
    for(l in 1:NROW(ty_hts)){
      Start <- Sys.time()
      Recon_PointF <- htsrec(basef = basef, C = C, comb = ty_hts[l], res = resmat, 
                             type = "M", keep = "recf")
      End <- Sys.time()
      time_hts[j,1,l] <- as.numeric(difftime(End, Start, units = "secs"))
      
      # Add rows to DF
      Df1 <- cbind(Fltr, "Forecasts" = as.vector(Recon_PointF),
                   "R-method" = "hts", "R-comb" = ty_hts[l], 
                   nn = all(Recon_PointF>=0), 
                   FoReco = "direct")
      Df1 <- Df1[c(names(DFbase), "R-comb", "nn", "FoReco")]
      DF <- rbind(DF, Df1)
      cat(l)
    }
  cat(" Forecast origin number ", j, " (out of ", test_length, ", ",j/test_length*100,"%)\n", sep = "")
}

DFhts <- DF
save(DFhts, time_hts, 
     file="./Reconciliation/Expenditure/ARIMA/EXP_htsrec.RData")
