all <- list()
nl_v <- c(1,1,2,3,4,12,4)
h <- 4
obj <- c(obj, "all", "i", "nl_v", "h")
i=0

### Mean ----
print("------- Mean -------")
try(expr = {
  load("./BaseForecasts/Expenditure/EXP_means.RData")
  DFmean <- mvdf
  DFmean <- DFmean %>% rename(Forecasts = Mean) %>%
    add_column(`F-method` = "Mean", `R-method` = "Mean", `R-comb` = "Mean", K = 1) %>%
    mutate(Series = factor(Series, series, ordered = TRUE))
  i = i + 1
  all[[i]] <- mcb_data(DFmean)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

### Mean sa ----
print("------- Mean -------")
try(expr = {
  load("./BaseForecasts/Expenditure/EXP_means_sa.RData")
  DFmean <- mvdf
  DFmean <- DFmean %>% rename(Forecasts = Mean) %>%
    add_column(`F-method` = "Mean_sa", `R-method` = "Mean_sa", `R-comb` = "Mean_sa", K = 1) %>%
    mutate(Series = factor(Series, series, ordered = TRUE))
  i = i + 1
  all[[i]] <- mcb_data(DFmean)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

### ARIMAlev ----
## base ----
print("--------------------------")
try(expr = {
  load("./BaseForecasts/Expenditure/EXP_arima_bf.RData")
  DFbase <- DFbase %>% 
    add_column(`R-comb` = "base", K = 1) %>%
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFbase)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## hts ----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_htsrec.RData")
  DFhts <- DFhts %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFhts)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## hts_mean ----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_htsrec_mean.RData")
  DFhts <- DFhts %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFhts)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## csmix ----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_csmix.RData")
  DFmix <- DFmix %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFmix)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## cslccd ----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_cslccd.RData")
  DFcslev <- DFcslev %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFcslev)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## cslccd sa ----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_cslccd_sa.RData")
  DFcslev <- DFcslev %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFcslev)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## mixCCC ----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_CCCmix.RData")
  DFmix <- DFmix %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFmix)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## cslccd_bLCCendo ----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_cslccd_bLCCendo.RData")
  DFcslev <- DFcslev %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFcslev)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## cslccd_mLCC endo----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_cslccd_mLCCendo.RData")
  DFcslev <- DFcslev %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFcslev)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## mixCCC endo----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_CCCmix_endo.RData")
  DFmix <- DFmix %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFmix)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## cslccd_mean_red endo----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_cslccd_mean_red_endo.RData")
  DFcslcc <- DFcslcc %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFcslcc)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## mixCCCred endo----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_CCCmix_red_endo.RData")
  DFmix <- DFmix %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFmix)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## mixCCCred ----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_CCCmix_red.RData")
  DFmix <- DFmix %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFmix)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## cslccd_mean ----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_cslccd_mean.RData")
  DFcslcc <- DFcslcc %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFcslcc)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## cslccd_mean_red ----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_cslccd_mean_red.RData")
  DFcslcc <- DFcslcc %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFcslcc)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## cslccd_bCCCred ----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_cslccd_bCCCred.RData")
  DFcslcc <- DFcslcc %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFcslcc)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## cslccd_mLCC ----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_cslccd_mLCC.RData")
  DFcslev <- DFcslev %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFcslev)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

## cslccd_bLCC ----
print("--------------------------")
try(expr = {
  load("./Reconciliation/Expenditure/ARIMA/EXP_cslccd_bLCC.RData")
  DFcslev <- DFcslev %>% 
    add_column(K = 1) %>% 
    mutate(Series = factor(Series, series, ordered = TRUE))
  i <- i + 1
  all[[i]] <- mcb_data(DFcslev)
  rm(list=setdiff(ls(), obj))
}, silent = TRUE)

### DATASET ----
if(length(all)>0){
  mae <- lapply(all, function(x) x[["mae"]])
  mae <- do.call(rbind, mae)
  
  mse <- lapply(all, function(x) x[["mse"]])
  mse <- do.call(rbind, mse)
  
  gmae <- lapply(all, function(x) x[["gmae"]])
  gmae <- do.call(rbind, gmae)
  
  gmse <- lapply(all, function(x) x[["gmse"]])
  gmse <- do.call(rbind, gmse)
  
  imae <- lapply(all, function(x) x[["imae"]])
  imae <- do.call(rbind, imae)
  
  imse <- lapply(all, function(x) x[["imse"]])
  imse <- do.call(rbind, imse)
}
