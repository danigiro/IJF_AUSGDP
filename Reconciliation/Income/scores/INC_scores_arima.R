rm(list = ls(all = TRUE))
libs <- c("FoReco","tidyverse")
invisible(lapply(libs, library, character.only = TRUE))
rm(libs)
source("./Reconciliation/Income/function/mcb_data_multilevel.R")
load("./BaseForecasts/Income/INC_arima_bf.RData")

series <- colnames(Inc)
DFbase <- DFbase %>% mutate(Series = factor(Series, series, ordered = TRUE))
m <- 1
nb <- 10
# test_list: list with a test observation matrix for each forecasts origin
# base_list: list with a base forecasts matrix for each forecasts origin
test_list <- list()
base_list <- list()
for(j in 1:max(DFbase$Replication)){
  test_list[[j]] <- DFbase %>% filter(Replication==j) %>% 
    select(Series, Actual, `Forecast Horizon`) %>% 
    pivot_wider(names_from = Series, values_from = Actual) %>%
    arrange(`Forecast Horizon`) %>% 
    select(-`Forecast Horizon`) %>% as.matrix()
  
  if(NROW(test_list[[j]])<4){
    test_list[[j]] <- rbind(test_list[[j]], matrix(0, nrow = 4-NROW(test_list[[j]]), ncol = NCOL(test_list[[j]])))
  }
  
  test_list[[j]] <- t(test_list[[j]])
  
  base_list[[j]] <- DFbase %>% filter(Replication==j) %>% 
    select(Series, Forecasts, `Forecast Horizon`) %>% 
    pivot_wider(names_from = Series, values_from = Forecasts) %>%
    arrange(`Forecast Horizon`) %>% 
    select(-`Forecast Horizon`) %>% as.matrix()
  
  if(NROW(base_list[[j]])<4){
    base_list[[j]] <- rbind(base_list[[j]], matrix(0, nrow = 4-NROW(base_list[[j]]), ncol = NCOL(base_list[[j]])))
  }
  
  base_list[[j]] <- t(base_list[[j]])
  
  cat(j, " ")
}

obj <- c("m", "nb", "test_list", "base_list", "obj", "series", "mcb_data")
rm(list=setdiff(ls(), obj))

source("./Reconciliation/Income/function/score_core.R")

save(mae, mse, imae, imse, gmae, gmse, 
     file = "./Reconciliation/Income/scores/INC_scores_arima.RData")
