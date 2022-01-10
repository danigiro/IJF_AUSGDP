rm(list = ls(all = TRUE))
library(forecast)
library(tidyverse)
library(zoo)

load("./BaseForecasts/Income/INC_basef.RData")
C <- S[1:(n-m),]

mvdf <- tibble("Date" = character(),
               "Series" = character(),
               "Forecast Horizon" = integer(),
               "Mean" = double(),
               "SeasVar" = double(),
               "MeanVar" = double(),
               "fc_scale" = double(),
               "Training window_length" = integer(),
               "Replication" = integer())
# Seconds to d h m s
dhms <- function(t){
  paste(t %/% (60*60*24), "d ", 
        paste(formatC(t %/% (60*60) %% 24, width = 2, format = "d", flag = "0"), "h ",
              formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0"), "m ",
              formatC(t %% 60, width = 2, format = "d", flag = "0"), "s",
              sep = ""), 
        sep="")
}

start_train <- c(1984, 4)
Time.index <- as.yearqtr(1984 + seq(0, 136)/4)
end_first_train <- c(1994, 2)
max_train=c(2017,4) #End of largest training set

first_train_length <- Inc %>% pull(.,1) %>% ts(start = c(1984, 4), frequency = 4) %>% window(end=c(1994, 2)) %>% length() #Length of first training set

test_length <- Inc %>% pull(.,1) %>% ts(start = c(1984, 4), frequency = 4) %>% window(start=end_first_train+c(0,1), end=max_train) %>% length #Maximum time the window expands

H <- 4

time_start <- Sys.time()
residuals_mean <- list()
for (j in 1:test_length) { #test_length
  time_start_par <- Sys.time()
  #Subsetting training and testing sets
  Train <- Inc[1:(first_train_length + j),]
  Test <- Inc[(first_train_length + j+1):(first_train_length + j + H),]
  
  date <- DF %>% filter(`F-method`=="ARIMA", 
                        Replication == j, 
                        Series == "Gdpe") %>% 
    pull(`Year, Qtr of forecast`)
  
  obj <- apply(Train, 2, function(x){
    dfm <- tibble(x = x, time = rep(1:4, length.out = NROW(Train))) 
    dfmm <- dfm %>%
      drop_na() %>%
      group_by(time) %>%
      summarise(mean = mean(x), .groups = "drop")
    dfv <- dfm %>%
      drop_na() %>%
      group_by(time) %>%
      summarise(var = var(x), .groups = "drop")
    n2 <- inner_join(dfm, dfmm, by = "time") %>% drop_na() %>%
      summarise(n2 = (x-mean)^2) %>% pull()
    fc_scale <- mean(abs(diff(x, 4)))
    list(v = sum(n2)/(length(n2)-4), m = pull(dfmm, var = "mean"),
         vs = pull(dfv, var = "var"), fc_scale = fc_scale)
  })
  
  
  mMatrix <- do.call("cbind", lapply(obj, function(x) x[["m"]]))
  var <- sapply(obj, function(x) x[["v"]])
  vMatrix <- do.call("cbind", lapply(obj, function(x) x[["vs"]]))
  fc_scale <- sapply(obj, function(x) x[["fc_scale"]])
  
  for(h in 1:sum(!is.na(Test[,1]))){
    mvdf <- mvdf %>% add_row("Date" = date[h],
                             "Series" = colnames(mMatrix),
                             "Forecast Horizon" = h,
                             "Mean" = mMatrix[h,],
                             "SeasVar" = vMatrix[h,], 
                             "MeanVar" = var,
                             "fc_scale" = fc_scale,
                             "Training window_length" = first_train_length + j,
                             "Replication" = j)
  }
  
  
  means <- do.call(rbind, rep(list(mMatrix[,colnames(Train)]), NROW(Train)/NROW(mMatrix)))
  residuals_mean[[j]] <- Train - means
  
  time_end <- Sys.time()
  parJ <- test_length-j
  par_time <- as.numeric(difftime(time_end, time_start_par, units = "secs"))
  tot_time <- as.numeric(difftime(time_end, time_start, units = "secs"))
  cat("\nTraining window n.", j, "\nElapsed time: ", dhms(tot_time),"\nEstimated time remaining: ", 
      dhms(mean(par_time)*parJ),"\n", sep="")
}

save(mvdf, residuals_mean, file = "./BaseForecasts/Income/INC_means_sa.RData")
# 
# DFbase <- DF %>% filter(`F-method`=="ARIMA")
# save(DFbase, resmat_ARIMA, Inc, C, file = "./BaseForecasts/Income/INC_arima_bf.RData")
# 
# 
# DFbase <- DF %>% filter(`F-method`=="ETS")
# save(DFbase, resmat_ETS, Inc, C, file = "./BaseForecasts/Income/INC_ets_bf.RData")
