#' mcb_data
#'
#' @param DF tibble of forecasts
#'
#' @return A list with two dataset: mae and mse
#' @export
#'
mcb_data <- function(DF, ast = ""){
  if(!("FoReco" %in% colnames(DF))){
    recf_list <- NULL
    for(j in 1:max(DF$Replication)){
      recf_list[[j]] <- DF %>% 
        filter(Replication==j) %>% arrange(Series) %>%
        select(Series, K, Forecasts, `Forecast Horizon`) %>% 
        pivot_wider(names_from = Series, values_from = Forecasts) %>%
        arrange(K, `Forecast Horizon`) %>% 
        select(-K, -`Forecast Horizon`) %>% 
        as.matrix()
      
      if(NROW(recf_list[[j]])<h){
        recf_list[[j]] <- rbind(recf_list[[j]], matrix(0, nrow = h-NROW(recf_list[[j]]), ncol = NCOL(recf_list[[j]])))
      }
      
      recf_list[[j]] <- t(recf_list[[j]])
    }
    Fmeth <- DF$`F-method`[1]
    Rmeth <- DF$`R-method`[1]
    comb_name <- ifelse(all(DF$Forecasts>=0), DF$`R-comb`[1], 
                        paste(DF$`R-comb`[1], ast, sep=""))
    ind_mat <- score_index(recf = recf_list, 
                           test = test_list, 
                           base = base_list, 
                           m = m, nb = nb, 
                           type = "mae", 
                           compact = FALSE,
                           nl = nl_v)
    
    ind_mat_duo <- score_index(recf = recf_list, 
                               test = test_list, 
                               base = base_list, 
                               m = m, nb = nb, 
                               type = "mae", 
                               compact = FALSE)
    
    mat2 <- ind_mat$Rel_mat %>% 
      as_tibble() %>% 
      add_column(`F-method` = Fmeth,
                 `R-method` = Rmeth,
                 `R-comb` = comb_name,
                 nn_type=all(DF$Forecasts>=0),
                 series = rownames(ind_mat$Rel_mat), 
                 .before = 1)%>%
      pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                   names_to = c("K", "H"), names_sep = "h") %>% 
      mutate(K = 1) 
    
    mat2_cum <- ind_mat$Rel_mat_cum %>% 
      as_tibble() %>% 
      add_column(`F-method` = Fmeth,
                 `R-method` = Rmeth,
                 `R-comb` = comb_name,
                 nn_type=all(DF$Forecasts>=0),
                 series = rownames(ind_mat$Rel_mat_cum), 
                 .before = 1)%>%
      pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                   names_to = c("K", "H"), names_sep = "h") %>% 
      mutate(K = 1)
    
    mat2_all <- ind_mat$Avg_ik[,NCOL(ind_mat$Avg_ik), drop = FALSE] %>% 
      as_tibble() %>% 
      add_column(`F-method` = Fmeth,
                 `R-method` = Rmeth,
                 `R-comb` = comb_name,
                 nn_type=all(DF$Forecasts>=0),
                 series = rownames(ind_mat$Avg_ik), 
                 .before = 1)%>%
      pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                   names_to = "K") %>% 
      add_column(H = "all")
    
    imae <- rbind(mat2_cum, mat2, mat2_all)
    
    mat3 <- ind_mat$Avg_k %>% 
      as_tibble() %>% 
      add_column(`F-method` = Fmeth,
                 `R-method` = Rmeth,
                 `R-comb` = comb_name,
                 nn_type=all(DF$Forecasts>=0),
                 series = rownames(ind_mat$Avg_k), 
                 .before = 1)%>%
      pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                   names_to = c("K", "H"), names_sep = "h") %>% 
      mutate(K = 1)
    
    mat3_cum <- ind_mat$Avg_k_cum %>% 
      as_tibble() %>% 
      add_column(`F-method` = Fmeth,
                 `R-method` = Rmeth,
                 `R-comb` = comb_name,
                 nn_type=all(DF$Forecasts>=0),
                 series = rownames(ind_mat$Avg_k_cum), 
                 .before = 1)%>%
      pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                   names_to = c("K", "H"), names_sep = "h") %>% 
      mutate(K = 1)
    
    
    mat3_duo <- ind_mat_duo$Avg_k %>% 
      as_tibble() %>% 
      add_column(`F-method` = Fmeth,
                 `R-method` = Rmeth,
                 `R-comb` = comb_name,
                 nn_type=all(DF$Forecasts>=0),
                 series = rownames(ind_mat_duo$Avg_k), 
                 .before = 1) %>%
      filter(series != "all") %>%
      pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                   names_to = c("K", "H"), names_sep = "h") %>% 
      mutate(K =1)
    
    mat3_duo_cum <- ind_mat_duo$Avg_k_cum %>% 
      as_tibble() %>% 
      add_column(`F-method` = Fmeth,
                 `R-method` = Rmeth,
                 `R-comb` = comb_name,
                 nn_type=all(DF$Forecasts>=0),
                 series = rownames(ind_mat_duo$Avg_k_cum), 
                 .before = 1) %>%
      filter(series != "all") %>%
      pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                   names_to = c("K", "H"), names_sep = "h") %>% 
      mutate(K = 1)
    
    gmae <- rbind(mat3, mat3_cum, mat3_duo, mat3_duo_cum)
    
    score_mae <-cbind(`F-method` = Fmeth,
                      `R-method` = Rmeth,
                      `R-comb` = comb_name,
                      nn_type=all(DF$Forecasts>=0), 
                      k = rownames(ind_mat$Avg_mat), 
                      ind_mat$Avg_mat)
    rownames(score_mae) <- NULL
    score_mae_duo <-cbind(`F-method` = Fmeth,
                          `R-method` = Rmeth,
                          `R-comb` = comb_name,
                          nn_type=all(DF$Forecasts>=0), 
                          k = rownames(ind_mat_duo$Avg_mat), 
                          ind_mat_duo$Avg_mat)
    rownames(score_mae_duo) <- NULL
    
    mae <- merge(score_mae, score_mae_duo)
    
    ind_mat <- score_index(recf = recf_list, 
                           test = test_list, 
                           base = base_list, 
                           m = m, nb = nb, 
                           type = "mse", 
                           compact = FALSE,
                           nl = nl_v)
    
    ind_mat_duo <- score_index(recf = recf_list, 
                               test = test_list, 
                               base = base_list, 
                               m = m, nb = nb, 
                               type = "mse", 
                               compact = FALSE)
    
    mat2 <- ind_mat$Rel_mat %>% 
      as_tibble() %>% 
      add_column(`F-method` = Fmeth,
                 `R-method` = Rmeth,
                 `R-comb` = comb_name,
                 nn_type=all(DF$Forecasts>=0),
                 series = rownames(ind_mat$Rel_mat), 
                 .before = 1)%>%
      pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                   names_to = c("K", "H"), names_sep = "h") %>% 
      mutate(K = 1) 
    
    mat2_cum <- ind_mat$Rel_mat_cum %>% 
      as_tibble() %>% 
      add_column(`F-method` = Fmeth,
                 `R-method` = Rmeth,
                 `R-comb` = comb_name,
                 nn_type=all(DF$Forecasts>=0),
                 series = rownames(ind_mat$Rel_mat_cum), 
                 .before = 1)%>%
      pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                   names_to = c("K", "H"), names_sep = "h") %>% 
      mutate(K = 1)
    
    mat2_all <- ind_mat$Avg_ik[,NCOL(ind_mat$Avg_ik), drop = FALSE] %>% 
      as_tibble() %>% 
      add_column(`F-method` = Fmeth,
                 `R-method` = Rmeth,
                 `R-comb` = comb_name,
                 nn_type=all(DF$Forecasts>=0),
                 series = rownames(ind_mat$Avg_ik), 
                 .before = 1)%>%
      pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                   names_to = "K") %>% 
      add_column(H = "all")
    
    imse <- rbind(mat2_cum, mat2, mat2_all)
    
    mat3 <- ind_mat$Avg_k %>% 
      as_tibble() %>% 
      add_column(`F-method` = Fmeth,
                 `R-method` = Rmeth,
                 `R-comb` = comb_name,
                 nn_type=all(DF$Forecasts>=0),
                 series = rownames(ind_mat$Avg_k), 
                 .before = 1)%>%
      pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                   names_to = c("K", "H"), names_sep = "h") %>% 
      mutate(K = 1)
    
    mat3_cum <- ind_mat$Avg_k_cum %>% 
      as_tibble() %>% 
      add_column(`F-method` = Fmeth,
                 `R-method` = Rmeth,
                 `R-comb` = comb_name,
                 nn_type=all(DF$Forecasts>=0),
                 series = rownames(ind_mat$Avg_k_cum), 
                 .before = 1)%>%
      pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                   names_to = c("K", "H"), names_sep = "h") %>% 
      mutate(K = 1)
    
    mat3_duo <- ind_mat_duo$Avg_k %>% 
      as_tibble() %>% 
      add_column(`F-method` = Fmeth,
                 `R-method` = Rmeth,
                 `R-comb` = comb_name,
                 nn_type=all(DF$Forecasts>=0),
                 series = rownames(ind_mat_duo$Avg_k), 
                 .before = 1) %>%
      filter(series != "all") %>%
      pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                   names_to = c("K", "H"), names_sep = "h") %>% 
      mutate(K = 1)
    
    mat3_duo_cum <- ind_mat_duo$Avg_k_cum %>% 
      as_tibble() %>% 
      add_column(`F-method` = Fmeth,
                 `R-method` = Rmeth,
                 `R-comb` = comb_name,
                 nn_type=all(DF$Forecasts>=0),
                 series = rownames(ind_mat_duo$Avg_k_cum), 
                 .before = 1) %>%
      filter(series != "all") %>%
      pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                   names_to = c("K", "H"), names_sep = "h") %>% 
      mutate(K = 1)
    
    gmse <- rbind(mat3, mat3_cum, mat3_duo, mat3_duo_cum)
    
    score_mse <-cbind(`F-method` = Fmeth,
                      `R-method` = Rmeth,
                      `R-comb` = comb_name,
                      nn_type=all(DF$Forecasts>=0), 
                      k = rownames(ind_mat$Avg_mat), 
                      ind_mat$Avg_mat)
    rownames(score_mse) <- NULL
    score_mse_duo <-cbind(`F-method` = Fmeth,
                          `R-method` = Rmeth,
                          `R-comb` = comb_name,
                          nn_type=all(DF$Forecasts>=0), 
                          k = rownames(ind_mat_duo$Avg_mat), 
                          ind_mat_duo$Avg_mat)
    rownames(score_mse_duo) <- NULL
    
    mse <- merge(score_mse, score_mse_duo)
    rownames(mse) <- NULL
  } else if (DF$`R-method`[1] %in% c("kah", "tcs", "ctbu", "mean") | length(unique(DF$FoReco))==1) {
    # part ----
    combin <- rbind(DF %>%
                      group_by(`R-comb`) %>%
                      summarise(nn=all(nn), .groups="drop"))
    
    imae <- NULL
    imse <- NULL
    mae <- NULL
    mse <- NULL
    gmae <- NULL
    gmse <- NULL
    DFpar <- NULL
    Fmeth <- DF$`F-method`[1]
    for(i in 1:NROW(combin)){
      recf_list <- list()
      DFpar <- DF %>% 
        filter(`R-comb`==pull(combin[i,1]))
      Rmeth <- DFpar$`R-method`[1]
      comb_name <- ifelse(combin[i,2], pull(combin[i,1]), 
                          paste(pull(combin[i,1]), ast, sep=""))
      for(j in 1:max(DFpar$Replication)){
        recf_list[[j]] <- DFpar %>% 
          filter(Replication==j) %>% arrange(Series) %>%
          select(Series, K, Forecasts, `Forecast Horizon`) %>% 
          pivot_wider(names_from = Series, values_from = Forecasts) %>%
          arrange(K, `Forecast Horizon`) %>% 
          select(-K, -`Forecast Horizon`) %>% 
          as.matrix() 
        
        if(NROW(recf_list[[j]])<h){
          recf_list[[j]] <- rbind(recf_list[[j]], matrix(0, nrow = h-NROW(recf_list[[j]]), ncol = NCOL(recf_list[[j]])))
        }
        
        recf_list[[j]] <- t(recf_list[[j]])
      }
      ind_mat <- score_index(recf = recf_list, 
                             test = test_list, 
                             base = base_list, 
                             m = m, nb = nb, 
                             type = "mae", 
                             compact = FALSE,
                             nl = nl_v)
      
      ind_mat_duo <- score_index(recf = recf_list, 
                                 test = test_list, 
                                 base = base_list, 
                                 m = m, nb = nb, 
                                 type = "mae", 
                                 compact = FALSE)
      
      mat2 <- ind_mat$Rel_mat %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Rel_mat), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1) 
      
      mat2_cum <- ind_mat$Rel_mat_cum %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Rel_mat_cum), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      mat2_all <- ind_mat$Avg_ik[,NCOL(ind_mat$Avg_ik), drop = FALSE] %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Avg_ik), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = "K") %>% 
        add_column(H = "all")
      
      imae <- rbind(imae, mat2_cum, mat2, mat2_all)
      
      mat3 <- ind_mat$Avg_k %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Avg_k), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      mat3_cum <- ind_mat$Avg_k_cum %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Avg_k_cum), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      
      mat3_duo <- ind_mat_duo$Avg_k %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat_duo$Avg_k), 
                   .before = 1) %>%
        filter(series != "all") %>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      mat3_duo_cum <- ind_mat_duo$Avg_k_cum %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat_duo$Avg_k_cum), 
                   .before = 1) %>%
        filter(series != "all") %>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      gmae <- rbind(gmae, mat3, mat3_cum, mat3_duo, mat3_duo_cum)
      
      score_mae <-cbind(`F-method` = Fmeth,
                        `R-method` = Rmeth,
                        `R-comb` = comb_name,
                        nn_type=pull(combin[i,2]), 
                        k = rownames(ind_mat$Avg_mat), 
                        ind_mat$Avg_mat)
      rownames(score_mae) <- NULL
      score_mae_duo <-cbind(`F-method` = Fmeth,
                            `R-method` = Rmeth,
                            `R-comb` = comb_name,
                            nn_type=pull(combin[i,2]), 
                            k = rownames(ind_mat_duo$Avg_mat), 
                            ind_mat_duo$Avg_mat)
      rownames(score_mae_duo) <- NULL
      
      mae <- rbind(mae, merge(score_mae, score_mae_duo))
      
      ind_mat <- score_index(recf = recf_list, 
                             test = test_list, 
                             base = base_list, 
                             m = m, nb = nb, 
                             type = "mse", 
                             compact = FALSE,
                             nl = nl_v)
      
      ind_mat_duo <- score_index(recf = recf_list, 
                                 test = test_list, 
                                 base = base_list, 
                                 m = m, nb = nb, 
                                 type = "mse", 
                                 compact = FALSE)
      
      mat2 <- ind_mat$Rel_mat %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Rel_mat), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1) 
      
      mat2_cum <- ind_mat$Rel_mat_cum %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Rel_mat_cum), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      mat2_all <- ind_mat$Avg_ik[,NCOL(ind_mat$Avg_ik), drop = FALSE] %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Avg_ik), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = "K") %>% 
        add_column(H = "all")
      
      imse <- rbind(imse, mat2_cum, mat2, mat2_all)
      
      mat3 <- ind_mat$Avg_k %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Avg_k), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      mat3_cum <- ind_mat$Avg_k_cum %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Avg_k_cum), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      
      mat3_duo <- ind_mat_duo$Avg_k %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat_duo$Avg_k), 
                   .before = 1) %>%
        filter(series != "all") %>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      mat3_duo_cum <- ind_mat_duo$Avg_k_cum %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat_duo$Avg_k_cum), 
                   .before = 1) %>%
        filter(series != "all") %>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      gmse <- rbind(gmse, mat3, mat3_cum, mat3_duo, mat3_duo_cum)
      
      score_mse <-cbind(`F-method` = Fmeth,
                        `R-method` = Rmeth,
                        `R-comb` = comb_name,
                        nn_type=pull(combin[i,2]), 
                        k = rownames(ind_mat$Avg_mat), 
                        ind_mat$Avg_mat)
      rownames(score_mse) <- NULL
      score_mse_duo <-cbind(`F-method` = Fmeth,
                            `R-method` = Rmeth,
                            `R-comb` = comb_name,
                            nn_type=pull(combin[i,2]), 
                            k = rownames(ind_mat_duo$Avg_mat), 
                            ind_mat_duo$Avg_mat)
      rownames(score_mse_duo) <- NULL
      
      mse <- rbind(mse, merge(score_mse, score_mse_duo))
      
      cat("Score combination number ", i, " (out of ", NROW(combin), ", ",
          i/NROW(combin)*100,"%) - (", comb_name,")\n",sep = "")
    }
  } else {
    # free + nn ----
    DF1 <- DF %>% filter(FoReco=="direct")
    DF2 <- DF %>% filter(nn=="TRUE")
    
    combin <- rbind(DF1 %>%
                      group_by(`R-comb`) %>%
                      summarise(nn=all(nn), .groups="drop"),
                    DF2 %>%
                      group_by(`R-comb`) %>%
                      summarise(nn=all(nn), .groups="drop")) %>% unique()
    
    imae <- NULL
    imse <- NULL
    mae <- NULL
    mse <- NULL
    gmae <- NULL
    gmse <- NULL
    DFpar <- NULL
    Fmeth <- DF$`F-method`[1]
    for(i in 1:NROW(combin)){
      recf_list <- list()
      
      if (pull(combin[i,2])) {
        DFpar <- DF2 %>% 
          filter(`R-comb`==pull(combin[i,1]))
        comb_name <- pull(combin[i,1])
      } else {
        DFpar <- DF1 %>% 
          filter(`R-comb`==pull(combin[i,1]))
        comb_name <- ifelse(combin[i,2], pull(combin[i,1]), 
                            paste(pull(combin[i,1]), ast, sep=""))
      }
      if (DFpar$`R-method`[1] == "hts") {
        Rmeth <- "cs"
      } else if (DFpar$`R-method`[1] == "thf") {
        Rmeth <- "t"
      } else {
        Rmeth <- DFpar$`R-method`[1]
      }
      for(j in 1:max(DFpar$Replication)){
        recf_list[[j]] <- DFpar %>% 
          filter(Replication==j) %>% arrange(Series) %>%
          select(Series, K, Forecasts, `Forecast Horizon`) %>% 
          pivot_wider(names_from = Series, values_from = Forecasts) %>%
          arrange(K, `Forecast Horizon`) %>% 
          select(-K, -`Forecast Horizon`) %>% 
          as.matrix() 
        
        if(NROW(recf_list[[j]])<h){
          recf_list[[j]] <- rbind(recf_list[[j]], matrix(0, nrow = h-NROW(recf_list[[j]]), ncol = NCOL(recf_list[[j]])))
        }
        
        recf_list[[j]] <- t(recf_list[[j]])
      }
      ind_mat <- score_index(recf = recf_list, 
                             test = test_list, 
                             base = base_list, 
                             m = m, nb = nb, 
                             type = "mae", 
                             compact = FALSE,
                             nl = nl_v)
      
      ind_mat_duo <- score_index(recf = recf_list, 
                                 test = test_list, 
                                 base = base_list, 
                                 m = m, nb = nb, 
                                 type = "mae", 
                                 compact = FALSE)
      
      mat2 <- ind_mat$Rel_mat %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Rel_mat), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1) 
      
      mat2_cum <- ind_mat$Rel_mat_cum %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Rel_mat_cum), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      mat2_all <- ind_mat$Avg_ik[,NCOL(ind_mat$Avg_ik), drop = FALSE] %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Avg_ik), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = "K") %>% 
        add_column(H = "all")
      
      imae <- rbind(imae, mat2_cum, mat2, mat2_all)
      
      mat3 <- ind_mat$Avg_k %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Avg_k), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      mat3_cum <- ind_mat$Avg_k_cum %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Avg_k_cum), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      
      mat3_duo <- ind_mat_duo$Avg_k %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat_duo$Avg_k), 
                   .before = 1) %>%
        filter(series != "all") %>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      mat3_duo_cum <- ind_mat_duo$Avg_k_cum %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat_duo$Avg_k_cum), 
                   .before = 1) %>%
        filter(series != "all") %>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      gmae <- rbind(gmae, mat3, mat3_cum, mat3_duo, mat3_duo_cum)
      
      score_mae <-cbind(`F-method` = Fmeth,
                        `R-method` = Rmeth,
                        `R-comb` = comb_name,
                        nn_type=pull(combin[i,2]), 
                        k = rownames(ind_mat$Avg_mat), 
                        ind_mat$Avg_mat)
      rownames(score_mae) <- NULL
      score_mae_duo <-cbind(`F-method` = Fmeth,
                            `R-method` = Rmeth,
                            `R-comb` = comb_name,
                            nn_type=pull(combin[i,2]), 
                            k = rownames(ind_mat_duo$Avg_mat), 
                            ind_mat_duo$Avg_mat)
      rownames(score_mae_duo) <- NULL
      
      mae <- rbind(mae, merge(score_mae, score_mae_duo))
      
      ind_mat <- score_index(recf = recf_list, 
                             test = test_list, 
                             base = base_list, 
                             m = m, nb = nb, 
                             type = "mse", 
                             compact = FALSE,
                             nl = nl_v)
      
      ind_mat_duo <- score_index(recf = recf_list, 
                                 test = test_list, 
                                 base = base_list, 
                                 m = m, nb = nb, 
                                 type = "mse", 
                                 compact = FALSE)
      
      mat2 <- ind_mat$Rel_mat %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Rel_mat), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1) 
      
      mat2_cum <- ind_mat$Rel_mat_cum %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Rel_mat_cum), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      mat2_all <- ind_mat$Avg_ik[,NCOL(ind_mat$Avg_ik), drop = FALSE] %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Avg_ik), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = "K") %>% 
        add_column(H = "all")
      
      imse <- rbind(imse, mat2_cum, mat2, mat2_all)
      
      mat3 <- ind_mat$Avg_k %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Avg_k), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      mat3_cum <- ind_mat$Avg_k_cum %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat$Avg_k_cum), 
                   .before = 1)%>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      
      mat3_duo <- ind_mat_duo$Avg_k %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat_duo$Avg_k), 
                   .before = 1) %>%
        filter(series != "all") %>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      mat3_duo_cum <- ind_mat_duo$Avg_k_cum %>% 
        as_tibble() %>% 
        add_column(`F-method` = Fmeth,
                   `R-method` = Rmeth,
                   `R-comb` = comb_name,
                   nn_type=pull(combin[i,2]),
                   series = rownames(ind_mat_duo$Avg_k_cum), 
                   .before = 1) %>%
        filter(series != "all") %>%
        pivot_longer(-c(`F-method`, `R-method`, `R-comb`, nn_type, series), 
                     names_to = c("K", "H"), names_sep = "h") %>% 
        mutate(K = 1)
      
      gmse <- rbind(gmse, mat3, mat3_cum, mat3_duo, mat3_duo_cum)
      
      score_mse <-cbind(`F-method` = Fmeth,
                        `R-method` = Rmeth,
                        `R-comb` = comb_name,
                        nn_type=pull(combin[i,2]), 
                        k = rownames(ind_mat$Avg_mat), 
                        ind_mat$Avg_mat)
      rownames(score_mse) <- NULL
      score_mse_duo <-cbind(`F-method` = Fmeth,
                            `R-method` = Rmeth,
                            `R-comb` = comb_name,
                            nn_type=pull(combin[i,2]), 
                            k = rownames(ind_mat_duo$Avg_mat), 
                            ind_mat_duo$Avg_mat)
      rownames(score_mse_duo) <- NULL
      
      mse <- rbind(mse, merge(score_mse, score_mse_duo))
      
      cat("Score combination number ", i, " (out of ", NROW(combin), ", ",
          i/NROW(combin)*100,"%) - (", comb_name,")\n",sep = "")
    }
  }
  return(list(imae = imae, imse = imse, gmae = gmae, gmse = gmse, mae = mae, mse = mse))
}
