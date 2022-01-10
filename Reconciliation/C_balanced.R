C_balanced <- function(C, nl){
  Csplit <- lapply(split(C, rep(1:length(nl), nl)), matrix, ncol = NCOL(C))
  
  
  obj <- lapply(Csplit, function(x){
    out <- list()
    out$Cb <- rbind(x, diag(1, NCOL(C))[colSums(x)==0,])
    out$add <- which(colSums(x)==0)
    return(out)
  })
  
  id_bal <- unlist(Map(function(x,y) c(x,y+NROW(C)),
                       y = lapply(obj, function(x) FoReco:::extract_data(x = x, name = "add")),
                       x = split(1:NROW(C), rep(1:length(nl), nl))), use.names = FALSE)
  
  Cb <- lapply(obj, function(x) FoReco:::extract_data(x = x, name = "Cb"))
  nl <- unname(sapply(Cb, NROW))
  Cb <- do.call("rbind", Cb)
  
  lev <- rep(1:length(nl), nl)
  cond <- TRUE
  i <- 1 # riga di confronto
  
  while(cond){
    r <- Cb[i,]
    if(sum(r)>1){
      Dr <- diag(NCOL(C))[which(r == 1), , drop = FALSE]
      j <- i
      #check <- TRUE
      if(i<=(NROW(Cb)-NROW(Dr)+1)){
        while(j %in% i:(NROW(Cb)-NROW(Dr)+1)){
          if(all(Cb[j:(j+NROW(Dr)-1), , drop = FALSE] == Dr) & lev[j]==lev[i]+1){
            Cb[j,] <- Cb[i,]
            Cb <- Cb[-c((j+1):(j+NROW(Dr)-1)),,drop = FALSE]
            
            id_bal[j] <- id_bal[i]
            id_bal <- id_bal[-c((j+1):(j+NROW(Dr)-1))]
            
            lev <- lev[-c((j+1):(j+NROW(Dr)-1))]
            break
            #      zorro2[i] <- j
            #      zorro2 <- zorro2[-c((i+1):(i+NROW(z)-1))]
            #      sost[[k]] <- list(s = i:(i+NROW(z)-1), t = j)
            #      k = k+1
          }else{
            j = j + 1
          }
        }
      }
    }
    i <- i+1
    if(i>NROW(Cb)){
      cond <- FALSE
    }
  }
  
  id_unbal <- which(id_bal %in% 1:NROW(C))
  id_unbal <- id_unbal[!duplicated(id_bal[id_unbal])]
  return(list(Cb = Cb, 
              #id_bal = c(id_bal, (NROW(C)+1):(NROW(C)+NCOL(C))), 
              id_bal = id_bal,
              id_unbal = id_unbal,
              nl = unname(tapply(lev, lev, length))))
}
