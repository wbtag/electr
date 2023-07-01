seat_dist02 <- function(parties, V, total, seats=200, threshold=0.05) {
  V[is.na(V)] <- 0
  regseats <- regseats(V,total,seats)
  adv <- adv(parties,V,total,threshold)
  adp <- adv[1]
  ncol <- ncol(adv)
  for (i in 2:ncol) {
    regv <- adv[i]
    colnames(regv) <- "V0"
    regv$V <- regv$V0
    regv$seats <- numeric(nrow(regv))
    r <- dhondt(regv,regseats[i-1])
    adp <- cbind(adp,r)
    rm(r)
  }
  rownames(adp) <- adp[,1]
  adp <- adp[-1]
  adp$Total <- rowSums(adp)
  nrow <- nrow(adp)
  adp <- rbind(adp,colSums(adp))
  rownames(adp)[nrow+1] <- "Total"
  print(adp[ncol])
  return(adp)
}


