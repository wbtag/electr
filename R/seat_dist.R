seat_dist <- function(parties,V,total,seats=200,threshold=0.05) {
  V[is.na(V)] <- 0
  regseats <- regseats(V,total)
  adv <- adv(parties,V,total,threshold=threshold)
  adp <- adv[1]
  ncol <- ncol(adv)
  for (i in 2:ncol) {
    a <- skr_1(adv$parties,adv[i],regseats[i-1])
    adp <- cbind(adp,a)
    rm(a)
  }
  rownames(adp) <- adp[,1]
  adp <- adp[-1]
  colnames(adp) <- colnames(V)
  rem <- seats-sum(rowSums(adp[1:ncol-1]))
  if (rem == 0) {
    adp$Total <- rowSums(adp[1:ncol-1])
    adp <- rbind(adp,colSums(adp))
    rownames(adp)[nrow(adv)+1] <- "Total"
    print(adp[ncol])
    adp
  } else {
    adr <- adv[1]
    for (i in 2:ncol) {
      a <- skr_2(adv$parties,adv[i],regseats[i-1],adp[i-1])
      adr <- cbind(adr,a)
      rm(a)
    }
    adr$Total <- rowSums(adr[2:ncol(adr)])
    rvc <- sum(adr$Total)/(rem+1)
    adr$man <- adr$Total/rvc
    p2s <- as.data.frame(adv[1])
    p2s <- cbind(p2s,numeric(nrow(adv)))
    adr <- adr[-1]
    while (rem > 0) {
      if (adr$man[which.max(adr$man)]<1) {
        break
      }
      row <- which.max(adr$Total)
      rem <- rem-floor(adr$man[row])
      p2s[row,2] <- p2s[row,2]+floor(adr$man[row])
      adr$man[row] <- adr$man[row]-p2s[row,2]
      adr$Total[row] <- 0
    }
    while (rem > 0) {
      row <- which.max(adr$man)
      p2s[row,2] <- p2s[row,2]+1
      adr$man[row] <- 0
      rem <- rem-1
    }
    if (rem < 0) {
      p2s <- cbind(p2s,c(1:nrow(adv)))
      colnames(p2s) <- c("p","S2","id")
      p2s1 <- subset(p2s,"S2">0)
      p2s0 <- subset(p2s,"S2"==0)
      while (rem < 0) {
        p2s1[which.min(adr$Total),2] <- p2s1[which.min(adr$Total),2]-1
        rem <- rem+1
      }
      p2s <- rbind(p2s0,p2s1)
      p2s %>% arrange(id)
    }
    adr <- adr[1:ncol-1]
    party <- 0
    while (sum(p2s[2]) > 0) {
      party <- party+1
      while(p2s[party,2] > 0) {
        col <- which.max(adr[party,])
        adp[party,col] <- adp[party,col]+1
        adr[party,col] <- adr[party,col] <- 0
        p2s[party,2] <- p2s[party,2]-1
      }
      if(party==nrow(adv)) {
        break
      }
    }
    adp$Total <- rowSums(adp[1:ncol-1])
    adp <- rbind(adp,colSums(adp))
    rownames(adp)[nrow(adv)+1] <- "Total"
    print(adp[ncol])
    adp
  }
}
