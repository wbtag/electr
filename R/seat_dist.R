seat_dist <- function(parties,V,total,seats=200,threshold=0.05) {
  regseats <- regseats(V,total)
  adv <- adv(parties,V,total,threshold=threshold)
  adp <- adv[1]
  for (i in 2:ncol(adv)) {
    a <- skr_1(adv$parties,adv[i],regseats[i-1])
    adp <- cbind(adp,a)
  }
  rownames(adp) <- adp[,1]
  adp <- adp[-1]
  colnames(adp) <- colnames(V)
  adp$Total <- rowSums(adp[1:ncol(adv)-1])
  if(sum(adp$Total) == 200) {
    adp
  } else {
    rem <- 200-sum(adp$Total)
    adr <- adv[1]
    for (i in 2:ncol(adv)) {
      a <- skr_2(adv$parties,adv[i],regseats[i-1],adp[i-1])
      adr <- cbind(adr,a)
    }
    adr_b <- adr
    adr$Total <- rowSums(adr[2:ncol(adr)])
    rvc <- sum(adr$Total)/(rem+1)
    print(rvc)
    adr$man <- adr$Total/rvc
    p2s <- as.data.frame(adv[1])
    p2s <- cbind(p2s,numeric(nrow(adv)))
    while (rem > 0) {
      if (adr$man[which.max(adr$man)]<1) {
        break
      }
      rem <- rem-floor(adr$man[which.max(adr$Total)])
      p2s[which.max(adr$Total),2] <- p2s[which.max(adr$Total),2]+floor(adr$man[which.max(adr$Total)])
      adr$man[which.max(adr$Total)] <- 0
      adr$Total[which.max(adr$Total)] <- 0
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
    while (sum(p2s[2]) > 0) {
      col <- which.max(adr[which.max(p2s[,2]),])
      row <- which.max(p2s[,2])
      adp[row,col] <- adp[row,col]+1
      adr[row,col] <- adr[row,col] <- 0
      p2s[row,2] <- p2s[row,2]-1
    }
    adp$Total <- rowSums(adp[1:ncol(adv)-1])
    adr$Total <- rowSums(adr[2:ncol(adr)])
    rem <- 200-sum(adp$Total)
    while (rem > 0) {
      p2s[which.max(adr$Total),2] <- p2s[which.max(adr$Total),2]+1
      adr$Total[which.max(adr$Total)] <- 0
      rem <- rem-1
    }
    adr <- adr[2:ncol(adv)]
    while (sum(p2s[2]) > 0) {
      col <- which.max(adr[which.max(p2s[,2]),])
      row <- which.max(p2s[,2])
      adp[row,col] <- adp[row,col]+1
      p2s[row,2] <- 0
    }
  adp$Total <- rowSums(adp[2:ncol(adv)-1])
  adp <- rbind(adp,colSums(adp))
  rownames(adp)[nrow(adv)+1] <- "Total"
  adp
  }
}
