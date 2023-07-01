rmc <- function(total,seats=200) {
  seats <- seats
  rmc <- sum(total)/seats
  rmc <- round(rmc)
}

adv <- function(parties,V,total,threshold=0.05) {
  thr <- ceiling(sum(total)*threshold)
  adv <- V[which(total>thr),]
  parties <- parties[which(total>thr)]
  as.data.frame(cbind(parties,adv))
}

regseats <- function(V,total,seats=200) {
  rmc <- rmc(total,seats)
  regvotes <- numeric(ncol(V))
  for (i in 1:ncol(V)) {
    regvotes[i] <- sum(V[,i])
  }
  regseats <- regvotes/rmc
  rem <- seats-sum(floor(regseats))
  if (rem > 0) {
    divrem <- (regseats-floor(regseats))*rmc
    regseats <- floor(regseats)
    while (rem > 0) {
      regseats[which.max(divrem)] <- regseats[which.max(divrem)] + 1
      divrem[which.max(divrem)] <- 0
      rem <- rem-1
    }
  }
  regseats
}

skr_1 <- function(str, V, S) {
  S <- S+2
  odp <- 2
  if(class(V) != "numeric") {
    V <- as.numeric(unlist(V))
  }
  Q <- sum(V)/S
  seats <- V/Q
  rem <- sum(seats)-odp-sum(floor(seats))
  rem <- round(rem)
  if(rem < 0) {
    divrem <- seats-floor(seats)
    seats <- floor(seats)
    while (rem < 0) {
      seats[which.min(as.numeric(unlist(divrem)))] <- seats[which.min(as.numeric(unlist(divrem)))]-1
      divrem[which.min(as.numeric(unlist(divrem)))] <- 0
      rem <- rem+1
    }
    seats <- as.numeric(seats)
    seats
  } else {
    seats <- floor(seats)
    seats
  }
}

skr_2 <- function(str,V,S,skr_1) {
  S <- S+2
  odp <- 2
  if(class(V) != "numeric") {
    V <- as.numeric(unlist(V))
  }
  Q <- sum(V)/S
  Q <- round(Q)
  Qv <- skr_1*Q
  divrem <- V-Qv
  divrem
}

dhondt <- function(V,seats) {
  dis <- 0
  while (seats > dis) {
    row <- which.max(V[,2])
    V[row,3] <- V[row,3] + 1
    V[row,2] <- V[row,1]/(1+V[row,3])
    dis <- dis + 1
  }
  return(V[3])
}
