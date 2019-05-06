S0 <- c(100, 80, 250)
ST <- c(80, 100, 300)


X <- 100
C <- 100

ass_or_cash <- function(S0=..., ST=...){
  C <- 100
  I <- ST/S0*100
  payoff <- max(I, C)
  return(payoff)
}

ass_or_cash(S0 = S0, ST = ST)

better_Ass <- function(S0=..., ST=...){
  I <- ST/S0*100
  payoff <- max(I)
  return(payoff)
}

better_Ass(S0 = S0, ST=ST)

worse_ass <- function(S0=..., ST=...){
  I <- ST/S0*100
  payoff <- min(I)
  return(payoff)
}

worse_ass(S0 = S0, ST = ST)

Max_Ass <- function(S0=..., ST=...){
  X <- 100
  I <- ST/S0*100
  payoff <- max(max(I)-X,0)
  return(payoff)
}

Max_Ass(S0=S0, ST=ST)

Min_Ass <- function(S0=..., ST=...){
  X <- 100
  I <- ST/S0*100
  payoff <- max(min(I)-X,0)
  return(payoff)
}

Min_Ass(S0 = S0, ST = ST)

Max_Ass_put <- function(S0=..., ST=...){
  X <- 100
  I <- ST/S0*100
  payoff <- max(-max(I)+X,0)
  return(payoff)
}

Max_Ass_put(S0 = S0, ST = ST)

Min_Ass_put <- function(S0=..., ST=...){
  X <- 100
  I <- ST/S0*100
  payoff <- max(-min(I)+X,0)
  return(payoff)
}

Min_Ass_put(S0 = S0, ST = ST)
