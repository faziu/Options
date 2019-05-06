S01 <- 100
S02 <- 80
vol1 <- 0.3
vol2 <- 0.2
r <- 0.05
q1 <- 0
q2 <- 0
T <- 1
C <- 80
cor12 <- 0.8
N <- 100

#Creating correlated variables using Cholesky Decompsiton
z1 <- rnorm(n = 100)
eps2 <- rnorm(n=100)
z2 <- cor12*z1+eps2*sqrt(1-cor12^2)
cor(z1,z2)

best_Ass <- function(S01,S02, vol1, vol2, r, q1, q2, T, C, N){
  sum_payoff = 0
  payoff = 0
  Sum = 0
  i <- 1
  for (i in 1:N) {
    ST1 <- S01*exp((r-0.5*vol1)*T+vol1*sqrt(T)*z1 )
    ST2 <- S02*exp((r-0.5*vol2)*T+vol2*sqrt(T)*z2 )
    I1 <- (ST1/S01)*100
    I2 <- (ST2/S02)*100
    payoff = max(I1, I2, C)
    sum_payoff = sum_payoff + payoff
  
  }
  price = sum_payoff/N*exp(-r*T)
  return(price)
}

best_Ass(S01,S02, vol1, vol2, r, q1, q2, T, C, N)
