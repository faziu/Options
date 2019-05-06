S <- 1:199
K <- 100
vol <- 0.25
r <- 0.05
t <- 1
q <- 0



kappa_binary__asset_call <- function(S,K,r,vol,q,t){
  d1=(log(S/K)+(r-q +0.5*vol*vol)*t)/(vol*sqrt(t))
  d2=(log(S/K)+(r-q - 0.5*vol*vol)*t)/(vol*sqrt(t))
  -exp(-q*t)*((dnorm(d1)*d2*S)/(vol)) 
}

curve_kappa_call <- kappa_binary__asset_call(S,K,r,vol,q,t)

kappa_binary__asset_put <- function(S,K,r,vol,q,t){
  d1=(log(S/K)+(r-q +0.5*vol*vol)*t)/(vol*sqrt(t))
  d2=(log(S/K)+(r-q - 0.5*vol*vol)*t)/(vol*sqrt(t))
  exp(-q*t)*((dnorm(d1)*d2*S)/(vol)) 
}

curve_kappa_put <- kappa_binary__asset_put(S,K,r,vol,q,t)
layout(matrix(c(1,1,2,2), 2, 2))
plot(curve_kappa_call, type = 'l', lty = 3)
plot(curve_kappa_put, type = 'l', lty = 2)


