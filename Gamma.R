S <- 1:200
K <- 100
vol <- 0.25
r <- 0.05
t <- 1
q <- 0

gamma_binary__asset_call <- function(S,K,r,vol,q,t){
  d1=(log(S/K)+(r-q +0.5*vol*vol)*t)/(vol*sqrt(t))
  d2=(log(S/K)+(r-q - 0.5*vol*vol)*t)/(vol*sqrt(t))
  -exp(-q*t)*((dnorm(d1)*d2)/(vol^2*S * t)) 
}

curve_gamma_call_asset <- gamma_binary__asset_call(S,K,r,vol,q,t)



gamma_binary__asset_put <- function(S,K,r,vol,q,t){
  d1=(log(S/K)+(r-q +0.5*vol*vol)*t)/(vol*sqrt(t))
  d2=(log(S/K)+(r-q - 0.5*vol*vol)*t)/(vol*sqrt(t))
  exp(-q*t)*((dnorm(d1)*d2)/(vol^2*S * t)) 
}
curve_gamma_put_asset <-gamma_binary__asset_put(S,K,r,vol,q,t)

gamma_binary__cash_call <- function(S,K,r,vol,q,t){
  d1=(log(S/K)+(r-q +0.5*vol*vol)*t)/(vol*sqrt(t))
  d2=(log(S/K)+(r-q - 0.5*vol*vol)*t)/(vol*sqrt(t))
  -exp(-q*t)*((dnorm(d2)*d1)/(vol^2*S^2 * t)) 
}

curve_gamma_call_cash <- gamma_binary__cash_call(S,K,r,vol,q,t)



gamma_binary__cash_put <- function(S,K,r,vol,q,t){
  d1=(log(S/K)+(r-q +0.5*vol*vol)*t)/(vol*sqrt(t))
  d2=(log(S/K)+(r-q - 0.5*vol*vol)*t)/(vol*sqrt(t))
  exp(-q*t)*((dnorm(d2)*d1)/(vol^2*S^2 * t)) 
}
curve_gamma_put_cash <-gamma_binary__cash_put(S,K,r,vol,q,t)



par(mfrow=c(2,2)) 
#layout(matrix(c(,1,2,2), 2, 2))
plot(curve_gamma_call_asset, type = 'l', lty = 1, lwd = 3, xlab = "Stock price", ylab = "Convexity", main = "Gamma Asset or Nothing Call Curve")
plot(curve_gamma_put_asset, type = 'l', lty = 1, lwd = 3, xlab = "Stock price", ylab = "Convexity", main = "Gamma Asset or Nothing Put Curve")
plot(curve_gamma_call_cash, type = 'l', lty = 1, lwd = 3, xlab = "Stock price", ylab = "Convexity", main = "Gamma Cash or Nothing Call Curve")
plot(curve_gamma_put_cash, type = 'l', lty = 1, lwd = 3, xlab = "Stock price", ylab = "Convexity", main = "Gamma Cash or Nothing Put Curve")
print("GAMMA is the second partial derivative of the price with respect to S. Having, from the Delta section, the formula for the Delta of a digital option, paying one unit of asset or nothing. 
      The pricing formula of an Asset or Nothing European Call Option ,that pays one unit of the underlying asset at maturity if the price of the underlying is higher than the strike and vice verce in case of put option, however in case of cash or nothing it pays the amount of cash")
