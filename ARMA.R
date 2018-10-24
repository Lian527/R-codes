#ARMAFUN
get.best.arma <- function(x.ts, maxord = c(1,0,1))
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for(q in 0:maxord[3])
      fit <- arima(x.ts, order = c(p,0,q), frequency(x.ts), method = "CSS")
      fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
      if (fit.aic < best.aic)
      {
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p,0,q)
      }
    
  list(best.aic, best.fit, best.model)
}
