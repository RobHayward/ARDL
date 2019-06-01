# This comes from 
# https://cran.r-project.org/web/packages/dynamac/vignettes/dynamac-vignette.html
# This study of ARDL
# install.packages('dynamac')
require(dynamac)
head(ineq)
# the data for inequality and factors that raise concern about inequality
require(urca)
# to provide the unit root tests
ts.plot(ineq$concern)
# concern over inequallty
ts.plot(ineq$incshare10)
# income share of the top 10 percent
ts.plot(ineq$urate)
# unemployment rate
summary(ur.df(ineq$concern, type = c('trend'), lags = 1))
summary(ur.pp(ineq$concern, type = c('Z-tau'), model = c('constant'), use.lag = 1))
# There are other tests that show that concern is unit root until differenced
#=============================================
#dynamac has ways to lag and difference. 
head(ineq$incshare10)
head(lshift(ineq$incshare10, 1))
head(dshift(ineq$incshare10))
#====================================
dynardl(concern ~ incshare10 + urate, data = ineq, 
        lags = list('concern' = 1, 'incshare10' = 1), 
        diffs = c('incshare10', 'urate'), 
        ec = TRUE, simulate = FALSE)
res1 <- dynardl(concern ~ incshare10 + urate, data = ineq, 
        lags = list('concern' = 1, 'incshare10' = 1), 
        diffs = c('incshare10', 'urate'), 
        ec = TRUE, simulate = FALSE)
summary(res1)
# check the residuals
dynardl.auto.correlated(res1)
# there is some evidence of auto-correlation
# add the lag of the differenced dependent
res2 <- dynardl(concern ~ incshare10 + urate, data = ineq, 
        lags = list('concern' = 1, 'incshare10' = 1), 
        diffs = c('incshare10', 'urate'), 
        lagdiffs = list('concern' = 1),
        ec = TRUE, simulate = FALSE) 
summary(res2)
dynardl.auto.correlated(res2)
# The residuals are now stationary.  Now for the Bounds test
length(res2$model$residuals)
coef(res2$model)
B <- coef(res2$model)
V <- vcov(res2$model)
R <- matrix(c(0, 0, 0, 0, 0, 1), nrow = 1)
k <- sum(R)
q <- 0
fstat <- (1/k) * t(R%*%B-q)%*%solve(R%*%V%*%t(R))%*%(R%*%B-q)
fstat
# Now the bounds test
pssbounds(obs = 47, fstat = fstat, tstat = -3.684, case = 3, k = 1)
pssbounds(res2)
#===================================================
#Now we want to see the effect on the dependent of a shock to one of the explanator# y variables.  Simulate = TRUE and shockvar = incomeshare. 
res22 <- dynardl(concern ~ incshare10 + urate, data = ineq, 
        lags = list('concern' = 1, 'incshare10' = 1), 
        diffs = c('incshare10', 'urate'), 
        lagdiffs = list('concern' = 1),
        ec = TRUE, simulate = TRUE, 
        shockvar = 'incshare10')
dynardl.simulation.plot(res22, type = 'area', response = 'levels')
# It appears that there is an immediate shock that dissipates slowly.  
# It is possible to extend the length of the simulation. 
#===================================================
#Now we want to see the effect on the dependent of a shock to one of the explanator# y variables
res23 <- dynardl(concern ~ incshare10 + urate, data = ineq, 
        lags = list('concern' = 1, 'incshare10' = 1), 
        diffs = c('incshare10', 'urate'), 
        lagdiffs = list('concern' = 1),
        ec = TRUE, simulate = TRUE, range = 30, 
        shockvar = 'incshare10')
dynardl.simulation.plot(res23, type = 'area', response = 'levels')
dynardl.simulation.plot(res23, type = 'spike', response = 'levels')
# https://cran.r-project.org/web/packages/dynamac/dynamac.pdf
head(res23$simulation)
plot(res23$simulation[, 1],  res23$simulation[, 9], type = 'l')

