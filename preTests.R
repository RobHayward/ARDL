# This is just a run-through of the tests for the ARDL - finding the level of 
# integration.  This is from  VARS handbook. 
require(urca)
set.seed(123456)
e <- rnorm(500)
rw.nd <- cumsum(e)
plot(rw.nd)
trd <- 1:500
rw.wd <- 0.5 * trd + cumsum(e)
plot(rw.wd)
dt <- e + 0.5 * trd
plot(dt)
summary(ur.df(rw.nd, type = 'trend', lags = 1))
summary(ur.df(rw.nd, type = 'drift', lags = 1))
summary(ur.df(rw.nd, type = 'none', lags = 1))
summary(ur.df(rw.wd, type = 'trend', lags = 1))
summary(ur.df(rw.wd, type = 'drift', lags = 1))
summary(ur.df(rw.wd, type = 'none', lags = 1))
summary(ur.df(dt, type = 'trend', lags = 1))
summary(ur.df(dt, type = 'drift', lags = 1))
summary(ur.df(dt, type = 'none', lags = 1))
