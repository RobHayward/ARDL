#This is the first file
# THe first part is a copy of Prepare.R from SVARDoc.R
d <- read.csv("../Data/VARdata.csv", header=TRUE, sep=",")
head(d)

# Create variables per GDP for capital flows-----------------------
# Divide by 10 to deal with BN GDP and MLN capital flow data
#d will be the raw data before capital flows
da <- d
da$CCA<-(d$CCA/d$NGDP2)/10
da$DCCA<-(d$DCCA/d$NGDP2)/10
da$CNB <- (d$CNB - d$COT)/d$NGDP2/10
da$CNE<-(d$CNE/d$NGDP2)/10
da$CNFDI<-(d$CNFDI/d$NGDP2)/10
da$COT<-(d$COT/d$NGDP2)/10
plot(da$RTWI, type = 'l')
plot(da$SPREAD1, type = 'l')
plot(da$DCCA/d$NGDP2, type = 'l')
head(da)
# delete unneeded variables---------------------------------------
#delete the first 52 rows with missing values and the debris
da<-da[-c(1:52),]
da<-da[,-c(1:2,9,10,12,13)]
### to shuffle data for the Chlosky decomposition
# da2<-da[,sample(1:12, size = 12, replace = FALSE)]
# head(da2)
# adjust sentiment data-------------------------
da$S1 <- da$S1*(-10)
da$S2 <- da$S2*(-10)
head(da)

#create and save time series for plot--------------  
dt<-ts(da,start=c(1986.1),frequency=4)
#pdf("Figures/ts2.pdf", paper= "a4", width = 10, height = 10, title = "ts")
par(mfcol=c(3,2), oma = c(0,0,0,0))
plot(dt[,c(2:7, 9, 11)], main = "Cumulative capital flow and exchange rate")
dev.off()
# Normalise--------------------------------------------
#normalise da by taking mean and dividing by standard error
#Do we need to normalise them all?  RTWI? 
#normalised data will be dan in stead of da
#head(da)
dan<-scale(da)
head(dan)
#  Dummy Variables###########################################################
# D1
# Q386 to Q188 (55 to 61 in d) and 3 to 9 in da is an interest rate shock for 
#spread 1.  It does not happen with spread 2. 
# is this needed ? d[120:121,]
da$D1=0
da$D1[c(3:9)]=1
# D2
#create a dummy for the shock of the 1994 interest rate increase. 
#2Q94 to 2Q95.  This is 86:90 in the original (d) and 34:38 in the (da) 
#34 plus 52 is 86; 38 plus 52 is 90
da$D2=0
da$D2[c(34:38)]=1
# D3
#q32007 is 139 q42008 is 144.  The dummy is designed to account for the sharp 
#flow in funds (particularly bonds and money market) in this period. 
#87 plus 52 is 139; 92 plus 52 is 144.
da$D3=0
da$D3[c(87:92)]=1
#dummies must be turned into matrix to use with VAR. 
# dum<-cbind(da$D2, da$D3)
dum<-cbind(da$D1,da$D2, da$D3)
#Any other dummies?  Maybe look at the residuals to see if anything is required.
#One possibility would be the dot.com burst.  Check equity and FDI flow.
colnames(dum)<- c("D1", "D2", "D3")
# maybe update to include new dummy.  Hau and Rey have breaks at  
# 1994. Assuming first quarter, these would be row 34 onwards for 1994
# that would be 66 for 2002.  This could be tried. 
# 
da$D4 = 0
da$D4[c(34:length(da$D4))] = 1
da$D5 = 0
da$D5[c(66:length(da$D5))] = 1
D4 <- da$D4
D5 <- da$D5
#=================================
#ARDL model
library(dynamac)
library(urca)
summary(ur.df(da$RTWI, type = c('none'), selectlags = 'BIC'))
summary(ur.df(diff(da$RTWI), type = 'trend', selectlags = 'BIC'))
plot(da$RTWI, type = 'l')
plot(diff(da$RTWI), type = 'l')
summary(ur.pp(da$RTWI, type = 'trend', selectlags = 'BIC'))
summary(ur.pp(diff(da$RTWI), type = 'trend', selectlags = 'BIC'))
summary(ur.kss(da$RTWI, type = 'none', selectlags = 'BIC'))
summary(ur.kss(diff(da$RTWI), type = 'drift', selectlags = 'BIC'))
?ur.df
?ur.pp
?ur.kpss
#====================================
library(dynamac)
res1 <- dynardl(RTWI ~ SPREAD1 + S1, data = da, 
                diffs = c('SPREAD1', 'S1'), 
                lags = list('RTWI' = 1, 'S1' = 1),
                ec = TRUE, simulate = TRUE, 
                shockvar = 'SPREAD1')
summary(res1)
dynardl.auto.correlated(res1)
dynardl.simulation.plot(res1)
