library(plm)
library(quantreg)

#compute quantile panel regression with fixed effects using Ivan Canay's QRPanel.R code
#data.frame must begin with "firm" then "year" then Yit then X1it, X2it, etc.
  SegPeaks.QRPanel = SegPeaks; SegPeaks.QRPanel[,1] = SegPeaks$Site_No
  SegPeaks.QRPanel[,2] = SegPeaks$Wateryear; SegPeaks.QRPanel[,3] = log10(SegPeaks$Peak)
  SegPeaks.QRPanel[,4] = SegPeaks$Scaled.Peak; SegPeaks.QRPanel[,5] = SegPeaks$Area
#convert Wateryears (now "year") to integers (haven't figured out how)
#SegPeaks.QRPanel$year = as.numeric(SegPeaks.QRPanel$year)
  names(SegPeaks.QRPanel) = c("firm","year","Peak","Scaled.Peak","Area","Precip","Urban.Frac","Dam.Storage")
  Eqfor = Peak ~ Precip + Urban.Frac; tau = 0.5
#QRPanel.path = "D:/Share/FDCs/Refs/regression/quantile regression/panel data/canay11-Rcode/"
#source(paste(QRPanel.path,"QRPanel.R",sep=""))
#two.step = pqr.estimator(SegPeaks.QRPanel, Eqfor, tau)
#above doesn't seem to work - try the "innards" of pqr.estimator:
  dataframe = SegPeaks.QRPanel
  data    <- pdata.frame(dataframe, index = c("firm", "year"), drop.index = T, row.names = T);
  within  <- plm(Eqfor,data=data,model="within");
  prelim  <- cbind(coef(within));    # This vector is K x 1 - only slopes.
	

  noprcp  <- plm(Peak~Urban.Frac,data=data,model="within");

# test  linU,linP; linU,logP, logU,linP, logU,logP
if( !exists("no_lm_out")){
if(write.file){
within2  <- plm(Peak ~ log10(Precip) + Urban.Frac,data=data,model="within");
within3  <- plm(Peak ~ Precip + log10(Urban.Frac+.001),data=data,model="within");
within4  <- plm(Peak ~ log10(Precip) + log10(Urban.Frac+.001),data=data,model="within");
closeAllConnections()
sink(paste(newadjpath,"logQ_U.P.model",bootver,rastr,".txt",sep=""))
print(summary(within)); cat("\n")
cat(paste("RMSE:",sqrt(sum(within$resid^2,na.rm=T)/(length(within$resid)-1))))
cat("\n ------------------------------------------ \n")
print(summary(within2)); cat("\n")
cat(paste("RMSE:",sqrt(sum(within2$resid^2,na.rm=T)/(length(within2$resid)-1))))
cat("\n ------------------------------------------ \n")
print(summary(within3)); cat("\n")
cat(paste("RMSE:",sqrt(sum(within3$resid^2,na.rm=T)/(length(within3$resid)-1))))
cat("\n ------------------------------------------ \n")
print(summary(within4)); cat("\n")
cat(paste("RMSE:",sqrt(sum(within4$resid^2,na.rm=T)/(length(within4$resid)-1))))
sink(); closeAllConnections()
}}

# Compute the estimated fixed effects (nT x 1 vectors
  hatalpha.i  <- cbind(fixef(within));      	                  # dimension n x 1
# Obtain se of fixed effects
  se.hatalpha.i <- cbind(summary(fixef(within))[,2])

#may want a weighted average because segments ("firms") have a different number of datapoints
#calculate weighted mean of intercept by adjusting for length of data for each station
 sumprod.hatalpha=sum(hatalpha.i*as.vector(table(dataframe$firm)))
 intercept <- sumprod.hatalpha/nrow(dataframe)
#intercept   <- median(hatalpha.i) + .1
#intercept   <- mean(hatalpha.i); # Intercept estimator
#the following seems to assume all segments have the same number of datapoints as first
#unos.T      <-rep(1,sum(dataframe[,1]==dataframe[1,1]));      # dimension T x 1
#need to compute hatalpha a different way because panel is "unbalanced"
#hatalpha    <- kronecker(hatalpha.i-intercept,unos.T); # dimension nT x 1    
  hatalpha    <- array(,dim=c(length(dataframe[,1]),1))
  hatalpha=rep(hatalpha.i-intercept,table(dataframe$firm))
  se.hatalpha=rep(se.hatalpha.i,table(dataframe$firm))

# Generate transformed variable and run QR
   Peak.FE.adj <- dataframe[,3]-hatalpha;     # Transformed Y

# Partial Residuals Adjustment - remove precip effect
# Peak.prcp.adj=dataframe[,3]-dataframe[,6]*within$coefficients[1]

# Original logQ
Orig.Peak=SegPeaks.QRPanel[,3]
if("dataframe" %in% search()) detach(dataframe); attach(dataframe)


# RA subset PLM 
data_sub    <- pdata.frame(SegPeaks_RA, index = c("Site_No", "Wateryear"), drop.index = T, row.names = T);
data_sub$Peak = log10(data_sub$Peak)
within_sub  <- plm(Eqfor,data=data_sub,model="within");
hatalpha.i.sub  <- cbind(fixef(within_sub));
se.hatalpha.i.sub <- cbind(summary(fixef(within_sub))[,2])
sumprod.hatalpha.sub=sum(hatalpha.i.sub*as.vector(table(SegPeaks_RA$Site_No)))
intercept.sub <- sumprod.hatalpha.sub/nrow(SegPeaks_RA)
hatalpha.sub = rep(hatalpha.i.sub-intercept.sub,table(SegPeaks_RA$Site_No))
Peak.FE.adj.sub <- log10(SegPeaks_RA$Peak)-hatalpha.sub;   

lmsub = lm(log10(Peak) ~ 0 + Precip + Urban.Frac + Site_No,data=SegPeaks_RA)
lmall = lm(log10(Peak) ~ 0 + Precip + Urban.Frac + Site_No,data=SegPeaks)

### Quant Reg ###
# Peak(FE adjusted ) ~ Urban.Frac + Precip
taus = c(.01,.02,.05,.075,0.1,.125,.175,.2,.25,.3,.4,.5,.6,.7,.75,.8,.9,.925,.95,.96,.97,.98,.99,.995,.998)
coef=matrix(0,length(taus),5)
colnames(coef)=c("Intercept","Urban.Frac","Precip","tau","EP")
for(i in 1:length(taus)){
	coef[i,]=c(rq(Peak.FE.adj~Urban.Frac+Precip,tau=taus[i])$coefficients,taus[i],1-taus[i])
	qr = rq(Peak.FE.adj~Urban.Frac+Precip,tau=taus[i])
}

coefalt=matrix(0,length(taus),4)
colnames(coefalt)=c("Intercept","Urban.Frac","tau","EP")
for(i in 1:length(taus)){
	coefalt[i,]=c(rq(Peak~Urban.Frac,tau=taus[i])$coefficients,taus[i],1-taus[i])
	}



#QR = RQ(Peak.FE.adj~Urban.Frac+Precip,tau=taus[i])


# objects returned:
# 'dataframe' Peaks adjusted for fixed effects
# 'Peak.FE.adj' Peaks adjusted for fixed effects
# 'Orig.Peak' Original Peaks
# 'taus' taus used for quantile regression
# 'coef' QR coefficient corresponding to tau values
# 'hatalpha.i' FE of stations
# 'se.hatalpha.i' standard error of the FE
######################################################