# quant reg Phase_II

#------------- Flags ------------------
write.file <- T
include.mobs <- F
MUKWONGA <- T
HDB <- T
RA <- T           #1/14/2015 Addition
optmprcp <- F      #5/22/2015 Addition
remove.PILF <- F   #6/19/2015 Addition
#---------------------------------------

#for(include.mobs in c(F,T)){
#for(MUKWONGA in c(F,T)){
#for(HDB in c(F,T)){
#for(RA in c(F,T)){
#print(c(include.mobs,MUKWONGA,HDB,RA))
#outversion = outstr[(MUKWONGA+1),(include.mobs+1),(HDB+1),(RA+1)]
#print(outversion)
#}}}}

VER <- "RA.RL15_no.OF"    #1/15/2015 UPDATED
outVER = "_RA.RL15_no.OF_reduced"  #1/15/2015 UPDATED
outstr = array(c(outer(c("","_w.MUK","_w.mobs","_w.MUK_w.mobs","_w.HDB","_w.MUK_w.HDB",
	"_w.HDB_w.mobs","_w.MUK_w.HDB_w.mobs"),c("","_RA"),FUN=paste, sep="")),dim = c(2,2,2,2))
outversion = outstr[(MUKWONGA+1),(include.mobs+1),(HDB+1),(RA+1)]
bootver = outstr[(MUKWONGA+1),1,(HDB+1),1]
segver = outstr[(MUKWONGA+1),1,1,1]
if(remove.PILF) pilfstr = "_noPILF" else pilfstr = ""
if(optmprcp) prcpstr = "_w.optm.prcp" else prcpstr = ""
if(RA) rastr = "_RA" else rastr = ""

source("T:/adjustment/with.dam.segments.noDet.newUrban.QRPanel_data.only.mod_new.R")
newadjpath <- paste(wdpath,"new.adjustments/",sep="")
source("T:/adjustment/SegPeakQR.R")

if(write.file) write.csv(c("Weighted Average of FE",intercept.sub), 
	"T:/adjustment/new.adjustments/weighted.avg.FE.csv", row.names=F)

bootfile <- paste(wdpath,"bootstrap/K1_QR_boot.SE.new",VER,bootver,pilfstr,".csv",sep="")

#----------- Regression Function -------------------------------------
# first, bootstrap with R=1000 (done in program 'QR_Bootstrap.R')
bootcoef=read.table(bootfile,sep=",",header=T)
btau.boot=bootcoef$BS.Urban.Frac 
boot.se=bootcoef$SE.Urban.Frac.
coef[,1:3] = as.matrix(bootcoef[,1:3])

btau.prcp=bootcoef$BS.Precip

#---------- plot: concluded that 7th order polynomial is best ---------------
#6/18/2014: new plot with segment definition - perhaps 7th order polynomial isn't the best..
#tried monotonic splines 

Poly<- function(x, degree = 1, coefs = NULL, raw = FALSE, ...) {
        notNA<-!is.na(x)
        answer<-poly(x[notNA], degree=degree, coefs=coefs, raw=raw, ...)
        THEMATRIX<-matrix(NA, nrow=length(x), ncol=degree)
        THEMATRIX[notNA,]<-answer
        attributes(THEMATRIX)[c('degree', 'coefs', 'class')]<- attributes(answer)[c('degree', 'coefs', 'class')]
        THEMATRIX
}

library(Hmisc);tau=taus

if(write.file)  pdf(paste(wdpath,"new.adjustments/bootcoefs_funcfit_w.errbar_phaseII",VER,bootver,pilfstr,".pdf",sep=""))
#plot with error bars on z-score scale
with(data=bootcoef,expr=errbar(qnorm(taus),btau.boot,btau.boot+boot.se,btau.boot-boot.se,add=F,pch=1,cap=.03,
	xlab="Non-exceedance Probability (z-score)",ylab="Regression Coefficient"))
  xx=seq(-3,3,0.001)
#polynomial fit
  modelpoly=lm(btau.boot~Poly(taus,7,raw=T),weights=1/boot.se^2)
  yypoly=predict(modelpoly,list(taus=pnorm(xx)))
  lines(xx,yypoly,col="black")
#legloc <- c(range(btau.boot)[1] + diff(range(btau.boot))/12,range(qnorm(taus))[1] + diff(range(qnorm(taus)))/12)

#plot without error bar on normal scale
  xx=seq(0,1,0.001)
  yypoly=predict(modelpoly,list(taus=xx))
plot(taus,btau.boot,xlab="non-exceedance probability (tau)",ylab="bU(tau)")
  lines(xx,yypoly,col="red"); 
#legloc <- c(range(btau.boot)[1] + diff(range(btau.boot))/8,range(taus)[1] + diff(range(taus))/12)
text = paste(c(paste("y = ",round(coef(modelpoly)[1],1),sep=""),sapply(1:7, function(x) paste(round(coef(modelpoly)[x+1],1),"x^",x,sep=""))),collapse=" + ")
  mtext(side=3, text)

dev.off()


#save(modelpoly,file=paste(wdpath,"new.adjustments/QR.polyfit.rda",sep=""))

if(write.file) sink(paste(wdpath,"new.adjustments/poly.fit.summary",pilfstr,".txt",sep=""))
summary(modelpoly)
sink()

#--------------- derivative of polynomial model ----------------------
if(write.file){
pdf(paste(wdpath,"new.adjustments/derivative_poly.fit_phaseII",VER,bootver,pilfstr,".pdf",sep=""))
X=seq(0,1,by=.001)
Y=predict(modelpoly,list(taus=X))
dY = diff(Y)/diff(X)
dX = rowMeans(embed(X,2))
plot(dX,dY,type="l", xlab="tau",ylab="dY",main="Derivative of Polynomial Fit")
abline(0,0,col="red")
maxloc <- c(dX[which(dY==max(dY))], dY[which(dY==max(dY))])
text(maxloc[1],maxloc[2],round(max(dY),digits=6))
dev.off()
}

#----------------- QR vs. bootstrap QR ------------------------------------
if(write.file){
pdf(paste(wdpath,"new.adjustments/qr.vs.bootstrap.coefs.phaseII",VER,bootver,pilfstr,".pdf",sep=""))
with(data=bootcoef,expr=errbar(qnorm(taus),btau.boot,btau.boot+boot.se,btau.boot-boot.se,add=F,pch=1,cap=.03,
	xlab="Non-exceedance Probability (z-score)",ylab="Regression Coefficient"))
points(qnorm(taus),coef[,2],col="red")
legend(.5,.9, cex=.8,c("Quantile Regression Coefficient","Bootstrapped-QR Coefficients"), col=c("red","black"),
	pch=c(1,1), bty=c("p","p"))
dev.off()
}

#------------------ Regional Adjustment----------------------------------------

# interpolated fixed effects for zero segments
fe.validpeaks=subset(AllPeaks,AllPeaks$Segment!=0)
fe.validpeaks=fe.validpeaks[order(fe.validpeaks$Site_No,fe.validpeaks$Wateryear),]
x=table(paste(fe.validpeaks$Site_No,fe.validpeaks$Segment,sep="."))

#7/2/2015 Edit: New method to compute FE for redundant stations. See doc "QR_Adjustment_Process_README" for details.
library(dplyr)
comp.fe.table <- data.frame(Site_No = SegPeaks$Site_No, 
	FE.Adj.Peaks = log10(SegPeaks$Peak) - SegPeaks$Precip*coef(within_sub)[1] - SegPeaks$Urban.Frac*coef(within_sub)[2]) %>%
	group_by(Site_No) %>% 
	summarise(FE = mean(FE.Adj.Peaks))

comp.hatalpha.i = comp.fe.table$FE;
comp.se.hatalpha.i = diag(sqrt(summary(lmall)$cov.unscaled*summary(lmsub)$sigma^2))[-c(1:2)]
#-------------------------------------------------------------------------------------------------------------
fe.validpeaks=cbind(fe.validpeaks[,1:3],rep(comp.hatalpha.i ,as.numeric(x))); names(fe.validpeaks)[4]="FE"
fe.validpeaks=fe.validpeaks[order(fe.validpeaks$Site_No,fe.validpeaks$Wateryear),]
fe.allpeaks=merge(fe.validpeaks,AllPeaks[,1:3], by=c("Wateryear","Site_No","Segment"),all.y=T)
fe.allpeaks=fe.allpeaks[order(fe.allpeaks[,2],fe.allpeaks[,1]),]; fe.allpeaks=fe.allpeaks[which(!duplicated(fe.allpeaks)),]
fe.allpeaks$FE[which(fe.allpeaks$Segment==0)]=0; 

for(i in unique(fe.allpeaks$Site_No)){
   mat=subset(fe.allpeaks,fe.allpeaks$Site_No==i)
   if( any(mat$FE == 0) ){
      whichseg=which(rle(mat$Segment)$values==0)[which(!which(rle(mat$Segment)$values==0) %in% c(1,length(rle(mat$Segment)$values)))]
      zerosegs=which(rep(seq(1,length(rle(mat$Segment)$values)),rle(mat$Segment)$lengths) %in% whichseg)
      if(length(whichseg)>0){
         for(j in 1:length(whichseg)){
            interpbtw=c(rle(mat$FE)$values[whichseg[j]-1],rle(mat$FE)$values[whichseg[j]+1])
            newFE=approx(x=c(0,1),y=interpbtw,xout=seq(1:rle(mat$Segment)$lengths[whichseg[j]])/(rle(mat$Segment)$lengths[whichseg[j]]+1))
            replacewhich=which(rep(seq(1,length(rle(mat$Segment)$lengths)),rle(mat$Segment)$lengths)==whichseg[j])
            fe.allpeaks$FE[which(fe.allpeaks$Site_No==i)][replacewhich]=newFE$y
         }
      }
   }
} 
#cbind(hatalpha.i,matrix(rep(rep(0,nrow(hatalpha.i)),3),ncol=3),se.hatalpha.i)

fe.allpeaks$Segment_wnegs=segs; 
fe.allpeaks=fe.allpeaks[,c(1:3,5,4)]
fe.allpeaks$FE[which(fe.allpeaks$FE==0)]=NA; 

if(write.file) write.csv(fe.allpeaks,paste(newadjpath,"fe_interp.allpeaks",VER,outversion,pilfstr,".csv",sep=""),row.names=F)


######## FE for SegPeaks RA Subset ###########################################################
# interpolated fixed effects for zero segments
fe.validpeaks_RA=subset(AllPeaks_RA,AllPeaks_RA$Segment!=0)
fe.validpeaks_RA=fe.validpeaks_RA[order(fe.validpeaks_RA$Site_No,fe.validpeaks_RA$Wateryear),]

x=table(paste(fe.validpeaks_RA$Site_No,fe.validpeaks_RA$Segment,sep="."))
fe.validpeaks_RA=cbind(fe.validpeaks_RA[,1:3],rep(hatalpha.i.sub,x)); names(fe.validpeaks_RA)[4]="FE"
fe.validpeaks_RA=fe.validpeaks_RA[order(fe.validpeaks_RA$Site_No,fe.validpeaks_RA$Wateryear),]
fe.AllPeaks_RA=merge(fe.validpeaks_RA,AllPeaks_RA[,1:3], by=c("Wateryear","Site_No","Segment"),all.y=T)
fe.AllPeaks_RA=fe.AllPeaks_RA[order(fe.AllPeaks_RA[,2],fe.AllPeaks_RA[,1]),]; fe.AllPeaks_RA=fe.AllPeaks_RA[which(!duplicated(fe.AllPeaks_RA)),]
fe.AllPeaks_RA$FE[which(fe.AllPeaks_RA$Segment==0)]=0; 

for(i in unique(fe.AllPeaks_RA$Site_No)){
   mat_RA=subset(fe.AllPeaks_RA,fe.AllPeaks_RA$Site_No==i)
   if( any(mat_RA$FE == 0) ){
      whichseg=which(rle(mat_RA$Segment)$values==0)[which(!which(rle(mat_RA$Segment)$values==0) %in% c(1,length(rle(mat_RA$Segment)$values)))]
      zerosegs=which(rep(seq(1,length(rle(mat_RA$Segment)$values)),rle(mat_RA$Segment)$lengths) %in% whichseg)
      if(length(whichseg)>0){
         for(j in 1:length(whichseg)){
            interpbtw=c(rle(mat_RA$FE)$values[whichseg[j]-1],rle(mat_RA$FE)$values[whichseg[j]+1])
            newFE=approx(x=c(0,1),y=interpbtw,xout=seq(1:rle(mat_RA$Segment)$lengths[whichseg[j]])/(rle(mat_RA$Segment)$lengths[whichseg[j]]+1))
            replacewhich=which(rep(seq(1,length(rle(mat_RA$Segment)$lengths)),rle(mat_RA$Segment)$lengths)==whichseg[j])
            fe.AllPeaks_RA$FE[which(fe.AllPeaks_RA$Site_No==i)][replacewhich]=newFE$y
         }
      }
   }
} 
#cbind(hatalpha.i.sub,mat_RArix(rep(rep(0,nrow(hatalpha.i.sub)),3),ncol=3),se.hatalpha.i.sub)

fe.AllPeaks_RA$Segment_wnegs=RA_segs; 
fe.AllPeaks_RA=fe.AllPeaks_RA[,c(1:3,5,4)]
fe.AllPeaks_RA$FE[which(fe.AllPeaks_RA$FE==0)]=NA; 
######################################################################################################



#--------------- table of fixed effects ----------------------------------------
fe_bystation=cbind(comp.hatalpha.i,matrix(rep(rep(0,nrow(hatalpha.i)),2),ncol=2),comp.se.hatalpha.i)
statnames <- dimnames(hatalpha.i)[[1]];
stat <- substr(statnames,1,8); seg <- substr(statnames, 10, 10)
for(i in stat) {
	fe_bystation[which(stat == i),2] = fe_bystation[max(which(stat == i)),1]
}
fe_bystation[,3]=fe_bystation[,2]-fe_bystation[,1];
dimnames(fe_bystation) <- NULL; fe_bystation=data.frame(matrix(c(stat, seg, fe_bystation),ncol=6));

names(fe_bystation) <- c("Station","Segment","Fixed Ef","Current FE","FE Difference","SE")

# add beginning and end of POR wy
library(dplyr)
seg.wy = group_by(SegPeaks, Site_No) %>% summarise(POR.start.yr = min(Wateryear), POR.end.yr = max(Wateryear))

fe_bystation$POR.start.yr = seg.wy$POR.start.yr
fe_bystation$POR.end.yr = seg.wy$POR.end.yr

if(write.file) write.csv(fe_bystation, paste(newadjpath,"fe_bystation",VER,outversion,pilfstr,".csv",sep=""),row.names=F)


#precip+fe adjusted all peaks
#AllPeak.adj=log10(AllPeaks$Peak)-AllPeaks$Precip*within$coefficients[1]-fe.allpeaks$FE+intercept
# fe adjusted all peaks
AllPeak.adj=log10(AllPeaks$Peak)-fe.allpeaks$FE+intercept.sub
AllPeak.adj.RA=log10(AllPeaks_RA$Peak)-fe.AllPeaks_RA$FE+intercept.sub

# mean of valid prcp
valid_prcp_mean = 1.26176

# fit precip on peak and fit precip
library(MASS)

ind=which(RA_segs_wnegs!=-5)
allprcp_fit = rlm(AllPeaks_RA$Precip[ind] ~ AllPeak.adj.RA[ind])
#plot(AllPeak.adj,AllPeaks$Precip); abline(allprcp_fit)
# as a function of prcp
prcp_coef = c(-coef(allprcp_fit)[1]/coef(allprcp_fit)[2],1/coef(allprcp_fit)[2])

points = matrix(0,length(taus),3)
colnames(points) = c("Urban.Frac","Precip","Peak")
for (g in 1:length(taus)){
   #Comment Added 5/18/2015: solve system of linear equation
   a = cbind(c(prcp_coef[2],bootcoef[g,7]),c(-1,-1))
   b = c(-prcp_coef[1], -bootcoef[g,5])
   line_eq = solve(a,b)
   #cm <- rbind(prcp_coef,bootcoef[g,c(5,7)])
   #line_eq = c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
   points[g,] = c(0,line_eq)
}
prcp_tau = points[,2]



#5/22/2015: force uniformity of taus by estimating precip that allows uniformity
if(optmprcp){          ### begin optmprcp
fr <- function(P){
  sum(bootcoef$BS.Intercept[t]+AllPeaks$Urban.Frac*btau.boot[t]+bootcoef$BS.Precip[t]*P >= AllPeak.adj, na.rm=T) - ceil(taus[t]*sum(!is.na(AllPeak.adj)))
}
unifP = NULL
for(t in 1:length(taus)){
  p = uniroot(fr,c(0,10))$root
  if(abs(fr(p)) <= 1) print(paste(t,": Successful for q = ",taus[t],sep="")) else print(paste(t,": Failed for q = ",taus[t],sep="")) 
  unifP  = c(unifP ,p)
}

allprecip = data.frame(Taus=taus,Fitted.Precip=prcp_tau, Mean.Precip=mean(AllPeaks$Precip), 
  Mean.Reg.Precip=mean(SegPeaks$Precip),Median.Precip=median(AllPeaks$Precip),Optm.Precip=unifP)

#---- calculate non-EP(i) using interpolation on tau ----
prcps = NULL
newtauiboot.interp=NULL
for (i in 1:length(AllPeaks$Urban.Frac)){
	newtauiboot.interp=rbind(newtauiboot.interp,approx(bootcoef$BS.Intercept+AllPeaks$Urban.Frac[i]*btau.boot+bootcoef$BS.Precip*unifP
	  ,taus,xout=AllPeak.adj[i],yleft=0, yright=1))
}
} else   {              

prcps = NULL
newtauiboot.interp=NULL
for (i in 1:length(AllPeaks$Urban.Frac)){
	newtauiboot.interp=rbind(newtauiboot.interp,approx(bootcoef$BS.Intercept+AllPeaks$Urban.Frac[i]*btau.boot+bootcoef$BS.Precip*prcp_tau
		,taus,xout=AllPeak.adj[i],yleft=0, yright=1))
}
}				### end optmprcp


newtauiboot.itp=matrix(unlist(newtauiboot.interp),ncol=2)[,2]

prcps = cbind(prcps, newtauiboot.itp)



# new btaus
btau.poly=predict(modelpoly,list(taus=newtauiboot.itp))


#----------- current segment FE adjustment -------------------------
urban.adj.wyr.sub = subset(urban.adj.wyr, urban.adj.wyr.frame$site %in% as.numeric(unique(fe.allpeaks$Site_No)))
fe.allpeaks$CurrentUF=rep(urban.adj.wyr.sub,table(fe.allpeaks$Site_No))
vec.cursegFE=NULL
for(i in unique(fe.allpeaks$Site_No)){
   mat=subset(fe.allpeaks,fe.allpeaks$Site_No==i)
   cursegFE=mat$FE[which(mat$Segment==max(mat$Segment))][1]
   vec.cursegFE=c(vec.cursegFE,rep(cursegFE,nrow(mat)))
}

fe.allpeaks$FE.Current=vec.cursegFE
fe.allpeaks$FE.Diff=fe.allpeaks$FE.Current-fe.allpeaks$FE
#--------------------------------------------------------------------------------------------------------------------


#-------------------------- Peak Adjustment ----------------------------------
Peak.adj.boot.itp.poly=fe.allpeaks$FE.Diff+log10(AllPeaks$Peak)+btau.poly*(fe.allpeaks$CurrentUF-AllPeaks$Urban.Frac)
#plot(AllPeaks$Urban.Frac,Peak.adj.boot.itp.poly)

### Peak mean adjustment
mean.reg=within$coefficients
Peak.mean.adj=fe.allpeaks$FE.Diff+log10(AllPeaks$Peak)+mean.reg[2]*(fe.allpeaks$CurrentUF-AllPeaks$Urban.Frac)

# write table
table=cbind(AllPeaks[,c(2,3,1,7,8)],fe.allpeaks[,c(6:8)],log10(AllPeaks$Peak),AllPeak.adj,Peak.mean.adj,
	Peak.adj.boot.itp.poly,AllPeaks$Peak,10^Peak.mean.adj,10^Peak.adj.boot.itp.poly,btau.poly,newtauiboot.itp)
colnames(table)=c("Station","Segment","Year","Precip","Urban Frac","Present Urban Frac","Current Seg FE","Current Seg FE Adjustment",
	"log10(Original Peaks)","log10(Peaks (Fixed Effects Adj))","log10(Peak Mean Adj)","log10(Peak QR Adj)",
	"Original Peaks","Peak Mean Adj","Peak QR Adj","b(i)","tau")

if(RA) peak.info = subset(peak.info, peak.info$Streamgage %in% RAsubset)
if(include.mobs){
   added.obs = as.numeric(peak.info$segment_wnegs_edit == -5)
   table$added.obs = added.obs
}

table$peak_rank = table$tau_rank = 0
for(i in unique(table$Station)){
   ind = which(table$Station == i)
   table$peak_rank[ind] = rank(table["Peak QR Adj"][ind,]) 
   table$tau_rank[ind] = rank(table["tau"][ind,]) 
}

table$is.redundant = as.numeric(!as.numeric(table$Station) %in% RAsubset)
table$peak.codes = AllPeaks$Codes
table$used.in.adj.reg = as.numeric(segs_wnegs >= 1)
table$used.in.adj.reg_RA = as.numeric(table$is.redundant == 0 & segs_wnegs >= 1)

if(write.file) write.csv(table,row.names=FALSE,paste(newadjpath,"newadjustment.phaseII",VER,outversion,prcpstr,pilfstr,".csv",sep=""))






if(FALSE){

#####################################################################################
#CDF of EP calculated through QR-Function Fit
#if(write.file) {pdf(paste(newadjpath,"CDF.peak_EP.pdf",sep=""))}
plot(ecdf(1-newtauiboot.itp),main="CDF of Peaks Exceedance Probability",xlab="Exceedance Probability", ylab="Proportion")
abline(v=taus, lty=3)
#dev.off()


#plot quantile regression lines
if(write.file) pdf(paste(newadjpath,"quantregplot",VER,outversion,prcpstr,pilfstr,".pdf",sep=""))
plot(AllPeaks$Urban.Frac,AllPeak.adj,cex=.25,type="n",xlab="Urban Frac", ylab="log10 Peak (Adjusted for Precip and FE)",xlim=c(-.05,1))
points(AllPeaks$Urban.Frac,AllPeak.adj,cex=.5,col=rgb(0,0,0,.7))
clip(-.02,1.01,0,6)
abline(bootcoef$BS.Intercept[12]+bootcoef$BS.Precip[12]*prcp_tau[12],bootcoef$BS.Urban.Frac[12],col="red")
mean_coef = coef(lm(AllPeak.adj~AllPeaks$Urban.Frac+AllPeaks$Precip))
abline(mean_coef[1]+valid_prcp_mean*mean_coef[3],mean_coef[2],col="blue") #the dreaded ols line
plottaus=c(.01,.02,.05,.1,.2,.4,.6,.8,.9,.96,.98,.99,.998)
t=which(bootcoef$Tau %in% plottaus)
for(i in t) {
	#abline(bootcoef$BS.Intercept[i]+within$coefficients[1]*valid_prcp_mean,bootcoef$BS.Urban.Frac[i],col="gray")
	abline(bootcoef$BS.Intercept[i]+bootcoef$BS.Precip[i]*prcp_tau[i],bootcoef$BS.Urban.Frac[i],col="black")
	pred=bootcoef$BS.Intercept[i]+bootcoef$BS.Precip[i]*prcp_tau[i]-.05*bootcoef$BS.Urban.Frac[i]
	clip(-.1,1.01,0,6); text(-.05,pred, paste("Q=",tau[i],sep=""), cex=0.5); clip(-.01,1.01,0,6)
}
legend(.55,1.4,cex=.8,c("Quantile Regressions","Median Regression (Q=.5)","Mean Regression"),lty=c(1,1,1),lwd=c(2.5,2.5,2.5),col=c("grey","red","blue"))

# plot with adjustment lines
plot(AllPeaks$Urban.Frac,AllPeak.adj,cex=.25,type="n",xlab="Urban Frac", ylab="log10 Peak (Adjusted for Precip and FE)",xlim=c(-.05,1))
Peak.QRadj.noFE = log10(AllPeaks$Peak)-fe.allpeaks$FE+intercept+btau.poly*(fe.allpeaks$CurrentUF-AllPeaks$Urban.Frac)
clip(-.02,1.01,0,6)
abline(bootcoef$BS.Intercept[12]+bootcoef$BS.Precip[12]*prcp_tau[12],bootcoef$BS.Urban.Frac[12],col="red")
abline(mean_coef[1]+valid_prcp_mean*mean_coef[3],mean_coef[2],col="blue") #the dreaded ols line
plottaus=c(.01,.02,.05,.1,.2,.4,.6,.8,.9,.96,.98,.99,.998)
t=which(bootcoef$Tau %in% plottaus)
for(i in t) {
	#abline(bootcoef$BS.Intercept[i]+within$coefficients[1]*valid_prcp_mean,bootcoef$BS.Urban.Frac[i],col="gray")
	abline(bootcoef$BS.Intercept[i]+bootcoef$BS.Precip[i]*prcp_tau[i],bootcoef$BS.Urban.Frac[i],col="black")
	pred=bootcoef$BS.Intercept[i]+bootcoef$BS.Precip[i]*prcp_tau[i]-.05*bootcoef$BS.Urban.Frac[i]
	clip(-.1,1.01,0,6); text(-.05,pred, paste("Q=",tau[i],sep=""), cex=0.5); clip(-.01,1.01,0,6)
}
points(table[,"Present Urban Frac"],Peak.QRadj.noFE,cex=.5,col=rgb(1,0,0,.5))
legend(.55,1.4,cex=.8,c("Quantile Regressions","Median Regression (Q=.5)","Mean Regression"),lty=c(1,1,1),lwd=c(2.5,2.5,2.5),col=c("grey","red","blue"))

# plot with lines
plot(AllPeaks$Urban.Frac,AllPeak.adj,cex=.25,type="n",xlab="Urban Frac", ylab="log10 Peak (Adjusted for Precip and FE)",xlim=c(-.05,1))
Peak.QRadj.noFE = log10(AllPeaks$Peak)-fe.allpeaks$FE+intercept+btau.poly*(fe.allpeaks$CurrentUF-AllPeaks$Urban.Frac)
segments(x0=AllPeaks$Urban.Frac,y0=AllPeak.adj,x1=table[,"Present Urban Frac"],
	y1=Peak.QRadj.noFE,col=rgb(0,0,0,.1))
points(AllPeaks$Urban.Frac,AllPeak.adj,cex=.5,col=rgb(0,0,0,.7))
clip(-.02,1.01,0,6)
abline(bootcoef$BS.Intercept[12]+bootcoef$BS.Precip[12]*prcp_tau[12],bootcoef$BS.Urban.Frac[12],col="red")
abline(mean_coef[1]+valid_prcp_mean*mean_coef[3],mean_coef[2],col="blue") #the dreaded ols line
plottaus=c(.01,.02,.05,.1,.2,.4,.6,.8,.9,.96,.98,.99,.998)
t=which(bootcoef$Tau %in% plottaus)
for(i in t) {
	#abline(bootcoef$BS.Intercept[i]+within$coefficients[1]*valid_prcp_mean,bootcoef$BS.Urban.Frac[i],col="gray")
	abline(bootcoef$BS.Intercept[i]+bootcoef$BS.Precip[i]*prcp_tau[i],bootcoef$BS.Urban.Frac[i],col="black")
	pred=bootcoef$BS.Intercept[i]+bootcoef$BS.Precip[i]*prcp_tau[i]-.05*bootcoef$BS.Urban.Frac[i]
	clip(-.1,1.01,0,6); text(-.05,pred, paste("Q=",tau[i],sep=""), cex=0.5); clip(-.01,1.01,0,6)
}
legend(.55,1.4,cex=.8,c("Quantile Regressions","Median Regression (Q=.5)","Mean Regression"),lty=c(1,1,1),lwd=c(2.5,2.5,2.5),col=c("grey","red","blue"))

#plot with adjustment and line
plot(AllPeaks$Urban.Frac,AllPeak.adj,cex=.25,type="n",xlab="Urban Frac", ylab="log10 Peak (Adjusted for Precip and FE)",xlim=c(-.05,1))
Peak.QRadj.noFE = log10(AllPeaks$Peak)-fe.allpeaks$FE+intercept+btau.poly*(fe.allpeaks$CurrentUF-AllPeaks$Urban.Frac)
segments(x0=AllPeaks$Urban.Frac,y0=AllPeak.adj,x1=table[,"Present Urban Frac"],
	y1=Peak.QRadj.noFE,col=rgb(0,0,0,.1))
points(AllPeaks$Urban.Frac,AllPeak.adj,cex=.5,col=rgb(0,0,0,.7))
clip(-.02,1.01,0,6)
abline(bootcoef$BS.Intercept[12]+bootcoef$BS.Precip[12]*prcp_tau[12],bootcoef$BS.Urban.Frac[12],col="red")
abline(mean_coef[1]+valid_prcp_mean*mean_coef[3],mean_coef[2],col="blue") #the dreaded ols line
plottaus=c(.01,.02,.05,.1,.2,.4,.6,.8,.9,.96,.98,.99,.998)
t=which(bootcoef$Tau %in% plottaus)
for(i in t) {
	#abline(bootcoef$BS.Intercept[i]+within$coefficients[1]*valid_prcp_mean,bootcoef$BS.Urban.Frac[i],col="gray")
	abline(bootcoef$BS.Intercept[i]+bootcoef$BS.Precip[i]*prcp_tau[i],bootcoef$BS.Urban.Frac[i],col="black")
	pred=bootcoef$BS.Intercept[i]+bootcoef$BS.Precip[i]*prcp_tau[i]-.05*bootcoef$BS.Urban.Frac[i]
	clip(-.1,1.01,0,6); text(-.05,pred, paste("Q=",tau[i],sep=""), cex=0.5); clip(-.01,1.01,0,6)
}
points(table[,"Present Urban Frac"],Peak.QRadj.noFE,cex=.5,col=rgb(1,0,0,.5))
legend(.55,1.4,cex=.8,c("Quantile Regressions","Median Regression (Q=.5)","Mean Regression"),lty=c(1,1,1),lwd=c(2.5,2.5,2.5),col=c("grey","red","blue"))
dev.off()


#-----  check for potentially anomolous FE changes -----
unf <- function(x){x = as.numeric(as.character(x)); return(x)}
#phaseI anolomous segments
phaseIpath = "//IGSASCEWFSHERCU/ILWSC_Data/SW_INV/UrbanHydrology/PhaseI_USACE/from DANAE_share/plm results/wPrecip.noDet.Saito/stuff_to_try/quantile_regression/"
phaseIadjfile = paste(phaseIpath,"output/new.adjustments/newadjustment.phaseI.csv",sep="")
phaseIIsegfile <- paste(segdefpath,"combined.info.allsegments.phaseII.FINAL",segver,".csv",sep="")

phaseIadj = read.csv(phaseIadjfile)
phaseIanom = unique(phaseIadj[which(phaseIadj$Current.Seg.FE.Adjustment>0),c("Station","Segment","Current.Seg.FE.Adjustment")])
phaseIIanom = unique(subset(table[which(table["Current Seg FE Adjustment"]>0),c("Station","Segment","Current Seg FE Adjustment")], Segment != 0)); 

phaseIstat = unique(phaseIadj$Station)
inphaseI = unf(phaseIIanom$Station[which(unf(phaseIIanom$Station) %in% phaseIstat)])
anominphaseI = unf(phaseIIanom$Station[which(unf(phaseIIanom$Station) %in% phaseIanom$Station)])
not.anominphaseI = unf(phaseIIanom$Station[which(!unf(phaseIIanom$Station) %in% phaseIanom$Station)])
# read in segment info from Saito.Segments
phaseIIsegs=read.csv(phaseIIsegfile, colClasses=c("Station"="character"))
anomseginfo=subset(phaseIIsegs, phaseIIsegs$Station %in% paste(unf(phaseIIanom$Station),".",phaseIIanom$Segment,sep=""))[,c(2,3,6,7)]


tab=cbind(phaseIIanom,as.numeric(unf(phaseIIanom$Station) %in% phaseIstat), as.numeric(unf(phaseIIanom$Station) %in% phaseIanom$Station),anomseginfo)
names(tab) = c("Station","Segment","Current Seg Diff","Station in PhaseI","Station Anomalous in PhaseI","Station Name",
	"County","Seg POR Begin","Seg POR End")
if(write.file) write.csv(tab, paste(newadjpath,"anomalous.segments.phaseII",VER,outversion,".csv",sep=""),row.names=F)




#---------- scatter plot of all peaks against time -------------------------
pch.code <- function(code,condition.code,pch,pch.code){
	no.code <- code %in% condition.code
	pch.string <- rep(0,length(code))
	pch.string[which(!no.code)] <- pch
	pch.string[which(no.code)] <- pch.code
	return(pch.string)
}

col.code <- function(code,condition.code,col,col.code){
	no.code <- code %in% condition.code
	col.string <- rep(0,length(code))
	col.string[which(!no.code)] <- col
	col.string[which(no.code)] <- col.code
	return(col.string)
}

pchs=pch.code(peak.info$adjusted.peak.flags,c(7),1,3)
cols=col.code(peak.info$adjusted.peak.flags,c(7),"black","blue")

# logQ plot of time: regular peaks, peaks w/o FE.prcp.UF, peaks w/o FE.prcp.UF
if(write.file) pdf(paste("T:/adjustment/new.adjustments/logQ_FE.prcp.UF_effects",VER,outversion,".pdf",sep=""))
leg.txt = c("Historic Peaks","Loess Fit")
with(peak.info,scatter.smooth(as.Date(as.character(Date),"%Y%m%d") ,log10(discharge),lpars=list(col="red",lwd=2),
	xaxt="n",xlab="Peak Dates",ylab="log10(Discharge)",main="Phase II Flood Peaks",pch=pchs,col=cols))
daterange=range(as.Date(as.character(peak.info$Date),"%Y%m%d"))
axis.Date(1,at=seq(daterange[1],daterange[2], "years"))
legend("bottomleft",leg.txt,lty=c(NA,1),pch=c(3,NA),col=c("blue","red"))

# same plot with FE  effect removed
## codes from 'with.dam.segments.noDet.newUrban.QRPanel_data.only.mod.R'
pchs=pch.code(codes,c(7,27),1,3)
cols=col.code(codes,c(7,27),"black","blue")
adjp <- cbind(as.character(AllPeaks.wdates$Dates), table[,"log10(Peaks (Fixed Effects Adj))"]) 
with(scatter.smooth(as.Date(adjp[,1]),adjp[,2],lpars=list(col="red",lwd=2),
	xaxt="n",xlab="Peak Dates",ylab="log10(Discharge)",main="Peaks w/o FE  Effect",pch=pchs,col=cols))
daterange=range(as.Date(adjp[,1]))
axis.Date(1,at=seq(daterange[1],daterange[2], "years"))
legend("bottomleft",leg.txt,lty=c(NA,1),pch=c(3,NA),col=c("blue","red"))


# same plot with FE + UF effect removed
newadjp <- cbind(as.character(AllPeaks.wdates$Dates), table[,"log10(Peaks (Fixed Effects Adj))"] 
	- table["Urban Frac"]*table["b(i)"]) 
with(newadjp,scatter.smooth(as.Date(newadjp[,1]),newadjp[,2],lpars=list(col="red",lwd=2),
	xaxt="n",xlab="Peak Dates",ylab="log10(Discharge)",main="Peaks w/o FE & UF Effect",pch=pchs,col=cols))
daterange=range(as.Date(newadjp[,1]))
axis.Date(1,at=seq(daterange[1],daterange[2], "years"))
legend("bottomleft",leg.txt,lty=c(NA,1),pch=c(3,NA),col=c("blue","red"))
dev.off()



} # end if false
