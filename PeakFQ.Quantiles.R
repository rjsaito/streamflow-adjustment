# Extract EMA info
dirstr <- "wPrecip.noDet"
setwd(paste("//IGSASCEWFSHERCU/ILWSC_Data/SW_INV/UrbanHydrology/PhaseII_ICT/FlowData/plm results/",dirstr,"/",sep=""))

inpath<-"//IGSASCEWFSHERCU/ILWSC_Data/SW_INV/UrbanHydrology/PhaseII_ICT/"
precpath <- paste(inpath, "Flowdata/matched_precip_output/", sep="")
peakpath <- paste(inpath, "Flowdata/flowdata&quantiles/", sep="")
BC.inpath <- paste(inpath,"Flowdata/BasinCharacteristics/",sep="")
statpath <- paste(inpath,"Flowdata/StationSelection/",sep="")
wdpath <- paste(inpath,"Flowdata/adjustment/",sep="")
segdefpath <- paste(inpath,"Flowdata/Saito.Segments/",sep="")
DApath <- paste(inpath,"GIS/Results/",sep="")
PFQpath <- paste(inpath,"FlowData/PeakFQ/",sep="")

source(paste(segdefpath,"date.convert.R",sep=""))
source(paste(segdefpath,"xlsxToR.R",sep=""))


## FLAGS ##
write.file <- F
ver <- "_station"
VER <- "_AllPeaks_FittedPrcp_regskew"
###########

outpath <- paste(PFQpath,"PeakFQ_Saito/pFQ",VER,"/Output",sep="")
dir.create(outpath,showWarnings = F)

#-- EMA Info Spreadsheet --
EMA <- readLines(paste(PFQpath,"PeakFQ_Saito/","pFQ",VER,"/PEAKFQ.PRT",sep=""))

EMAsummary <- data.frame(Station=as.numeric(), State=as.character(), BeginYear = as.numeric(), EndYear = as.numeric(), 
	Hist.Period.Length = as.numeric(), Num.Peaks = as.numeric(), Peaks.not.used = as.numeric(), Systematic.Peaks = as.numeric(),
	Historic.Peaks = as.numeric(), MGB.LO.Threshold = as.numeric(), Num.Low.Outliers = as.numeric(), LP3.Log10.Mean = as.numeric(), 
	LP3.Log10.StDev = as.numeric(),LP3.Log10.Skew = as.numeric(), EMA.Est.MSE.Skew.wo.REGINFO = as.numeric(),
	EMA.Est.MSE.Skew.w.Systematic = as.numeric(), EMA.Wtd.Reg.Skew = as.numeric(),
	stringsAsFactors=FALSE, check.rows=F)


trim.trailing <- function (x) sub("\\s+$", "", x)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

statsum_ind <- which(EMA == "                     I N P U T   D A T A   S U M M A R Y")
for(i in 1:length(statsum_ind)){
   EMAsummary[i,] <- NA
   EMAsummary$Station[i] <- substr(EMA[statsum_ind[i]-3],gregexpr(0,EMA[statsum_ind[i]-3])[[1]][1],(gregexpr(0,EMA[statsum_ind[i]-3])[[1]][1]+7))
   EMAsummary$State[i] <- substrRight(trim.trailing(EMA[statsum_ind[i]-3]),2)
   EMAsummary$BeginYear[i] <- substr(EMA[statsum_ind[i]+6],58,63)
   EMAsummary$EndYear[i] <- substr(EMA[statsum_ind[i]+7],58,63)
   EMAsummary$Hist.Period.Length[i] <- substr(EMA[statsum_ind[i]+8],58,63)
   EMAsummary$Num.Peaks[i] <- substr(EMA[statsum_ind[i]+2],58,63)
   EMAsummary$Peaks.not.used[i] <- substr(EMA[statsum_ind[i]+3],58,63)
   EMAsummary$Systematic.Peaks[i] <- substr(EMA[statsum_ind[i]+4],58,63)
   EMAsummary$Historic.Peaks[i] <- substr(EMA[statsum_ind[i]+5],58,63)
   EMAsummary$Gen.Skew[i] <- substr(EMA[statsum_ind[i]+9],58,63)
   EMAsummary$SE.Gen.Skew[i] <- substr(EMA[statsum_ind[i]+10],58,63)
   EMAsummary$MSE.Gen.Skew[i] <- substr(EMA[statsum_ind[i]+11],58,63)
   if(i != length(statsum_ind)) endline <- (statsum_ind[i+1]-1) else endline <- length(EMA)
   if("EMA003I-PILFS" %in% substr(EMA[statsum_ind[i]:endline],5,17)){
	  EMAsummary$MGB.LO.Threshold[i] <- substr(EMA[which(substr(EMA[statsum_ind[i]:endline],5,17) 
		== "EMA003I-PILFS")+statsum_ind[i]-1],78,83) 
	  EMAsummary$Num.Low.Outliers[i] <- substr(EMA[which(substr(EMA[statsum_ind[i]:endline],5,17) 
		== "EMA003I-PILFS")+statsum_ind[i]-1],71,73) 
   } else EMAsummary$MGB.LO.Threshold[i] <- EMAsummary$Num.Low.Outliers[i] <- NA
   if(any(regexpr("EMA W/O REG. INFO",  EMA[statsum_ind[i]:endline])>0)){
      EMAsummary$LP3.Log10.Mean[i] <- substr(EMA[which(substr(EMA[statsum_ind[i]:endline],2,18) 
		== "EMA W/O REG. INFO")+statsum_ind[i]-1],26,34)
      EMAsummary$LP3.Log10.StDev[i] <- substr(EMA[which(substr(EMA[statsum_ind[i]:endline],2,18) 
		== "EMA W/O REG. INFO")+statsum_ind[i]-1],39,46)
      EMAsummary$LP3.Log10.Skew[i] <- substr(EMA[which(substr(EMA[statsum_ind[i]:endline],2,18) 
		== "EMA W/O REG. INFO")+statsum_ind[i]-1],50,57)
      EMAsummary$EMA.Est.MSE.Skew.wo.REGINFO[i] <- substr(EMA[which(substr(EMA[statsum_ind[i]:endline],2,42) 
		== "EMA ESTIMATE OF MSE OF SKEW W/O REG. INFO")+statsum_ind[i]-1],57,64)
      EMAsummary$EMA.Est.MSE.Skew.w.Systematic[i] <- substr(EMA[which(substr(EMA[statsum_ind[i]:endline],2,56) 
		== "EMA ESTIMATE OF MSE OF SKEW W/SYSTEMATIC ONLY (AT-SITE)")+statsum_ind[i]-1],57,64)
      EMAsummary$EMA.Wtd.Reg.Skew[i] <- substr(EMA[which(substr(EMA[statsum_ind[i]:endline],2,18) 
		== "EMA W/REG. INFO  ")+statsum_ind[i]-1],50,57)
   } else EMAsummary$LP3.Log10.Mean[i] <- EMAsummary$LP3.Log10.StDev[i] <- EMAsummary$LP3.Log10.Skew[i] <-
	EMAsummary$EMA.Est.MSE.Skew.wo.REGINFO[i] <- EMAsummary$EMA.Est.MSE.Skew.w.Systematic[i] <- NA
}

EMAsummary$State[which(EMAsummary$State == "D.")] = "IN"; EMAsummary$State[which(EMAsummary$State == " W")] = "WI"
EMAsummary[,3:16] <- sapply(EMAsummary[,3:16], as.numeric)

#if(write.file) write.csv(EMAsummary, paste(outpath, "/EMAsummary",ver,".csv",sep=""), row.names=F)

# Extract Quantiles
stations = EMAsummary$Station    
start = grep("ANNUAL FREQUENCY CURVE -- DISCHARGES AT SELECTED EXCEEDANCE PROBABILITIES", EMA) + 6
end = start + 14
quants = NULL
for(i in 1:length(stations))
  quants = rbind(quants, cbind(stations[i], read.table(text=EMA[start[i]:end[i]], sep="", header=F)))
names(quants) = c("Site_No","EP","EMA.Reg.Est","EMA.NoReg.Est","Var.Est","CI.95.Low","CI.95.High")

if(write.file) write.csv(quants, paste(outpath, "/PeakFQ.Quantiles.csv",sep=""), row.names=F)

  
# need only these quantiles, merge with EMA summary
quantiles = c(2,5,10,25,50,100,200,500)
quants.red = subset(quants, unique(1/quants$EP) %in% quantiles)
quants.red$quants = paste("logQ",round(1/quants.red$EP),sep="")
library(reshape2)
quants.cast.reg = dcast(data=quants.red, Site_No ~ quants, value.var = "EMA.Reg.Est")
quants.cast.noreg = dcast(data=quants.red, Site_No ~ quants, value.var = "EMA.NoReg.Est")
quants.cast.reg = quants.cast.reg[,c(1,4,7,2,6,8,3,5,9)]
quants.cast.noreg = quants.cast.noreg[,c(1,4,7,2,6,8,3,5,9)]
quants.cast.reg[,2:9] = log10(quants.cast.reg[,2:9]) 
quants.cast.noreg[,2:9] = log10(quants.cast.noreg[,2:9])
names(quants.cast.reg) = paste(names(quants.cast.reg), "Reg",sep=".") 
names(quants.cast.noreg) = paste(names(quants.cast.noreg), "NoReg",sep=".") 

EMAsummary = merge(EMAsummary, quants.cast.reg, by=1)
EMAsummary = merge(EMAsummary, quants.cast.noreg, by=1)

EMAsummary[,17:20] = EMAsummary[,c(18:20,17)]
names(EMAsummary)[17:20] = names(EMAsummary)[c(18:20,17)]

if(write.file) write.csv(EMAsummary, paste(outpath, "/EMAsummary",ver,"_weighted.csv",sep=""), row.names=F)



