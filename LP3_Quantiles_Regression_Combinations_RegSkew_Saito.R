#########################################################################
### Updates
### 3/11/2015: Added New Slope (DEM) Variables
### 3/12/2015: Reg Combinations with Regional Skew info
### 5/7/2015: New NLCD11 Adjustment for NWI and SSURGO

write.file = F

setwd("//IGSASCEWFSHERCU//UrbanHydrology\\PhaseII_ICT\\QuantileFitting\\Quantiles\\new_adjustments\\")
senspath = "Y:/PhaseII_ICT/FlowData/adjustment/sensitivity_tests/"
sensout = "Y:/PhaseII_ICT/QuantileFitting/Quantiles/new_adjustments/Sensitivity_Analysis/"
senssubout = paste(sensout,"by.subset/",sep="")
BC.inpath = "T:/BasinCharacteristics/"
PeakFQ.path = "T:/PeakFQ/PeakFQ_Saito/"

segpath <- "//IGSASCEWFSHERCU//ILWSC_Data/SW_INV/UrbanHydrology/PhaseII_ICT/Flowdata/Saito.Segments/"
source(paste(segpath,"date.convert.R",sep=""))

#read in (unadjusted, for now) peak flows in peakfq (watstore) format
#because we need to know how to handle that format anyway in order to
#create an adjusted peak flow file to use with peakfq
#Quantile Choices by Skew
#Please specify which values were used in generating the quantile data in order to read in the correct file
#You will need to enter the log.skew, reg.skew, MSE, and if you want the weighted or unweighted quantiles

# all subsets 
subsets = c("all","RA","stat.bf1980","stat.w1980","rec.bf1980","rec.afw1980",
	"watershed.large","watershed.small","urban.large","urban.noadj",
	"stat.bf1980.RA","stat.w1980.RA","rec.bf1980.RA","rec.afw1980.RA",
	"watershed.large.RA","watershed.small.RA","urban.large.RA","urban.noadj.RA",
	"AB79") # 1/7/2015: new subset

#######---- Relevant Data for Regression Combinations ------########
Basin_Dir = "//IGSASCEWFSHERCU/ILWSC_Data/SW_INV/UrbanHydrology/PhaseII_ICT/QuantileFitting/Basin Characteristics/"
Quantile_Dir = "//IGSASCEWFSHERCU/ILWSC_Data/SW_INV/UrbanHydrology/PhaseII_ICT/QuantileFitting/Quantiles//new_adjustments//"
Moment_Dir = "//IGSASCEWFSHERCU/ILWSC_Data/SW_INV/UrbanHydrology/PhaseII_ICT/QuantileFitting/Width Functions//"
Output_Dir = "//IGSASCEWFSHERCU/ILWSC_Data/SW_INV/UrbanHydrology/PhaseII_ICT/QuantileFitting/Regression_Combinations//"
library(MASS)
library(car)

#First, we need to read in all of the data (a lot of it)
#Stations to Remove (These are stations that have been flagged to use or not)
file.name=paste(Basin_Dir,"ILINWI.station_list.w_useflag.wMUK.corrected.csv",sep="")
UseFlag = read.csv(file.name,sep=",")
UseFlag2 <- subset(UseFlag, UseFlag[,2]==1)
names(UseFlag2)[1]<-"site_no"

#subset
#UseFlag2 = UseFlag2[which(UseFlag2[,1] %in% as.numeric(stat.nums)),]

#---------------------------------------------------------
#Drainage Area ILINS
file.name=paste(Basin_Dir,"final.ILINSS.DAs.csv",sep="")
ILINSSDA = read.csv(file.name,sep=",")
names(ILINSSDA)[1]<-"site_no"; names(ILINSSDA)[2]<-"DA"

#Drainage Area HDB
file.name = paste(Basin_Dir,"Urban_Hydro_22HDB_stats_NLCD.csv",sep="")
HDBDA = read.csv(file.name,sep=",")
names(HDBDA)[1]<-"site_no"; names(HDBDA)[5]<-"HDBDA"


for (z in 1:length(HDBDA[,1])){
	for (y in 1:length(ILINSSDA[,1])){
		if (HDBDA[z,1]==ILINSSDA[y,1]){
			ILINSSDA[y,2] <- HDBDA[z,5]
		}	
	}
}

hdb.stats = HDBDA[,1]
#-----------------------------------------------------------

#Imperviousness
#file.name = paste(Basin_Dir,"Urban_Hydro_stats_IMPERV.csv",sep="")
#11/19/2014: New NLCD2011 Impervious Stats available
file.name = paste(Basin_Dir,"ALL_NLCD11_Impervious_stats.csv",sep="")
Imperv = read.csv(file.name,sep=",",skip=1, stringsAsFactors=F)
IMPERV <- Imperv[1:189,c("NAME","MEAN")]
names(IMPERV)[1]<-"site_no"; names(IMPERV)[2]<-"HYDRO_Imperv"

#file.name = paste(Basin_Dir,"Urban_Hydro_22HDB_stats_IMPERV.csv",sep="")
#IMPERVHDB = read.csv(file.name,sep=",")
#11/19/2014: HDB in same file
IMPERVHDB = Imperv[c(193:214,218:223),c("NAME","MEAN")]

for (z in 1:length(IMPERVHDB[,1])){
      ind = which(IMPERVHDB[z,1]==IMPERV[,1])
	IMPERV[ind,2] <- IMPERVHDB[z,2]
} 
IMPERV[,1] = as.numeric(as.character(IMPERV[,1])); IMPERV[,2] = as.numeric(IMPERV[,2]);

# Theobald Imperviousness
file.name = paste(Basin_Dir,"Urban_Hydro_stats_ALL.w22HDB_BHCcombined.Est4.csv",sep="")
IMPERV.THEO = read.csv(file.name,sep=",")
IMPERV.THEO <- subset(IMPERV.THEO[,c("STATION","Fraction_7.10")],IMPERV.THEO$Decade == 2010)
names(IMPERV.THEO) <-c("site_no","Imperv_Theo_Est4.FracUrban")


#Will also use NLCD codes as another measure of imperviousness
#file.name = paste(Basin_Dir, "FINAL_DA_NLCD06.csv", sep="")
#NLCD06 = read.csv(file.name,sep=",")
#11/19/2014: Replace with new NLCD2011 Data
file.name = paste(Basin_Dir, "FINAL_DA_NLCD11.csv", sep="")
NLCD11.FIN = read.csv(file.name,sep=",",skip=1,stringsAsFactors=F)
NLCD11 = NLCD11.FIN[,1:16]

###HDB NWIS Deliniation
#file.name = paste(Basin_Dir, "Urban_Hydro_22HDB_stats_NLCD06.csv", sep="")
#HDB06 = read.csv(file.name,sep=",")
#11/19/2014: Replace with new NLCD2011 Data
HDB11 = NLCD11.FIN[c(1:21,26:31,35),18:33]
HDB11[,1] = substr(HDB11[,1],1,7)
HDB11 = data.matrix(HDB11)

for (z in 1:length(HDB11[,1])){
      ind = which(as.numeric(substr(HDB11[z,1],1,7))==NLCD11[,1])
	NLCD11[ind,] <- HDB11[z,]
}

#Update Wetland to square miles 
NLCD11[,2:16] = NLCD11[,2:16]*.000000386102
NLCD11Wetland = NLCD11

# Add Columns
NLCD11$Imperv_21_22_23_24 = rowSums(NLCD11[,3:6])/rowSums(NLCD11[,2:16])
NLCD11$Imperv_22_23_24 = rowSums(NLCD11[,4:6])/rowSums(NLCD11[,2:16])
NLCD11$Imperv_23_24 = rowSums(NLCD11[,5:6])/rowSums(NLCD11[,2:16])
NLCD11$Imperv_24 = NLCD11[,6]/rowSums(NLCD11[,2:16])
names(NLCD11)[1] <- "site_no";


#-----------------------------------------------------------
#Slope
file.name = paste(Basin_Dir, "Urban_Hydro_stats_SSURGO_SlopeGrad.csv", sep="")
BasinSlope = read.csv(file.name,sep=",")
BasinSlope <- BasinSlope[,c(1,3)]

#HDB
file.name = paste(Basin_Dir, "Urban_Hydro_22HDB_stats_SSURGO_SlopeGrad.csv", sep="")
BasinSlopeHDB = read.csv(file.name,sep=",")
BasinSlopeHDB <- BasinSlopeHDB[,c(1,3)]
for (z in 1:length(HDBDA[,1])){
	for (y in 1:length(ILINSSDA[,1])){
		if (HDBDA[z,1]==ILINSSDA[y,1]){
			BasinSlope[y,2] <- BasinSlopeHDB[z,2]
		}	
	}
}
#
names(BasinSlope)[1] <- "site_no"; names(BasinSlope)[2] <-"SSURGO_SLOPE"

#Other Slope Data
file.name= paste(Basin_Dir,"Urban_Hydro_stats_ELEV.csv",sep="")
BasinSlope2 = read.csv(file.name,sep=",")
BasinSlope2 <- BasinSlope2[,c(1,12)]

#HDB
file.name= paste(Basin_Dir,"Urban_Hydro_22HDB_stats_ELEV.csv",sep="")
BasinSlope2HDB = read.csv(file.name,sep=",")
BasinSlope2HDB <- BasinSlope2HDB[,c(1,7)]
for (z in 1:length(HDBDA[,1])){
	for (y in 1:length(ILINSSDA[,1])){
		if (HDBDA[z,1]==ILINSSDA[y,1]){
			BasinSlope2[y,2] <- BasinSlope2HDB[z,2]
		}	
	}
}
#
names(BasinSlope2)[1]<-"site_no"; names(BasinSlope2)[2]<-"HYDRO_SLOPE"
#---------------------------------------------------------

#Start of Wetland Possibilities
#Drainage will be a fraction of Excessive + Mod.Wee + Somewhat Excessive + Well/Sum
file.name= paste(Basin_Dir,"Urban_Hydro_stats_SSURGO_DrainageClass.csv",sep="")
DrainageClass = read.csv(file.name,sep=",")

#HDB
file.name= paste(Basin_Dir,"Urban_Hydro_22HDB_stats_SSURGO_DrainageClass.csv",sep="")
DrainageClassHDB = read.csv(file.name,sep=",")
for (z in 1:length(HDBDA[,1])){
	for (y in 1:length(ILINSSDA[,1])){
		if (HDBDA[z,1]==ILINSSDA[y,1]){
			DrainageClass[y,] <- DrainageClassHDB[z,]
		}	
	}
}

#
names(DrainageClass)[1] <- "site_no"
DrainageClass$Wet_DrainageClass0 = DrainageClass[,9]/rowSums(DrainageClass[,2:10])
DrainageClass$Wet_DrainageClass1 = DrainageClass[,7]/rowSums(DrainageClass[,2:10])
DrainageClass$Wet_DrainageClass2 = rowSums(DrainageClass[,c(4,7)])/rowSums(DrainageClass[,2:10])
DrainageClass$Wet_DrainageClass3 = rowSums(DrainageClass[,c(4,6,7)])/rowSums(DrainageClass[,2:10])
DrainageClass$Wet_DrainageClass1a = rowSums(DrainageClass[,c(7,9)])/rowSums(DrainageClass[,2:10])
DrainageClass$Wet_DrainageClass2a = rowSums(DrainageClass[,c(4,7,9)])/rowSums(DrainageClass[,2:10])
DrainageClass$Wet_DrainageClass3a = rowSums(DrainageClass[,c(4,6,7,9)])/rowSums(DrainageClass[,2:10])

#
#Calculated by All Hydric/Sum
file.name= paste(Basin_Dir,"Urban_Hydro_stats_SSURGO_HydricClass.csv",sep="")
HydricClass = read.csv(file.name,sep=",")

#HDB
file.name= paste(Basin_Dir,"Urban_Hydro_22HDB_stats_SSURGO_HydricClass.csv",sep="")
HydricClassHDB = read.csv(file.name,sep=",")
for (z in 1:length(HDBDA[,1])){
	for (y in 1:length(ILINSSDA[,1])){
		if (HDBDA[z,1]==ILINSSDA[y,1]){
			HydricClass[y,] <- HydricClassHDB[z,]
		}	
	}
}

#
names(HydricClass)[1] <- "site_no"
HydricClass$Wet_HydricClass1 = HydricClass[,2]/rowSums(HydricClass[,2:6])
HydricClass$Wet_HydricClass2 = rowSums(HydricClass[,c(2,4)])/rowSums(HydricClass[,2:6])
HydricClass$Wet_HydricClass1a = rowSums(HydricClass[,c(2,5)])/rowSums(HydricClass[,2:6])
HydricClass$Wet_HydricClass2a = rowSums(HydricClass[,c(2,4,5)])/rowSums(HydricClass[,2:6])


##
#Used the Mean Value
file.name= paste(Basin_Dir,"Urban_Hydro_stats_SSURGO_WTD.csv",sep="")
WaterTableDepth = read.csv(file.name,sep=",")

#HDB
file.name= paste(Basin_Dir,"Urban_Hydro_22HDB_stats_SSURGO_WTD.csv",sep="")
WaterTableDepthHDB = read.csv(file.name,sep=",")

for (z in 1:length(HDBDA[,1])){
	for (y in 1:length(ILINSSDA[,1])){
		if (HDBDA[z,1]==ILINSSDA[y,1]){
			WaterTableDepth[y,3] <- WaterTableDepthHDB[z,3]
		}	
	}
}
#
names(WaterTableDepth)[1] <- "site_no"
names(WaterTableDepth)[3] <- "Wet_WaterTableDepth"

##
#Calculated by addition of 50-74 percent and 75-100/sum
file.name = paste(Basin_Dir, "Urban_Hydro_stats_SSURGO_Ponding.csv",sep="")
Ponding = read.csv(file.name,sep=",")

#
file.name = paste(Basin_Dir, "Urban_Hydro_22HDB_stats_SSURGO_Ponding.csv",sep="")
PondingHDB = read.csv(file.name,sep=",")
for (z in 1:length(HDBDA[,1])){
	for (y in 1:length(ILINSSDA[,1])){
		if (HDBDA[z,1]==ILINSSDA[y,1]){
			Ponding[y,] <- PondingHDB[z,]
		}	
	}
}
#
Ponding$Wet_Ponding1 = Ponding[,5]/rowSums(Ponding[,c(2:6)])
Ponding$Wet_Ponding2 = rowSums(Ponding[,4:5])/rowSums(Ponding[,c(2:6)])
Ponding$Wet_Ponding3 = rowSums(Ponding[,3:5])/rowSums(Ponding[,c(2:6)])
names(Ponding)[1] <- "site_no"

#############
#Calculated by addition of frequent + very frequent/Sum
file.name = paste(Basin_Dir, "Urban_Hydro_stats_SSURGO_FloodFreq.csv",sep="")
FloodFreq = read.csv(file.name,sep=",")

####################################
file.name = paste(Basin_Dir, "Urban_Hydro_22HDB_stats_SSURGO_FloodFreq.csv",sep="")
FloodFreqHDB = read.csv(file.name,sep=",")
FloodFreqHDB = FloodFreqHDB[,1:7]
for (z in 1:length(HDBDA[,1])){
	for (y in 1:length(ILINSSDA[,1])){
		if (HDBDA[z,1]==ILINSSDA[y,1]){
			FloodFreq[y,] <- FloodFreqHDB[z,]
		}	
	}
}
#
FloodFreq$Wet_FloodFreq1 = rowSums(FloodFreq[,c(2,7)])/rowSums(FloodFreq[,c(2:7)])
FloodFreq$Wet_FloodFreq2 = rowSums(FloodFreq[,c(2,4,7)])/rowSums(FloodFreq[,c(2:7)])
FloodFreq$Wet_FloodFreq1a = rowSums(FloodFreq[,c(2,6,7)])/rowSums(FloodFreq[,c(2:7)])
FloodFreq$Wet_FloodFreq2a = rowSums(FloodFreq[,c(2,4,6,7)])/rowSums(FloodFreq[,c(2:7)])
names(FloodFreq)[1] <- "site_no"


##############################Haven't used
file.name= paste(Basin_Dir,"Urban_Hydro_stats_SSURGO_HydroD.csv",sep="")
HydroD = read.csv(file.name,sep=",")
names(HydroD)[1] <- "site_no"

for (i in 1:length(HydroD[,1])){
HydroD[i,8] = HydroD[i,3]/HydroD[i,4]
}
names(HydroD)[8] <- "CropPer"

####################################
#file.name = paste(Basin_Dir, "FINAL_DA_NLCD06.csv", sep="")
#NLCD11Wetland already an object

NLCD11Wetland$Wet_11_90_95 = rowSums(NLCD11Wetland[,c(2,15,16)])/rowSums(NLCD11Wetland[,2:16])
NLCD11Wetland$Wet_90_95 = rowSums(NLCD11Wetland[,c(15,16)])/rowSums(NLCD11Wetland[,2:16])
NLCD11Wetland$Wet_11_90 = rowSums(NLCD11Wetland[,c(2,15)])/rowSums(NLCD11Wetland[,2:16])
NLCD11Wetland$Wet_11_95 = rowSums(NLCD11Wetland[,c(2,16)])/rowSums(NLCD11Wetland[,2:16])
NLCD11Wetland$Wet_11 = NLCD11Wetland[,2]/rowSums(NLCD11Wetland[,2:16])
#### 3/11/2015 New Wetland Vars
NLCD11Wetland$FracCrop_81_82 = rowSums(NLCD11Wetland[,13:14])/rowSums(NLCD11Wetland[,2:16])
NLCD11Wetland$FracForest_41_42_43 = rowSums(NLCD11Wetland[,8:10])/rowSums(NLCD11Wetland[,2:16])
	
names(NLCD11Wetland)[1] <- "site_no";
NLCD11Wetland <- NLCD11Wetland[,c(1,17:23)]

# 11/19/2014: Add NWI Wetland variables
file.name = paste(Basin_Dir, "Urban_Hydro_stats_WETLAND.csv",sep="")
NWI = read.csv(file.name)

# HDB
file.name = paste(Basin_Dir, "Urban_Hydro_22HDB_stats_WETLAND.csv",sep="")
NWIHDB = read.csv(file.name,sep=",")
NWIHDB[,1] = substr(NWIHDB[,1],1,7)
NWIHDB = data.frame(NWIHDB)


for (z in NWIHDB[,1]){
  NWI[NWI[,1] == z,] == NWIHDB[NWIHDB[,1] == z,]
}

NWI$NWI.fraction_total = NWI[,3]/NWI[,2]
NWI$NWI.fraction_emergent = NWI[,5]/NWI[,2]
NWI$NWI.fraction_forested = NWI[,8]/NWI[,2]
NWI$NWI.fraction_emergent.forested = rowSums(NWI[,c(5,8)])/NWI[,2]
names(NWI)[1] = "site_no"


### 5/7/2015: New NLCD11 Adjustment for NWI and SSURGO
library(xlsx)
Adj.NWI = read.xlsx(paste(Basin_Dir,"NLCD11_Urban_with_NWI_and_SSURGO.TMO.xlsx", sep=""), sheetName="NLCD11 URB & Wetlands", endRow = 180)
Adj.NWI.HDB = read.xlsx(paste(Basin_Dir,"NLCD11_Urban_with_NWI_and_SSURGO.TMO.xlsx", sep=""), sheetName="NLCD11 URB & Wetlands", startRow = 182, endRow = 204)
Adj.SSURGO = read.xlsx(paste(Basin_Dir,"NLCD11_Urban_with_NWI_and_SSURGO.TMO.xlsx", sep=""), sheetName="NLCD11 URB & SSURGO Drain Class", endRow = 190)
Adj.SSURGO.HDB = read.xlsx(paste(Basin_Dir,"NLCD11_Urban_with_NWI_and_SSURGO.TMO.xlsx", sep=""), sheetName="NLCD11 URB & SSURGO Drain Class", startRow = 192, endRow = 214)

for (z in 1:nrow(Adj.NWI.HDB)){
      ind = which(Adj.NWI[,1]==Adj.NWI.HDB[z,1])
	Adj.NWI[ind,] <- Adj.NWI.HDB[z,]
      ind2 = which(Adj.SSURGO[,1]==Adj.SSURGO.HDB[z,1])
	Adj.SSURGO[ind2,] <- Adj.SSURGO.HDB[z,]
}

Adj.NWI = merge(Adj.NWI, NWI, by=1, all.y=T) 
Adj.NWI$Adj.NWI.fraction_total = Adj.NWI$NWI.fraction_total  - Adj.NWI[,3]/Adj.NWI[,2]
Adj.NWI$Adj.NWI.fraction_total[is.na(Adj.NWI$Adj.NWI.fraction_total)] = Adj.NWI$NWI.fraction_total[is.na(Adj.NWI$Adj.NWI.fraction_total)]
Adj.SSURGO = merge(Adj.SSURGO, DrainageClass, by=1, all.y=T) 
Adj.SSURGO$Adj.Wet_DrainageClass1a  = Adj.SSURGO$Wet_DrainageClass1a   - Adj.SSURGO[,3]/Adj.SSURGO[,2]

Adj.Wetlands = merge(Adj.NWI[,c(1,27)], Adj.SSURGO[,c(1,29)], by=1, all=T)


#### 3/11/2015 New Slope Variables 
DEM_quant = read.csv(paste(Basin_Dir,"DEM_quantiles.combined_combined.csv",sep=""))
DEM_slope = data.frame(site_no = substr(DEM_quant$station,1,7), DEM_1_0 = DEM_quant$X1 - DEM_quant$X0,
	DEM_.999_.001 = DEM_quant$X0.999  - DEM_quant$X0.001, DEM_.99_.01 = DEM_quant$X0.99 - DEM_quant$X0.01,
	DEM_.9_.1 = DEM_quant$X0.9 - DEM_quant$X0.1, DEM_.75_.25 = DEM_quant$X0.75 - DEM_quant$X0.25)

# DEM /DA
# elevation are in hundredths of feet, so convert to miles divide by sqrt(DA)
DEM.DA_slope = merge(DEM_slope, ILINSSDA, by=1)
DEM.DA_slope[,2:6] = DEM.DA_slope[,2:6]/( (5280) * sqrt(DEM.DA_slope$DA) )
names(DEM.DA_slope) = c("site_no","DEM.DA_1_0","DEM.DA_.999_.001","DEM.DA_.99_.01","DEM.DA_.9_.1","DEM.DA_.75_.25","DA")

# DEM / Perimeter
DEM.P_quant = read.csv(paste(Basin_Dir,"DEM_quantiles.combined-perimeters_added_Saito.csv",sep=""))
DEM.P_slope = DEM.P_quant[,c("site_no","X.1.0....perimeter.2" ,"..999..001....perimeter.2")]
#revert transformation
DEM.P_slope[,2:3] = 10^DEM.P_slope[,2:3]
names(DEM.P_slope) = c("site_no","DEM.P_1_0","DEM.P_.999_.001")

DEM.P.EXP.4_slope = DEM.P_quant[,c("site_no","X.1.0....perimeter.3" ,"..999..001....perimeter.3")]
#revert transformation
DEM.P.EXP.4_slope[,2:3] = DEM.P.EXP.4_slope[,2:3]^2
names(DEM.P.EXP.4_slope) = c("site_no","DEM.P_1_0_EXP.4","DEM.P_.999_.001_EXP.4")


##########Dam Storage
file.name = paste(Basin_Dir, "dam.storage.phaseII.csv", sep="")
DamStorage = read.csv(file.name,sep=",")
DamStorage = DamStorage[,c(1,7)]
names(DamStorage)[1] <- "site_no"; names(DamStorage)[2] <- "DStor"
#############

#New Homogeneic Moments Data (6/25)
file.name = paste(Moment_Dir,"/WF_moments/Homogeneous/WF_moments2_NJB.csv", sep="")
NewMoment = read.csv(file.name,sep=",")
names(NewMoment)[1] <- "site_no"

#Homogeneous Filter Moments
file.name = paste(Moment_Dir, "/Filter_Moments/Homogeneous/K_2/Moments.csv",sep="")
HomoFilterMoment = read.csv(file.name,sep=",")
names(HomoFilterMoment)[1] <- "site_no"

#####Convolution Homogeneous Filter Max
file.name = "//IGSASCEWFSHERCU/UrbanHydrology/PhaseII_ICT/QuantileFitting/Width Functions/Filtered_Data/Homogeneous/HomogenousFilterMax.csv"
Convo = read.csv(file.name,sep=",")
names(Convo)[1] <- "site_no"

#Heterogeneity Moments (6/25)
file.name = paste(Moment_Dir, "WF_moments/Heterogeneous/WFHetero_moments_NJB.csv", sep="")
HeteroMoment = read.csv(file.name,sep=",")
names(HeteroMoment)[1] <- "site_no"
for (i in 2:11){
	names(HeteroMoment)[i] <- paste("Het",names(HeteroMoment)[i],sep="")
}

#Heterogeneous Filter Moments (Original)
file.name = paste(Moment_Dir, "/Filter_Moments/Heterogeneous/Original/K_2/Moments.csv",sep="")
HetFilterMoment = read.csv(file.name,sep=",")
names(HetFilterMoment)[1] <- "site_no"

#Heterogeneous Filter Maximums
file.name = "Y:/PhaseII_ICT/QuantileFitting/Width Functions/Filtered_Data/Heterogeneous/Original/HeterogenousFilterMax.csv"
ConvoHet = read.csv(file.name,sep=",")
names(ConvoHet)[1] <- "site_no"; names(ConvoHet)[2] <- "MaxHet.25"
names(ConvoHet)[3] <- "MaxHet.5"; names(ConvoHet)[4] <- "MaxHet1"
names(ConvoHet)[5] <- "MaxHet2"; names(ConvoHet)[6] <- "MaxHet4"

#Heterogeneity Moments - Triple Velocity
file.name = paste(Moment_Dir, "WF_moments/Heterogeneous/WFHetero_moments2_NJB.csv", sep="")
HeteroMomentTV = read.csv(file.name,sep=",")
names(HeteroMomentTV)[1] <- "site_no"
for (i in 2:11){
	names(HeteroMomentTV)[i] <- paste("Het",names(HeteroMomentTV)[i],"TV",sep="")
}

#Heterogeneous Filter Moments - Triple Velocity
file.name = paste(Moment_Dir, "/Filter_Moments/Heterogeneous/TripleVelocity/K_4/Moments.csv",sep="")
HetFilterMomentTV = read.csv(file.name,sep=",")
names(HetFilterMomentTV)[1] <- "site_no"
for (i in 2:11){
	names(HetFilterMomentTV)[i] <- paste(names(HetFilterMomentTV)[i],"TV",sep="")
}

#Heterogeneous Filter Maximums - Triple Velocity
file.name = "Y:/PhaseII_ICT/QuantileFitting/Width Functions/Filtered_Data/Heterogeneous/TripleVelocity/HeterogenousFilterMax2.csv"
ConvoHetTV = read.csv(file.name,sep=",")
names(ConvoHetTV)[1] <- "site_no"; names(ConvoHetTV)[2] <- "MaxHetTV.25"
names(ConvoHetTV)[3] <- "MaxHetTV.5"; names(ConvoHetTV)[4] <- "MaxHetTV1"
names(ConvoHetTV)[5] <- "MaxHetTV2"; names(ConvoHetTV)[6] <- "MaxHetTV4"

Merge <- merge(UseFlag2,ILINSSDA, by="site_no",all.x=T)
Merge <- merge(Merge, NLCD11, by="site_no",all.x=T)
Merge <- merge(Merge, NLCD11Wetland, by="site_no",all.x=T)
Merge <- merge(Merge, BasinSlope, by="site_no",all.x=T)
Merge <- merge(Merge, BasinSlope2, by="site_no",all.x=T)
Merge <- merge(Merge, IMPERV, by="site_no",all.x=T)
Merge <- merge(Merge, IMPERV.THEO, by="site_no",all.x=T)
Merge <- merge(Merge, DrainageClass, by="site_no",all.x=T)
Merge <- merge(Merge, HydricClass, by="site_no",all.x=T)
Merge <- merge(Merge, WaterTableDepth, by="site_no",all.x=T)
Merge <- merge(Merge, Ponding, by="site_no",all.x=T)
Merge <- merge(Merge, FloodFreq, by="site_no",all.x=T)
Merge <- merge(Merge, NWI, by="site_no",all.x=T)
Merge <- merge(Merge, Adj.Wetlands, by=1 ,all.x=T)
Merge <- merge(Merge, DEM_slope, by="site_no",all.x=T)
Merge <- merge(Merge, DEM.DA_slope[,-7], by="site_no",all.x=T)
Merge <- merge(Merge, DEM.P_slope, by="site_no",all.x=T)
Merge <- merge(Merge, DEM.P.EXP.4_slope, by="site_no",all.x=T)

Merge <- merge(Merge, DamStorage, by="site_no", all.x=T)
Merge <- merge(Merge, NewMoment, by="site_no", all.x=T)
#Merge <- merge(Merge, HomoFilterMoment, by="site_no", all.x=T)
#Merge <- merge(Merge, Convo, by="site_no", all.x=T)
#Merge <- merge(Merge, HeteroMoment, by="site_no",all.x=T)
#Merge <- merge(Merge, HetFilterMoment, by="site_no", all.x=T)
#Merge <- merge(Merge, ConvoHet, by="site_no", all.x=T)
#Merge <- merge(Merge, HeteroMomentTV, by="site_no",all.x=T)
#Merge <- merge(Merge, HetFilterMomentTV, by="site_no",all.x=T)
#Merge <- merge(Merge, ConvoHetTV, by="site_no",all.x=T)
All.Merge <- Merge
if(write.file) write.csv(All.Merge,paste(Basin_Dir,"All_Basin_Chars_New.csv",sep=""),row.names=F) 

#Models = read.csv("Y:/PhaseII_ICT/QuantileFitting/Regression_Combinations/Combo_Choices/AllInput.noWF.csv", stringsAsFactors=F)
Models = read.csv("Y:/PhaseII_ICT/QuantileFitting/Regression_Combinations/Combo_Choices/AllInput.noWF.07.10.2015.csv", stringsAsFactors=F)
#######################################################################

# checking transformations
Wetland.vars = c("Wet_11_90_95","Wet_11_90","Wet_11_95","Wet_90_95","Wet_11",
	"Wet_DrainageClass0","Wet_DrainageClass1","Wet_DrainageClass2","Wet_DrainageClass3",
	"Wet_DrainageClass1a","Wet_DrainageClass2a","Wet_DrainageClass3a",
	"Wet_HydricClass1","Wet_HydricClass2","Wet_HydricClass1a","Wet_HydricClass2a",
	"Wet_WaterTableDepth","Wet_Ponding1","Wet_Ponding2","Wet_Ponding3",
	"Wet_FloodFreq1","Wet_FloodFreq2","Wet_FloodFreq1a","Wet_FloodFreq2a",
	"NWI.fraction_total","NWI.fraction_emergent","NWI.fraction_forested","NWI.fraction_emergent.forested",
	"FracCrop_81_82","FracForest_41_42_43")
Slope.vars = c("HYDRO_SLOPE","SSURGO_SLOPE","DEM_1_0","DEM_.999_.001","DEM_.99_.01","DEM_.9_.1","DEM_.75_.25")
Imperv.vars = c("Imperv_21_22_23_24","Imperv_22_23_24","Imperv_23_24","Imperv_24","HYDRO_Imperv","Imperv_Theo_Est4.FracUrban")



###################
subset = adj.subset = "RA"
PeakFQ = T
pFQ = "station"
####################

#for(pFQ in c("station")){

if(!file.exists(paste(senssubout,subset,sep=""))) dir.create(paste(senssubout,subset,sep=""))
subsensout = paste(senssubout,subset,"/",sep="")
adjpath = paste("T:/adjustment/sensitivity_tests/",subset,"/",sep="")

#quantiles = read.csv(paste(PeakFQ.path,"pFQ_QR_FINAL_",pFQ,".skew/Output/EMAsummary_",pFQ,"_weighted.csv",sep=""))
quantiles = read.csv("S:/pFQ_AllPeaks_FittedPrcp_regskew/Output/EMAsummary_station_weighted.csv")

quantile.data = data.frame(stat.nums = quantiles$Station, npeaks=quantiles$Num.Peaks, nNAs = quantiles$Peaks.not.used, 
	log.mean = quantiles$LP3.Log10.Mean, log.stdev = quantiles$LP3.Log10.StDev, log.skew = quantiles$LP3.Log10.Skew,
	gen.skew = quantiles$Gen.Skew, gen.skew.stdev = quantiles$SE.Gen.Skew, gen.skew.mse = quantiles$MSE.Gen.Skew,
	MSE.skewstat.EMA = quantiles$EMA.Est.MSE.Skew.w.Systematic,  MSE.skewgen = quantiles$MSE.Gen.Skew, 
      EMA.Wtd.Reg.Skew = quantiles$EMA.Wtd.Reg.Skew,
	quantiles[,21:28])
names(quantile.data)[13:20] = sub("^(.*)[.].*", "\\1", names(quantile.data)[13:20])

quantile.data2 = data.frame(stat.nums = quantiles$Station, npeaks=quantiles$Num.Peaks, nNAs = quantiles$Peaks.not.used, 
	MSE.skewstat.EMA = quantiles$EMA.Est.MSE.Skew.w.Systematic, gen.skew = quantiles$Gen.Skew,  
	MSE.skewgen = quantiles$MSE.Gen.Skew, quantiles[,21:28])
names(quantile.data2)[7:14] = sub("^(.*)[.].*", "\\1", names(quantile.data2)[7:14])

#if(write.file) write.csv(quantile.data2,paste(subsensout,"QuantData.RegSkew_",adj.subset,"_",pFQ,".reg.skew.wtd.csv",sep=""),row.names=F)
Quantiles = quantile.data
names(Quantiles)[1] <- "site_no"
Quantiles[,1] = as.numeric(as.character(Quantiles[,1]))


UseFlag.sub = UseFlag2[which(UseFlag2[,1] %in% as.numeric(quantile.data2$stat.nums)),]

Merge.sub <- merge(All.Merge, UseFlag.sub, by="site_no",all.y=T) 
Merge.sub <- merge(Merge.sub, Quantiles, by="site_no",all.y=T) 

Data = Merge.sub
if(write.file) write.table(Data,paste(subsensout,"InputData.RegSkew_",adj.subset,"_",pFQ,".reg.skew.wtd_9.10.15.csv",sep=""),sep=",",row.names=FALSE) 
 
# correlation table
options(stringsAsFactors = FALSE)

# only produce one set of correlation table per subset, using the all option for adjustment
if(pFQ == "constant"){
  #no transformations
  variables = unique(unlist(Models[,(1+(0:6)*3)]))
  variables = variables[!is.na(variables)]
  temp.data = Data[,variables]
  #reorder variables
  temp.data = temp.data[,c(which(names(temp.data) == "DA"),which(names(temp.data) == "DStor"),which(grepl("Imperv",names(temp.data))), 
	which(grepl("SLOPE",names(temp.data))),which(grepl("DEM",names(temp.data))), which(grepl("Wet",names(temp.data))), 
	which(grepl("NWI",names(temp.data))),which(grepl("Frac",names(temp.data))))]
  temp.data = temp.data[,names(temp.data) != "Imperv_Theo_Est4.FracUrban.1"]
  cor.table = cor(temp.data, use = "pairwise.complete.obs")
  if(write.file) write.csv(cor.table, paste(subsensout,"Correlation.Table_noT_",subset,"_reg.skew.csv",sep=""))
  library(corrplot)
  if(write.file){
    pdf(paste(subsensout,"Correlation.Plot_noT_",subset,"_reg.skew.pdf",sep=""))
    corrplot(cor.table,method="ellipse",diag=F, tl.cex=.7, title = paste('Correlation Plot Subset noT "',subset,'"',sep=''), mar=c(1,0,1,0))
    dev.off()
  }
  
  #with transformations
  var.addt = Models[t(sapply(variables, function(x) which(Models == x, arr.ind=T)[1,])) + matrix(rep(c(0,1), each=length(variables)),ncol=2)]
  var.expt = Models[t(sapply(variables, function(x) which(Models == x, arr.ind=T)[1,])) + matrix(rep(c(0,2), each=length(variables)),ncol=2)]
  transinfo = cbind(variables, var.addt, var.expt)

  imperv = c("Imperv_21_22_23_24","Imperv_22_23_24","Imperv_23_24","Imperv_24","HYDRO_Imperv","Imperv_Theo_Est4.FracUrban")
  real.imperv = transinfo[(transinfo[,1] %in% imperv),]; real.imperv[,2] = 0; real.imperv[,3] = 1
  sqrt.imperv = transinfo[(transinfo[,1] %in% imperv),]; sqrt.imperv[,2] = 0; sqrt.imperv[,3] = .5
  transinfo = rbind(real.imperv = transinfo[(transinfo[,1] %in% imperv),], real.imperv, sqrt.imperv, real.imperv = transinfo[(!transinfo[,1] %in% imperv),])
  #reorder (1) DA (2) Imperv (3) Slope (4) Wetland
  transinfo = transinfo[c(which(transinfo[,1] == "DA"),which(transinfo[,1] == "DStor"),1:18, which(grepl("SLOPE",transinfo[,1])), 
	which(grepl("DEM",transinfo[,1])), which(grepl("Wet",transinfo[,1])), which(grepl("NWI",transinfo[,1])),
	which(grepl("FracCrop",transinfo[,1])), which(grepl("FracForest",transinfo[,1]))),]


  transinfo = data.frame(transinfo)
  transinfo[,2] = as.numeric(transinfo[,2]) ; transinfo[,3] = as.numeric(transinfo[,3])
  temp.data = Data[,as.character(transinfo[,1])]
  #rename imperv vars
  names(temp.data)[3:20] = as.vector(outer(imperv, c("log10","real","sqrt"), paste, sep="."))
  for(x in 1:nrow(transinfo)){
    if(transinfo[x,3] == 0) temp.data[,x] = log10(temp.data[,x] + transinfo[x,2]) else
    temp.data[,x] = (temp.data[,x] + transinfo[x,2])^transinfo[x,3]
  }
  #correlation
  cor.table = cor(temp.data, use = "pairwise.complete.obs")
  if(write.file) write.csv(cor.table, paste(subsensout,"Correlation.Table_",subset,"_reg.skew.csv",sep=""))
  library(corrplot)
  if(write.file){
    pdf(paste(subsensout,"Correlation.Plot_",subset,"_reg.skew.pdf",sep=""))
    corrplot(cor.table,method="ellipse",diag=F, tl.cex=.5, title = paste('Correlation Plot Subset "',subset,'"',sep=''), mar=c(1,0,1,0))
    dev.off()
  }
}


suppressMessages(attach(Data))
CSV_Output = data.frame(NULL)
q=dim(Models)[2]/3
quants = c("logQ2", "logQ5", "logQ10", "logQ25", "logQ50", "logQ100", "logQ200", "logQ500")
col.adj = c(1,-7,-14,-21,-28,-35,-42)

ptm = proc.time()
CSV_Output = matrix(0, nrow=length(Models[,1])*length(quants), ncol = 55)

len = length(Models[,1])
pb <- winProgressBar(title = "progress bar", min = 0,max = len, width = 300)

for (i in 1:length(Models[,1])){
	n=length(which(is.na(Models[i,])==FALSE))/3 #Determines the number of Variables
	for (j in 1:n){
		assign(paste("Variable",j,sep=""),get(as.character(Models[i,3*j-2]))+Models[i,3*j-1])
		if (Models[i,3*j]==0){
			assign(paste("Variable",j,sep=""),log10(get(paste("Variable",j,sep=""))))
		}else{
			assign(paste("Variable",j,sep=""),get(paste("Variable",j,sep=""))^(Models[i,3*j]))
		}
	}
      for (k in quants){
         qrow = which(k == quants)
         vars = lapply(1:7, function(var) paste("Variable",1:var,sep=""))
         formula = as.formula(paste(k,paste(vars[[n]],collapse="+"),sep="~"))
         Model = lm(formula, data=Data, weights=(npeaks/mean(npeaks)))
         if(n==1) stats = c(Models[i,1],Models[i,2],Models[i,3],Model$coefficients[2],summary(Model)$coefficients[2,2],summary(Model)$coefficients[1,4]) else 
	   stats =  as.vector(sapply(1:n, function(x) c(Models[i,(3*(x-1)+1)],Models[i,(3*(x-1)+2)],Models[i,(3*(x-1)+3)],Model$coefficients[(x+1)],
	    		summary(Model)$coefficients[(x+1),2],summary(Model)$coefficients[(x+1),4],vif(Model)[x])))
         CSV_Output[(8*(i-1)+qrow),] = c(summary(Model)$r.squared,summary(Model)$adj.r.squared,summary(Model)$sigma,
		 Model$coefficients[1],summary(Model)$coefficients[1,2],summary(Model)$coefficients[1,4],stats,rep(NA,7+5*q+col.adj[n]))
     }
     Sys.sleep(0.1); setWinProgressBar(pb, i, title=paste( round(i/len*100, 0),"% done"))
}
close(pb)
(time2 = proc.time() - ptm)


VariableName1=VariableName2=VariableName3=VariableName4=VariableName5=VariableName6=VariableName7=NULL
for (i in 1:length(Models[,1])){
	VariableName1 = c(VariableName1,rep(as.character(Models[i,1]),8))
	VariableName2 = c(VariableName2,rep(as.character(Models[i,4]),8))
	VariableName3 = c(VariableName3,rep(as.character(Models[i,7]),8))
	VariableName4 = c(VariableName4,rep(as.character(Models[i,10]),8))
	VariableName5 = c(VariableName5,rep(as.character(Models[i,13]),8))
	VariableName6 = c(VariableName6,rep(as.character(Models[i,16]),8))
	VariableName7 = c(VariableName7,rep(as.character(Models[i,19]),8))
}

j=1
for (i in seq(7,37,by=7)){
	CSV_Output[,i]=get(paste("VariableName",j,sep=""))
	j=j+1
}

ColumnNames = c("Quantile","R^2","AdjR^2","Sigma","InterceptEst","InterceptSE","InterceptSig",
			rep(c("Variable","VariableAddTran","VariableExpTrans","Estimate","EstimateSE","Significance","VIF"),7))
CSV_Output = cbind(rep(c("Q2","Q5","Q10","Q25","Q50","Q100","Q200","Q500"),dim(Models)[1]),CSV_Output)		
colnames(CSV_Output) = ColumnNames
if(write.file) write.table(CSV_Output,paste(subsensout,"Output_Final.New.RegSkew_QR.",adj.subset,"_",pFQ,".reg.skew.wtd.csv",sep=""),sep=",",col.names=ColumnNames,row.names=FALSE)

#} #end pFQ loop



# 3/16/2015: Checking for DEM transformations
temp = All.Merge[,c("DEM_1_0","DEM_.999_.001","DEM_.99_.01",
	"DEM_.9_.1","DEM_.75_.25","DA")]
temp.DA = temp; temp.DA$DA = log10(temp.DA$DA)
temp.all = log10(temp)
temp.noT = merge(All.Merge[,c("site_no")],DEM_noT,by=1)[,-1]
temp.noT.sqrt = temp.noT; temp.noT.sqrt$DA = sqrt(temp.noT.sqrt$DA)
temp.noT.log = temp.noT; temp.noT.log$DA = log10(temp.noT.log$DA)
pdf(file.choose(new=T))
pairs(temp, main="No Trans")
pairs(temp.DA, main="log10(DA)")
pairs(temp.all, main="log10(all)")
pairs(temp.noT, main="DEM before /sqrt(DA)")
pairs(temp.noT.sqrt, main="DEM before /sqrt(DA), sqrt(DA)")
pairs(temp.noT.log, main="DEM before /sqrt(DA), log10(DA)")
dev.off()
