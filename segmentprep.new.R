############################################################################################################################
# Segment Definition: Phase II
# 6/6/2014 - Riki Saito
#This program will create new segment definitions based on the criteria of 
# cumulative change in dam/basinDA and storage.
#The criteria is as following as of now: segment if change(damDA/basinDA) >.1 
# or change(storage (in)) > .4
#############################################################################################################################

#----------- FLAGS ----------------
write.file <- F
MUKWONGA <- T
#----------------------------------'=

versions = array(c("FINAL","FINAL_w.MUK"),c(2,1))
version = versions[(MUKWONGA+1)]
ver = gsub("FINAL","",version)


if (Sys.getenv("COMPUTERNAME") == "IGSASCEWWSFEYNM")
  setwd("T:/") else 
  setwd("//IGSASCEWFSHERCU/ILWSC_Data/SW_INV/UrbanHydrology/PhaseII_ICT/FlowData//")

source("Saito.Segments/xlsxToR.R")
stationpath="StationSelection/"
segpath="DefiningSegments/"

#------------- CALL RELEVANT DATA -------------------------------
# Files Used:
# "dam_statistics-phaseII.TMOedits-corrected2.newSAITOedit.xlsx"
# "Combined_info_143_gages-TMOedits-corrected2.new.SAITOedit.xlsx"
# "dam_info_by_station(table8)-phaseI_withnotes&addiyions_from_phase2_investigations.new.SAITOedit.xlsx"
# "ILINWI_prelim_peak_stations.csv"

file.name="Saito/dam_statistics-phaseII.TMOedits-corrected2.new.SAITOedit_sheet1.csv"
DamBasicInfo=read.csv(paste(segpath,file.name,sep=""))

file.name="Saito/dam_statistics-phaseII.TMOedits-corrected2.new.SAITOedit_sheet2.csv"
DamvBasins.corrected=read.csv(paste(segpath,file.name,sep=""))
i=order(DamvBasins.corrected$NAME)
DamvBasins.corrected=DamvBasins.corrected[i,]

file.name="Saito/dam_statistics-phaseII.TMOedits-corrected2.new.SAITOedit_sheet3.csv"
DamvBasins.cfPhaseI.corr=data.frame(read.csv(paste(segpath,file.name,sep=""),skip=1))
DamvBasins.cfPhaseI.corr$DAM.SQ.MILES[which(!DamvBasins.cfPhaseI.corr$Hand.modified.dam.areas..sq.miles. %in% c(NA,'NA'))]=as.vector(na.omit(DamvBasins.cfPhaseI.corr$Hand.modified.dam.areas..sq.miles.))
#correct values in damvbasins.corrected
#DamvBasins.corrected=DamvBasins.corrected[which(!DamvBasins.cfPhaseI.corr$Hand.modified.dam.areas..sq.miles. %in% c(NA,'NA'))]
whichstat=unique(DamvBasins.cfPhaseI.corr$DAM_NAME[which(!DamvBasins.cfPhaseI.corr$Hand.modified.dam.areas..sq.miles. %in% c(NA,'NA'))])
whichvals=unique(DamvBasins.cfPhaseI.corr$Hand.modified.dam.areas..sq.miles.[which(!DamvBasins.cfPhaseI.corr$Hand.modified.dam.areas..sq.miles. %in% c(NA,'NA'))])
trim.trailing <- function (x) sub("\\s+$", "", x)
for(i in 1:length(whichstat)){
   DamvBasins.corrected$DAM.SQ.MILES[which(DamvBasins.corrected$DAM_NAME == trim.trailing(whichstat[i]))] = whichvals[i]
}

file.name="Saito/Combined_info_143_gages-TMOmod-phaseI.new_sheet1.csv"
combined_info_phaseI_sheet1=read.csv(paste(segpath,file.name,sep=""))

file.name="Saito/dam_info_by_station(table8)-phaseI_withnotes&additions_from_phase2_investigations.new.SAITOEdit.csv"
DamInfo_table8=read.csv(paste(segpath,file.name,sep=""))
i=apply(DamInfo_table8, 1, function(x) all(x %in% c(NA,"")))
DamInfo_table8=DamInfo_table8[!i,]
DamInfo_table8=DamInfo_table8[order(DamInfo_table8$station, DamInfo_table8$dam.name),]

prelim_peak=read.csv(paste(stationpath,"ILINWI_prelim_peak_stations.csv",sep=""))
i=order(prelim_peak$site_no)
prelim_peak=prelim_peak[i,]
# 7/1/2014: Fix Basin DA for station 5536207
prelim_peak[which(prelim_peak$site_no == 5536207),"drain_area_va"] = .48


#new dam info with previously missing completion date
file.name="Saito/damw.missingdates.info.csv"
damwmissingdates=read.csv(paste(segpath,file.name,sep=""))

# list with flag to keep station
file.name="ILINWI.station_list.w_useflag.csv"
keepstation=read.csv(paste(stationpath,file.name,sep=""))

if(MUKWONGA) keepstation[which(keepstation$site_no==5544200),2] = 1
#------------------------------------------------------------------------------------------------------

missmerge=merge(damwmissingdates, DamBasicInfo[,c(3,8:16,23:25)], by.x="Dam", by.y="DAM_NAME", all.x=T)
#write.csv(missmerge,paste(segpath,"Saito/dam.missdates.wcoordinates.csv",sep=""),row.names=F)


#---------------- combine tables to create all dam-station dataframe ----------------------------
dam_info_byallseg=DamvBasins.corrected[,c(5,1,4,6,7,8)]
names(dam_info_byallseg)=c("Site_No","Dam","River","Dam_SQ_Miles","Basin_SQ_Miles","Dam DA/Basin DA")
dam_info_byallseg$Dam=toupper(dam_info_byallseg$Dam)

# to obtain dam characteristics (NID2013), match-merge
DamBasicInfo$DAM_NAME=toupper(DamBasicInfo$DAM_NAME)

merged.alldams=merge(dam_info_byallseg, DamBasicInfo[c("DAM_NAME","YEAR_COMPL","YEAR_MODIF","DRAINAGE_A","MAX_STORAG","SideChanne")], by.x="Dam", by.y="DAM_NAME", all=T)
merged.alldams=merged.alldams[order(merged.alldams$Site_No, merged.alldams$Dam),]
names(merged.alldams)[7:11]=c("Year_Comp_NID2013","Year_Mod_NID2013","Dam_DA_NID2013","Max_Stor_NID2013","SideChannel")
#if column contains "Side Channel", then dam is offline
offstr=c("Side channel reervoir -- Not contructed yet?","side channel reservoir","Side channel reservoir")
merged.alldams$SideChannel=as.character(merged.alldams$SideChannel)
for(i in 1:nrow(merged.alldams)){
    if(merged.alldams$SideChannel[i] %in% offstr) merged.alldams$SideChannel[i]=1 else merged.alldams$SideChannel[i]=NA
}

  # obtain values from [DamvBasins.cfPhaseI.corr] from sheet "dam_statistics-phaseII.TMOedits-corrected$DamvBasins-cdfPhaseI-corr" (OCF)
DamvBasins.cfPhaseI.corr$DAM_NAME=toupper(DamvBasins.cfPhaseI.corr$DAM_NAME)
merged.alldams2=merge(merged.alldams, DamvBasins.cfPhaseI.corr[,c(1,4,6,10,11,14,18)], by.x=c("Dam","Site_No") , by.y=c("DAM_NAME","NAME"), all=T)
merged.alldams2=merged.alldams2[order(merged.alldams2$Site_No, merged.alldams2$Dam),]
names(merged.alldams2)[12:16]=c("Max_Stor_OCF","Year_Comp_OCF","Dam_DA_OCF","Basin_DA_OCF","phaseIDamName")

# match merge with [DamInfo_table8] from "dam_info_by_station(table8)-phaseI.." (phaseI)
merged.alldams3=merge(merged.alldams2, DamInfo_table8[,c(1:4,6:8,10)], by.x=c("phaseIDamName","Site_No") , by.y=c("dam.name","station"), all.x=T)
merged.alldams3=merged.alldams3[order(merged.alldams3$Site_No, merged.alldams3$Dam),]
names(merged.alldams3)[17:22]=c("Basin_DA_phaseI","Dam_DA_phaseI","Year_Comp_phaseI","Year_Mod_phaseI","Max_Stor_phaseI","Offline")
offline=which(merged.alldams2$SideChannel==merged.alldams2$Offline)
merged.alldams3$Offline[offline]=1;  merged.alldams3$Offline[-offline]=0
merged.alldams3=merged.alldams3[,-12]

  # add peak begin/end dates, daily record begin.end, gage type, etc. from [Combined_info_143_gages_phaseI]
merged.alldams4=merge(merged.alldams3, combined_info_phaseI_sheet1[,c(1,4,9,11,12,13,14,15)], by.x=c("Site_No") , by.y=c("Name"), all=T)
names(merged.alldams4)[22:28]=c("Basin_DA_CombInfo","gage_type","Peak_Record_Begin_CombInfo","Peak_Record_End_CombInfo",
	"Peak_Count_CombInfo","Daily_Record_Begin_CombInfo","Daily_Record_End_CombInfo")

  # merge with prelim peaks for peak begin and end dates (prelim)
merged.alldams5=merge(merged.alldams4, prelim_peak[,c(2,3,10,13,15,16,17)], by.x=c("Site_No") , by.y=c("site_no"), all=T)
names(merged.alldams5)[29:34]=c("Station_Name","County","Basin_DA_Prelim","Peak_Record_Begin_Prelim","Peak_Record_End_Prelim",
	"Peak_Count_Prelim")
merged.alldams5$Offline[which(merged.alldams5$Offline %in% c(NA,'NA'))]=0

# now merge with new dam info (dams with missing dates)
merged.alldams6=merge(merged.alldams5, damwmissingdates[,c(1,2,23,25)], by=c("Site_No","Dam"), all.x=T)


#sort and write out table
merged.alldams6=merged.alldams6[order(merged.alldams6$Site_No),]
#remove duplicated rows
merged.alldams6=merged.alldams6[-which(duplicated(merged.alldams6)),]
# stations in phase I only
phaseIstat=unique(combined_info_phaseI_sheet1$Name)[-1]
merged.alldams6$PhaseIIstatonly=as.numeric(!merged.alldams6$Site_No %in% phaseIstat)
# dams in phase I only
phaseIdam=unique(merged.alldams6$Dam[which(!merged.alldams6$phaseIDamName %in% c(""," ",NA,"NA"))])
merged.alldams6$PhaseIIdamonly=as.numeric(!merged.alldams6$Dam %in% phaseIdam)
merged.alldams6$PhaseIIdamonly[which(apply(merged.alldams6[,2:3], 1, function(row) all(row %in%  c(""," ",NA,"NA"))))]=NA
#add phaseI dam names to "Dam"
namemiss=which(merged.alldams6$Dam %in% c(NA,'NA'))
merged.alldams6$Dam[namemiss]=toupper(gsub("_"," ",merged.alldams6$phaseIDamName[namemiss]))
#drop
merged.alldams6=merged.alldams6[,-3]
#add gage type
crest=read.csv("DefiningSegments/Saito/gagetype_phaseII.csv")
source("Saito.Segments/date.convert.R")
array=matrix(0,nrow=6,ncol=3); colnames(array)=c("bf1980","af1980","csg")
array[,1]=c(1,0,0,1,0,0); array[,2]=c(0,1,0,0,1,0); array[,3]=c(1,1,1,0,0,0)
for(i in crest$site_no){
 stat=which(merged.alldams6$Site_No==i)
 bf_80=as.numeric(date.convert.WY(as.character(merged.alldams6$Peak_Record_End_Prelim[stat[1]]))<1981)
 af_80=as.numeric(date.convert.WY(as.character(merged.alldams6$Peak_Record_Begin_Prelim[stat[1]]))>1980)
 csg=crest$CSG[which(crest$site_no==i)]
 type=which(duplicated(rbind(array,c(bf_80,af_80,csg)),fromLast=T))
 merged.alldams6$gage_type[stat]=type
}

#fix dam information
merged.alldams6=merged.alldams6[order(merged.alldams6$Dam),]
rep.row<-function(x,n) matrix(rep(x,each=n),nrow=n); counter=0
for(p in unique(merged.alldams6$Dam)[-c(1,length(unique(merged.alldams6$Dam)))]){
   counter=counter+1
   mat=subset(merged.alldams6, merged.alldams6$Dam==p)
   replace=mat[,c(4,7:13,16:19)]
   new=rep.row(suppressWarnings(apply(replace,2,function(col) max(col,na.rm=T))),nrow(mat))
   merged.alldams6[which(merged.alldams6$Dam==p),c(4,7:13,16:19)]= new
}
merged.alldams6=merged.alldams6[order(merged.alldams6$Site_No),]

#remove stations not used
usestat=keepstation$site_no[which(keepstation$use.1==1)]
nousestat=keepstation$site_no[which(keepstation$use.1==0)]
merged.alldams6=merged.alldams6[which(merged.alldams6$Site_No %in% usestat),]

#remove dams not relevant
removedam=c("CORNELL AVENUE DAM","LAKE OF THE FOUR SEASONS (DAM A)","LAKE OF THE FOUR SEASONS (DAM B)",
	"LAKE OF THE FOUR SEASONS (LOWER) C")
merged.alldams6=merged.alldams6[which(!merged.alldams6$Dam %in% removedam),]

#reorder, using only relevant columns
merged.alldams6=merged.alldams6[,c(1:3,28:29,4:6,13:14,16,15,11,19,10,12,17:18,7:8,20,22:27,30:37)]
merged.alldams6=merged.alldams6[order(merged.alldams6$Site_No),]
#ind=which(rownames(merged.alldams6)=="NA"); merged.alldams6=merged.alldams6[-c(ind:nrow(merged.alldams6)),]

outputdam=merged.alldams6[,c(1:15,33,16:20,32,21:31,34:35)]

if(write.file) write.csv(outputdam, paste("Saito.Segments/merged.alldams.",version,".csv",sep=""), row.names=F)

#remove dams not used in segmentation
removedam=c("SKOKIE LAGOONS DREDGE DISPOSAL DAM")
merged.alldams6=merged.alldams6[which(!merged.alldams6$Dam %in% removedam),]

#Ultimately, you want your data took look like 'merged.alldams6' for the rest of the progam to run.
#-------------------------------------------------------------------------------------


#-------- new output reduced ----------------------------------------------
ocfY=which(colnames(outputdam)=="Year_Comp_OCF")
pIY=which(colnames(outputdam)=="Year_Comp_phaseI")
NIDY=which(colnames(outputdam)=="Year_Comp_NID2013")
modY=which(colnames(outputdam)=="Year_Hand_Modified")
outputdam[,c(ocfY,pIY,NIDY)]=replace(as.matrix(outputdam[,c(ocfY,pIY,NIDY)]), as.matrix(outputdam[,c(ocfY,pIY,NIDY)]) %in% c(1,NA,'NA'),0)

allcompdate=rep(0,nrow(outputdam))
for(k in 1:nrow(outputdam)){
   if(!outputdam$Year_Hand_Modified[k] %in% c(NA,'NA')) allcompdate[k] = as.numeric(outputdam$Year_Hand_Modified[k]) else
   if(as.numeric(outputdam$Year_Comp_OCF[k])>0 & nchar(outputdam$Year_Comp_OCF[k])==4) allcompdate[k]=as.numeric(outputdam$Year_Comp_OCF[k]) else
   if(as.numeric(outputdam$Year_Comp_phaseI[k])>0) allcompdate[k]=as.numeric(outputdam$Year_Comp_phaseI[k]) else
   if(as.numeric(outputdam$Year_Comp_NID2013[k])>0) allcompdate[k]=as.numeric(outputdam$Year_Comp_NID2013[k])
}

whichDA=apply(outputdam[,c("Dam_SQ_Miles","Dam_DA_OCF","Dam_DA_phaseI")], 1, function(row) min(which(!row %in% c(0,NA,'NA'))))
whichstor=apply(outputdam[,c("Max_Stor_OCF","Max_Stor_phaseI","Max_Stor_NID2013")], 1, function(row) min(which(!row %in% c(0,NA,'NA'))))
names(whichDA)=names(whichstor)=seq(1:length(whichDA))
outputdam[which(!is.na(outputdam$Dam) & is.na(outputdam$Dam_SQ_Miles)),"Dam_SQ_Miles"] = 0

newoutputdam=outputdam[,c(1:5,31:32,6:7,34:35,23)]
newoutputdam$Max_Stor=0; ind=whichstor[which(whichstor!=Inf)]
newoutputdam$Max_Stor[which(whichstor!=Inf)]=outputdam[,c("Max_Stor_OCF","Max_Stor_phaseI","Max_Stor_NID2013")][cbind(as.numeric(names(ind)),ind)]
newoutputdam$Year_Compl=allcompdate; newoutputdam$Cum_Dam_DA=0;  newoutputdam$Cum_Max_Stor=0; 
#sort by station number and year completed
newoutputdam=newoutputdam[order(newoutputdam$Site_No,newoutputdam$Year_Compl),]

for(i in unique(newoutputdam$Site_No)){
   newoutputdam$Cum_Dam_DA[which(newoutputdam$Site_No==i)]=cumsum(as.numeric(newoutputdam$Dam_SQ_Miles[which(newoutputdam$Site_No==i)]))
   newoutputdam$Cum_Max_Stor[which(newoutputdam$Site_No==i)]=cumsum(as.numeric(newoutputdam$Max_Stor[which(newoutputdam$Site_No==i)]))
}

newoutputdam$Cum_Dam.Basin_DA = newoutputdam$Cum_Dam_DA/newoutputdam$Basin_SQ_Miles
newoutputdam$Cum_Max.Stor.In = (newoutputdam$Cum_Max_Stor/newoutputdam$Basin_SQ_Miles)*(43560*12/(5280^2))

OUTputdam = newoutputdam[,c(1:9,15,17,13,16,18,14,10:11,12)]
newoutputdam=newoutputdam[,c(1:9,15,17,13,16,18,14,10:11)]
outputdam = newoutputdam

# add empty rows between stations
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

for(i in unique(newoutputdam$Site_No)[-1]){
  newoutputdam=insertRow(newoutputdam, rep(NA,ncol(newoutputdam)), min(which(newoutputdam$Site_No==i)))
}
names(newoutputdam)=c("Site No","Dam","River","Station Name","County","Peak POR Begin","Peak POR End","Dam DA","Basin DA","Cumulative Dam DA",
	"Dam/Basin DA","Max Storage","Cum Max Storage (ac-ft)","Cum Max Storate (in)","Year Completed","PhaseII Station","PhaseII Dam")

newoutputdam[which(duplicated(newoutputdam[,1]) & !is.na(newoutputdam[,1])),c(1,4:7,9,16)] = NA
if(write.file) write.csv(newoutputdam, paste("Saito.Segments/allstation-dams.phaseII.",version,".csv",sep=""), row.names=F)
#----------------------------------------------------------------------------------------


#-------- check for dams duplicated in some station -----------------
  #sort by dam name
dupdams=NULL
merged.alldams6=merged.alldams6[order(merged.alldams5$Dam),]
for(g in unique(merged.alldams6$Dam)){
  mat=subset(merged.alldams6, merged.alldams6$Dam==g)
  if(nrow(mat)>1){
	if(length(unique(mat$Site_No))!= length(mat$Site_No)) dupdams=c(dupdams,g)
  }
}
dupdams; dupdams=dupdams[which(dupdams!="")]
merged.alldams6[which(duplicated(merged.alldams6[,1:2]) & merged.alldams6$Dam %in% dupdams),1:2]
#--------------------------------------------------------------------------


#----------------- dam data with missing completion dates ---------------------------------
ocfY=which(colnames(merged.alldams6)=="Year_Comp_OCF")
pIY=which(colnames(merged.alldams6)=="Year_Comp_phaseI")
NIDY=which(colnames(merged.alldams6)=="Year_Comp_NID2013")
modY=which(colnames(merged.alldams6)=="Year_Hand_Modified")

dams.only=subset(merged.alldams6, !is.na(merged.alldams6$Dam))
dams.only=merge(dams.only, missmerge[!duplicated(missmerge$Dam),c(1,4,5)], by="Dam", all.x=T)

#dams for which compl date is missing for both NID2013 and phaseI data
missingdams=NULL
for(i in unique(dams.only$Dam)){
   mat=subset(dams.only, dams.only$Dam==i)
   datemiss=all( as.matrix(mat[,c(pIY,NIDY)]) %in% c(0,"0",1,NA,"NA")) & all(nchar(as.character(mat[,ocfY]))!=4)
   if(datemiss) missingdams=c(missingdams, i)
}

# some are hand-modified
modified=unique(merged.alldams6$Dam[which(!merged.alldams6$Year_Hand_Modified %in% c(""," ",NA,"NA"))])
missingdams=missingdams[which(!missingdams %in% modified)]

dam.datemiss=subset(dams.only, dams.only$Dam %in% missingdams)
#find number of valid (including ambiguous) dates in each row
dam.datemiss$ndates=rep(0,nrow(dam.datemiss))
for(i in 1:nrow(dam.datemiss)){
  dam.datemiss$ndates[i]=sum(!dam.datemiss[i,c(pIY,NIDY,modY)] %in% c(0,1,NA,'NA'),nchar(as.character(dam.datemiss[i,ocfY]))>=4)
}
#for each dam index row with most valid dates
ind=NULL
for(i in unique(dam.datemiss$Dam)){
   ind=c(ind,which(dam.datemiss$Dam==i &  dam.datemiss$ndates==max(dam.datemiss$ndates[which(dam.datemiss$Dam==i)])))
}
dam.datemiss=dam.datemiss[ind,]
if(write.file) write.csv(dam.datemiss[,c(1,2,3,6,16:20,13:15,21,22:37)], paste("Saito.Segments/dams.w.missingdate.",version,".csv",sep=""), row.names=F)
#-----------------------------------------------------------------------------------------



#-------------------- condense to a by-station table -----------------------------
colnames=c("Station","Name","County","Basin DA","Gage Type","Peak Record Begin Date","Peak Record End Date","Peak Count","Peak Record Begin WY",
	"Peak Record End WY","Year of first dam built during POR","Year of last dam built during POR","Dam DA for unknown completion date",
      "Dam DA built after POR","Dam DA at POR begin WY","Dam DA at POR end WY","Dam/Basin DA at POR begin WY","Dam/Basin DA at POR end WY","Change in Dam/Basin DA over POR",
	"Dam max storage at POR begin WY","Dam max storage at POR end WY","Change in Dam max stor (ac-ft)",
	"Change in Dam max stor (in)","Segmentation Needed")
bystation=matrix(0,nrow=length(unique(merged.alldams6$Site_No)),ncol=length(colnames))
colnames(bystation)=colnames

ocfY=which(colnames(merged.alldams6)=="Year_Comp_OCF")
pIY=which(colnames(merged.alldams6)=="Year_Comp_phaseI")
NIDY=which(colnames(merged.alldams6)=="Year_Comp_NID2013")
modY=which(colnames(merged.alldams6)=="Year_Hand_Modified")

#replace unknown dam completion years with 0
alldams=merged.alldams6
alldams[,c(ocfY,pIY,NIDY)]=replace(as.matrix(alldams[,c(ocfY,pIY,NIDY)]), as.matrix(alldams[,c(ocfY,pIY,NIDY)]) %in% c(1,NA,'NA'),0)
alldams=alldams[order(alldams$Site_No),]

isEmpty <- function(x) return(length(x)==0)
sites=unique(alldams$Site_No)[-195]; sites=sites[order(sites)]; iter=1; warn=0
for(j in na.omit(sites)){
   suppressWarnings(remove(allDA))
   mat=subset(alldams, alldams$Site_No==j)
  #using prelim peak start dates. NOTE: several different date formats exist in data
   #POR must be in between 1940 and 2009
   absmin=paste("10","1","1939",sep="/");absmax=paste("9","30","2009",sep="/")
   begindate<-max(date.convert(as.character(mat[1,]$Peak_Record_Begin_Prelim)),date.convert(absmin))
   enddate<-min(date.convert(as.character(mat[1,]$Peak_Record_End_Prelim)),date.convert(absmax))
   if(nchar(begindate)==4) WY=as.numeric(begindate) else WY=date.convert.WY(format(as.Date(begindate), format="%m/%d/%Y"))
   if(nchar(enddate)==4) WYend=as.numeric(enddate) else WYend=date.convert.WY(format(as.Date(enddate), format="%m/%d/%Y"))
  #find Dam DA at beginning of wateryear
  #DA at beginning of peak record is 0 unless dam was built before record begin date
  #order of priority for DA, storage, YEAR_COMPL: 1) OCF 2) phaseI 3) NID2013
   beginDA=beginmaxstor=0 #for dams with missing dates, assume smallest beginning DA and storage
  #dam completion date
   compdate=rep(0,nrow(mat))
   for(k in 1:nrow(mat)){
	if(!mat$Year_Hand_Modified[k] %in% c(NA,'NA')) compdate[k] = as.numeric(mat$Year_Hand_Modified[k]) else
      if(as.numeric(mat$Year_Comp_OCF[k])>0 & nchar(mat$Year_Comp_OCF[k])==4) compdate[k]=as.numeric(mat$Year_Comp_OCF[k]) else
      if(as.numeric(mat$Year_Comp_phaseI[k])>0) compdate[k]=as.numeric(mat$Year_Comp_phaseI[k]) else
      if(as.numeric(mat$Year_Comp_NID2013[k])>0) compdate[k]=as.numeric(mat$Year_Comp_NID2013[k])
   }

   if(any(!mat$Dam %in% c(0,'0',NA,'NA'))){
	if(any(compdate<=WYend & compdate>=WY)) lastdam=max(compdate[which(compdate<=WYend & compdate>=WY)], na.rm=T) else lastdam=NA
      if(all(compdate>WY)){
	   firstdam=min(as.numeric(compdate),na.rm=T)
	} else{
	   l=which(as.numeric(compdate)<=WY & !duplicated(mat$Dam))
         if(any(as.numeric(compdate)>=WY)) firstdam=min(compdate[which(as.numeric(compdate)>=WY)],na.rm=T) else firstdam=NA
	   #find best dam DA before completed before beginning WY
  	   #order of priority: 0) corrected (DA) 1) OCF 2) phaseI 3) NID2013
         whichDA=whichstor=rep(0,length(l))
	   DA=mat[l,c("Dam_SQ_Miles","Dam_DA_OCF","Dam_DA_phaseI")]
	   #do the same for storage
	   stor=mat[l,c("Max_Stor_OCF","Max_Stor_phaseI","Max_Stor_NID2013")]
         #whichstor=apply(stor, 1, function(row) if(length(which(!row %in% c(0,NA,'NA')))>0) min(which(!row %in% c(0,NA,'NA'))))
	   for(n in 1:length(l)){
 	     if(length(which(!DA[n,] %in% c(0,NA,'NA')))>0) whichDA[n]=min(which(!DA[n,] %in% c(0,NA,'NA'))) else whichDA[n]=4
	     if(length(which(!stor[n,] %in% c(0,NA,'NA')))>0) whichstor[n]=min(which(!stor[n,] %in% c(0,NA,'NA'))) else whichstor[n]=4
	   }
	   DA[,4]=stor[,4]=rep(0,length(l))
	   #sum DA and storage
	   for(m in 1:length(l)) {
		  beginDA=sum(beginDA,as.numeric(DA[m,whichDA[m]]))
		  beginmaxstor=sum(beginmaxstor,as.numeric(stor[m,whichstor[m]]))
	   }
	}
      endDA=endmaxstor=0; whichallDA=whichallstor=rep(0,nrow(mat))
      allDA=mat[,c("Dam_SQ_Miles","Dam_DA_OCF","Dam_DA_phaseI")]; allDA$noDA=rep(0,nrow(allDA))
      allstor=mat[,c("Max_Stor_OCF","Max_Stor_phaseI","Max_Stor_NID2013")]; allstor$nostor=rep(0,nrow(allstor))

	ordercompdate=compdate[order(compdate)]
 	orderDA=cbind(allDA[order(compdate),],ordercompdate)
 	orderstor=cbind(allstor[order(compdate),],ordercompdate)

      for(n in 1:nrow(mat)){
 	   #if(length(which(!allDA[n,-4] %in% c(0,NA,'NA')))>0) whichallDA[n]=min(which(!allDA[n,-4] %in% c(0,NA,'NA'))) else whichallDA[n]=4
 	   if(length(which(!orderDA[n,-4] %in% c(0,NA,'NA')))>0) whichallDA[n]=min(which(!orderDA[n,-4] %in% c(0,NA,'NA'))) else whichallDA[n]=4
	   #if(length(which(!allstor[n,-4] %in% c(0,NA,'NA')))>0) whichallstor[n]=min(which(!allstor[n,-4] %in% c(0,NA,'NA'))) else whichallstor[n]=4
	   if(length(which(!orderstor[n,-4] %in% c(0,NA,'NA')))>0) whichallstor[n]=min(which(!orderstor[n,-4] %in% c(0,NA,'NA'))) else whichallstor[n]=4
	}
	#for DA: sum the dam DA build before WYend and are not duplicated (to prevent double-counting of modified dams)
	whichendDA=which(ordercompdate<=WYend & !duplicated(mat$Dam[order(compdate)])); 
	whichendstor=which(ordercompdate<=WYend)
	endDA=sum(as.numeric(orderDA[cbind(whichendDA,whichallDA[whichendDA])]))

	endmaxstor=sum(as.numeric(orderstor[cbind(whichendstor,whichallstor[whichendstor])]))
   }else{
      firstdam=lastdam=segmentation=NA
	beginDA=beginmaxstor=endDA=endmaxstor=dam.basin.ratio.beg=dam.basin.ratio.end=BasinDA=change.da=change.maxstor.in=0
   }
  #compute and assign other attributes
   BasinDA=as.numeric(mat$Basin_SQ_Miles[1])
   if(BasinDA %in% c(NA,'NA')) BasinDA=as.numeric(mat$Basin_DA_Prelim[1]);   
   isEmpty <- function(x) return(length(x)==0); if(isEmpty(BasinDA)) BasinDA=NA
   dam.basin.ratio.beg=beginDA/BasinDA
   dam.basin.ratio.end=endDA/BasinDA
   change.da=dam.basin.ratio.end-dam.basin.ratio.beg
   change.maxstor=endmaxstor-beginmaxstor
   change.maxstor.in=(change.maxstor/BasinDA)*43560*12/(5280^2)
   segmentation=(change.da>.1 & change.maxstor.in>.4)     ### 7/1/2014 CRITERIA CHANGED
   name=as.character(mat[1,"Station_Name"])
   county=as.character(mat[1,"County"])
   gagetype=mat[1,"gage_type"]
   # 6/5/2014 added
   if(exists("allDA")){
     x=cbind(seq(1:nrow(allDA)),whichallDA)
     damDAunknownyr=sum(as.numeric(allDA[x][which(compdate==0)]))
     damDAafterPOR=sum(as.numeric(allDA[x][which(compdate>enddate)]))
   } else damDAunknownyr=damDAafterPOR=NA

   bystation[iter,]=c(j,name,county,BasinDA,gagetype,begindate,enddate,mat[1,"Peak_Count_Prelim"],WY,WYend,firstdam,lastdam,damDAunknownyr,
      damDAafterPOR,beginDA,endDA,dam.basin.ratio.beg,dam.basin.ratio.end,change.da,beginmaxstor,endmaxstor,change.maxstor,change.maxstor.in,
	as.numeric(segmentation))

   iter=iter+1
}

if(write.file) write.csv(bystation[-nrow(bystation),],paste("Saito.Segments/combined.info.bystation.phaseII.",version,".csv",sep=""),row.names=F)


#----------- stations that need segmentation ----------------------
SEGstations=sites[which(!bystation[-nrow(bystation),"Segmentation Needed"] %in% c(0,NA,'NA'))]

#7/1/2014: update on stations that do not need segments
#suggested from "Saito.Segments/Compare.info.phaseI.vs.phaseII.TMOComments.xlsx"

remseg.stations <- c(5528000,5530000,5530990,5540110,5550500)
addseg.stations <- c(5528230,5535150,5536235,5536275,5536570,5539890,5539900,5545750,5550450)
orig.criteria <- c(5528230,5528500,5531300,5531500,5535150,5535800,5536000,5536235,5536275,5536570,5539000,5539890,5540060,5550450)

segstations <- unique(c(SEGstations[which(!SEGstations %in% remseg.stations)],addseg.stations))
segstations <- segstations[order(segstations)]

length(segstations)
nphaseIsegs=sum(segstations %in% phaseIstat)
#--------------------------------------------------------------------


#----------------- autosegmentation ------------------------------
start=1; iter=1; seginfo=matrix(0,nrow=300,ncol=ncol(bystation)-1); newsegcounter=0; whichnewseg=NULL
substrRight <- function(x, n) substr(x, nchar(x)-n+1, nchar(x))
invwhich <- function(indices, totlength) is.element(seq_len(totlength), indices)
for (i in segstations){
   mat=subset(alldams, alldams$Site_No==i)
   WY=date.convert.WY(as.vector(mat[1,]$Peak_Record_Begin_Prelim))
   WYend=min(date.convert.WY(as.vector(mat[1,]$Peak_Record_End_Prelim)),2009)
   #find best dam completion year
   compdate=rep(0,nrow(mat))
   for(k in 1:nrow(mat)){
	if(!mat$Year_Hand_Modified[k] %in% c(NA,'NA')) compdate[k] = as.numeric(mat$Year_Hand_Modified[k]) else
      if(mat$Year_Comp_OCF[k]>0 & nchar(mat$Year_Comp_OCF[k])==4) compdate[k]=mat$Year_Comp_OCF[k] else
      if(mat$Year_Comp_phaseI[k]>0) compdate[k]=mat$Year_Comp_phaseI[k] else
      if(mat$Year_Comp_NID2013[k]>0) compdate[k]=mat$Year_Comp_NID2013[k]
   }
   compdate=as.numeric(compdate)
   #index of dams that were built after WY
   PORdam=which(as.numeric(compdate)>WY)

   #retrieve dam DA and max storage
   PORDA=cbind(mat[PORdam,c("Dam_SQ_Miles","Dam_DA_OCF","Dam_DA_phaseI")],rep(0,length(PORdam)))
   PORstor=cbind(mat[PORdam,c("Max_Stor_OCF","Max_Stor_phaseI","Max_Stor_NID2013")],rep(0,length(PORdam)))
   PORvalidDA=PORvalidstor=matrix(0,nrow=length(PORdam)+1,ncol=8)

   ind=which(bystation[,"Station"]==i)
   beginDA=as.numeric(bystation[ind,"Dam DA at POR begin WY"])
   basinDA=as.numeric(bystation[ind,"Basin DA"])
   beginstor=as.numeric(bystation[ind,"Dam max storage at POR begin WY"])
   PORvalidDA[1,]=c(beginDA,0,WY,0,0,0,0,0); PORvalidstor[1,]=c(beginstor,0,WY,0,0,0,0,0)
   colnames(PORvalidDA)=c("DamDA","CumDamDA","BreakYear","BreakEndYear","Dam.BasinRatio","ChangeRatio",
	"YearsAfterOldBreak","YearsBeforeNewBreak")
   colnames(PORvalidstor)=c("Storage","CumStorage","BreakYear","BreakEndYear","StorDif(ac-ft)",
	"StorDif(in)","YearsAfterOldBreak","YearsBeforeNewBreak")

   #find best DA and storage values notice: if all missing, use 0.
   whichDA=whichstor=rep(0,length(PORdam))
   for(n in 1:length(PORdam)){
 	 whichDA[n]=min(which(!PORDA[n,] %in% c(0,NA,'NA'))); if(whichDA[n]==Inf) whichDA[n]=4
	 whichstor[n]=min(which(!PORstor[n,] %in% c(0,NA,'NA'))); if(whichstor[n]==Inf) whichstor[n]=4
	 PORvalidDA[n+1,1]=PORDA[n,whichDA[n]]; PORvalidstor[n+1,1]=PORstor[n,whichstor[n]]; 
   }

   PORvalidDA=data.matrix(PORvalidDA)
   PORvalidDA[-1,3]=PORvalidstor[-1,3]=as.numeric(compdate[PORdam])
   #reorder by year
   PORvalidDA=PORvalidDA[order(PORvalidDA[,3]),]; PORvalidstor=PORvalidstor[order(PORvalidstor[,3]),]
   #4th column is year next break
   PORvalidDA[,4]=PORvalidstor[,4]= c(as.numeric(compdate[PORdam]),as.numeric(WYend))[order(c(as.numeric(compdate[PORdam]),as.numeric(WYend)))]
   #add column for cumulative DA and storage 
   PORvalidDA[,2]=cumsum(PORvalidDA[,1]); PORvalidstor[,2]=cumsum(PORvalidstor[,1])
   #calculate dam DA/basin DA ratio
   PORvalidDA[,5]=as.numeric(PORvalidDA[,2])/basinDA; 

   #some dams during same year, keep only last one (cumulative of whole year), treat as one break
   damDAbyyear=aggregate(as.numeric(PORvalidDA[,1]), by=list(PORvalidDA[,3]), FUN=sum)
   storbyyear=aggregate(as.numeric(PORvalidstor[,1]), by=list(PORvalidstor[,3]), FUN=sum)
   PORvalidDA=PORvalidDA[which(!duplicated(PORvalidDA[,3],fromLast=T)),]
   PORvalidstor=PORvalidstor[which(!duplicated(PORvalidstor[,3],fromLast=T)),]
   PORvalidDA[,1]=damDAbyyear$x;   PORvalidstor[,1]=storbyyear$x

  #break at 1940 if POR goes further back
  bf1940=as.numeric(PORvalidDA[,4])>1940
  if(any(!bf1940)){
 	which.validyr=min(which(bf1940))
      PORvalidDA[which.validyr,1]=PORvalidDA[which.validyr,2]; PORvalidDA=PORvalidDA[-c(1:(which.validyr-1)),]
      PORvalidstor[which.validyr,1]=PORvalidstor[which.validyr,2]; PORvalidstor=PORvalidstor[-c(1:(which.validyr-1)),]
      PORvalidDA[1,3]=PORvalidDA[1,3]=max(PORvalidDA[1,3],1940)
  }

  nbreak=nrow(PORvalidDA)-1
  for(j in 1:nbreak){
     #years after old break
     if(j == 1) PORvalidDA[j,7]=PORvalidstor[j,7]=as.numeric(PORvalidDA[j,3])-as.numeric(WY) 
     PORvalidDA[j+1,7]=PORvalidstor[j+1,7]=as.numeric(PORvalidDA[j+1,3])-as.numeric(PORvalidDA[j,3])
     #years before next break
     PORvalidDA[j,8]=PORvalidstor[j,8]=as.numeric(PORvalidDA[j+1,3])-as.numeric(PORvalidDA[j,3])
     if(j+1 == nrow(PORvalidDA)) PORvalidDA[j+1,8]=PORvalidstor[j+1,8]=as.numeric(WYend)-as.numeric(PORvalidDA[j+1,3])

     #change in DA calculated as ratio of dam/basin before vs after dam
     PORvalidDA[j+1,6]=as.numeric(PORvalidDA[j+1,5])-as.numeric(PORvalidDA[j,5])
     PORvalidstor[j+1,5]=as.numeric(PORvalidstor[j+1,2])-as.numeric(PORvalidstor[j,2])
     PORvalidstor[j+1,6]=(as.numeric(PORvalidstor[j+1,5])/basinDA)*43560*12/(5280^2)
   }

   #remove dams build after end of POR
   remove=which(as.numeric(PORvalidDA[,3])>as.numeric(WYend))
   if(length(remove)>0) {PORvalidDA=PORvalidDA[-remove,]; PORvalidstor=PORvalidstor[-remove,]}

   #restructuring
   if(length(PORvalidDA)==8) {PORvalidDA=rbind(PORvalidDA); PORvalidstor=rbind(PORvalidstor)}

   #seg breaks for dams/years with change DA >.1  AND  change storage >.4 and are at least 5 years long are segments
   ## 7/2/2014: change criteria for some stations ##
   if(i %in% orig.criteria)  seg = which((PORvalidDA[,6]>.1) | (PORvalidstor[,6]>.4)) else
	seg = which((PORvalidDA[,6]>.1) & (PORvalidstor[,6]>.4))

   if(length(seg)>0){
     #if segment truncates first or last segment <4, remove.
     #yearsbwbreak=PORvalidDA[,8]; shortseg=which(as.numeric(yearsbwbreak)<4 & invwhich(seg, length(yearsbwbreak)))
  
	yearsbwbreak=rep(0,nrow(PORvalidDA)); years=0
	for(u in 1:(length(seg)+1)){
		if(u==1) startyears=1 else startyears=seg[u-1]
		if(u==(length(seg)+1)) endyears=nrow(PORvalidDA) else if(seg[u]==1) endyears=1 else endyears=seg[u]-1
		yearsbwbreak[startyears:endyears]=sum(as.numeric(PORvalidDA[startyears:endyears,8]))
	}
	shortseg=which(as.numeric(yearsbwbreak)<4 & invwhich(seg, length(yearsbwbreak)))

     if(!length(shortseg)==0){
	  #dam breaks at end of POR
	  if(length(yearsbwbreak) %in% shortseg) {
	     repeat{
  	        PORvalidDA=PORvalidDA[-length(yearsbwbreak),];
  	        PORvalidstor=PORvalidstor[-length(yearsbwbreak),]; 
	        if(length(PORvalidDA)==8){break}
		  WYend=PORvalidDA[length(yearsbwbreak)-1,4]
		  #newseg = which((PORvalidDA[,6]>.1) & (PORvalidstor[,6]>.4))
   	        if(i %in% orig.criteria)  newseg = which((PORvalidDA[,6]>.1) | (PORvalidstor[,6]>.4)) else
		      newseg = which((PORvalidDA[,6]>.1)&(PORvalidstor[,6]>.4))
	        yearsbwbreak=PORvalidDA[,8]; shortseg=which(yearsbwbreak<4 & invwhich(newseg, length(yearsbwbreak)))
              if(!length(yearsbwbreak) %in% shortseg){break}
	     }
           if(length(PORvalidDA)==8) PORvalidDA=rbind(PORvalidDA); PORvalidstor=rbind(PORvalidstor)
           #seg = which((PORvalidDA[,6]>.1) & (PORvalidstor[,6]>.4))
   	     if(i %in% orig.criteria)  seg = which((PORvalidDA[,6]>.1) | (PORvalidstor[,6]>.4)) else
		   seg = which((PORvalidDA[,6]>.1)&(PORvalidstor[,6]>.4))
     	     newyearsbwbreak=PORvalidDA[,8]; newshortseg=which(newyearsbwbreak<4 & invwhich(seg, length(newyearsbwbreak)))
        }
	  if(!length(yearsbwbreak) %in% shortseg) {newyearsbwbreak=yearsbwbreak; newshortseg=shortseg}
        #dam breaks during middle of POR
        if(any(seq(2,length(newyearsbwbreak)-1) %in% newshortseg)){ 
	     newsegs=shortseg[which(!shortseg %in% c(1,length(yearsbwbreak)))]
 	     for(h in length(newsegs):1){
	       #combine dam DA/stor with following dam
	       PORvalidDA[newsegs[h]+1,1]=sum(as.numeric(PORvalidDA[c(newsegs[h]+1,newsegs[h]),1])); 
	       PORvalidstor[newsegs[h]+1,1]=sum(as.numeric(PORvalidstor[c(newsegs[h]+1,newsegs[h]),1])); 
 	       #use column 4 as end date of break
             PORvalidDA[newsegs[h]-1,4]=PORvalidDA[newsegs[h],3]
             PORvalidstor[newsegs[h]-1,4]=PORvalidstor[newsegs[h],3]
	       #remove row
	       PORvalidDA=PORvalidDA[-newsegs[h],]; PORvalidstor=PORvalidstor[-newsegs[h],]
  	     }
        }
	  #dam breaks at beginning of POR
        if(1 %in% shortseg) {
	     PORvalidDA=PORvalidDA[-1,]; PORvalidDA[1,1]=PORvalidDA[1,2]; WY=PORvalidDA[1,3]
	     PORvalidstor=PORvalidstor[-1,]; PORvalidstor[1,1]=PORvalidstor[1,2]; 
        }
     } 
    
     if(length(seg)>0){
        #recalculate dam DA/basin DA ratio
        PORvalidDA[,5]=as.numeric(PORvalidDA[,2])/basinDA; 
        nbreak=nrow(PORvalidDA)-1
	  WY=max(WY,1940); WYend=min(WYend,2009)
        for(j in 1:nbreak){
           #years after old break
           if(j == 1) PORvalidDA[j,7]=PORvalidstor[j,7]=as.numeric(PORvalidDA[j,3])-as.numeric(WY) 
           PORvalidDA[j+1,7]=PORvalidstor[j+1,7]=as.numeric(PORvalidDA[j+1,3])-as.numeric(PORvalidDA[j,3])
           #years before next break
           PORvalidDA[j,8]=PORvalidstor[j,8]=as.numeric(PORvalidDA[j+1,3])-as.numeric(PORvalidDA[j,3])
           if(j+1 == nrow(PORvalidDA)) PORvalidDA[j+1,8]=PORvalidstor[j+1,8]=as.numeric(WYend)-as.numeric(PORvalidDA[j+1,3])

           #change in DA calculated as ratio of dam/basin before vs after dam
           PORvalidDA[j+1,6]=as.numeric(PORvalidDA[j+1,5])-as.numeric(PORvalidDA[j,5])
           PORvalidstor[j+1,5]=as.numeric(PORvalidstor[j+1,2])-as.numeric(PORvalidstor[j,2])
           PORvalidstor[j+1,6]=(as.numeric(PORvalidstor[j+1,5])/basinDA)*43560*12/(5280^2)
         }
      }

      #recalculate seg breaks
      #seg = which((PORvalidDA[,6]>.1) & (PORvalidstor[,6]>.4))
   	if(i %in% orig.criteria)  seg = which((PORvalidDA[,6]>.1) | (PORvalidstor[,6]>.4)) else
	    seg = which((PORvalidDA[,6]>.1)&(PORvalidstor[,6]>.4))
    } 

   #recalculate change in DA/stor across each segment to confirm
   #find segment not at beginning or end of POR
   options(digits=4)
   segseg=seg; 
   repeat{
	DAbf=storbf=DAaf=storaf=startseg=endseg=rep(0,(length(segseg)+1))
      for(b in 1:(length(segseg)+1)){
 	   #if(b==1) startseg[b]=1 else if(b==(length(segseg)+1)) startseg[b]=nrow(PORvalidDA) else startseg[b]=segseg[b-1]
	   #if(b==1) startseg[b]=1 else startseg[b]=segseg[b-1]+1
 	   if(b==1) startseg[b]=1 else if(b==(length(segseg)+1) & nrow(PORvalidDA)==segseg[b-1]) startseg[b]=nrow(PORvalidDA) else startseg[b]=segseg[b-1]
         if(!b==(length(segseg)+1) & segseg[b]==1) endseg[b]=1 else if(b==(length(segseg)+1)) endseg[b]=nrow(PORvalidDA) else endseg[b]=segseg[b]-1
	   DAbf[b]=as.numeric(PORvalidDA[startseg[b],2]); storbf[b]=as.numeric(PORvalidstor[startseg[b],2])
 	   DAaf[b]=as.numeric(PORvalidDA[endseg[b],2]); storaf[b]=as.numeric(PORvalidstor[endseg[b],2])
	}

	#if change is still too great add another segment
	if ( any((DAaf-DAbf)/basinDA > .1) &  any(((storaf-storbf)/basinDA)*(43560*12/(5280^2)) >.4 )){
	   whichseg=which( (DAaf-DAbf)/basinDA > .1 & ((storaf-storbf)/basinDA)*(43560*12/(5280^2)) >.4  )
	   #locate dam with max change
	   #for(p in 1:length(whichseg)){
	     #changes=cbind(4*as.numeric(PORvalidDA[(startseg[whichseg[p]]):endseg[whichseg[p]],6]), 
		  #as.numeric(PORvalidstor[(startseg[whichseg[p]]):endseg[whichseg[p]],6]))
	     #if(whichseg[p]==1) addseg=0 else addseg=segseg[whichseg[p]-1]
	     #newseg=as.vector(c(segseg,(which(changes==max(changes), arr.ind=T)[1,1]+ addseg)))
	   #}
	   #changes=cbind(4*as.numeric(PORvalidDA[(startseg[whichseg]+1):endseg[whichseg],6]), 
		#as.numeric(PORvalidstor[(startseg[whichseg]+1):endseg[whichseg],6]))

	   ### 7/1/2014 change criteria to break at greatest change in storage ONLY ##########
	   changes=c(as.numeric(PORvalidstor[(startseg[whichseg]+1):endseg[whichseg],6]))
	   if(whichseg==1) addseg=1 else addseg=segseg[whichseg-1]
	   newseg=as.vector(c(segseg,(which(changes==max(changes)) + addseg)))
	   newseg=newseg[order(newseg)]
	} else {break}
      segseg=newseg
	newsegcounter=newsegcounter+1
	whichnewseg=c(whichnewseg,i)
   } 
   seg=unique(c(seg,segseg))
   seg=seg[order(seg)]

   #7/2/2014: add segments
   if(i == 5545750) seg = 12
   if(i == 5536275) seg = 4
   if(i == 5539900) seg = 2
   if(i == 5532000) seg = 5

   #segmentation
   if(nrow(PORvalidDA)>0){
     begindate<-as.character(mat[1,]$Peak_Record_Begin_Prelim)  
     #if first break was removed set new beginning WY
     WY=max(WY,1940)
     if(date.convert.WY(begindate)!=WY) begindate=paste("10","1",as.numeric(WY)-1,sep="/")
     enddate<-as.character(mat[1,]$Peak_Record_End_Prelim) 
     #if last break was removed set new ending WY
     if(date.convert.WY(enddate)!=WYend) enddate=paste("9","30",WYend,sep="/")

     segments=length(seg)+1; end=start+length(seg)
     seginfo[start:end,4]=basinDA; 

     count=1; breakat=1
     for(k in start:end){
	  seginfo[k,1]=paste(i,count,sep=".")
	  seginfo[k,2]=as.character(mat[1,"Station_Name"])
	  seginfo[k,3]=as.character(mat[1,"County"])
	  seginfo[k,5]=mat[1,"gage_type"]

	  if(k==start) seginfo[k,6]=begindate else seginfo[k,6] = paste("10","1",PORvalidDA[seg[count-1],3],sep="/")
    	  if(all(k==c(start,end))) {WYend=as.numeric(PORvalidDA[nrow(PORvalidDA),4])-1; enddate=paste("9","30",WYend,sep="/")}
 	  if(k==end) seginfo[k,7]=enddate else seginfo[k,7] = paste("9","30",as.numeric(PORvalidDA[seg[count]-1,4])-1,sep="/")

	  seginfo[k,9]<-date.convert.WY(seginfo[k,6])
    	  seginfo[k,10]<-date.convert.WY(seginfo[k,7])

	  if(count <= length(seg) & seg[count]==1) until=1 else until=seg[count]-1; if(until %in% c(NA,'NA')) until=nrow(PORvalidDA)
        #firstsegdam=min(compdate[(breakat):until]); if(length(firstsegdam)==0) firstsegdam=NA
	  #lastsegdam=max(compdate[(breakat):until]); if(length(lastsegdam)==0) lastsegdam=NA
	  
	  if(length(breakat:until)>1){
          firstsegdam=min(PORvalidDA[(breakat+1):until,3]); if(length(firstsegdam)==0) firstsegdam=NA
	    lastsegdam=max(PORvalidDA[(breakat+1):until,3]); if(length(lastsegdam)==0) lastsegdam=NA
        } else firstsegdam=lastsegdam=NA
	  seginfo[k,11]=firstsegdam; seginfo[k,12]=lastsegdam

	  #DA info
	  seginfo[k,13]=bystation[which(bystation[,1]==i),13]
	  seginfo[k,14]=bystation[which(bystation[,1]==i),14]

	  seginfo[k,15]=PORvalidDA[breakat,2];  seginfo[k,16]=PORvalidDA[until,2]
	  seginfo[k,17]=PORvalidDA[breakat,5];  seginfo[k,18]=PORvalidDA[until,5]
	  seginfo[k,19]=as.numeric(seginfo[k,18])-as.numeric(seginfo[k,17])

	  #storage info
	  seginfo[k,20]=PORvalidstor[breakat,2];  seginfo[k,21]=PORvalidstor[until,2]
	  seginfo[k,22]=as.numeric(seginfo[k,21])-as.numeric(seginfo[k,20])
        seginfo[k,23]=(as.numeric(seginfo[k,22])/basinDA)*43560*12/(5280^2)
	  count=count+1
        breakat=1+until
      }
    start=start+segments
    }
  iter=iter+1
}

seginfo=seginfo[which(apply(seginfo,1,function(row) !all(row==0))),]

#some segments <4 years not being caught, remove
short=which(as.numeric(seginfo[,10])-as.numeric(seginfo[,9]) <4)
invaliddat=seginfo[short,]; seginfo=seginfo[-short,]
whichstat=segstations[which(segstations %in% substr(invaliddat[,1],1,7))]
for(t in whichstat){
   statnames=seginfo[which(substr(seginfo[,1],1,7) %in% t),1]
   if(length(statnames)==1) statnames = paste(substr(statnames,1,7),1,sep=".")
   if(!length(statnames)==1) {
	for(h in 1:length(statnames)) statnames[h]=paste(substr(statnames[h],1,7),h,sep=".")
   }
   seginfo[which(substr(seginfo[,1],1,7) %in% t),1] = statnames
}


#add peak counts and gage type
seginfo[,8]=as.numeric(seginfo[,10])-as.numeric(seginfo[,9])+1

array=matrix(0,nrow=6,ncol=3); colnames(array)=c("bf1980","af1980","csg")
array[,1]=c(1,0,0,1,0,0); array[,2]=c(0,1,0,0,1,0); array[,3]=c(1,1,1,0,0,0)
newtype=which(seginfo[,5] %in% c(3,6)) 
csg=as.numeric(seginfo[newtype,5]==3)
bf_80=as.numeric(seginfo[newtype,10]<1981); bf_80[which(is.na(bf_80))]=0
af_80=as.numeric(seginfo[newtype,9]>1980)
seginfo[newtype,5]=apply(cbind(bf_80,af_80,csg),1,function(row) which(duplicated(rbind(array,row),fromLast=T)))

colnames(seginfo)=c("Number","Name","County","BasinDA","Gage Type","peak record begin date","peak record ending date",
	"peak count","peak record begin WY","peak record end WY","year first dam during POR","year last dam during POR",
      "Dam DA for unknown completion date","Dam DA built after POR",  
	"Dam DA at POR begin","Dam DA at POR end","Dam/Basin DA at POR begin","Dam/Basin DA at POR end",
	"Change Dam/Basin DA","Dam Max Stor at POR begin","Dam Max Stor at POR end","Change Max Stor (ac-ft)",
	"Change Max Stor (in)")
#---------------------------------------------------------------------------


#-------------- outputs ----------------------------
#remove some segments
remove.segments <- c("5531300.2","5536570.2","5535200.2")
seginfo <- seginfo[which(!seginfo[,1] %in% remove.segments),]

#make edits in seg info
seginfo[which(seginfo[,"Number"] == "5530000.2"),"peak record begin date"] = "10/1/1978"
seginfo[which(seginfo[,"Number"] == "5531300.3"),"Number"] = "5531300.2"
seginfo[which(seginfo[,"Number"] == "5531300.4"),"Number"] = "5531300.3"
seginfo[which(seginfo[,"Number"] == "5536560.2"),"peak record begin date"] = "10/1/1973"

#make edits in by station
bystation[which(bystation[,"Station"] == 5527900),"Peak Record End Date"] = "1976-03-05"
bystation[which(bystation[,"Station"] == 5536310),"Peak Record End Date"] = "1974-09-30" 
bystation[which(bystation[,"Station"] == 5551030),"Peak Record End Date"] = "1979-09-30" 

#newly created segments 
#if(write.file) write.csv(seginfo,paste("Saito.Segments/combined.info.bysegment.phaseII.",version,".csv",sep=""),row.names=F)

#all combined
newseginfo=cbind(seginfo,rep(NA,nrow(seginfo)))
allcombined=rbind(bystation,newseginfo); options(digits=4)
allcombined=allcombined[order(allcombined[,1]),]
#if(write.file) write.csv(allcombined,paste("Saito.Segments/combined.info.all.phaseII.",version,".csv",sep=""),row.names=F)

#stations in phaseII only
whichphaseII=which(!substr(allcombined[,1],1,7) %in% phaseIstat)
phaseIIinfo=allcombined[whichphaseII,]
if(write.file) write.csv(phaseIIinfo,paste("Saito.Segments/combined.info.phaseII.only.",version,".csv",sep=""),row.names=F)

#stations+segments (removed pre-segmented station)
redstation=subset(bystation, !bystation[,1] %in% segstations)
allsegs=rbind(redstation,newseginfo); allsegs=allsegs[order(allsegs[,1]),]
allsegs=allsegs[,-ncol(allsegs)]
#format dates
ind1 = which( !is.na(date.convert(allsegs[,"Peak Record End Date"])))
allsegs[,"Peak Record End Date"][ind1] = date.convert(allsegs[,"Peak Record End Date"])[ind1]
ind2 = which( !is.na(date.convert(allsegs[,"Peak Record Begin Date"])))
allsegs[,"Peak Record Begin Date"][ind2] = date.convert(allsegs[,"Peak Record Begin Date"])[ind2]
if(write.file) write.csv(allsegs,paste("Saito.Segments/combined.info.allsegments.phaseII.",version,".csv",sep=""),row.names=F)

#compare with segmentation from phase I
phaseI.combinfo=read.csv("DefiningSegments/Saito/Combined_info_143_gages-TMOmod-phaseI.new2_sheet4.csv")
phaseI.combinfo$phase=1; phaseI.combinfo$DA1=NA; phaseI.combinfo$DA2=NA
phaseI.combinfo=phaseI.combinfo[,c(1:12,23:24,13:22)]
phaseIIcombinfo=data.frame(allcombined[,-ncol(allcombined)]); phaseIIcombinfo$phase=2
names(phaseI.combinfo)=names(phaseIIcombinfo)
compare=rbind(as.matrix(phaseIIcombinfo),as.matrix(phaseI.combinfo))
compare=compare[order(compare[,1]),]
if(write.file) write.csv(compare,paste("Saito.Segments/Compare.info.phaseI.vs.phaseII.",version,".csv",sep=""),row.names=F)
#----------------------------------------------------------------------------------




#--------------- prepare peak_info file -----------------------
# segment definintions
# update to most recent file
#segdef=read.csv("Saito.Segments/combined.info.allsegments.phaseII.FINAL.csv", colClasses=c("Station"="character",
	#"Peak.Record.Begin.Date"="character","Peak.Record.End.Date"="character"))[-1,]
segdef = allsegs[-1,]

# basin characteristics from previous peak_info w/o segments
#read-in most recently updated version
peakinfo.noseg=read.csv("BasinCharacteristics/peak_info_noseg_edits.from.phaseI.csv")

peakinfo.wsegs=peakinfo.noseg
#first, set all to zero
peakinfo.wsegs$segment=0

for(i in segdef[,"Station"]){
	ind=which(segdef[,"Station"]==i)
	stat=substr(i,1,7)
	PORstart=segdef[,"Peak Record Begin Date"][ind]
	PORend=segdef[,"Peak Record End Date"][ind]
	whichrow= which(peakinfo.noseg$Streamgage == stat & 
		PORstart <= as.Date(as.character(peakinfo.noseg$Date), "%Y%m%d") &
		PORend >= as.Date(as.character(peakinfo.noseg$Date), "%Y%m%d"))
  if(nchar(i)==7) peakinfo.wsegs$segment[whichrow]=1 else{
	seg=substr(i,9,9)
	peakinfo.wsegs$segment[whichrow]=seg
  }
}

#column for segments that retains negative values
seg.wnegs=peakinfo.wsegs$segment
seg.wnegs[which(peakinfo.noseg$segment_wnegs_edit<0)]=peakinfo.noseg$segment_wnegs_edit[which(peakinfo.noseg$segment_wnegs_edit<0)]

#addCol <- function(data, newcol, colname, col){
#	newcol <- cbind(newcol); colnames(newcol) <- colname
#	newdata <- cbind(data[1:(col-1)],newcol,data[col:ncol(data)])
#	return(newdata)
#}

peakinfo.wsegs$segment_wnegs_edit=seg.wnegs

# add dam storage
for(i in 1:nrow(peakinfo.wsegs)){
   station = peakinfo.wsegs$Streamgage[i]
   WY = date.convert.WY(peakinfo.wsegs$Date[i],fmt="nodlm")
   dams = outputdam[which(outputdam$Site_No == station),]
   damstor = dams$Cum_Max_Stor[max(which(WY >= dams$Year_Compl))]
   peakinfo.wsegs$dam.storage.acft[i] = damstor
}

peakinfo.wsegs$dam.storage.acft[is.na(peakinfo.wsegs$dam.storage.acft)] = 0

if(write.file) write.csv(peakinfo.wsegs,paste("BasinCharacteristics/peak_info_wsegs_new",ver,".csv",sep=""),row.names=F)


#6/26/2014 (SAITO): after discussion on how to handle the changing perception threshold of
#historic peaks (due to adjustments), we decided to add observations between historic peaks
#and start of POR as censored values with the (adjusted) lowest historic peak as the censoring
#level. But first, we need to create a peak info file with these new observations:

InsertRow <- function(existingDF, newrow, r) {
  if(r==1) existingDF <- rbind(newrow,existingDF) else 
	existingDF <- rbind(existingDF[1:(r-1),],newrow,existingDF[r:nrow(existingDF),])
  existingDF
}

addConsec <- function(vector,start,end){
  newvector <- c(vector[1:(start)],seq((vector[start]+1),(vector[end]-1)),vector[end:length(vector)])
  newvector
}

peakinfo.wsegs.wnewobs = peakinfo.wsegs
for(j in as.numeric(sites[-length(sites)])){
   if(any(peakinfo.wsegs$Streamgage == j & peakinfo.wsegs$adjusted.peak.flags %in% c(7,27))){
	 whichpeaks <- which(peakinfo.wsegs.wnewobs$Streamgage == j)
       dates <- as.character(peakinfo.wsegs.wnewobs["Date"][whichpeaks,])
       WY <- date.convert.WY(format(as.Date(dates, "%Y%m%d"),"%m/%d/%Y")); newWY <- WY
	 codes <- peakinfo.wsegs.wnewobs$adjusted.peak.flags[whichpeaks]; whichhist <- which(codes %in% c(7,27))
	 for(i in length(whichhist):1) newWY <- addConsec(newWY,whichhist[i],whichhist[i] + 1)
	 histpeakthreshold = min(peakinfo.wsegs.wnewobs[whichpeaks,"discharge"][codes %in% c(7,27)])
	 addcols <- which(!newWY %in% WY)+ min(peakinfo.wsegs.wnewobs$Streamgage == j) 
	 if(length(addcols)>0) {
	    newrows = matrix(cbind(j, 1, -5, as.numeric(paste(newWY[addcols],"0101",sep="")),newWY[addcols],histpeakthreshold,"","  ","4B","","","","","","","","","","",""),ncol=20)
	    for(k in 1:nrow(newrows)) peakinfo.wsegs.wnewobs = InsertRow(peakinfo.wsegs.wnewobs,newrows[k,],addcols[k])
  	 }
   }
} 

#remove code 7
peakinfo.wsegs.wnewobs$adjusted.peak.flags = replace(peakinfo.wsegs.wnewobs$adjusted.peak.flags, peakinfo.wsegs.wnewobs$adjusted.peak.flags %in% c(7,27),"  ")
#sort by streamgage and date
peakinfo.wsegs.wnewobs <- peakinfo.wsegs.wnewobs[order(peakinfo.wsegs.wnewobs$Streamgage, peakinfo.wsegs.wnewobs$Date),]

if(write.file) write.csv(peakinfo.wsegs.wnewobs,paste("BasinCharacteristics/peak_info_wsegs_new",ver,"_w.mobs.csv",sep=""),row.names=F)
# --------------------------------------------------------------------------------------




#----------table9 ----------------------
write.file <- T
write.phaseI <- F
include.mobs <- F
HDB <- T
MUKWONGA <- T
VER <- 2
source("T:/adjustment/with.dam.segments.noDet.newUrban.QRPanel_data.only.mod.R")
unf <- function(x){x = as.numeric(as.character(x)); return(x)}

ind = which(!duplicated(peakinfo.wsegs[,1:2]) & peakinfo.wsegs[,2] > 0)
table9 = data.frame("Station Number" = peakinfo.wsegs$Streamgage[ind], "Segment" = unf(peakinfo.wsegs$segment[ind]))
table9["Segment"][is.na(table9["Segment"])]
table9["Beginning water year"] = date.convert.WY(peakinfo.wsegs$Date[which(!duplicated(peakinfo.wsegs[,1:2]) & peakinfo.wsegs[,2] > 0)],fmt="nodlm")
table9["Ending water year"] = date.convert.WY(peakinfo.wsegs$Date[which(!duplicated(peakinfo.wsegs[,1:2], fromLast = T) & peakinfo.wsegs[,2] > 0)],fmt="nodlm")
table9["Drainage area (square miles)"] = allsegs[,"Basin DA"][-1]
table9["Urban fraction at adjustment year (2010)"] = rep(urban.adj.wyr,table(table9["Station.Number"]))
ind2 = which(peakinfo.wsegs[,2] > 0 & peakinfo.wsegs[,3] > 0)
damstor.avg = aggregate(dam.storage.acft~Streamgage+segment ,peakinfo.wsegs[ind2,],mean)
table9 = merge(table9,damstor.avg, by=1:2, all.x=T)
names(table9)[7] = "Segment-average maximum dam storage (acre-feet)"
fe_bystation = read.csv(paste("T:/adjustment/new.adjustments/fe_bystation",VER,"_w.MUK_w.HDB.csv",sep=""))
table9["Fixed effect estimate"] = fe_bystation$Fixed.Ef
table9["Standard error of fixed effect estimate"] = fe_bystation$SE
table9["Intra-station fixed effect differences"] = fe_bystation$FE.Difference
names(table9)[1] = "Station Number"
if(write.file) write.csv(table9,paste(BC.inpath,"segment_info_phaseII(table9).csv",sep=""),row.names=F)

#phase I table
if(write.phaseI){
reportpath = "Z:/stuff_to_try/quantile_regression/output/phase I report/"
oldtable9 = read.csv(paste(reportpath,"tables needed/segment_info(table9).csv",sep=""))
newtable9.phaseI = read.csv(paste(reportpath,"segment_info_new(table9).csv",sep=""))
inphaseII = oldtable9[which(oldtable9[,1] %in% table9[,1]),1]
notinphaseII = oldtable9[which(!oldtable9[,1] %in% table9[,1]),1]

table9.phaseI = subset(table9, table9[,1] %in% phaseIstat)
addstats = cbind(na.omit(notinphaseII),1,c(1949,1949,1949,1959,1956,1959),c(2003,1993,1982,2009,1976,1972),0,0,0,0,0,0)
colnames(addstats) = colnames(table9.phaseI)
table9.phaseI = rbind(table9.phaseI,addstats)
table9.phaseI = table9.phaseI[order(table9.phaseI[,1]),]

fe_phaseI = read.csv("Z:/stuff_to_try/quantile_regression/output/new.adjustments/fe_bystation_new3.csv")
fe_phaseI = fe_phaseI[order(fe_phaseI[,1],fe_phaseI[,2]),]
adj_phaseI = read.csv("Z:/stuff_to_try/quantile_regression/output/new.adjustments/newadjustment.phaseI_new2.csv")
currentUF_phaseI = adj_phaseI[which(!duplicated(adj_phaseI$Station)),c(1,5)]
DA_phaseI = oldtable9[which(!duplicated(oldtable9$Station.number )),c(1,5)]


DA_from_phaseII = table9[which(table9[,1] %in% phaseIstat & !duplicated(table9[,1]) ),c(1,5)]

outtable9.phaseI = table9.phaseI
outtable9.phaseI["Drainage area (square miles)"] = rep(DA_phaseI[-nrow(DA_phaseI),2],table(outtable9.phaseI[,1]))
for(id in 1:nrow(DA_from_phaseII)){
  outtable9.phaseI[outtable9.phaseI[,1] %in% DA_from_phaseII[id,1],5] <- as.numeric(DA_from_phaseII[id,2])
}
outtable9.phaseI["Urban fraction at adjustment year (2010)"] = rep(currentUF_phaseI[,2],table(outtable9.phaseI[,1]))
outtable9.phaseI["Fixed effect estimate"] = fe_phaseI$Fixed.Ef
outtable9.phaseI["Standard error of fixed effect estimate"] = fe_phaseI$SE
outtable9.phaseI["Intra-station fixed effect differences"] = fe_phaseI$FE.Difference
if(write.file) write.csv(outtable9.phaseI,paste(BC.inpath,"segment_info_phaseI(table9).csv",sep=""),row.names=F)

#which(table9[,1] %in% inphaseII & 
#paste(table9.phaseI[,1],table9.phaseI[,2],sep=".") %in% paste(oldtable9[,1],oldtable9[,2],sep=".")
#paste(newtable9.phaseI[,1],newtable9.phaseI[,2],sep=".") %in% paste(table9.phaseI[,1],table9.phaseI[,2],sep=".")
}

#--------- table8 ------------------------
OUTputdam = data.frame(OUTputdam, stringsAsFactors=FALSE)
statDA = data.frame(bystation[,c("Station","Basin DA")])

for(k in unique(OUTputdam[,1])){
   ind = which(OUTputdam[,1] == k)
   DA = unf(statDA[which(statDA[,1] == k),2])
   OUTputdam[ind,9] = DA
}

table8 = NULL
statindex = which(!duplicated(OUTputdam$Site_No))
for (i in statindex){
   station = OUTputdam[i,1]
   statinfo = rbind(c(OUTputdam[i,1],as.character(OUTputdam[i,4]),OUTputdam[i,9],rep("",2),
	paste("POR: ",date.convert.WY(as.character(OUTputdam[i,6]))," - ",
	date.convert.WY(as.character(OUTputdam[i,7])),sep=""),rep("",3)))
   if (!is.na(OUTputdam[i,2])){
	daminfo = cbind(NA,as.matrix(OUTputdam[which(outputdam[,1] == station),c(2,9,8,10,15,12,13,18)]))
	daminfo = matrix(daminfo,nrow=nrow(daminfo))
   } else daminfo = NULL
   table8 = rbind(table8,statinfo,daminfo)
}

Insertrow <- function(existingDF, newrow, r) {
  newDF = rbind(existingDF[1:(r-1),],newrow,existingDF[r:nrow(existingDF),],deparse.level = 0)
  newDF
}

outtable8 = table8
for(i in na.omit(unique(table8[,1]))[-1]){
  outtable8=Insertrow(outtable8, rep("",ncol(outtable8)), min(which(outtable8[,1]==i)))
}

outtable8 = data.frame(outtable8, stringsAsFactors = FALSE)
outtable8[which(is.na(outtable8[,1])),1] = ""
sta = which(nchar(outtable8[,1])>0)
outtable8[sta,1] = paste(outtable8[sta,1],outtable8[sta,2],sep=" - ")
outtable8[sta,2] = ""

nsta = which(nchar(outtable8[,1])==0)
outtable8[nsta,3] = ""
outtable8[is.na(outtable8[,3]),3] = ""
trim.leading <- function (x)  sub("^\\s+", "", x)
outtable8[which(trim.leading(outtable8[,6]) == 0),6] = NA

names(outtable8) = c("Station","Dam Name","Station Area (sq mi)","Dam Area (sq mi)","Cumulative Dam Area",
	"Year Completed/Modified","Max Storage","Cumulative Max Storage","In-line (0) or Off-line (1)")

if(write.file) write.csv(outtable8,paste(BC.inpath,"dam_info_by_station_phaseII(table8).csv",sep=""),row.names=F)

#phase I table
if(write.phaseI){
reportpath = "Z:/stuff_to_try/quantile_regression/output/phase I report/"
oldtable9 = read.csv(paste(reportpath,"tables needed/segment_info(table9).csv",sep=""))
newtable9.phaseI = read.csv(paste(reportpath,"segment_info_new(table9).csv",sep=""))
inphaseII = oldtable9[which(oldtable9[,1] %in% table9[,1]),1]
notinphaseII = oldtable9[which(!oldtable9[,1] %in% table9[,1]),1]

s5521000 = c(5521000,"IROQUOIS RIVER AT ROSEBUD, IN", 35.6 ,rep("",2),"POR: 1949 - 2003", rep("",3))
s5523000 = c(5523000,"BICE DITCH NEAR SOUTH MARION, IND.", 21.8, rep("",2),"POR: 1949 - 1993", rep("",3))
s5523500 = c(5523500,"SLOUGH CREEK NEAR COLLEGEVILLE, IND.", 83.7, rep("",2),"POR: 1949 - 1982", rep("",3))
s5536195 = c(5536195,"LITTLE CALUMET RIVER AT MUNSTER, IN", 90.0, rep("",2),"POR: 1959 - 2009", rep("",3))
s5555400 = c(5555400,"VERMILION RIVER TRIBUTARY AT LOWELL, IL", 0.14, rep("",2),"POR: 1956 - 1976", rep("",3))
s5555775 = c(5555775,"VERMILION CREEK TRIBUTARY AT MERIDEN, IL", 0.36, rep("",2),"POR: 1959 - 1972", rep("",3))

library(zoo)
ph1table8 = table8[which(na.locf(table8[,1]) %in% phaseIstat),]
ph1table8[,1] = na.locf(ph1table8[,1])

for(data in c("s5521000","s5523000","s5523500","s5536195")){
   ph1table8 = Insertrow(ph1table8,get(data),min(which(ph1table8[,1] > get(data)[1])))
}
ph1outtable8 = rbind(ph1table8,s5555400,s5555775)
row.names(ph1outtable8) = NULL

ph1outtable8[which(duplicated(ph1outtable8[,1])),1] = ""
sta = which(nchar(ph1outtable8[,1])>0)
ph1outtable8[sta,1] = paste(ph1outtable8[sta,1],ph1outtable8[sta,2],sep=" - ")
ph1outtable8[sta,2] = ""

nsta = which(nchar(ph1outtable8[,1])==0)
ph1outtable8[nsta,3] = ""
ph1outtable8[is.na(ph1outtable8[,3]),3] = ""

for(i in na.omit(unique(ph1outtable8[,1]))[-1]){
  ph1outtable8=Insertrow(ph1outtable8, rep("",ncol(ph1outtable8)), min(which(ph1outtable8[,1]==i)))
}

ph1outtable8 = data.frame(ph1outtable8, stringsAsFactors = FALSE)
ph1outtable8[which(trim.leading(ph1outtable8[,6]) == 0),6] = NA

names(ph1outtable8) = c("Station","Dam Name","Station Area (sq mi)","Dam Area (sq mi)","Cumulative Dam Area",
	"Year Completed/Modified","Max Storage","Cumulative Max Storage","In-line (0) or Off-line (1)")

if(write.file) write.csv(ph1outtable8,paste(BC.inpath,"dam_info_by_station_phaseI(table8).csv",sep=""),row.names=F)
}







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

pchs=pch.code(peakinfo.wsegs$adjusted.peak.flags,c(7),1,3)
cols=col.code(peakinfo.wsegs$adjusted.peak.flags,c(7),"black","blue")

# logQ plot of time: regular peaks, peaks w/o FE.prcp.UF, peaks w/o FE.prcp.UF
ignore <- F
if(ignore){
pdf("T:/adjustment/new.adjustments/logQ_FE.prcp.UF_effects.pdf")
leg.txt = c("Historic Peaks","Loess Fit")
with(peakinfo.wsegs,scatter.smooth(as.Date(as.character(Date),"%Y%m%d") ,log10(discharge),lpars=list(col="red",lwd=2),
	xaxt="n",xlab="Peak Dates",ylab="log10(Discharge)",main="Phase II Flood Peaks",pch=pchs,col=cols))
daterange=range(as.Date(as.character(peakinfo.wsegs$Date),"%Y%m%d"))
axis.Date(1,at=seq(daterange[1],daterange[2], "years"))
legend("bottomleft",leg.txt,lty=c(NA,1),pch=c(3,NA),col=c("blue","red"))

# same plot with FE + precip effect removed
## codes from 'with.dam.segments.noDet.newUrban.QRPanel_data.only.mod.R'
pchs=pch.code(codes,c(7,27),1,3)
cols=col.code(codes,c(7,27),"black","blue")

adjp <- cbind(AllPeaks.wdates$Dates, table[,"log10(Peaks (Fixed Effects Adj))"]) 
with(adjp,scatter.smooth(adjp[,1],adjp[,2],lpars=list(col="red",lwd=2),
	xaxt="n",xlab="Peak Dates",ylab="log10(Discharge)",main="Peaks w/o FE + Precip Effect",pch=pchs,col=cols))
daterange=range(adjp[,1])
axis.Date(1,at=seq(daterange[1],daterange[2], "years"))
legend("bottomleft",leg.txt,lty=c(NA,1),pch=c(3,NA),col=c("blue","red"))

# same plot with FE + precip + UF effect removed
newadjp <- cbind(AllPeaks.wdates$Dates, table["log10(Peaks (Fixed + Precip Effects Adj))"] 
	- table["Urban Frac"]*table["b(i)"]) 
with(newadjp,scatter.smooth(newadjp[,1],newadjp[,2],lpars=list(col="red",lwd=2),
	xaxt="n",xlab="Peak Dates",ylab="log10(Discharge)",main="Peaks w/o FE + Precip + UF Effect",pch=pchs,col=cols))
daterange=range(newadjp[,1])
axis.Date(1,at=seq(daterange[1],daterange[2], "years"))
legend("bottomleft",leg.txt,lty=c(NA,1),pch=c(3,NA),col=c("blue","red"))
dev.off()
}

# check for stations with non-consecutive WY
#for(i in unique(peakinfo.wsegs.wnewobs$Streamgage)){
#	WY = date.convert.WY(peakinfo.wsegs.wnewobs$Date[peakinfo.wsegs.wnewobs$Streamgage==i],fmt="nodlm")
#	if(any(!diff(WY) %in% c(1,2))) print(i)
#}

#--------------------------------------------------------------------------------


# NEXT: FlowData/adjustment/QR_New.Adjustment_PhaseII.R



#---------- rewrite peak info with new precip and urban frac values (HDB)

re.write <- T
if(re.write){

#----------- FLAGS -------------
#for (include.mobs in c(F)){
#for (MUKWONGA in c(T)){
#for( HDB in c(T)){

include.mobs <- F
MUKWONGA <- T
HDB <- T

#fill precip at censored values to minimum precip of that station
precip.fill.cens.flag <- 1

#remove stations to ignore
ignore.stat <- 0
#-----------------------------

seg = array(c("","_w.MUK","_w.mobs","_w.MUK_w.mobs"),dim = c(2,2))
segversion = seg[(MUKWONGA+1),(include.mobs+1)]

urban = array(c("","_w.mobs","_w.HDB","_w.HDB_w.mobs"),dim=c(2,2))
urbanversion = urban[(include.mobs+1),(HDB+1)]

peakinfo.file = paste("peak_info_wsegs_new",segversion,".csv",sep="")
urban.file = paste("urban.by.peaks.frame.Est4",urbanversion,".csv",sep="")

#NB: For regression presented in draft report at:
#http://il.water.usgs.gov/proj/urbanpeaks/AdminReport_UrbanPeakFlow_revised_9-21-12_watermarked.pdf,
#i.e., logQi(t) = ai + 0.5117Ui(t) + 0.0846*Pi(t) + ei(t),
#choices of flags and other adjustable parameters used are:
#  adj.wyr<-2010; scale.flg <- 0; segflg = 2;
#  prec.typ.flg <- 2; prec.fill.flg <- 1; prec.log.flg <- 0
#  urban.log.flg <- 0; urban.flag=1 

#set year to which flows are to be adjusted: must be a whole decade between 1940 and 2010
adj.wyr<-2010; adj.wyr.str = as.character(adj.wyr)
urbanwyr.file = paste("urban.adj.wyr",adj.wyr.str,".frame.Est4",urbanversion,".csv",sep="")

scale.flg <- 0

#set by.station.flg to 1 to create by.station plots and html tables
#(if not needed makes the total volume of output much smaller)
by.station.flg <- 0

#indicate how to handle segments of records resulting from reservoir construction during record
#segflg = 1 #"nosegs": ignore segments entirely by setting all segment values to 1
segflg = 2 #"withsegs": treat segments as found in peak_info file, i.e., utilize separate segments
#segflg = 3 #"nosegsites": remove stations having separate segments (still removes water years whose
#             segment numbers are zero).
#could have another option to use only the longest segment in each multi-segment station... 

#prec.typ.flg=1 means nearest raingage to basin centroid; 2 means Thiessen polygons
prec.typ.flg <- 2; prec.fill.flg <- 1; prec.log.flg <- 0

urban.log.flg<-0

#set paths
dirstr <- "wPrecip.noDet"
if (Sys.getenv("COMPUTERNAME") == "IGSASCEWLT-TMO")
  setwd(paste("D:/Share/UrbanHydrology/Tomasek_UrbanFF/plm results/",dirstr,"/",sep="")) else 
  setwd(paste("//IGSASCEWFSHERCU/ILWSC_Data/SW_INV/UrbanHydrology/PhaseII_ICT/FlowData/plm results/",
              dirstr,"/",sep=""))

if (Sys.getenv("COMPUTERNAME") == "IGSASCEWLT-TMO")
  inpath<-"D:/Share/UrbanHydrology/Tomasek_UrbanFF/" else 
  inpath<-"//IGSASCEWFSHERCU/ILWSC_Data/SW_INV/UrbanHydrology/PhaseII_ICT/"
precpath <- paste(inpath, "Flowdata/matched_precip_output/", sep="")
peakpath <- paste(inpath, "Flowdata/flowdata&quantiles/", sep="")
BC.inpath <- paste(inpath,"Flowdata/BasinCharacteristics/",sep="")
statpath <- paste(inpath,"Flowdata/StationSelection/",sep="")
wdpath <- paste(inpath,"Flowdata/adjustment/",sep="")
segdefpath <- paste(inpath,"Flowdata/Saito.Segments/",sep="")

phaseIpath<-"//IGSASCEWFSHERCU/ILWSC_Data/SW_INV/UrbanHydrology/PhaseI_USACE/from DANAE_share/plm results/wPrecip.noDet.Saito/stuff_to_try/quantile_regression"


#date convert source
source(paste(segdefpath,"date.convert.R",sep=""))

#6/19/2014 Adition (R Saito)
#list of stations to use or ignore
liststat=read.csv(paste(statpath,"ILINWI.station_list.w_useflag.csv",sep=""))
usestat=liststat$site_no[which(liststat$use.1==1)]
if(MUKWONGA) usestat=c(usestat,5544200)


#read in basic peak info
peak.info<-read.table(paste(BC.inpath,peakinfo.file,sep=""),sep=",",header=TRUE)
#6/19/2014 Addition (R Saito)
if(ignore.stat) peak.info=peak.info[which(peak.info$Streamgage %in% usestat),]
peaks=peak.info$discharge; npeaks=length(peaks); peak.sites=peak.info$Streamgage
sites=names(table(peak.sites)); nsites=length(sites)
#try putting back on the leading "0"'s on the site names (station numbers)
sites = paste("0",sites,sep=""); peak.sites = paste("0",peak.sites,sep="") #coerces peak.sites to character
segs=peak.info$segment; segs_wnegs=peak.info$segment_wnegs_edit
dam.storage=peak.info$dam.storage.acft; areas=peak.info$area
#segflag=peak.info$peak.flags
segflag=peak.info$adjusted.peak.flags
water.years<-format(as.Date(as.character(peak.info$Date),"%Y%m%d"),"%Y")
water.years[as.numeric(format(as.Date(as.character(peak.info$Date),"%Y%m%d"),"%m"))>=10] <- 
  as.numeric(water.years[as.numeric(format(as.Date(as.character(peak.info$Date),"%Y%m%d"),"%m"))>=10])+1

# 6/26/2014 (SAITO) 
peak.dates<-date.convert(peak.info$Date,fmt="nodlm")
old.codes <- as.character(peak.info$peak.flags)
codes <- as.character(peak.info$adjusted.peak.flags)

#define which version urbanized land use to use
urban.flag=1 # use fraction exurban (gridcodes7-9 + fraction "urban" (C/I/T) - original method 
#urban.flag=2 # use fraction gridcode9 + fraction "urban" (C/I/T) 
#urban.flag=3 # use fraction exurban only 
#urban.flag=4 # use gridcode9 only 
#urban.flag=5 # use a weighted average of gridcodes7-9 and 10

#create output filestr and outpath
if (segflg==1) filestr = "nosegs" else
if (segflg==2) filestr = "withsegs" else
if (segflg==3) filestr = "nosegsites"

if (scale.flg) filestr <- paste(filestr,"scaled",sep=".")

if (prec.typ.flg == 1) filestr <- paste(filestr,".Prec-cP",sep="") else
if (prec.typ.flg == 2) filestr <- paste(filestr,".Prec-Th",sep="")

if (prec.fill.flg) filestr <- paste(filestr, "_fill", sep="")
if (prec.log.flg) filestr <- paste(filestr, "_log", sep="")

#add type of urbanization used to filestr
if (urban.flag==1) filestr=paste(filestr,".Theo-7to10",sep="") else
if (urban.flag==2) filestr=paste(filestr,".Theo-9to10",sep="") else
if (urban.flag==3) filestr=paste(filestr,".Theo-7to9",sep="") else 
if (urban.flag==4) filestr=paste(filestr,".Theo-9",sep="") else
if (urban.flag==5) filestr=paste(filestr,".Theo-7to10wtd",sep="") 
if (urban.log.flg) filestr=paste(filestr,"_log",sep="")

#at least temporarily, keep these "newUrban" results separate:
filestr = paste(filestr,".newUrban.QRPanel",sep="")

#now that filestr is complete, create a directory to hold the output (if it does not exist)
outpath=paste(getwd(),"/",filestr,sep="")
dir.create(outpath,showWarnings=F)
#create also the "by.station" sub-directory and its sub-directories
dir.create(paste(outpath,"/","by.station",sep=""),showWarnings=F)
if (by.station.flg) {
  dir.create(paste(outpath,"/","by.station/adj.csv.table",sep=""),showWarnings=F)
  dir.create(paste(outpath,"/","by.station/adj.csv.table/fe_only",sep=""),showWarnings=F)
  dir.create(paste(outpath,"/","by.station/adj.csv.table/fe&sw",sep=""),showWarnings=F)
  dir.create(paste(outpath,"/","by.station/adj.xtable",sep=""),showWarnings=F)
  dir.create(paste(outpath,"/","by.station/adj.xtable/fe_only",sep=""),showWarnings=F)
  dir.create(paste(outpath,"/","by.station/adj.xtable/fe&sw",sep=""),showWarnings=F)
  dir.create(paste(outpath,"/","by.station/adj.jpeg",sep=""),showWarnings=F)
  dir.create(paste(outpath,"/","by.station/adj.jpeg/fe_only",sep=""),showWarnings=F)
  dir.create(paste(outpath,"/","by.station/adj.jpeg/fe&sw",sep=""),showWarnings=F)
}
dir.create(paste(outpath,"/","by.segment",sep=""),showWarnings=F)

#read in urbanization explanatory variables (by.peaks)
urban.frame = read.table(paste(BC.inpath,urban.file,sep=""),sep=",",header=TRUE)
# 6/19/2014 Addition (R Saito)
if(ignore.stat) urban.frame=urban.frame[which(urban.frame$site %in% usestat),]

areas = urban.frame$area
if (urban.flag==1) urban=urban.frame$urban.1 else if (urban.flag==2) urban=urban.frame$urban.2 else
if (urban.flag==3) urban=urban.frame$urban.3 else if (urban.flag==4) urban=urban.frame$urban.4 else
if (urban.flag==5) urban=urban.frame$urban.5 
urban.logfile <- paste(outpath,"/urban.logfile.",filestr,".txt",sep=""); sink(urban.logfile)
urban.gt1.subs = which(urban>1); cat("Urban fraction values by peak > 1","\n")
if (length(urban.gt1.subs) > 0) {
  cat("  Fraction: ",urban[urban.gt1.subs],"; subscript: ",urban.gt1.subs,"\n")
  urban[urban.gt1.subs] = 1.
} else cat("  None","\n")

if (urban.log.flg) urban = log10(urban+0.010)

#read in urbanization at adjustment water year
#NB: We have not created a version of the urbanization at adjustment water year that is logged -
#    will need to change the way the adjustment is done as well.
urban.adj.wyr.frame<-read.table(
  paste(BC.inpath,urbanwyr.file,sep=""), sep=",",header=TRUE)
##6/19/2014 Addition (R Saito)##
if(ignore.stat) urban.adj.wyr.frame=urban.adj.wyr.frame[which(urban.adj.wyr.frame$site %in% usestat),]#
if (urban.flag==1) urban.adj.wyr=urban.adj.wyr.frame$urban.adj.wyr.1 else
if (urban.flag==2) urban.adj.wyr=urban.adj.wyr.frame$urban.adj.wyr.2 else
if (urban.flag==3) urban.adj.wyr=urban.adj.wyr.frame$urban.adj.wyr.3 else
if (urban.flag==4) urban.adj.wyr=urban.adj.wyr.frame$urban.adj.wyr.4 else
if (urban.flag==5) urban.adj.wyr=urban.adj.wyr.frame$urban.adj.wyr.5                      
site.area = urban.adj.wyr.frame$area
cat("Urban fraction values at the adjustment water year > 1", "\n")
urban.adj.wyr.gt1.subs = which(urban.adj.wyr>1)
if (length(urban.adj.wyr.gt1.subs) > 0) {
  cat("  Fraction: ",urban.adj.wyr[urban.adj.wyr.gt1.subs],"; subscript: ",urban.adj.wyr.gt1.subs,"\n")
  urban.adj.wyr[urban.adj.wyr.gt1.subs] = 1.
} else cat("  None", "\n")
sink()            

#read in precip 
#if (prec.typ.flg == 1) {
  cP_precip<-read.table(paste(precpath,"cP_peakprecip_max_bds3_ads1.1940-2009_w22HDB.csv",sep=""),sep=',',header=TRUE)
  #precip<-cP_precip$Prcp.Depth
#precip <- peak.info$cP_precip 
#} else if (prec.typ.flg == 2) {
  Thiess_precip<-read.table(paste(precpath,"Thiess_peakprecip_max_bds3_ads1.1940-2009_w22HDB.csv",sep=""), 
                 sep=',',header=TRUE)
  #precip<-Thiess_precip$Prcp.Depth
#precip <- peak.info$Thiess_precip 
#}

if (prec.fill.flg) precip[precip==0] <- median(precip[precip>0])
if (prec.log.flg) {
  if (prec.fill.flg) precip <- log10(precip) else precip <- log10(precip + 0.10)
}

# 6/26/2014 (SAITO) fill missing precip with minimum of station
if (precip.fill.cens.flag==1) {
  for(i in sites) {
    precip[which(peak.sites == i & is.na(precip))] = min(precip[which(peak.sites == i)],na.rm=T)
  }
}

cP_precip$WY <- date.convert.WY(format(as.Date(cP_precip$YYYY.MM.DD),"%m/%d/%Y"))
Thiess_precip$WY <- date.convert.WY(format(as.Date(Thiess_precip$YYYY.MM.DD),"%m/%d/%Y"))
peak.info$WY <- date.convert.WY(peak.info$Date,fmt="nodlm")

newinfo <- peak.info
newinfo2 <- merge(newinfo,cP_precip[,c(1,6,8)],by=c("Streamgage","WY"),all.x=T)
newinfo3 <- merge(newinfo2,Thiess_precip[,c(1,4,6)],by=c("Streamgage","WY"),all.x=T)
newinfo4 <- merge(newinfo3,urban.frame[,c(1,2,5:9)],by.x=c("Streamgage","WY"),
	by.y=c("site","WY"),all.x=T)

newinfo4[,c(11,12,14:18)] <- newinfo4[,c(22:28)]
names(newinfo4)[c(14:18)] <- c("urban.1","urban.2","urban.3","urban.4","urban.5")

newinfo4 <- newinfo4[,-c(2,22:28)]
if(write.file) write.csv(newinfo4,paste(BC.inpath,"peak_info_wsegs_new",segversion,".csv",sep=""),row.names=F)

#}}}

}

files.outputted <- c(
paste("Saito.Segments/merged.alldams.",version,".csv",sep=""),
paste("Saito.Segments/allstation-dams.phaseII.",version,".csv",sep=""),
paste("Saito.Segments/dams.w.missingdate.",version,".csv",sep=""),
paste("Saito.Segments/combined.info.bystation.phaseII.",version,".csv",sep=""),
paste("Saito.Segments/combined.info.phaseII.only.",version,".csv",sep=""),
paste("Saito.Segments/combined.info.allsegments.phaseII.",version,".csv",sep=""),
paste("Saito.Segments/Compare.info.phaseI.vs.phaseII.",version,".csv",sep=""),
paste("BasinCharacteristics/peak_info_wsegs_new",ver,".csv",sep=""),
paste("BasinCharacteristics/peak_info_wsegs_new",ver,"_w.mobs.csv",sep=""),
paste("segment_info_phaseII(table9).csv",sep=""),
paste("segment_info_phaseI(table9).csv",sep=""),
paste("dam_info_by_station_phaseII(table8).csv",sep=""),
paste("dam_info_by_station_phaseI(table8).csv",sep=""),
paste("peak_info_wsegs_new",segversion,".csv",sep="")
)
