# Runs the analysis of peaks as panel models using area, urbanization, precipitation,
# and has three options for considering changes in reservoir storage:
# (1) ignoring them as before;
# (2) dividing up the station record into segments that are believed to be at least approximately
# homogeneous wrt reservoir storage;
# (3) removing the change-in-reservoir-storage-affected station records entirely.
# 
# "noDet" version, i.e., including no consideration of detention.
# 
# Author: TMO, adapted from with.dam.segments.noDet.R, 9/2012
# Changes from previous script: 
# 1. Addition of peak_info.csv file that contains station numbers, drainage areas,
#    peak flow values, record segments (homogeneous wrt reservoir storage),
#    and reservoir storage.
#    Previously the segments and reservoir storage were not included and the
#    other basic information was read in from one of the precipitation files.
# 2. Drainage area for scaling of peak flows now comes from TOTAL.SQ.MI rather than
#    TOTAL in the Theobald information table where the latter excludes undefined areas.
# 3. Theobald urbanization fractions (for each peak, but not for the adjustment water year)
#    are read in a from a spreadsheet rather than begin recomputed every time.
# 4. xcorr and gls model calculations have been dropped. 
# 5. stats package function approx() is now used for linear interpolation,
#    rather than function LinearInterpolation that Brad Tomasek wrote.
# 6. Removed opportunity of imposing conditions on minimum period of record before
#    and after assumed year of detention ("minPOR", maybe keep in "Det" version). 
# 7. Added by-segment and global partial residuals plots and plots of observed versus fitted 
# 8. Added histogram plots of number of stations reporting, beginning, ending and impacted by 
#    completion of dams each WY, of trend magnitudes and p-values,
#    and of changes in peak flow moments (before and after adjustment)
# 9. Added "by.station.flg" to control whether single station plots are created (since
#    these are voluminous and take up a lot of disk space and often aren't needed).
#10. More changes to Theobald urbanization fractions:
#    a. They were recomputed using fraction of total basin area rather than fraction of 
#       defined basin area.
#    b. Change (a) required recomputing the gridcode9 versus gridcode10 relation, which is
#       done using code10.v.code9.R in BasinCharacteristics folder
#    c. If gridcode10_2000 was > 0 but gridcode9_2000 was 0, the prediction of gridcode10
#       was set to gridcode10_2000 instead of 0, as would have occurred in the old method.
#       All the changes a.,b.,c. are contained in BasinCharacteristics\UH_HousingDataframe_TMO3.xlsx
#    d. A script, process.Theobald.R in the BasinCharacteristics folder, was written, to 
#       process all the urbanization fractions, by peak, for the detention water year, and
#       for the adjustment water year, so that only the values are read into this script.
#    e. The changes a.,b.,c. removed most of the urban fraction values > 1, but e.g. for the
#       adjustment water year for gridcodes7-10 for one station, there was a value of 1.002,
#       so code was added to this script to check for these cases, set them to 1., and output
#       notes on this calculation to a logfile, "urban.logfile...txt".
#11. For the fixed effects case, the difference in fixed effects between the first and subsequent
#    segments (subsequent - first) is added to the log(adjusted peak flow), and the fixed effects
#    and related statistics adn these difference are output to a file "fixef.fe.",filestr,".csv".
####################################################################################################

########### Additional Changes #####################################################################
#1. 4/15/2013: An option to create .pdf and .eps all-segment partial residual plots was added.
#2. 6/6/2013: Added Urban*Precip term to fe regressions and added pglm regressions per suggestion of Tim Cohn
#3. 6/6/2013: Added option of setting urban.log.flg to set urban = log10(urban+0.01) per Chris Konrad's suggestion
#4. 9/17/13: Added code intended to implement Ivan Canay's "simple approach" to quantile regression for panel data (Canay, 2011).
####################################################################################################

#----------- FLAGS -------------
#include.mobs <- F
#MUKWONGA <- T
#HDB <- T

#fill precip at censored values to minimum precip of that station
precip.fill.cens.flag <- 1

#remove stations to ignore
ignore.stat <- 1
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
RApath <- paste(inpath,"QuantileFitting/RedundancyAnalysis/Results/",sep="")

phaseIpath<-"//IGSASCEWFSHERCU/ILWSC_Data/SW_INV/UrbanHydrology/PhaseI_USACE/from DANAE_share/plm results/wPrecip.noDet.Saito/stuff_to_try/quantile_regression"


#date convert source
source(paste(segdefpath,"date.convert.R",sep=""))

#6/19/2014 Adition (R Saito)
#list of stations to use or ignore
#liststat=read.csv(paste(statpath,"ILINWI.station_list.w_useflag.csv",sep=""))
liststat=read.csv(paste(statpath,"ILINWI.station_list.w_useflag.wMUK.corrected.csv",sep=""))
usestat=liststat$site_no[which(liststat$use.1==1)]
#if(MUKWONGA) usestat=c(usestat,5544200)

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
peak.dates.orig = peak.info$Date
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
if (prec.typ.flg == 1) {
  #cP_precip<-read.table(paste(precpath,"cP_peakprecip_max_bds3_ads1.1940-2009_w22HDB.csv",sep=""),sep=',',header=TRUE)
  #precip<-cP_precip$Prcp.Depth
precip <- peak.info$cP_precip 
} else if (prec.typ.flg == 2) {
  #Thiess_precip<-read.table(paste(precpath,"Thiess_peakprecip_max_bds3_ads1.1940-2009_w22HDB.csv",sep=""), 
  #               sep=',',header=TRUE)
  #precip<-Thiess_precip$Prcp.Depth
precip <- peak.info$Thiess_precip 
}

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

if (segflg==1) segs[] = 1 else
if (segflg==3) {
  temp.sites = sites; temp.peak.sites = peak.sites
  for (S in 1:nsites) {
    subs = peak.sites==sites[S]    
  #if the maximum segment number for a given station is 2 or more, it has separate segments
  #so for this segflg it should be removed
    if (max(segs[subs]) > 1) {  
      temp.sites[S] = ""; temp.peak.sites[subs] = "" 
    }
  }
#now create trimmed versions of arrays created so far
  sites.nchar = nchar(temp.sites); keep.site.subs = which(sites.nchar>0); nsites = length(keep.site.subs)
  sites = sites[keep.site.subs]; urban.adj.wyr = urban.adj.wyr[keep.site.subs]
  #urban.at.det = urban.at.det[keep.site.subs]
  site.area = site.area[keep.site.subs]
  peak.sites.nchar = nchar(temp.peak.sites); keep.peak.subs = which(peak.sites.nchar>0)
  npeaks = length(keep.peak.subs); peaks = peaks[keep.peak.subs]; peak.sites = peak.sites[keep.peak.subs]
  segs = segs[keep.peak.subs]; areas = areas[keep.peak.subs]; urban = urban[keep.peak.subs]
  precip = precip[keep.peak.subs]; water.years = water.years[keep.peak.subs]
  dam.storage = dam.storage[keep.peak.subs]
}


#first remove peaks with 0 for segment value
seg.subs = segs_wnegs>=1; 
seg.peaks = peaks[seg.subs]; n.seg.peaks = length(seg.peaks)
seg.peak.sites = peak.sites[seg.subs]; seg.water.years = water.years[seg.subs]
seg.areas = areas[seg.subs]; seg.dam.storage = dam.storage[seg.subs]
seg.segs = segs[seg.subs]; seg.urban = urban[seg.subs]; seg.precip = precip[seg.subs]
#next split up records by segment values
peak.sites.by.seg = character(n.seg.peaks); seg.site.avg.damS = NULL
seg.urban.adj.wyr = NULL; seg.site.area = NULL; #seg.urban.at.det = NULL
seg.min.WY = NULL; seg.max.WY = NULL
for (S in 1:nsites){
  site.segs = seg.segs[seg.peak.sites==sites[S]]
  min.seg = min(site.segs); max.seg = max(site.segs)
  for (i in min.seg:max.seg) {
    subs = which(seg.peak.sites==sites[S]&seg.segs==i)
    peak.sites.by.seg[subs] = paste(sites[S],as.character(i),sep='.')
    seg.site.avg.damS = c(seg.site.avg.damS,mean(seg.dam.storage[subs]))
    seg.urban.adj.wyr = c(seg.urban.adj.wyr,urban.adj.wyr[S])
    #seg.urban.at.det = c(seg.urban.at.det,urban.at.det[S])
    seg.site.area = c(seg.site.area,site.area[S])
    seg.min.WY = c(seg.min.WY,min(seg.water.years[subs]))
    seg.max.WY = c(seg.max.WY,max(seg.water.years[subs]))
  }
}

seg.peak.sites = peak.sites.by.seg; seg.sites=names(table(seg.peak.sites)); n.seg.sites=length(seg.sites)   
seg.sites.split = unlist(strsplit(seg.sites,".",fixed=T))
seg.site.sites = seg.sites.split[seq(from=1,length=n.seg.sites,by=2)]
seg.site.segs = seg.sites.split[seq(from=2,length=n.seg.sites,by=2)]

# 1/14/2015 Addition
RAsubset = read.csv(paste(RApath,"RA-RL15_no.OF_maxDAR.205/Summary.Keep.w.short_RA-RL15_no.OF.new.DAR.205.csv",sep=""))[,1]
if(RA) RAstr = "RA." else RAstr = ""

OrigSites=data.frame("Site_No"=sites, "Area"=site.area, "Urban.at.AdjWyr"=urban.adj.wyr)
# 1/14/2015 Addition
if(RA) OrigSites = subset(OrigSites, as.numeric(as.character(OrigSites$Site_No)) %in% RAsubset)
OrigSites$Site_No = factor(OrigSites$Site_No)
write.table(OrigSites,paste(outpath,"/OrigSites.",RAstr,filestr,".csv",sep=""),sep=",", 
            row.names=FALSE,col.names=TRUE)
SegSites=data.frame("Site.Seg"=seg.sites, "Site"=seg.site.sites, "Seg"=seg.site.segs, 
  "Begin.WY"=seg.min.WY, "End.WY"=seg.max.WY, "Area"=seg.site.area, "Urban.at.AdjWyr"=seg.urban.adj.wyr, 
  "Avg.Dam.Storage"=seg.site.avg.damS)
# 1/14/2015 Addition
if(RA) SegSites = subset(SegSites, as.numeric(as.character(SegSites$Site)) %in% RAsubset)
SegSites$Site = factor(SegSites$Site)
#write.table(SegSites,paste(outpath,"/SegSites.",RAstr,filestr,".csv",sep=""),sep=",", 
#            row.names=FALSE,col.names=TRUE)

scaled.peaks <- peaks/areas
AllPeaks=data.frame("Wateryear"=water.years, "Site_No"=peak.sites, "Segment"=segs, "Area"=areas, 
  "Peak"=peaks, "Scaled.Peak"=scaled.peaks, "Precip"=precip, "Urban.Frac"=urban, "Dam.Storage"=dam.storage,
  "Codes" = segflag, stringsAsFactors = F)
# 1/14/2015 Addition


RA_segs = subset(segs, as.numeric(as.character(AllPeaks$Site_No)) %in% RAsubset)
RA_segs_wnegs = subset(segs_wnegs, as.numeric(as.character(AllPeaks$Site_No)) %in% RAsubset)
if(RA) {
   newsegs = subset(segs, as.numeric(as.character(AllPeaks$Site_No)) %in% RAsubset)
   newsegs_wnegs = subset(segs_wnegs, as.numeric(as.character(AllPeaks$Site_No)) %in% RAsubset)
   AllPeaks = subset(AllPeaks, as.numeric(as.character(AllPeaks$Site_No)) %in% RAsubset)
} else{
  newsegs = segs
  newsegs_wnegs = segs_wnegs
}
AllPeaks_RA = subset(AllPeaks, as.numeric(as.character(AllPeaks$Site_No)) %in% RAsubset)
AllPeaks_RA$Site_No = as.factor(as.character(AllPeaks_RA$Site_No))

#write.table(AllPeaks,paste(outpath,"/AllPeaks.",RAstr,filestr,".csv",sep=""),sep=",",row.names=FALSE,col.names=TRUE)
#log10.peaks <- log10(peaks); log10.scaled.peaks <- log10(scaled.peaks)

# 6/26/2014 (SAITO) added new df with peak dates
AllPeaks.wdates=data.frame("Wateryear"=water.years, "Dates"=peak.dates, "Site_No"=peak.sites, "Segment"=segs, "Area"=areas, 
  "Peak"=peaks, "Scaled.Peak"=scaled.peaks, "Precip"=precip, "Urban.Frac"=urban, "Dam.Storage"=dam.storage,
  "Codes" = segflag)
# 1/14/2015 Addition
if(RA) AllPeaks.wdates = data.frame(subset(AllPeaks.wdates, as.numeric(as.character(AllPeaks.wdates$Site_No)) %in% RAsubset))
AllPeaks.wdates$Site_No = factor(AllPeaks.wdates$Site_No)
write.table(AllPeaks,paste(outpath,"/AllPeaks.",RAstr,filestr,".csv",sep=""),sep=",",row.names=FALSE,col.names=TRUE)
log10.peaks <- log10(peaks); log10.scaled.peaks <- log10(scaled.peaks)

seg.scaled.peaks <- seg.peaks/seg.areas
SegPeaks=data.frame("Wateryear"=seg.water.years, "Site_No"=seg.peak.sites, "Area"=seg.areas, 
  "Peak"=seg.peaks, "Scaled.Peak"=seg.scaled.peaks, "Precip"=seg.precip, "Urban.Frac"=seg.urban, 
  "Dam.Storage"=seg.dam.storage, stringsAsFactors = F)
# 1/14/2015 Addition
if(RA) SegPeaks = subset(SegPeaks, round(as.numeric(as.character(SegPeaks$Site_No))) %in% RAsubset)
SegPeaks$Site_No = factor(SegPeaks$Site_No)
#write.table(SegPeaks,paste(outpath,"/SegPeaks.",RAstr,filestr,".csv",sep=""),sep=",",row.names=FALSE,col.names=TRUE)
log10.seg.peaks <- log10(seg.peaks); log10.seg.scaled.peaks <- log10(seg.scaled.peaks)


SegPeaks_RA = subset(SegPeaks, round(as.numeric(as.character(SegPeaks$Site_No))) %in% RAsubset)
SegPeaks_RA$Site_No = as.factor(as.character(SegPeaks_RA$Site_No))


segs = newsegs
segs_wnegs = newsegs_wnegs
