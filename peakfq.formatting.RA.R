#------------- Flags ------------------
write.file <- T
include.mobs <- F
MUKWONGA <- T
HDB <- T
RA <- F #1/15/2015 Update
typ <- "RA.RL15_no.OF"
optm.prcp <- F;         if(optm.prcp) prcp = "_w.optm.prcp" else prcp = ""
ign.cens.hist <- F
peaktype <- 3  #one of three: "Orig" = 1, "Mean.Adj" = 2, "QR.Adj" = 3


senspath = "Y:/PhaseII_ICT/FlowData/adjustment/sensitivity_tests/"
PFQpath <- "//IGSASCEWFSHERCU//ILWSC_Data/SW_INV/UrbanHydrology/PhaseII_ICT/Flowdata/PeakFQ/"
infpath <- "Y:/PhaseII_ICT/FlowData/PeakFQ/PeakFQ_Saito"
BCpath <- "//IGSASCEWFSHERCU//ILWSC_Data/SW_INV/UrbanHydrology/PhaseII_ICT/Flowdata/BasinCharacteristics/"
segpath <- "//IGSASCEWFSHERCU//ILWSC_Data/SW_INV/UrbanHydrology/PhaseII_ICT/Flowdata/Saito.Segments/"
adjpath <- "//IGSASCEWFSHERCU//ILWSC_Data/SW_INV/UrbanHydrology/PhaseII_ICT/Flowdata/adjustment/new.adjustments/"

source(paste(segpath,"date.convert.R",sep=""))
source("T:/adjustment/with.dam.segments.noDet.newUrban.QRPanel_data.only.mod_new.R")

for(include.mobs in c(F)){
# Peak FQ Formatting
  # read original formatted data
  inLines=readLines(paste(infpath,"/peak_data.peakfq_format_mod.txt",sep=""))
  nLines=length(inLines); ncharline=nchar(inLines[5])
  #remove one observation at station 5536570, date 19790820
  inLines = inLines[-4626]
  #remove peaks before 1940 and after 2009
  keeplines <- which(!(substr(inLines,1,1)==3 & (date.convert.WY(substr(inLines,17,24),fmt="nodlm") < 1940 | date.convert.WY(substr(inLines,17,24),fmt="nodlm") > 2009) ))
  inLines <- inLines[keeplines]
  nLines=length(inLines)
  inlines <- inLines
  nlines=length(inlines)
  #- read peak.info file -
  segstrings = array(c(outer(c("","_w.MUK","_w.mobs","_w.MUK_w.mobs","_w.HDB","_w.MUK_w.HDB",
	"_w.HDB_w.mobs","_w.MUK_w.HDB_w.mobs"),c("","_RA"),FUN=paste, sep="")),dim = c(2,2,2,2))
  segstr = segstrings[(MUKWONGA+1),(include.mobs+1),1,1]
  adjstr = segstrings[(MUKWONGA+1),(include.mobs+1),(HDB+1),(RA+1)]
  mobstr = segstrings[1,(include.mobs+1),1,1]
  #read adjused peaks
  adjpeaks <- read.csv(paste(adjpath,"newadjustment.phaseII",typ,adjstr,prcp,".csv",sep=""))
  #isna = which(is.na(adjpeaks[,"log10.Peak.QR.Adj."]))
  peakinfo <- read.csv(paste(BCpath,"peak_info_wsegs_new",segstr,".csv",sep="")) 
  if(RA) peakinfo = subset(peakinfo, peakinfo$Streamgage %in% RAsubset)
  peak = adjpeaks[,c("Original.Peaks","Peak.Mean.Adj","Peak.QR.Adj")[peaktype]]   
  #peak[isna] = NA

  outstr = subset
  zero.format <- function(x) {
	x[which(nchar(x)<3)] <- formatC(x[which(nchar(x)<3)],digits=1,format="f")
	return(x)
  }

if(include.mobs==T){
   outlines <- NULL; startover <- F
   for(i in 1:nlines){
      if(substr(inlines[i],1,1) == "Y"){
	   outlines <- c(outlines, inlines[(i-3):i]) 
	   station <- substr(inlines[i],3,9)
	   whichlines <- which(peakinfo$Streamgage == station)
	   startover <- T
      } else if(startover){
	   sublines <- inlines[which(substr(inlines,1,9) == paste(30,station,sep=""))]
	   newlines <- rep(paste(rep(" ",ncharline),collapse=""),sum(peakinfo$Streamgage == station))	
	   substr(newlines,1,9) <- paste(30,station,sep="")
	   substr(newlines,17,24) <- format(as.character(peakinfo$Original.Date[whichlines]),width=8, justify="left")
	   substr(newlines,25,31) <- format(zero.format(as.numeric(peakinfo$discharge[whichlines])),width=7, justify="right")
	   substr(newlines,32,33) <- as.character(peakinfo$adjusted.peak.flags[whichlines])
	   otherinfo <- substr(sublines,47,64)
	   whereotherinfo <- which(format(as.character(peakinfo$Original.Date[whichlines]),width=8,justify="left") %in% substr(sublines,17,24))
	   substr(newlines[whereotherinfo],47,64) <- otherinfo 
	   outlines <- c(outlines,newlines)
	   startover <- F
      }
   }
} else outlines <- inlines

if(RA){
   ignore.lines <- which(!substr(outlines,3,9) %in% RAsubset)
   outlines <- outlines[-ignore.lines]
   nlines=length(outlines)
}

remove = which(!as.numeric(substr(outlines, 2,9)) %in% usestat)
outlines = outlines[-remove]

  npeaks=length(outlines[which(substr(outlines,1,1)==3)])
  if (npeaks != length(peak))  stop("Number of peaks in input vs. output do not match") else{  #catch error
  # replace the peaks with adjusted peaks
  options(digits=6)
  dec <- function(x) 6 - (floor(log10(x))+1)
  #if(peaktype!=1){
    j=0
    for (i in 1:length(outlines)) {
      if (substr(outlines[i],1,1) == "3") {
         j=j+1
         substr(outlines[i],25,31) <- format(zero.format(round(peak[j],digits=dec(peak[j]))),width=7, justify="right")
      }
    }
  #}

  if(ign.cens.hist) outlines = outlines[-intersect(grep(3,substr(outlines,1,1)), union(grep(4,substr(outlines,32,33)), grep(7,substr(outlines,32,33))))]


  suppressWarnings(dir.create(paste(PFQpath,"PeakFQ_Saito/pFQ_AllPeaks_FittedPrcp2",mobstr,sep="")))
  if(write.file) writeLines(outlines, file(paste(PFQpath,"PeakFQ_Saito/pFQ_AllPeaks_FittedPrcp2",mobstr,"/peakfq.txt",sep=""))); 
  } #close error func

}
