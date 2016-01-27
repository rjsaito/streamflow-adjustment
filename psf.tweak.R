
#T:\PeakFQ\PeakFQ_Saito\pFQRA.RL15_no.OF_w.MUK_w.HDB_RA(sent_to_AVeilleux,1-22-15)\PEAKFQ._W.MUK_W.HDB_RA
#psf = readLines(file.choose())
#wch.st = which(substr(psf,1,7)=="Station")

#Y:\PhaseII_ICT\QuantileFitting\RedundancyAnalysis\Results\RA-RL15_no.OF_maxDAR.205\Summary.Keep.w.short_RA-RL15_no.OF.new.DAR.205
#RAstat = read.csv(file.choose())
#stats = RAstat[,1]

new.psf = psf[1:4]
for(i in 1:length(wch.st)){
  if(substr(psf[wch.st[i]],10,17) %in% stats) {
    if(i != length(wch.st))
      new.psf = c(new.psf,psf[wch.st[i]:(wch.st[i+1]-1)]) else
      new.psf = c(new.psf,psf[wch.st[i]:length(psf)]) 
  }
}

adj.wmobs =read.csv(file.choose())
include.mobs <- T
MUKWONGA <- T
HDB <- T
RA <- F  #1/14/2015 Addition

source("T:/adjustment/with.dam.segments.noDet.newUrban.QRPanel_data.only.mod_new.R")

wmobs.thresh = NULL
for(i in 1:nrow(adj.wmobs)){
  if(!i %in% c(1,nrow(adj.wmobs))){
    if(any(adj.wmobs$added.obs[c(i-1,i,i+1)] == 1))
      wmobs.thresh = rbind(wmobs.thresh, unlist(c(adj.wmobs[i,c(1:4,15,18)],as.character(peak.info$peak.flags[i]))))
  }
}
wmobs.thresh = data.frame(wmobs.thresh,stringsAsFactors = F)
#stations that require modification of thresholds (RA only)
if(RA){
  stats.mod = list(2,2,1,c(2,3),c(2,3),c(2,3),2,2)
  names(stats.mod) = c(5529300,5530480,5531050,5531200,5531300,5531380,5536201,5536310)
} else{

}


#substr(new.psf[which(grepl("Station",new.psf))[1:length(which(grepl("Station",new.psf))) %% 2 != 0]], 9,20)

#wmobs.thresh$Mod.Peak = wmobs.thresh$Peak.QR.Adj
#for(i in names(stats.mod))
  #if(i %in% wmobs.thresh$Station){
    #wmobs.thresh$Mod.Peak[wmobs.thresh$Station == i & wmobs.thresh$V7 == "7"]
write.csv(wmobs.thresh,file.choose(new=T),row.names=F)


#writeLines(new.psf,file.choose(new=T))
#new.wch.st = which(substr(new.psf,1,7)=="Station")