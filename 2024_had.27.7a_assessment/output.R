## Extract results of interest, write TAF output tables
# take the outputs of the ASAP and retrospective and create plots etc...
## Before:
## After:

library(icesTAF)
mkdir("output")
library(FLCore)
library(FLAssess)
library(FLash)
library(knitr)
library(lattice)


library(ASAPplots)      
library(dplyr)
library("Hmisc")

#### Create  the stock object
#open new graphics device
dev.new()

asap <- dget('model/ORIGINAL.rdat') #read in the outputs of the ASAP run
retrofiles <- paste0('model/RETRO_ORIGINAL_',sprintf('%03d',0:5),'.rdat') #read in the outputs of the retrospective files
retro <- lapply(retrofiles,FUN=function(x) dget(x))
asap.std <- read.table('model/ORIGINAL.std',header=T,fill=T)

#### Some housekeeping
years <- asap$parms$styr:asap$parms$endyr
nyears <- length(years)
ages <- 1:asap$parms$nages -1 # note the age offset
nages <- length(ages)
nindices <- asap$parms$nindices
indices <- c('NI-Q1','NI-Q4','NI-MIK','FSP')
fbarage <- asap$options$Freport.agemin:asap$options$Freport.agemax - 1 # note the age offset
#create the plots to save function
SavePlot0<-function(plotname,width=6,height=4){
  file <- file.path("output",paste0('Had7a_asap_',plotname,'.png'))
  dev.print(png,file,width=width,height=height,units='in',res=300,pointsize=8)
}
bubbles <- function(x,z,cex,  key.space = 'right',...){
  maxz <- max(abs(z),na.rm=T)
  panel.fun <- function(x,z,subscripts,cex,...){
    pt.cex <- sqrt(abs(z)/maxz)*cex
    pt.bg <- ifelse(z<0, '#00000000','#00000050')
    lpoints(x,cex=pt.cex[subscripts],pch=21,fill=pt.bg[subscripts],col=1,...)
  }
  text <- as.character(round(seq(maxz,-maxz,length=6),2))
  key = list(space = key.space, text = list(text),
             points = list(pch = c(21), cex=sqrt(abs(seq(cex,-cex,length=6)))^2, 
                           fill = rep(c('#00000050','#00000000'),each=3)),
             rep = FALSE)
  xyplot(x,z=z,cex=cex,panel=panel.fun,key=key,...)
}
catch <- data.frame(years,observed=c(asap$catch.obs),predicted=c(asap$catch.pred))
xyplot(observed+predicted~years,data=catch,type='b',auto.key=T,xlab='Year',ylab='Catch (t)')
a <- SavePlot0('Fleet_Catch',4,4)

res1 <- data.frame(year=years,age=rep(ages,each=nyears),obs=c(asap$catch.comp.mats$catch.fleet1.ob),pred=c(asap$catch.comp.mats$catch.fleet1.pr))
res1$res <- res1$obs-res1$pred
res2 <- merge(res1,with(res1,aggregate(list(obsbar=obs),list(age=age),mean)))
res2$sres <- res2$res/res2$obsbar
res2$sres <- ifelse(is.finite(res2$sres),res2$sres,NA)

xyplot(obs+pred~year|factor(age),data=res1,type='l',auto.key=T,as.table=T,scales=list(y='free',alternating=F),ylab='Proportion-at-age')
a <- SavePlot0('FleetCaaRes1')

panfun <- function(x, y) {
  panel.xyplot(x, y)
  panel.abline(h=0)
}
xyplot(res~year|factor(age),data=res1,type='l',auto.key=T,as.table=T,ylab='Proportion-at-age residuals',panel=panfun)
a <- SavePlot0('FleetCaaRes2')

bubbles(age~year,data=res1,z=res1$res,cex=5,xlab='Year',ylab='Age')
a <- SavePlot0('FleetResidualsAge',6,3.5)

bubbles(age~year,data=res2,z=res2$sres,cex=5,xlab='Year',ylab='Age')
a <- SavePlot0('FleetStResidualsAge',6,3.5)

ind1 <- NULL
for(i in 1:nindices){
  ind1 <- rbind(ind1, data.frame(years=years[asap$index.year.counter[[i]]],name=indices[i],observed=asap$index.obs[[i]],predicted=asap$index.pred[[i]]))
}
xyplot(observed+predicted~years|name,data=ind1,type='b',xlab='Year',ylab='Index',scales=list(alternating=1,y=list(relation='free')),auto.key=T)
a <- SavePlot0('IndexFit')
res1 <- NULL
for(i in 1:nindices){
  iob <- grep('ob',names(asap$index.comp.mats))[i]
  ipr <- grep('pr',names(asap$index.comp.mats))[i]
  res1 <- rbind(res1,data.frame(year=years,age=rep(ages,each=nyears),name=indices[i],obs=unlist(asap$index.comp.mats[iob]),pred=unlist(asap$index.comp.mats[ipr])))
}
res1$res <- res1$obs-res1$pred
res2 <- merge(res1,with(res1,aggregate(list(obsbar=obs),list(age=age,name=name),mean)))
res2$sres <- res2$res/res2$obsbar
#key <- simpleKey(text=c('obs','pred'),points=F,lines=T,space='right')
key <- simpleKey(text=c('obs','pred'),points=F,lines=T,space='top')
xyplot(obs+pred~year|paste(name,'Age',age),data=res1,type='l',key=key,scales=list(alternating=1),par.strip.text=list(cex=0.6),ylab='Proportion-at-age',layout=c(nages,nindices),as.table=T)

a <- SavePlot0('IndexCaa',6,7)
a <- SavePlot0('IndexCaa',6,4)



res1 <- subset(res1,res!=0)
bubbles(age~year|name,data=res1,z=res1$res,cex=3,xlab='Year',ylab='Age',layout=c(1,nindices),scales=list(alternating=1),key.space='top')
a <- SavePlot0('IndexResidualsAge',3.1,6)

res2 <- subset(res2,sres!=0 & is.finite(sres))
bubbles(age~year|name,data=res2,z=res2$sres,cex=3,xlab='Year',ylab='Age',layout=c(1,nindices),scales=list(alternating=1),key.space='top')
a <- SavePlot0('IndexStResidualsAge',3.1,6)


#Selectivity at age in catches. Age 0 is fixed at 0% and ages 3+ are fixed at 100%.

sel <- stack(as.data.frame(asap$fleet.sel.mats$sel.m.fleet1))
sel <- data.frame(years,sel)
sel$block <- c(asap$fleet.sel.blocks)
sel$age <- as.numeric(as.character(sel$ind))-1
key <- simpleKey(text=paste('Block',unique(sel$block)),points=T,space='right')
key$points$pch <- unique(sel$block)
key$space<-'top'
xyplot(values~age,groups=block,data=sel,xlab='Age',ylab='Selectivity',type='b',key=key,pch=key$points$pch)
a <- SavePlot0('Fleet_S')

res1 <- subset(res1,res!=0)
bubbles(age~year|name,data=res1,z=res1$res,cex=3,xlab='Year',ylab='Age',layout=c(1,nindices),scales=list(alternating=1),key.space='top')
a <- SavePlot0('IndexResidualsAge',3.1,6)

res2 <- subset(res2,sres!=0 & is.finite(sres))
bubbles(age~year|name,data=res2,z=res2$sres,cex=3,xlab='Year',ylab='Age',layout=c(1,nindices),scales=list(alternating=1),key.space='top')
a <- SavePlot0('IndexStResidualsAge',3.1,6)

sel <- stack(as.data.frame(asap$fleet.sel.mats$sel.m.fleet1))
sel <- data.frame(years,sel)
sel$block <- c(asap$fleet.sel.blocks)
sel$age <- as.numeric(as.character(sel$ind))-1
key <- simpleKey(text=paste('Block',unique(sel$block)),points=T,space='right')
key$points$pch <- unique(sel$block)
key$space<-'top'
xyplot(values~age,groups=block,data=sel,xlab='Age',ylab='Selectivity',type='b',key=key,pch=key$points$pch)
a <- SavePlot0('Fleet_S')

sel1 <- data.frame(name=paste('index',1:nindices),age=rep(ages,each=nindices),sel=c(asap$index.sel))
sel1$sel <- ifelse(sel1$sel<0,NA,sel1$sel)
xyplot(sel~age|name,data=sel1,type='b',xlab='Age',ylab='Selectivity',scales=list(alternating=1),ylim=c(-.04,1.04))
a <- SavePlot0('IndexSelectivity')

sel2 <- data.frame(ages,subset(sel,years==max(years))$values,t(asap$index.sel))
names(sel2) <- c('Age','Catch',indices[1:nindices])
for(i in 2:ncol(sel2)) sel2[,i] <- ifelse(sel2[,i]<0,NA,sel2[,i])
write.csv(sel2,'Had7a_asap_sel.csv',row.names=F)
ind3 <- NULL
for(i in 1:nindices){
  ind3 <- rbind(ind3, data.frame(years=years[asap$index.year.counter[[i]]],name=indices[i],q=asap$q.indices[[i]]))
}
xyplot(q~years|name,data=ind3,type='b',scales=list(alternating=1))
a <- SavePlot0('IndexQ')

ees <- rbind(data.frame(years,type='input',value=c(asap$fleet.catch.Neff.init))
             ,data.frame(years,type='estimated',value=c(asap$fleet.catch.Neff.est)))
xyplot(value~years,groups=type,data=ees,type='b',auto.key=T,xlab='Year',ylab='Effective sample size (log scale)',scales=list(alternating=1,y=list(log=T)))
a <- SavePlot0('Fleet_EES')

ind2 <- NULL
for(i in 1:nindices){
  ind2 <- rbind(ind2, data.frame(years,name=indices[i],observed=asap$index.Neff.init[i,],predicted=asap$index.Neff.est[i,]))
}
xyplot(observed+predicted~years|name,data=ind2,type='b',auto.key=T,xlab='Year',ylab='Effective sample size',scales=list(alternating=1,y=list(log=T)))
a <- SavePlot0('IndexEffSampSize')

snaa <- asap$N.age
snaa.df <- stack(as.data.frame(snaa))
snaa.df$year <- years
# something wrong with cohorts i think
snaa.df$cohort <- snaa.df$year - as.numeric(as.character(snaa.df$ind))-1
key <- simpleKey(as.character(ages),space='right',title='age',cex.title=1)
key$points$pch <- 1:nages
xyplot(log(values)~cohort,groups=factor(ind),data=snaa.df,type='b',key=key,pch=1:nages,scales=list(alternating=1),xlab='Cohort',ylab='Log stock numbers at age',cex=0.6)
a <- SavePlot0('StockNos')
obj <- unlist(asap$like)[-1]
par(las=1,mar=c(5,12,4,2))
b <- barplot(obj,names=gsub('lk.','',names(obj)),horiz=T,xlab='Value')
text(obj,b,ifelse(obj==0,0,''),pos=4)
a <- SavePlot0('ObjectiveFunction')

rmse <- unlist(asap$RMSE)
rmse.n <- unlist(asap$RMSE.n)
ylim <- c(0,max(rmse)*1.2)
plot(rmse~rmse.n,ylim=ylim,xlab='Number of residuals',ylab='RMSE')
text(rmse~rmse.n,labels=gsub('rmse.','',names(rmse)),pos=3)
abline(h=1)
lines(c(1,3,5,10,20,30,40,50,100),c(.063,.348,.473,.634,.737,.786,.815,.832,.883),lty=3)
lines(c(1,3,5,10,20,30,40,50,100),c(1.960,1.619,1.487,1.351,1.253,1.211,1.183,1.162,1.116),lty=3)
a <- SavePlot0('RMSE')
par(mfrow=c(2,2))
xlim <- range(years)
ylim <- c(0,max(unlist(lapply(retro,function(x) x$catch.pred/1000))))
plot(NA,xlim=xlim,ylim=ylim,xlab='Year',ylab='Kt',main='Catch')
a <- lapply(retro,function(x) lines(as.numeric(colnames(x$catch.pred)),x$catch.pred/1000))
ylim <- c(0,max(unlist(lapply(retro,function(x) x$SSB/1000))))
plot(NA,xlim=xlim,ylim=ylim,xlab='Year',ylab='Kt',main='SSB')
a <- lapply(retro,function(x) lines(as.numeric(colnames(x$catch.pred)),x$SSB/1000))
ylim <- c(0,max(unlist(lapply(retro,function(x) x$F.report))))
plot(NA,xlim=xlim,ylim=ylim,xlab='Year',ylab='F',main=paste0('Fbar ',paste(range(fbarage),collapse='-')))
a <- lapply(retro,function(x) lines(as.numeric(colnames(x$catch.pred)),x$F.report))
ylim <- c(0,max(unlist(lapply(retro,function(x) x$N.age[,1]/1000))))
plot(NA,xlim=xlim,ylim=ylim,xlab='Year',ylab='Millions',main='Recruits age 0')
a <- lapply(retro,function(x) lines(as.numeric(colnames(x$catch.pred)),x$N.age[,1]/1000))

a <- SavePlot0('Retrospective',6,6)

#lan <- c(stock0@landings)
#dis <- c(stock0@discards)
catch <- c(asap$catch.obs)
catch.pred <- c(asap$catch.pred) # asap predicted catch
#catchInt <- c(landings(stf1)[,nyears+1]+discards(stf1)[,nyears+1]) # catch in intermediate year, assuming fsq
#landInt <- c(landings(stf1)[,nyears+1])
ssb <- c(asap$SSB)
#ssbInt <- c(ssb(stf1)[,nyears+1]) # ssb in intermediate year (1 jan)
tsb <- c(apply(asap$N.age * asap$WAA.mats$WAA.ssb,1,sum))
recr <- c(asap$N.age[,1])
fbar <- c(asap$F.report)
ssbSTD <- subset(asap.std,name=='SSB')$std # standard deviation
recrSTD <- subset(asap.std,name=='recruits')$std # standard deviation
fbarSTD <- subset(asap.std,name=='Freport')$std # standard deviation

par(mfrow=c(2,2))



xlim <- range(years)+0:1
ylim <- c(0,max(catch)/1000)
plot(NA,xlim=xlim,ylim=ylim,xlab='Year',ylab='Kt',main='Catch')
points(years,catch/1000)
lines(years,catch.pred/1000)
legend('topright',c('Observed','Predicted'),lty=c(NA,1),pch=c(1,NA),bty='n')
#lines(years,lan/1000,lty=2)
#points(max(years)+1,catchInt/1000)
#points(max(years)+1,landInt/1000,pch=2)
#legend('topright',c('Catch','Landings','Catch Fsq','Land Fsq'),lty=c(1,2,NA,NA),pch=c(NA,NA,1,2),bty='n',y.intersp=0.8)

ylim <- c(0,max(ssb+ssbSTD)/1000)
plot(NA,xlim=xlim,ylim=ylim,xlab='Year',ylab='Kt',main='SSB')
polygon(c(years,rev(years)),c((ssb-ssbSTD),rev((ssb+ssbSTD)))/1000,border=0,col='grey') 
lines(years,ssb/1000)
#lines(years,c(ssb(xsa+stock))/1000,col=2)
#legend('topright',c('asap','xsa'),lty=1,col=1:2,bty='n')
#points(max(years)+1,ssbInt/1000)
#legend('topleft',c('StDev',paste('1 jan',max(years)+1)),fill=c('grey',NA),border=NA,,pch=c(NA,1),bty='n')

ylim <- c(0,max(fbar,2))
plot(NA,xlim=xlim,ylim=ylim,xlab='Year',ylab='F',main=paste0('Fbar ',paste(range(fbarage),collapse='-')))
polygon(c(years,rev(years)),c((fbar-fbarSTD),rev((fbar+fbarSTD))),border=0,col='grey') 
lines(years,fbar)
#lines(years,c(fbar(xsa+stock)),col=2)
#legend('topright',c('asap','xsa'),lty=1,col=1:2,bty='n')

#points(max(years)+1,fsq)
#legend('bottomleft',c('StDev','Fsq'),pch=c(NA,1),fill=c('grey',NA),border=NA,bty='n')

ylim <- c(0,max(recr/1000))
plot(NA,xlim=xlim,ylim=ylim,xlab='Year',ylab='Millions',main='Recruits age 0')
polygon(c(years,rev(years)),c((recr-recrSTD),rev((recr+recrSTD)))/1000,border=0,col='grey') 
lines(years,recr/1000)
#lines(years,c(xsa@stock.n[1,])/1000,col=2)
#legend('topright',c('asap','xsa'),lty=1,col=1:2,bty='n')

#points(max(years)+1,GM/1000)
#legend('topleft',c('StDev','GM'),pch=c(NA,1),fill=c('grey',NA),border=NA,bty='n')

a <- SavePlot0('Summary',6,6)

#create the stock object...
la <- readVPAFile(taf.data.path("had7ALA.txt"))
ln <- readVPAFile(taf.data.path("had7ALN.txt"))
di<- readVPAFile(taf.data.path("had7ADI.txt"))
dn <- readVPAFile(taf.data.path("had7ADN.txt"))
dw <- readVPAFile(taf.data.path("had7ACW.txt"))
lw <- readVPAFile(taf.data.path("had7ACW.txt"))
sw <- readVPAFile(taf.data.path("had7ACW.txt"))
nm<-  readVPAFile(taf.data.path("had7ANM.txt"))
mo<-  readVPAFile(taf.data.path("had7AMO.txt"))
pf<-  readVPAFile(taf.data.path("had7APF.txt"))

pm<-  readVPAFile(taf.data.path("had7APM.txt"))
# compute catch
catch.n <- ln + dn
catch <- di + la

#tun <- readFLIndices(taf.data.path,("had7atun.txt"))
#for(i in 1:length(tun)) type(tun[[i]])<-"numbers" #make sure numbers are correct
#shortNames <- function(x){    #and create short names
#  x=strsplit(x,':')[[1]][1]
#  x=gsub(x,pattern=' ',replacement='')
#  x=gsub(x,pattern='-',replacement='')
#  x=gsub(x,pattern='&',replacement='')
#  x
#}
#tun <- lapply(tun,function(x) x=transform(x, name=shortNames(name(x))))
#names(tun) <- unlist(lapply(tun, name))
# create the FL stock object from input data
stock <- FLStock(ln)
landings(stock) <- la
discards(stock) <- di
catch(stock) <- la+di
landings.n(stock) <- ln
discards.n(stock)<- dn
catch.n(stock) <- dn+ln
landings.wt(stock) <- lw
discards.wt(stock) <- dw
catch.wt(stock) <- (lw*ln+dw*dn)/(ln+dn)
catch.wt(stock)[(ln+dn)==0] <- 0 # fix divide by zero
stock.wt(stock) <- sw
m(stock) <- nm
mat(stock) <- mo
harvest.spwn(stock) <- pf
m.spwn(stock) <- pm
range(stock)['minfbar'] <- 2
range(stock)['maxfbar'] <- 4
stock<-(trim(stock, year=1993:2023))
years <- asap$parms$styr:asap$parms$endyr
nyears <- length(years)
ages <- 1:asap$parms$nages -1 # note the age offset
nages <- length(ages)
fbarage <- asap$options$Freport.agemin:asap$options$Freport.agemax - 1
stock@stock.n@.Data <- array(t(asap$N.age),dim=c(nages,nyears,1,1,1,1))
stock@stock.wt@.Data<-array(t(asap$WAA.mats$WAA.ssb),dim=c(nages,nyears,1,1,1,1))
stock@harvest@.Data <- array(t(asap$F.age),dim=c(nages,nyears,1,1,1,1))
stock@harvest@units <- 'f'
c(catch(stock))-asap$catch.obs
# catch proportions-at-age
round(apply(catch.n(stock)@.Data,1,function(x) x/apply(catch.n(stock)@.Data,2,sum))-asap$catch.comp.mat$catch.fleet1.ob,5)
# catch wt
round(catch.wt(stock)[,,drop=T]-t(asap$WAA.mats$WAA.catch.all),3)
# stock wt
round(stock.wt(stock)[,,drop=T]-t(asap$WAA.mats$WAA.ssb),3)
# Natural mortality
round(m(stock)[,,drop=T]-t(asap$M.age),3)
# Maturity
round(mat(stock)[,,drop=T]-t(asap$maturity),3)
# N ages
diff(range(stock)[1:2])+1
asap$parms$nages
# N years
diff(range(stock)[4:5])+1
asap$parms$nyears
# Fbar
range(stock)[6:7]
c(asap$options$Freport.agemin,asap$options$Freport.agemax) -1 #note the offset
stock0 <- stock # keep a copy of the original stock object
stock@stock.n@.Data <- array(t(asap$N.age),dim=c(nages,nyears,1,1,1,1))
stock@harvest@.Data <- array(t(asap$F.age),dim=c(nages,nyears,1,1,1,1))
stock@harvest@units <- 'f'
stock@stock.wt<-stock@catch.wt

save(stock,file='output/Had7a_SO.Rdata')
########################################## now do forecast



Advice<- 2263 #advice for intermediate year
TAC<- 2263 #TAC for intermediate year

#TACc<-  3156-668
FMSY<-0.28
FMSY_L<-0.20
FMSY_U<-0.35
BLIM<-2994
BPA<-4160
FLIM<-0.50
FPA<-0.41
BTrig<-4281

p <- apply(landings.n(stock)/catch.n(stock),1,mean,na.rm=T)
p <- c(ifelse(is.na(p),0,p))
#### set up for the catch scenarios table
catchoptions <- function(Basis='',SSBint=NULL,TAC=NULL) {
  out <- data.frame(
    Basis
    ,Catch=round(c(landings(stf1)[,nyears+2]+discards(stf1)[,nyears+2]))
    ,Land=round(c(landings(stf1)[,nyears+2]))
    ,Dis=round(c(discards(stf1)[,nyears+2]))
    ,FCatch=round(mean(harvest(stf1)[as.character(fbarage),nyears+2]),5)
    ,FLand=round(mean((harvest(stf1)*landings.n(stf1)/catch.n(stf1))[as.character(fbarage),nyears+2]),5)
    ,FDis=round(mean((harvest(stf1)*discards.n(stf1)/catch.n(stf1))[as.character(fbarage),nyears+2]),5)
    ,SSB=round(c(ssb(stf1)[,nyears+3]),5)
    ,dSSB=paste0(round(100*(round(c(ssb(stf1)[,nyears+3]),5)-SSBint)/SSBint,5),"%")
    ,dTac=round(100*(round(c(landings(stf1)[,nyears+2]+discards(stf1)[,nyears+2]))-TAC)/TAC,5)
    ,dAdvice=round(100*(round(c(landings(stf1)[,nyears+2]+discards(stf1)[,nyears+2]))-Advice)/Advice,5)
  )
  names(out) <- paste0(names(out),c(rep(max(years)+2-2000,3),'',rep(max(years)+2-2000,3),max(years)+3-2000,'',''))
  return(out)
}

#Set up an empty stf object and feed it status-quo recruitment
stf0 <- stf(stock,nyears=3, wts.nyears=3, fbar.nyears=3)

# geometric mean recruitment, minus last 2 years
GM <- round(exp(mean(log(asap$N.age[1:(nyears-2),1]))),0)
stock.n(stf0)[1,nyears+1] <- GM
stock.n(stf0)[1,nyears+2] <- GM
stock.n(stf0)[1,nyears+3] <- GM

#Create  FLSR object (now necsessary for GM recruitment, since project was changed to fwd and fwdcontrol)

srr <- as.FLSR(stf0, model="geomean")
params(srr)['a',] <- GM
params(srr)

#Calculate F status quo, (last 3 years)
fsq1<-mean(harvest(stf0)[as.character(fbarage),ac(2021)])
fsq2<-mean(harvest(stf0)[as.character(fbarage),ac(2022)])
fsq3<-mean(harvest(stf0)[as.character(fbarage),ac(2023)])
fsq<-(fsq1+fsq2+fsq3)/3
ctrl <- fwdControl(data.frame(year=max(years)+1:3,val=c(fsq,fsq,0),quantity=c('f','f','f')))
#ctrl <- stf(data.frame(year=max(years)+1:3,val=c(fsq,fsq,0),quantity=c('f','f','f')))
stf1  <- fwd(stf0, ctrl=ctrl,sr=srr)

#Summarise the basis for the catch options
round(fsq,3) # F status-quo (assumption for intermediate year)
SSBint <- round(c(ssb(stf1)[,nyears+2]),0)
SSBint #SSB at the start of the year after the intermediate year (or the end of the intermediate year)
GM # recruitment assumption (geometric mean)
round(c(landings(stf1)[,nyears+1]))+round(c(discards(stf1)[,nyears+1])) # catch intermediate year
round(c(landings(stf1)[,nyears+1])) # landings intermediate year
round(c(discards(stf1)[,nyears+1])) # discards intermediate year
F2024<-mean(harvest(stf1)[as.character(fbarage),nyears])
F2025<-mean(harvest(stf1)[as.character(fbarage),nyears+1])

#management options table
out <- NULL
ctrl <- fwdControl(data.frame(year=max(years)+1:3,val=c(fsq,FMSY_L,0),quantity=c('f','f','f')))
stf1  <- fwd(stf0, ctrl=ctrl, sr=srr)
out <- rbind(out,catchoptions('FMSYLower',SSBint,TAC))

fbar(stf1)

ctrl <- fwdControl(data.frame(year=max(years)+1:3,val=c(fsq,FMSY_U,0),quantity=c('f','f','f')))
stf1  <- fwd(stf0, ctrl=ctrl, sr=srr)
out <- rbind(out,catchoptions('FMSYUpper',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,val=c(fsq,FMSY,0),quantity=c('f','f','f')))
stf1  <- fwd(stf0, ctrl=ctrl, sr=srr)
out <- rbind(out,catchoptions('FMSY',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,val=c(fsq,0,0),quantity=c('f','f','f')))
stf1  <- fwd(stf0, ctrl=ctrl, sr=srr)
out <- rbind(out,catchoptions('F = 0',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,val=c(fsq,FPA,0),quantity=c('f','f','f')))
stf1  <- fwd(stf0, ctrl=ctrl, sr=srr)
out <- rbind(out,catchoptions('F = Fpa',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,val=c(fsq,FLIM,0),quantity=c('f','f','f')))
stf1  <- fwd(stf0, ctrl=ctrl, sr=srr)
out <- rbind(out,catchoptions('F = Flim',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,val=c(fsq,BLIM,0),quantity=c('f','ssb','f')))
stf1  <- fwd(stf0, ctrl=ctrl, sr=srr)
out <- rbind(out,catchoptions('Blim',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,val=c(fsq,BPA,0),quantity=c('f','ssb','f')))
stf1  <-fwd(stf0, ctrl=ctrl, sr=srr)
out <- rbind(out,catchoptions('Bpa',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,val=c(fsq,BTrig,0),quantity=c('f','ssb','f')))
stf1  <- fwd(stf0, ctrl=ctrl, sr=srr)
out <- rbind(out,catchoptions('Btrig',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,val=c(fsq,fsq,0),quantity=c('f','f','f')))
stf1  <- fwd(stf0, ctrl=ctrl, sr=srr)
out <- rbind(out,catchoptions('Fsq',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,val=c(fsq,SSBint,0),quantity=c('f','ssb','f')))
stf1  <- fwd(stf0, ctrl=ctrl, sr=srr)
out <- rbind(out,catchoptions('SSB2025=SSB2026',SSBint,TAC))

ctrl <- fwdControl(data.frame(year=max(years)+1:3,val=c(fsq,TAC,0),quantity=c('f','catch','f')))
stf1  <- fwd(stf0, ctrl=ctrl, sr=srr)
out <- rbind(out,catchoptions('TAC Rollover',SSBint,TAC))
out$FLand24[out$FLand25=="NaN"]<-0
out$FDis24[out$FDis25=="NaN"]<-0

write.csv(out,'output/Had7a_ManOpts.csv',row.names=F)

#Input tables
ctrl <- fwdControl(data.frame(year=max(years)+1:3,val=c(fsq,fsq,fsq),quantity=c('f','f','f')))
stf1  <- fwd(stf0, ctrl=ctrl, sr=srr) # status-quo forecast

stfin <- function(i){
  out <- data.frame(Age=ages
                    ,N=round(c(stock.n(stf1)[,i]))
                    ,M=c(m(stf1)[,i])
                    ,Mat=c(mat(stf1)[,i])
                    ,PF=c(harvest.spwn(stf1)[,i])
                    ,PM=c(m.spwn(stf1)[,i])
                    ,SWt=round(c(stock.wt(stf1)[,i]),3)
                    ,Sel=round(c(harvest(stf0)[,i]*p),3)
                    ,CWt=round(c(landings.wt(stf1)[,i]),3)
                    ,DSel=round(c(harvest(stf0)[,i]*(1-p)),3)
                    ,DCWt=round(c(discards.wt(stf1)[,i]),3)
  )
  return(out)
}

stfin1 <- stfin(nyears+1)
stfin2 <- stfin(nyears+2)
stfin3 <- stfin(nyears+3)



write.csv(stfin1,'output/had7a_stfin1.csv',row.names=F)
write.csv(stfin2,'output/had7a_stf_stfin2.csv',row.names=F)
write.csv(stfin3,'output/had7a_stf_stfin3.csv',row.names=F)


stfout <- function(i){
  out <- data.frame(Age=ages
                    ,F=round(c(harvest(stf1)[,i])*p,3)
                    ,CatchNos=round(c(landings.n(stf1)[,i]))
                    ,Yield=round(c((landings.n(stf1)*landings.wt(stf1))[,i]),0)
                    ,DF=round(c(harvest(stf1)[,i])*(1-p),3)
                    ,DCatchNos=round(c(discards.n(stf1)[,i]))
                    ,DYield=round(c((discards.n(stf1)*discards.wt(stf1))[,i]),0)
                    ,StockNos=round(c(stock.n(stf1)[,i]))
                    ,Biomass=round(c((stock.n(stf1)*stock.wt(stf1))[,i]))
                    ,SSNos=round(c((stock.n(stf1)*mat(stf1))[,i]))
                    ,SSB=round(c((stock.n(stf1)*stock.wt(stf1)*mat(stf1))[,i])),3
  )
  out <- rbind(out,colSums(out))
  nrows <- nrow(out)
  out[nrows,1] <- 'Total'
  out[nrows,2] <- round(mean((harvest(stf1)[,i]*p)[as.character(fbarage)]),3)
  out[nrows,5] <- round(mean((harvest(stf1)[,i]*(1-p))[as.character(fbarage)]),3)
  return(out)
}

stfout1 <- stfout(nyears+1)
stfout2 <- stfout(nyears+2)
stfout3 <- stfout(nyears+3)


write.csv(stfout1,'output/had7a_stf_stfout1.csv',row.names=F)
write.csv(stfout2,'output/had7a_stf_stfout2.csv',row.names=F)
write.csv(stfout3,'output/had7a_stf_stfout3.csv',row.names=F)

#### Contribution plot

par(mfrow=c(1,2),mar=c(5,8,4,1),cex=0.8)
nrows <- nrow(stfout2)
yield <- stfout2[-nrows,'Yield']
prop <- paste0(round(100*yield/sum(yield)),'%')
labels <- paste(max(years)-ages+2,rep(c('GM','ASAP'),c(2,nages-2)))
b <- barplot(yield,horiz=T,names=labels,las=1,xlab='Tonnes',main=paste('Landings yield',max(years)+2),xlim=c(0,max(yield, na.rm = T)*1.25))
text(yield,b,prop,adj=-0.2)

ssb <- stfout3[-nrows,'SSB']
prop <- paste0(round(100*ssb/sum(ssb)),'%')
labels <- paste(max(years)-ages+3,rep(c('GM','ASAP'),c(3,nages-3)))
b <- barplot(ssb,horiz=T,names=labels,las=1,xlab='Tonnes',main=paste('SSB',max(years)+3),xlim=c(0,max(ssb,na.rm = T)*1.25))
text(ssb,b,prop,adj=-0.2)
a <- SavePlot0('stf_contrib',6,3)

#### Summary tables

lan <- c(stock0@landings)
dis <- c(stock0@discards)
catch <- c(asap$catch.obs)
catch.pred <- c(asap$catch.pred) # asap predicted catch
catchInt <- c(landings(stf1)[,nyears+1]+discards(stf1)[,nyears+1]) # catch in intermediate year, assuming fsq
landInt <- c(landings(stf1)[,nyears+1])
ssb <- c(asap$SSB)
ssbInt <- c(ssb(stf1)[,nyears+1]) # ssb in intermediate year (1 jan)
tsb <- c(apply(asap$N.age * asap$WAA.mats$WAA.ssb,1,sum))
recr <- c(asap$N.age[,1])
fbar <- c(asap$F.report)
ssbSTD <- subset(asap.std,name=='SSB')$std # standard deviation
recrSTD <- subset(asap.std,name=='recruits')$std # standard deviation
fbarSTD <- subset(asap.std,name=='Freport')$std # standard deviation

asap.sum1 <- data.frame(Year=c(years,paste0(max(years)+1,'*'))
                        ,Lan=c(lan,NA)
                        ,Dis=c(dis,NA)
                        ,Cat=c(catch,NA)
                        ,CatPred=c(catch.pred,NA)
                        ,Tsb=c(tsb,NA)
                        ,Ssb=c(ssb,ssbInt)
                        ,SsbCv=c(ssbSTD/ssb,NA)
                        ,Recr=c(recr,GM)
                        ,RecrCv=c(recrSTD/recr,NA)
                        ,Fbar=c(fbar,fsq)
                        ,FbarCv=c(fbarSTD/fbar,NA)
)

write.csv(asap.sum1,'output/had7a_asap_summary1.csv',row.names=F)

asap.sum2 <- data.frame(year=c(years,max(years+1))
                        ,recrlo=c(recr-recrSTD*1.96,NA)
                        ,recr=c(recr,GM)
                        ,recrhi=c(recr+recrSTD*1.96,NA)
                        ,tsblo=NA
                        ,tsb=c(tsb,NA)
                        ,tsbhi=NA
                        ,ssblo=c(ssb-ssbSTD*1.96,NA)
                        ,ssb=c(ssb,ssbInt)
                        ,ssbhi=c(ssb+ssbSTD*1.96,NA)
                        ,catch=c(catch,NA)
                        ,lan=c(lan,NA)
                        ,dis=c(dis,NA)
                        ,ibc=NA
                        ,ur=NA
                        ,yssb=NA
                        ,flo=c(fbar-fbarSTD*1.96,NA)
                        ,f=c(fbar,fsq)
                        ,fhi=c(fbar+fbarSTD*1.96,NA)
)

write.csv(asap.sum2,'output/had7a_asap_summary2.csv',row.names=F)

asap.sen <- data.frame(Age=ages
                       ,M=c(apply(m(stf0)[,nyears+1:3],1,mean))
                       ,Mat=c(apply(mat(stf0)[,nyears+1:3],1,mean))
                       ,PF=c(apply(harvest.spwn(stf0)[,nyears+1:3],1,mean))
                       ,PM=c(apply(m.spwn(stf0)[,nyears+1:3],1,mean))
                       ,Sel=c(apply(harvest(stf0)[,nyears+1:3],1,mean))*p
                       ,WeCa=c(apply(landings.wt(stf0)[,nyears+1:3],1,mean))
                       ,Fd=c(apply(harvest(stf0)[,nyears+1:3],1,mean))*(1-p)
                       ,WeCad=c(apply(discards.wt(stf0)[,nyears+1:3],1,mean))
                       ,Fi=0
                       ,WeCai=0
)

write.csv(asap.sen,'output/had7a_asap_sen.csv',row.names=F)

#### Summary plot

par(mfrow=c(2,2))

xlim <- range(years)+0:1
ylim <- c(0,max(catch)/1000)
plot(NA,xlim=xlim,ylim=ylim,xlab='Year',ylab='Kt',main='Catch')
lines(years,catch/1000)
lines(years,lan/1000,lty=2)
points(max(years)+1,catchInt/1000)
points(max(years)+1,landInt/1000,pch=2)
legend('topleft',c('Catch','Landings','Catch Fsq','Land Fsq'),lty=c(1,2,NA,NA),pch=c(NA,NA,1,2),bty='n',y.intersp=0.8)

ylim <- c(0,max(ssb+ssbSTD,ssbInt)/1000)
plot(NA,xlim=xlim,ylim=ylim,xlab='Year',ylab='Kt',main='SSB')
polygon(c(years,rev(years)),c((ssb-ssbSTD),rev((ssb+ssbSTD)))/1000,border=0,col='grey') 
lines(years,ssb/1000)
points(max(years)+1,ssbInt/1000)
legend('topleft',c('StDev',paste('1jan',max(years)+1)),fill=c('grey',NA),border=NA,,pch=c(NA,1),bty='n')

ylim <- c(0,max(fbar+fbarSTD))
plot(NA,xlim=xlim,ylim=ylim,xlab='Year',ylab='F',main=paste0('Fbar ',paste(range(fbarage),collapse='-')))
polygon(c(years,rev(years)),c((fbar-fbarSTD),rev((fbar+fbarSTD))),border=0,col='grey') 
lines(years,fbar)
points(max(years)+1,fsq)
legend('bottomleft',c('StDev','Fsq'),pch=c(NA,1),fill=c('grey',NA),border=NA,bty='n')

ylim <- c(0,max(recr/1000))
plot(NA,xlim=xlim,ylim=ylim,xlab='Year',ylab='Millions',main='Recruits age 0')
polygon(c(years,rev(years)),c((recr-recrSTD),rev((recr+recrSTD)))/1000,border=0,col='grey') 
lines(years,recr/1000)
points(max(years)+1,GM/1000)
legend('topleft',c('StDev','GM'),pch=c(NA,1),fill=c('grey',NA),border=NA,bty='n')

a <- SavePlot0('Summary',6,6)
# finally reset the working directory to the project root
setwd("..")

