ubase='http://www.cherryblossom.org/results/'
mURLs=c('1999/cb99m.html','2001/oof_m.html','2002/oofm.htm','2003/CB03-M.HTM','2004/men.htm','2005/CB05-M.htm','2006/men.htm','2007/men.htm','2008/men.htm','2010/2010cucb10m-m.htm','2011/2011cucb10m-m.htm','2012/2012cucb10m-m.htm')
wURLs=append(x=mURLs,values='2009/09cucb-F.htm',after=9)
MtoWp=c('m\\.','M\\.','men\\.'); MtoWr=c('f\\.','F\\.','women\\.')
for(j in 1:3) wURLs=sub(pattern=MtoWp[j],replacement=MtoWr[j],x=wURLs)
allURLs=paste(ubase,c(mURLs,wURLs),sep='')
mFiles=c('cb99',paste('cb',1:8,sep='0'),'cb10','cb11','cb12')
mFiles=paste(mFiles,'-m.txt',sep='')
wFiles=sub(pattern='m',replacement='w',x=mFiles)
wFiles=append(x=wFiles,values='cb09-w.txt',after=9)
library('XML')
extractResTable=function(turl,fileName){
  doc=htmlParse(turl)
  preNode=getNodeSet(doc,path='//pre')
  txt=xmlValue(preNode[[1]])
  els=strsplit(txt,split='\\r\\n')[[1]]
  return(els)}
allEls=mapply(extractResTable,allURLs,c(mFiles,wFiles))
doc00m=htmlParse(paste(ubase,'2000/Cb003m.htm',sep='')); doc00f=htmlParse(paste(ubase,'2000/Cb003f.htm',sep='')) 
fN00m=getNodeSet(doc00m,path='//font'); fN00f=getNodeSet(doc00f,path='//font')
txt00m=xmlValue(fN00m[[4]]); txt00f=xmlValue(fN00f[[4]])
allEls[[26]]=strsplit(txt00m,split='\\r\\n')[[1]]; allEls[[27]]=strsplit(txt00f,split='\\r\\n')[[1]]
doc=htmlParse(paste(ubase,'2009/09cucb-M.htm',sep=''))
nodes09=getNodeSet(doc,path='//div/pre') 
rawtxt=lapply(nodes09,xmlSApply,xmlValue)
allEls[[28]]=sapply(rawtxt,paste,collapse='')
allEls[[14]][2:3]=allEls[[2]][4:5]
spRow=allEls[[4]][3]
spRow=paste(substr(spRow,1,22),substring(spRow,23),sep='=')
allEls[[4]][3]=spRow; allEls[[16]][3]=spRow
spRow=allEls[[7]][8]
spRow=paste(substr(spRow,1,6),substring(spRow,7),sep='=')
allEls[[7]][8]=spRow; allEls[[19]][8]=spRow
clean06=function(runner){
  ind=regexpr(pattern='\\d+/\\d+',text=runner)
  if(attr(ind,'match.length')<9) runner=paste(substr(runner,1,5),substring(runner,6),sep=' ')
  return(runner) }
allEls[[7]][9:5243]=sapply(allEls[[7]][9:5243],clean06,USE.NAMES=F)
allEls[[19]][9:5443]=sapply(allEls[[19]][9:5443],clean06,USE.NAMES=F)
spRow=paste(substr(spRow,1,64),substring(spRow,66),sep=' ')
hdRow=allEls[[7]][7]
hdRow=paste(substr(hdRow,1,5),substring(hdRow,6),sep=' ')
allEls[[7]][7:8]=c(hdRow,spRow); allEls[[19]][7:8]=c(hdRow,spRow)
allEls=mapply(FUN=grep,pattern='^\\s*$|^[#*]',x=allEls,value=T,invert=T)
allEls=mapply(FUN=gsub,pattern='\u00A0|\u0060|[*#+]',replacement=' ',x=allEls)
allEls=allEls[c(1,26,2:9,28,10:13,27,14:25)]
yr=substr(as.character(1999:2012),3,4)
mFileNames=paste('cb',yr,'-m.txt',sep='')
wFileNames=paste('cb',yr,'-w.txt',sep='')
names(allEls)=substr(c(mFileNames,wFileNames),12,17)
save(allEls,file='CherryBlossom.RData')
invisible(mapply(FUN=writeLines,text=allEls,con=c(mFileNames,wFileNames)))
shortCols=c('name','home','ag','gun','net','time','5 mi')
extractVars=function(fi){
  findColLocs=function(spRow){
    spLocs=gregexpr(' ',spRow)[[1]]
    rowLen=nchar(spRow)
    if(substring(text=spRow,first=rowLen)!=' ')
      return(c(0,spLocs,rowLen+1))
    else return(c(0,spLocs)) }
  selectCols=function(colNames,hdRow,schLocs){
    helper=function(name){
      startPos=regexpr(name,hdRow)[[1]]
      if(startPos==-1) return(c(NA,NA))
      ind=sum(startPos>=schLocs)
      c(schLocs[ind]+1,schLocs[ind+1]-1) }
    sapply(colNames,helper) }
  els=readLines(fi)
  eqI=grep(pattern='^=',x=els)
  spRow=els[eqI]; hdRow=tolower(els[eqI-1]); body=els[-(1:eqI)]
  locCol=selectCols(shortCols,hdRow,findColLocs(spRow))
  Values=mapply(substr,x=list(body),start=locCol[1,],stop=locCol[2,])
  colnames(Values)=shortCols
  invisible(Values) }
createDF=function(Res,year){
  convertTime=function(x){
    Spcs=strsplit(x,split=':')
    pcs=sapply(Spcs,as.numeric)
    helper=function(y){
      if(length(y)==2) y[1]+y[2]/60
      else 60*y[1]+y[2]+y[3]/60 }
    sapply(pcs,helper,USE.NAMES=F) }
  useTime=if(!is.na(Res[1,'net'])) Res[,'net']
  else if(!is.na(Res[1,'gun'])) Res[,'gun']
  else Res[,'time']
  ind=grep(pattern='\\d',x=useTime)
  Res=Res[ind,]
  runTime=convertTime(useTime[ind])
  halfTime=convertTime(Res[,'5 mi'])
  Results=data.frame(year=rep(year,nrow(Res)),name=Res[,'name'],home=Res[,'home'],age=as.numeric(Res[,'ag']),halfTime,runTime,stringsAsFactors=F)
  invisible(Results) }
mResMat=lapply(mFileNames,extractVars)
wResMat=lapply(wFileNames,extractVars)
mDF=mapply(FUN=createDF,Res=mResMat,year=1999:2012,SIMPLIFY=F); wDF=mapply(FUN=createDF,Res=wResMat,year=1999:2012,SIMPLIFY=F)
mCB=do.call(rbind,mDF); wCB=do.call(rbind,wDF)
ReviseStr=function(x){
  x=tolower(x)
  x=gsub(pattern='^\\s+|\\s+$|[.,/]',replacement='',x)
  x=gsub(pattern='\\s+',replacement=' ',x)
  return(x) }
mCB$home=ReviseStr(mCB$home)
wCB$home=ReviseStr(wCB$home)
library(dplyr)
mCB=na.omit(mCB) %>% filter(grepl('\\w+$',home) & halfTime>20)
wCB=na.omit(wCB) %>% filter(grepl('\\w+$',home) & halfTime>20)
getState=function(home){
  mch=regexpr('\\w+$',home)
  return(regmatches(home,m=mch)) }
mCB=mutate(mCB,state=getState(home))
wCB=mutate(wCB,state=getState(home))
catState=function(state){
  if(state %in% c('dc','md','va')) return('close')
  if(nchar(state)<3) return('us-far')
  return('intl') }
mCB$where=factor(sapply(mCB$state,catState))
wCB$where=factor(sapply(wCB$state,catState))
alRamp=function(c1,c2,alpha=128){
  stopifnot(alpha>=0 & alpha<=256)
  function(n) paste(colorRampPalette(c(c1,c2))(n),format(as.hexmode(alpha),upper.case=T),sep="") }
filter(wCB,where=='close') %>% select(halfTime,runTime) %>% smoothScatter(colramp=alRamp('white','blue'),col='blue',nrpoints=270,xlab='5 mile time',ylab='10 mile time',xlim=c(20,100),ylim=c(50,175))
title(main='10 vs 5 Mile Time by Location')
par(new=T)
filter(wCB,where=='us-far') %>% select(halfTime,runTime) %>% smoothScatter(colramp=alRamp('white','green'),col='green',nrpoints=60,axes=F,ann=F)
points(runTime~halfTime,data=wCB,subset=where=='intl',col='red',pch=16,cex=0.5)
abline(a=0,b=2)
legend('topleft',fill=c('blue','green','red'),legend=c('DC/MD/VA','US: other','Intl.'),bty='n')
wCB=filter(wCB,!where=='intl')
set.seed(1)
ind=sample(1:33521,size=2e3)
AgeTime=as.matrix(wCB[ind,c(4,6)])
ratio=wCB$runTime/wCB$halfTime; ratio=ratio[ind]
library(fields)
TpsMod=Tps(x=AgeTime,Y=ratio)
viz=predictSurface(TpsMod)
plot.surface(viz,type='C',xlab='Age',ylab='10 mile time')
title(main='Smoothed Ratio from Age and 10 Mile Time')