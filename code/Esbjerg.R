setwd("~/GitHub/SeaLevelDecisions/code")
require(maps)
require(maptools)
require(ncdf4)
require(forecast)
require(excursions)
require(Matrix)
require(chron)
require(zoo)
source('~/GitHub/SeaLevelProjections/Sealevel_functions.R')
source('~/GitHub/SeaLevelDecisions/code/ci_bergen.R')
denmark=read.table("~/GitHub/SeaLevelProjections/denmark_sea_level/denmark.PSMSL.txt")#All Danish sea level stations with current data]
#from PSMSL.org
denmark[,1]=as.character(denmark[,1])

denmarkurl=paste("http://www.psmsl.org/data/obtaining/rlr.monthly.data/",denmark[,2],".rlrdata",sep="")

N=length(denmark[,2])
denmarkdata=list(N,NULL)
for(i in 1:N){

	tmp=read.table(denmarkurl[i],na.strings="-99999",sep=";")
	tmp=cbind(floor(tmp[,1]),round(12*(tmp[,1]-floor(tmp[,1]))+.5),tmp[,2:4])

	if(tmp[1,2]>1) tmp=tmp[-(1:(13-tmp[1,2])),]#start at first full year
	#if(tmp[1,2]>1) tmp=tmp[-(1:(13-tmp[1,2])),]#start at first full year
	NN=dim(tmp)[1]
	if(tmp[NN,2]<12) tmp=tmp[-((NN+1-tmp[NN,2]):N),]#delete final year if not full
	tmp.rlr=tmp[,3]
	tmp.rlr=matrix(tmp.rlr,ncol=12,byrow=T)
	tmp.mp=medpolish(tmp.rlr,na.rm=T)
	tmp.mppred=(tmp.mp$overall+outer(tmp.mp$row,tmp.mp$col,"+"))
	for(j in 1:12) tmp.rlr[which(is.na(tmp.rlr[,j])),j]=tmp.mppred[which(is.na(tmp.rlr[,j])),j]
	tmp.ann=apply(tmp.rlr,1,"mean")/10 #cm
	tmp.yrs=unique(floor(tmp[,1]))
	tmp.ann=tmp.ann-mean(tmp.ann[which(is.element(tmp.yrs,1970:1999))],na.rm=T)
	denmarkdata[i]=list(data.frame(years=unique(floor(tmp[,1])),data=tmp.ann))
	}
	
	gia.DK=giase.DK=rep(NA,N)
	index=c(1,2,4,5,6,14) #with gia 
	gia.DK[index]=c(-.012,-.306,-.028,-.009,-.010,0.06)
	giase.DK[index]=c(.01,.14,.008,.01,.01,.03)

#Get global model, data and CMIP5 output 
load('~/GitHub/SeaLevelProjections/data&global.Rdata')

pdf("esbjerg_90.pdf")
par(mfrow=c(1,2),pty="s")
i=14
m=length(denmarkdata[[i]]$data)-1
 reg=0:m
 adj=mean(reg[which(is.element(denmarkdata[[i]]$years,1970:1999))])
reg=reg-adj
	newdata=denmarkdata[[i]]$data+gia.DK[i]*reg
	
	xlim=range(denmarkdata[[i]]$years)
	ylim=range(c(denmarkdata[[i]]$data,newdata))

plot(xlim,ylim,type="n",xlab="Year",ylab="Sea level anomaly (cm)")
lines(denmarkdata[[i]]$years,denmarkdata[[i]]$data,lwd=2)
lines(denmarkdata[[i]]$years,newdata,lwd=2,col="red")

	sta=list(data=denmarkdata[[i]]$data,name=denmark[i,1],years=denmarkdata[[i]]$years,gia=gia.DK[i],se.gia=giase.DK[i])
	
	###
### Estimate local relationship from data
###

#common years to use for estimating local relationship

tmp=intersect(sta$years,seal$years)
sea.common.years=which(is.element(seal$years,tmp)) 
sta.common.years=which(is.element(sta$years,tmp))

sta.un=sta$data #uncorrected station series
stafit=arsta(seal,sta)

summary(stafit$ar)

plot(seal$sealevel[sea.common.years],stafit$data[sta.common.years],xlab="Global sea level anomaly (cm)",ylab="Local sea level anomaly (cm)")

order=stafit$ar$arma[1]+stafit$ar$arma[2]
ifelse(length(stafit$ar$coef)-order==1,
{
a3= 0 #intercept 0
a4 = stafit$ar$coef[order+1] 
} 
,
{
	a3=stafit$ar$coef[order+1]
a4=stafit$ar$coef[order+2] })

abline(a3,a4,lwd=2)
	
models=models8.5
K=length(models) 
begin=max(min(sta.common.years),which(is.element(sta$years,1950)))#index of1950 or earliest available date thereafter
alpha=0.1
cis=ci_bergen(models,sta,stafit,sta.common.years,alpha)
par(mfrow=c(1,1))
	xlim=c(1950,2100)
	ylim=c(-25,115)

par(mfrow=c(1,1))
plot(xlim,ylim,xlim=xlim,ylim=ylim,type='n',xlab="Year",	ylab="Anomaly (cm)",main="RCP 8.5")
lines(sta$years[begin:max(sta$years)],sta$data[begin:max(sta$years)],col='blue')

for(ii in 1:K){
	lines(2000:2100,cis$pred[[ii]],col=2,lwd=0.5)
}

lines(2000:2100,cis$res$b,lwd=2)
lines(2000:2100,cis$res$a,lwd=2)
lines(2000:2100,cis$res$a.marginal,col="purple",lty=3)
lines(2000:2100,cis$res$b.marginal,col="purple",lty=3)

F.20 = F.40 = rep(0,101)
for(k in 1:K){
	F.20 = F.20 + cis$res.20[[k]]$F/K
	F.40 = F.40 + cis$res.40[[k]]$F/K
}

plot(range(2000,2100),c(0,1),type="n",col=1,xlab="Year",ylab="Probability",main="P(Y(s)<20, s<t)")
lines(2000:2100,F.20,col=4,lwd=2)
lines(2000:2100,F.40,col=2,lwd=2)
legend(2000,0, c("20 cm","40 cm"), 
lty=c(1,1), lwd=c(2.5,2.5),col=c(2,4),xjust=0,yjust=0) 


mu=NULL
for (i in 1:K)
mu=rbind(mu,cis$pred[[i]])
x = seq(from=0,to=50,length.out = 1000)
year = 51


plot(c(0,50), c(0,0.25), xlab="Anomaly (cm)",ylab="Density",main="Esbjerg 2050 RCP 8.5",type="n")
v1=apply(matrix(unlist(cis$pred),ncol=101,byrow=T),2,median)[year]
segments(v1,0,v1,0.25,lwd=2)
hist(mu[,year],xlim=c(0,50), breaks=(0:51), ylim=c(0,0.50),freq=FALSE,add=T,lwd=2)

lines(x,Fmix.pdf(x,matrix(unlist(cis$pred),ncol=101,byrow=T)[,year],cis$sd.g[year]),col="red",lwd=2)
lines(x,Fmix.pdf(x,matrix(unlist(cis$pred),ncol=101,byrow=T)[,year],cis$sd[year]),col="blue",lwd=2)
dev.off()

