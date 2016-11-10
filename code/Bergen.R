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
norway=read.table("~/GitHub/SeaLevelProjections/Norway_sea_level/Norway.PSMSL.txt")#All Norwegian sea level stations with current data
#from PSMSL.org
norway[,1]=as.character(norway[,1])


norwayurl=paste("http://www.psmsl.org/data/obtaining/rlr.monthly.data/",norway[,2],".rlrdata",sep="")

N=length(norway[,2])
norwaydata=list(N,NULL)

for (i in 1:N){
	tmp=read.table(norwayurl[i],na.strings="-99999",sep=";")
	tmp=cbind(floor(tmp[,1]),round(12*(tmp[,1]-floor(tmp[,1]))+.5),tmp[,2:4])

	if(tmp[1,2]>1) tmp=tmp[-(1:(13-tmp[1,2])),]#start at first full year
	if(tmp[1,2]>1) tmp=tmp[-(1:(13-tmp[1,2])),]#start at first full year
	NN=dim(tmp)[1]
	if(tmp[NN,2]<12) tmp=tmp[-((NN+1-tmp[NN,2]):NN),]#delete final year if not full
	tmp.rlr=tmp[,3]
	tmp.rlr=matrix(tmp.rlr,ncol=12,byrow=T)
	tmp.mp=medpolish(tmp.rlr,na.rm=T)
	tmp.mppred=(tmp.mp$overall+outer(tmp.mp$row,tmp.mp$col,"+"))
	for(j in 1:12) tmp.rlr[which(is.na(tmp.rlr[,j])),j]=tmp.mppred[which(is.na(tmp.rlr[,j])),j]
	tmp.ann=apply(tmp.rlr,1,"mean")/10 #cm
	tmp.yrs=unique(floor(tmp[,1]))
	tmp.ann=tmp.ann-mean(tmp.ann[which(is.element(tmp.yrs,1970:1999))],na.rm=T)
	norwaydata[i]=list(data.frame(years=tmp.yrs,data=tmp.ann))
	}
	

#deal with Bergen missing data 1942-43
#use average difference between Bergen and all stations
#in 1941 and 1944 to estimate vaues for 1942 and 1943

tmp0=tmp1=tmp2=tmp3=NULL
for (i in 1:N)
{
	tmp0=c(tmp0,norwaydata[[i]]$data[which(is.element(norwaydata[[i]]$years,1941))])
tmp1=c(tmp1,norwaydata[[i]]$data[which(is.element(norwaydata[[i]]$years,1942))])
tmp2=c(tmp2,norwaydata[[i]]$data[which(is.element(norwaydata[[i]]$years,1943))])
tmp3=c(tmp3,norwaydata[[i]]$data[which(is.element(norwaydata[[i]]$years,1944))])
}
m0=mean(tmp0,na.rm=T)
m1=mean(tmp1,na.rm=T)
m2=mean(tmp2,na.rm=T)
m3=mean(tmp3,na.rm=T)
adj=mean(c(norwaydata[[16]]$data[which(is.element(norwaydata[[16]]$years,1941))]-m0,norwaydata[[16]]$data[which(is.element(norwaydata[[16]]$years,1944))]-m3))
norwaydata[[16]]$data[which(is.element(norwaydata[[16]]$years,1942))]=m1-adj
norwaydata[[16]]$data[which(is.element(norwaydata[[16]]$years,1943))]=m2-adj

index=c(2,4,8,9,11,13,16,20)#sufficient data + having gia 
gia.NO=rep(NA,N)
giase.NO=rep(NA,N)
gia.NO[index]=c(0.17,0.27,0.26,0.42,0.39,0.18,0.26,0.54)
giase.NO[index]=c(0.06,0.029,0.114,0.031,0.033,0.06,0.07,0.06)
#Get global model, data and CMIP5 output 
load('~/GitHub/SeaLevelProjections/data&global.Rdata')

pdf("bergen_90.pdf")
par(mfrow=c(1,2),pty="s")
i=16

	m=length(norwaydata[[i]]$data)-1
 reg=0:m
 adj=mean(reg[which(is.element(norwaydata[[i]]$years,1970:1999))])
reg=reg-adj
	newdata=norwaydata[[i]]$data+gia.NO[i]*reg
	
	xlim=range(norwaydata[[i]]$years)
	ylim=range(c(norwaydata[[i]]$data,newdata))

plot(xlim,ylim,type="n",xlab="Year",ylab="Sea level anomaly (cm)")
lines(norwaydata[[i]]$years,norwaydata[[i]]$data,lwd=2)
lines(norwaydata[[i]]$years,newdata,lwd=2,col="red")

	sta=list(data=norwaydata[[i]]$data,name=norway[i,1],years=norwaydata[[i]]$years,gia=gia.NO[i],se.gia=giase.NO[i])
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
	ylim=range(c(cis$res$a,cis$res$b,sta$data[begin:which(is.element(sta$years,max(sta$years)))]))


plot(xlim,ylim,xlim=xlim,ylim=ylim,type='n',xlab="Year",	ylab="Anomaly (cm)",main="RCP 8.5")
lines(sta$years[begin:max(sta$years)],sta$data[begin:max(sta$years)],col='blue')


for(ii in 1:K){
	lines(2000:2100,cis$pred[[ii]],col=2,lwd=0.5)
}

lines(2000:2100,cis$res$b,lwd=2)
lines(2000:2100,cis$res$a,lwd=2)
lines(2000:2100,cis$res$a.marginal,col="purple",lty=3)
lines(2000:2100,cis$res$b.marginal,col="purple",lty=3)

F.25 =  rep(0,101)
for(k in 1:K){
	F.25 = F.25 + cis$res.25[[k]]$F/K
	}

plot(range(2000,2100),c(0,1),type="n",col=1,xlab="Year",ylab="Probability",main="P(Y(s)<25, s<t)")
lines(2000:2100,F.25,col=4,lwd=2)
#lines(2000:2100,F.40,col=2,lwd=2)
#legend(2000,0, c("25 cm"), 
#lty=c(1,1), lwd=c(2.5,2.5),col=c(2,4),xjust=0,yjust=0) 


mu=NULL
for (i in 1:K)
mu=rbind(mu,cis$pred[[i]])
x = seq(from=0,to=40,length.out = 1000)
year = 51


plot(c(0,45), c(0,0.2), xlab="Anomaly (cm)",ylab="Density",main="Bergen 2050 RCP 8.5",type="n")
v1=apply(matrix(unlist(cis$pred),ncol=101,byrow=T),2,median)[year]
segments(v1,0,v1,0.2,lwd=2)
hist(mu[,year],xlim=c(0,40), breaks=(0:41), ylim=c(0,0.50),freq=FALSE,add=T,lwd=2)

lines(x,Fmix.pdf(x,matrix(unlist(cis$pred),ncol=101,byrow=T)[,year],cis$sd.g[year]),col="red",lwd=2)
lines(x,Fmix.pdf(x,matrix(unlist(cis$pred),ncol=101,byrow=T)[,year],cis$sd[year]),col="blue",lwd=2)
dev.off()
# #Simulate paths from model
# nsample = 10000
# k=sample(1:K,nsample,replace=T) #vector of nsample climate models
# sim=matrix(nrow=nsample,ncol=101)
# for (i in 1:nsample)
# sim[i,]=rnorm(101,mu[k[i],],sd)
# save(sim,file="Simulation.Rdata")
	
	
