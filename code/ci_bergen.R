ci_bergen=function(models,sta,stafit,sta.common.years,alpha=.1)
{
load('~/GitHub/SeaLevelProjections/data&global.Rdata')#global temperature-sealevel relation, historical data,temperature projections etc.
 source('~/GitHub/SeaLevelProjections/Sealevel_functions.R')
s1999 = seal$sealevel[which(is.element(seal$years,1999))]
K = length(models) 
proj=which(is.element(cmip5$years,2000:2100))
modelpath=matrix(0,K,101)
count=0
for (i in models) {
  count=count+1
  modelpath[count,]=cmip5$data[proj,i,4]
}
sd1999 =seal$se[which(is.element(seal$years,1999))]
n.o =  length(sta.common.years)
n.p=101
n = n.o+n.p
#Calculate covariance matrices for MA processes

cov2 = ARMAcov(armodel$ar,n)
cov1 = ARMAcov(stafit$ar,n)  
Sigma2 = toeplitz(cov2) 
Sigma1 = toeplitz(cov1)

#covariance for non-differentiated global mean sea level:
Sigma.i2 = apply(apply(Sigma2,1,cumsum),1,cumsum)

#Total covariance for station
Sigma = a4^2*Sigma.i2 + Sigma1
#add the variance caused by the uncertainty in the starting value:

Sigma = Sigma + diag(rep(sd1999^2,dim(Sigma)[1]))

#add the variance due to gia

times=(0:(n-1))-stafit$adj
Sigma=Sigma+diag(sta$se.gia^2*times^2,dim(Sigma)[1])


#Submatrices for observations and predictions:

Sigma.o = Sigma[1:n.o,1:n.o]
Sigma.p = Sigma[(n.o+1):n,(n.o+1):n]
Sigma.op = Sigma[1:n.o,(n.o+1):n]

#Calculate  precision matrix
R.o = chol(Sigma.o)
c = mean(seal$sealevel - cumsum(armodel$ar$coef[3] 
+armodel$ar$coef[4]*temperature$temp),na.rm=T)
x.o = sta$data[1:n.o]#!!! (lacks gia correction but its ok)
Q.o = chol2inv(R.o)
Sigma.pred = Sigma.p - t(Sigma.op) %*% Q.o %*% Sigma.op
Q.pred = Matrix(chol2inv(chol(Sigma.pred)))
sd = sqrt(diag(Sigma.pred))

# #Marginal quantiles for gaussian mixtures:
# Fmix = function(x,mu,sd) sum( pnorm(x,mean=mu,sd=sd)/length(mu) )
# Fmix_inv = function(p,mu,sd,br=c(-1000,1000))
# {
   # G = function(x) Fmix(x,mu,sd) - p
   # return( uniroot(G,br)$root ) 
# }

#calculate the estimates for each climate model:
tmp = c+cumsum(armodel$ar$coef[3]+armodel$ar$coef[4]*temperature$temp[sta.common.years])
mu.o = a3+a4*tmp[sta.common.years]
mu.corr = t(Sigma.op) %*% (Q.o %*% (x.o - mu.o))
mu.pred = Matrix(0,K,n.p)
mu.pred <- Q <- list()
w = rep(1/K,K)

for(k in 1:K){
 
  mu.pred[[k]] = as.vector(a3 + a4*(cumsum(armodel$ar$coef[3]+armodel$ar$coef[4]*modelpath[k,])) + as.vector(s1999)-sta$gia*(stafit$reg0+(0:100)))
  Q[[k]] = as(Q.pred,"sparseMatrix")
}

res <- simconf.mixture(alpha=alpha, mu = mu.pred, Q = Q, w = w)


print(c("8.5","20cm",min((2000:2100)[res$b>20]),min((2000:2100)[apply(matrix(unlist(mu.pred),ncol=101,byrow=T), 2, mean, na.rm=T)>20]),min((2000:2100)[res$a>20])))
print(c("8.5","40cm",min((2000:2100)[res$b>40]),min((2000:2100)[apply(matrix(unlist(mu.pred),ncol=101,byrow=T), 2, mean, na.rm=T)>40]),min((2000:2100)[res$a>40])))

 res.20 = res.40 = NULL
for(k in 1:K){
	
	res.20[[k]] = excursions(mu = mu.pred[[k]],Q=Q.pred,ind = 1:101,
						   u=20, alpha=1,type='<')
		res.40[[k]] = excursions(mu = mu.pred[[k]],Q=Q.pred,ind = 1:101,
						   u=40, alpha=1,type='<')
}
#Calculate standard deviations for global sea level
Sigma.g = a4^2*Sigma.i2
Sigma.g.o = Sigma.g[1:n.o,1:n.o]
Sigma.g.p = Sigma.g[(n.o+1):n,(n.o+1):n]
Sigma.g.op = Sigma.g[1:n.o,(n.o+1):n]
Q.g.o = chol2inv(chol(Sigma.g.o))
Sigma.g.pred = Sigma.g.p - t(Sigma.g.op) %*% Q.g.o %*% Sigma.g.op
sd.g = sqrt(diag(Sigma.g.pred))




return(list(pred=mu.pred,res=res,res.20=res.20,res.40=res.40,sd=sd,sd.g=sd.g))
}

