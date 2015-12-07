library(msm)
library(coda)
#data cleaning
y = read.table("desktop/data.txt",header=TRUE)
wd<-(y[,2]!="Sunday"&y[,2]!="Saturday")
we<-(y[,2]=="Sunday"|y[,2]=="Saturday")
y1<-y[wd,1]
y2<-y[we,1]
n<-length(y1)
m<-length(y2)
#gibbs sampling
m1<-mean(log(y1));m2<-mean(log(y2))
tau1<-0.001;tau2<-0.001
a1=b1=a2=b2=0.001

mu<-c(m1,m2)
tau<-c(0,0)
burnin=1000
S=21000
MU<-SIGMA<-NULL

for (s in 1:S){
  #update tau
  tau[1]=rgamma(1,a1+n/2,b1+sum((log(y1)-mu[1])^2)/2)
  tau[2]=rgamma(1,a2+m/2,b2+sum((log(y2)-mu[2])^2)/2)
  #update mu
  tau1hat<-n*tau[1]+tau1
  m1hat<-(tau1*m1+tau[1]*sum(log(y1)))/tau1hat
  mu[1]<-rtnorm(1,m1hat,1/sqrt(tau1hat),mu[2],Inf)
  
  tau2hat<-m*tau[2]+tau2
  m2hat<-(tau2*m2+tau[2]*sum(log(y2)))/tau2hat
  mu[2]<-rtnorm(1,m2hat,1/sqrt(tau2hat),-Inf,mu[1])
  
  #store result
  if (s>burnin){
    MU = rbind(MU,mu)
    SIGMA<-rbind(SIGMA,1/sqrt(tau))
  }
  
}

#plots
#traceplot of MU and SIGMA
seqx<-seq(1,20000,by=1)
plot(seqx,MU[,1],type='l',main='traceplot of muy1',col='blue',xlab='iteration',ylab='muy1')
plot(seqx,MU[,2],type='l',main='traceplot of muy2',col='blue',xlab='iteration',ylab='muy2')
plot(seqx,SIGMA[,1]^2,type='l',main='traceplot for sigma2y1',col='red',xlab='iteration',ylab='sigma2y1')
plot(seqx,SIGMA[,2]^2,type='l',main='traceplot for sigma2y2',col='red',xlab='iteration',ylab='sigma2y2')

#running average
MUy1<-MU[,1]
ramuy1<-c()
ramuy1[1]<-MUy1[1]
for (i in 2:20000)
{
  ramuy1[i]<-(ramuy1[i-1]*(i-1)+MUy1[i])/i
}
seqx<-seq(1,20000,by=1)
plot(seqx,ramuy1,type='l',col="green",xlab='iteration',ylab='muy1')

MUy2<-MU[,2]
ramuy2<-c()
ramuy2[1]<-MUy2[1]
for (i in 2:20000)
{
  ramuy2[i]<-(ramuy2[i-1]*(i-1)+MUy2[i])/i
}
seqx<-seq(1,20000,by=1)
plot(seqx,ramuy2,type='l',col="green",xlab='iteration',ylab='muy2')

SIGMA12<-SIGMA[,1]^2
rasigma12<-c()
rasigma12[1]<-SIGMA12[1]^2
for (i in 2:20000)
{
  rasigma12[i]<-(rasigma12[i-1]*(i-1)+SIGMA12[i])/i
}
seqx<-seq(1,20000,by=1)
plot(seqx,rasigma12,type='l',col="yellow",xlab='iteration',ylab='sigma12')

SIGMA22<-SIGMA[,2]^2
rasigma22<-c()
rasigma22[1]<-SIGMA2[1]^2
for (i in 2:20000)
{
  rasigma22[i]<-(rasigma22[i-1]*(i-1)+SIGMA22[i])/i
}
seqx<-seq(1,20000,by=1)
plot(seqx,rasigma22,type='l',col="yellow",xlab='iteration',ylab='sigma22')

#posterior point estimates and 95% CI for each parameter
mu.mean<-apply(MU,2,mean)
mu.quantile<-apply(MU,2,quantile,probs=c(.025,.975))

sigma.mean<-apply(SIGMA,2,mean)
sigma.quantile<-apply(SIGMA,2,quantile,probs=c(.025,.975))

p1<-mean(MU[,1]>MU[,2])
p2<-mean(SIGMA[,1]>SIGMA[,2])
 NN H N HJNHJJN
# probability of y1>y2
N=S-burnin
y1test<-exp(rnorm(N,MU[,1],SIGMA[,1]))
y2test<-exp(rnorm(N,MU[,2],SIGMA[,2]))
p3<-mean(y1test>y2test)
