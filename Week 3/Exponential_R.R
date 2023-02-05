rm(list=ls())
library(MASS)
library(stats4)


################# Coalmine Dataset #####################

coal.dis=c(1 ,4 ,4, 7 ,11, 13 ,15, 15 ,17 ,18, 19 ,19 ,20 ,20 ,22 ,23 ,28 ,29, 31, 32, 36 ,37 ,47 ,48, 49 ,50 ,54 ,54, 55, 59, 59 ,61, 61,
66, 72, 72 ,75, 78, 78, 81, 93 ,96 ,99 ,108 ,113 ,114 ,120 ,120 ,120 ,123 ,124 ,129, 131 ,137 ,145, 151, 156, 171,
176 ,182 ,188 ,189 ,195 ,203, 208, 215, 217 ,217 ,217 ,224, 228 ,233, 255 ,271, 275, 275 ,275 ,286 ,291, 312,
312 ,312, 315, 326, 326 ,329, 330, 336, 338, 345, 348, 354, 361, 364 ,369, 378 ,390, 457 ,467 ,498 ,517 ,566,
644, 745, 871, 1312, 1357, 1613, 1630)

hist(coal.dis,breaks=15,prob=TRUE, main = "Histogram of Coalmine Data")
summary(coal.dis)
var(coal.dis)

Exp=fitdistr(coal.dis,"exponential")
pa=as.vector(Exp$estimate);pa
l_exp = logLik(Exp)[1];l_exp

EE=function(alp,lam)
{
	x=coal.dis
	a1=sum((alp-1)*log(1-exp(-lam*x),base=exp(1)))
	-(length(x)*log(alp*lam)-lam*sum(x)+a1)
	
}
s=mle(EE,start=list(alp=5,lam=5))
pa3=as.vector(coef(s));pa3
l_ee = logLik(s)[1];l_ee

p1=ks.test(coal.dis,"pexp",0.00429)

pEE=function(x,alp,lam)
{
	(1-exp(-lam*x))^alp
}

p2=ks.test(coal.dis,"pEE",coef(s)[1],coef(s)[2])


# Exponential distribution
hist(coal.dis,breaks=15,prob=TRUE, main = "Exponential Distribution")
s=seq(0,max(coal.dis),length=10000)
lamb_hat_e = pa[1]
ed = lamb_hat_e*exp(-1*lamb_hat_e*s)
lines(s,ed,col="red",lwd=2)
legend("topright", c("Exponential"), col=c("red"), lwd=10)

#Exponentiated exponential distribution
hist(coal.dis,breaks=15,prob=TRUE, main = "Exponentiated Exponential Distribution")
s=seq(0,max(coal.dis),length=10000)
alph_hat=pa3[1]
lamb_hat=pa3[2]
eed = lamb_hat*alph_hat*(1-exp(-1*lamb_hat*s))^(alph_hat-1)*exp(-1*lamb_hat*s)
lines(s,eed,col="blue",lwd=2)
legend("topright", c("Exponentiated exponential"), col=c("blue"), lwd=10)


# Combined plot
hist(coal.dis,breaks=15,prob=TRUE, main = "Combined plot")
s=seq(0,max(coal.dis),length=10000)
lines(s,ed,col="red",lwd=2)
lines(s,eed,col="blue",lwd=2)
legend("topright", c("Exponential", "Exponentiated exponential"), col=c("red", "blue"), lwd=10)

#------Table
Dist=c("Exponential","Exponentiated exponential")
par1=c(pa[1],pa3[1])
par2=c(pa[2],pa3[2])
Likelihood=c(l_exp,l_ee)
p_value=c(p1$p.value,p2$p.value)
cbind(Dist,par1,par2,Likelihood,p_value)

################# Guinea Pigs Dataset #####################

pigs = c(12,15,22,24,24,32,32,33,34,38,38,43,44,48,52,53,54,54,55,56,57,58,58,59,60,60,60,60,61,62,63,65,65,67,68,70,70,72,73,75,76,76,81,83,84,85,87,91,95,96,98,99,109,110,121,127,129,131,143,146,146,175,175,211,233,258,258,263,297,341,341,376)

summary(pigs)
sd(pigs)
############# Exponential Distribution #############
Exp=fitdistr(pigs,"exponential")
pa=as.vector(Exp$estimate);pa
l_exp = logLik(Exp)[1];l_exp

############# Exponentiated exponential Distribution #############
EE=function(alp,lam)
{
  x=pigs
  a1=sum((alp-1)*log(1-exp(-lam*x),base=exp(1)))
  -(length(x)*log(alp*lam)-lam*sum(x)+a1)
  
}
s=mle(EE,start=list(alp=5,lam=5))
pa1=as.vector(coef(s));pa1
l_ee = logLik(s)[1];l_ee

############ Kolmogorov-Smirnov Tests ################

p1=ks.test(pigs,"pexp",0.01001809)

pEE=function(x,alp,lam)
{
  (1-exp(-lam*x))^alp
}

p2=ks.test(pigs,"pEE",pa1[1],pa1[2])

############### Histogram ##################

# Exponential distribution
hist(pigs,breaks=15,prob=TRUE, main = "Exponential Distribution")
s=seq(0,max(pigs),length=10000)
lamb_hat_e = pa[1]
ed = lamb_hat_e*exp(-1*lamb_hat_e*s)
lines(s,ed,col="red",lwd=2)
legend("topright", c("Exponential"), col=c("red"), lwd=10)

#Exponentiated exponential distribution
hist(pigs,breaks=15,prob=TRUE, main = "Exponentiated Exponential Distribution")
s=seq(0,max(pigs),length=10000)
alph_hat=pa1[1]
lamb_hat=pa1[2]
eed = lamb_hat*alph_hat*(1-exp(-1*lamb_hat*s))^(alph_hat-1)*exp(-1*lamb_hat*s)
lines(s,eed,col="blue",lwd=2)
legend("topright", c("Exponentiated exponential"), col=c("blue"), lwd=10)


# Combined plot
hist(pigs,breaks=15,prob=TRUE, main = "Combined plot")
s=seq(0,max(pigs),length=10000)
lines(s,ed,col="red",lwd=2)
lines(s,eed,col="blue",lwd=2)
legend("topright", c("Exponential", "Exponentiated exponential"), col=c("red", "blue"), lwd=10)

#------Table
Dist=c("Exponential","Exponentiated exponential")
par1=c(pa[1],pa1[1])
par2=c(pa[2],pa1[2])
Likelihood=c(l_exp,l_ee)
p_value=c(p1$p.value,p2$p.value)
cbind(Dist,par1,par2,Likelihood,p_value)



	
