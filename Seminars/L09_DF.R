# Author: Yukai Yang
# Date: 07 Oct 2015
# for the course: 2ST111 Time Series Econometrics
# Statistiska Institutionen
# Lecture 9
# Dickey-Fuller tests

# the number n
iN = 100
# number of repetitions
iM = 10000

# error terms, i.i.d. (0, sigma^2)

df = 5 # degrees of freedom if used in the following
eps = rnorm(iN*iM)
#eps = runif(iN*iM,min=-1,max=1)*sqrt(3)
#eps = rt(iN*iM,df=df)*sqrt(1-2/df)
#eps = (rchisq(iN*iM,df=df)-df)/sqrt(2*df)

dim(eps) = c(iN,iM)

mz = apply(eps,2,cumsum) # zt process, RW

# Wiener process
WP = mz / sqrt(iN)

# rho-test
# for each column of WP
tmpfunc <- function(vw)
  return(.5*(vw[iN]*vw[iN]-1) / (sum(vw**2)/iN))

DFr = apply(WP,2,tmpfunc)
denDFr = density(DFr)

# t-test
# for each column of WP
tmpfunc <- function(vw)
  return(.5*(vw[iN]*vw[iN]-1) / sqrt(sum(vw**2)/iN))

DFt = apply(WP,2,tmpfunc)
denDFt = density(DFt)

plot(denDFr$x,denDFr$y,type='l',main='Dickey-Fuller distributions',xlim=c(-5,3),ylim=c(0,.6),ylab='density',xlab='',col='red')
lines(denDFt$x,dnorm(denDFt$x),col='black')
lines(denDFt$x,denDFt$y,col='blue')

prob = c(.01,.05,.1)
qnorm(prob/2)
quantile(DFr,prob=prob)
quantile(DFt,prob=prob)


