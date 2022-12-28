# Author: Yukai Yang
# Date: 07 Oct 2015
# for the course: 2ST111 Time Series Econometrics
# Lecture 9
# Wiener process

# the number n
iN = 10000

# variance of the errors
sigma = 1

# error terms, i.i.d. (0, sigma^2)

df = 5 # degrees of freedom if used in the followed
eps = rnorm(iN)
#eps = runif(iN,min=-1,max=1)*sqrt(3)
#eps = rt(iN,df=df)*sqrt(1-2/df)
#eps = (rchisq(iN,df=df)-df)/sqrt(2*df)


vz = eps*sigma
vz = cumsum(vz) # zt process, RW

# Wiener process
WP = vz / sqrt(iN) / sigma

plot(WP,x=1:iN/iN,type='l',col='red'); abline(0,0)

