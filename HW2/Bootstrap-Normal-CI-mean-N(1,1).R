# Constructing Normal Bootstrap CI for Population Mean from a N(1,1)
# source("bootsample.R")
# source("bootstats.R")

set.seed(100)
n = 100 # Size of a single sample  
B = c(200,500,1000,2000,5000,10000,50000,100000,500000)
# Number of bootstrap replicates


quartz(width=12,height=8)
par(mfrow=c(3,3))


x = rnorm(n,1,1)

normal.CI = data.frame(matrix(ncol = 9, nrow = 0))
alpha=.05
zval=qnorm(p=alpha/2, lower.tail=FALSE)

for(j in 1:9){
boot.samples = bootsampling(x,B[j])

xbar = function(w){mean(w)}
x.mean = boot.stats(boot.samples,xbar)


hist(x.mean$theta, breaks=35,freq=F, xlim=c(-0.5+1,1+0.5), ylim=c(0,6),
     main=paste("Bootstrap Sampling Distribution: B =",B[j]))

normal.CI=rbind(normal.CI,cbind(mean(x)-zval*x.mean$se,mean(x)+zval*x.mean$se))
}

