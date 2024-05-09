# Sampling Distribution of Sample Mean from a N(1,1)
source("bootsample.R")

set.seed(100)
n = 100 # Size of a single sample  
B = c(200,500,1000,2000,5000,10000,50000,100000,500000)
# Number of bootstrap replicates

xseq = seq(-0.4+1,1+0.4,length=200)
quartz(width=12,height=8)
par(mfrow=c(3,3))


x = rnorm(n,1,1)

for(j in 1:9){
boot.samples = bootsampling.1(x,B[j])

xmedian = apply(boot.samples, 2, mean)
hist(xmedian, breaks=35,freq=F, xlim=c(-0.4+1,0.4+1), ylim=c(0,7),
     main=paste("Bootstrap Sampling Distribution: B =",B[j]))
lines(xseq, dnorm(xseq, mean=1,sd=(1/sqrt(n))))
legend("topright",expression(bar(x)*" PDF"),lty=1,bty="n")
}

