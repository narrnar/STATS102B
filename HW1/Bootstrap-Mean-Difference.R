# Sampling Distribution of Sample Mean from a N(N,1)
source("bootsample-1.R")
source("bootstats.R")

set.seed(100)
n = 100 # Size of a single sample  
B = c(200,500,1000,2000,5000,10000,50000,100000,500000)
# Number of bootstrap replicates

xseq = seq(-0.7+2,2+0.7,length=200)
quartz(width=12,height=8)
par(mfrow=c(3,3))


x = rnorm(n,3,1)
y = rnorm(n,1,1)
z=cbind(x,y)

for(j in 1:9){
  boot.samples = bootsampling(z,B[j])
  
  mean.diff=function(w){mean(w[,1])-mean(w[,2])}
  
  mdiff = boot.stats(boot.samples,mean.diff)
  
  hist(mdiff$theta, breaks=35,freq=F, xlim=c(-0.7+2,0.7+2), ylim=c(0,4),
       main=paste("Bootstrap Sampling Distribution: B =",B[j]))
  legend("topright",expression(bar(x)-bar(y)*" PDF"),lty=1,bty="n")
}

