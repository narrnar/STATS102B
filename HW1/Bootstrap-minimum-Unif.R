# Sampling Distribution of Sample Minimum from U(0,1)
source("bootsample.R")

set.seed(100)
n = 100 # Size of a single sample  
B = c(200,500,1000,2000,5000,10000,50000,100000,500000)
# Number of bootstrap replicates

xseq = seq(0,1,length=200)
quartz(width=12,height=8)
par(mfrow=c(3,3))


x = runif(n,0,1)

for(j in 1:9){
  boot.samples = bootsampling.1(x,B[j])
  
  xmin = apply(boot.samples, 2, min)
  hist(xmin, breaks=20,freq=F, xlim=c(0,0.2), ylim=c(0,150),
      main=paste("Bootstrap Sampling Distribution: B =",B[j]))
  lines(xseq, dnorm(xseq, mean=1,sd=(1/sqrt(n))))
  legend("topright",expression(min(x)*" PDF"),lty=1,bty="n")
  #plot(density(xmin),main="Smooth Density Plot")
}

