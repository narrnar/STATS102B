# Ideal Sampling Framework: 
# Example: Mean of a Normal Distribution
set.seed(100)
n = 5000 # Size of a single sample  
M = c(50,100,200,500) #0,5000,10000,50000,100000,500000) # Number of samples of size n to be drawn

xseq = seq(-0.4+1,1+0.4,length=200)
quartz(width=12,height=8)
par(mfrow=c(2,2))

for(j in 1:4){
  X = replicate(M[j], rnorm(n,1,1)) # rnorm generates draws from a N(1,1) distribution
  xbar = apply(X, 2, mean)
  hist(xbar, breaks=20,freq=F, xlim=c(-0.1+1,0.1+1), ylim=c(0,40),
       main=paste("Sampling Distribution: M =",M[j]))
  lines(xseq, dnorm(xseq, mean=1,sd=(1/sqrt(n))))
  legend("topright",expression(bar(x)*" PDF"),lty=1,bty="n")
}
