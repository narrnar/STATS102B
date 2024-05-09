# Ideal Sampling Framework: 
# Example: Minimum of a Uniform Distribution
set.seed(100)
n = 20 # Size of a single sample  
M = c(100,200,500,1000,5000,10000,50000,100000,500000) # Number of samples of size n to be drawn

xseq = seq(0,1,length=200)
quartz(width=12,height=8)
par(mfrow=c(3,3))

pdf.min = function(x) {    # pdf function for the minimum
  n*(1-x)^(n-1)
}


for(j in 1:9){
  X = replicate(M[j], runif(n,0,1)) # rnorm generates draws from a Uniform distribution
  xmin = apply(X, 2, min)
  hist(xmin, breaks=200,freq=F, xlim=c(0,1), ylim=c(0,30),
       main=paste("Sampling Distribution: M =",M[j]))
  # lines(xseq, dexp(xseq, rate=500))
  curve(pdf.min,0, 1, col="red", lwd=2, add=T)
  legend("topright",expression(min(x)*" PDF"),lty=1,bty="n")
}
