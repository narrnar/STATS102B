# Interpretation of Confidence Intervals
# Example: Population Mean of a Normal Distribution
set.seed(202)
n = 100 # Size of a single sample  
M = c(100) #,200,500) #0,5000,10000,50000,100000,500000) # Number of samples of size n to be drawn

xseq = seq(-0.4+1,1+0.4,length=200)
quartz(width=12,height=8)
par(mfrow=c(1,2))

for(j in 1){
  X = replicate(M[j], rnorm(n,1,1)) 
  xbar = apply(X, 2, mean)
  xsd = apply(X, 2, sd)
  # lbound/ubound correspond to C_L/U_L in the notes
  lbound = qnorm(0.01,xbar,sqrt(xsd)/sqrt(n))
  ubound = qnorm(0.99,xbar,sqrt(xsd)/sqrt(n))
}

count=0
count=sum(lbound<1 & ubound>1,na.rm=TRUE)


  hist(xbar, breaks=20,freq=F, xlim=c(-0.1+1,0.1+1), ylim=c(0,40),
       main=paste("Sampling Distribution: M =",M[j]))
  lines(xseq, dnorm(xseq, mean=1,sd=(1/sqrt(n))))
  legend("topright",expression(bar(x)*" PDF"),lty=1,bty="n")



  hist(xsd, breaks=20,freq=F, xlim=c(-0.1+1,0.1+1), ylim=c(0,40),
    main=paste("Sampling Distribution: M =",M[j]))
    legend("topright",expression(sd(x)*" PDF"),lty=1,bty="n")

   
quartz(width=12,height=8)
par(mfrow=c(1,1))
plot(c(1,1), c(lbound[1], ubound[1]), type='l', 
           ylim=c(min(lbound)-.01, max(ubound)+.01), 
           xlim=c(1,100), xlab='# of samples',
           ylab='CI')
abline(h=1,col="blue",lwd=1.2)
for (i in 2:100){
     if (lbound[i]<1 & ubound[i]>1){
         lines(c(i,i), c(lbound[i], ubound[i]))
       }else{
           lines(c(i,i),c(lbound[i], ubound[i]),col="red",lwd=2)
         }
  }