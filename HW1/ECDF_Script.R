set.seed(1)
par(mfrow=c(2,3))
n = c(20,50,100,250,500,1000)
xseq = seq(-4,4,length=100)
for(j in 1:6){
  x = rnorm(n[j])
  plot(ecdf(x),main=paste("n = ",n[j]))
  lines(xseq,pnorm(xseq),col="blue")
}