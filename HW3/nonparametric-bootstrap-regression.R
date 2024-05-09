# Sampling Distribution of regression coefficients
# based on the Parametric Bootstrap
source("bootsample.R")
source("bootstats.R")

set.seed(100)
n = 100 # Size of a single sample  
B = c(200,500,1000,2000,5000,10000)
# Number of bootstrap replicates


# simulate predictors for regression from N(1,1)
x=rnorm(n,1,1)
# simulate errors from U[-2,2]
epsilon=runif(n,min=-2,max=2)
# construct response y=3+2*x + epsilon
y = 3 + 2*x + epsilon

z.data=cbind(y,x)

# obtain least squares estimates
betas = lm(y~x)
# store and print the summary of the model
# including the beta coefficients
betas.summary=summary(betas)

# store the intercept and slope for use in CIs
beta.int=betas.summary$coefficients[1,1]
beta.slope=betas.summary$coefficients[2,1]

# obtain fitted values of y 
yhat = betas$fitted.values


quartz(width=12,height=8)
par(mfrow=c(3,4))


# store the results of various types of bootstrap CIs
alpha=.05
normal.CI.int = data.frame(matrix(ncol = 3, nrow = 0))
normal.CI.slope = data.frame(matrix(ncol = 3, nrow = 0))
basic.CI.int = data.frame(matrix(ncol = 3, nrow = 0))
basic.CI.slope = data.frame(matrix(ncol = 3, nrow = 0))
percentile.CI.int = data.frame(matrix(ncol = 3, nrow = 0))
percentile.CI.slope = data.frame(matrix(ncol = 3, nrow = 0))
BC.CI.int = data.frame(matrix(ncol = 3, nrow = 0))
BC.CI.slope = data.frame(matrix(ncol = 3, nrow = 0))


for(j in 1:length(B)){
  boot.samples = bootsampling(z.data,B[j])
  
  # create function to obtain the beta coefficients 
  betahat = function(z) {lm(z[,1]~z[,2])$coefficients}
  
  # calculate the bootstrap beta coefficients
  # using our function boot.stats
  
  betas.boot = boot.stats(boot.samples,betahat)
  
  hist(betas.boot$theta[1,], breaks=20,freq=F, xlim=c(-0.5+3,3+0.5), ylim=c(0,6),
       main=paste("Boot Sampling Distri: B =",B[j]))
  legend("topright",expression("intercept PDF"),lty=1,bty="n")
  
  hist(betas.boot$theta[2,], breaks=20,freq=F, xlim=c(-0.5+2,2+0.5), ylim=c(0,6),
       main=paste("Boots Sampling Distri: B =",B[j]))
  legend("topright",expression("slope PDF"),lty=1,bty="n")
  

#obtain normal CI
normal.CI.int=rbind(normal.CI.int,cbind(beta.int-zval*sqrt(betas.boot$cov[1,1]),beta.int+zval*sqrt(betas.boot$cov[1,1])))
normal.CI.slope=rbind(normal.CI.slope,cbind(beta.slope-zval*sqrt(betas.boot$cov[2,2]),beta.slope+zval*sqrt(betas.boot$cov[2,2])))

# obtain basic CI
basic.CI.int=rbind(basic.CI.int,cbind(2*beta.int-quantile(betas.boot$theta[1,],probs=(1-alpha/2)),
                                  2*beta.int-quantile(betas.boot$theta[1,],probs=(alpha/2))))  
basic.CI.slope=rbind(basic.CI.slope,cbind(2*beta.slope-quantile(betas.boot$theta[2,],probs=(1-alpha/2)),
                              2*beta.slope-quantile(betas.boot$theta[2,],probs=(alpha/2))))  

# obtain percentile CI
percentile.CI.int=rbind(percentile.CI.int,cbind(quantile(betas.boot$theta[1,],probs=(alpha/2)),
                                        quantile(betas.boot$theta[1,],probs=(1-alpha/2))))
percentile.CI.slope=rbind(percentile.CI.slope,cbind(quantile(betas.boot$theta[2,],probs=(alpha/2)),
                                        quantile(betas.boot$theta[2,],probs=(1-alpha/2))))

# obtain BC CI
z0int = qnorm(sum(betas.boot$theta[1,]<beta.int)/B[j])
z0slope = qnorm(sum(betas.boot$theta[2,]<beta.slope)/B[j])
A1int= pnorm(2*z0int+qnorm(alpha/2))
A2int= pnorm(2*z0int+qnorm(1-alpha/2))
A1slope= pnorm(2*z0slope+qnorm(alpha/2))
A2slope= pnorm(2*z0slope+qnorm(1-alpha/2))

BC.CI.int=rbind(BC.CI.int,cbind(quantile(betas.boot$theta[1,],probs=(A1int)),
                        quantile(betas.boot$theta[1,],probs=(A2int)) ))

BC.CI.slope=rbind(BC.CI.slope,cbind(quantile(betas.boot$theta[2,],probs=(A1slope)),
                                quantile(betas.boot$theta[2,],probs=(A2slope)) ))
  
  
}