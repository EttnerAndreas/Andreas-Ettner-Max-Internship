library(dplyr)
library(ggplot2)
library(Metrics)
library(corpcor)
library(nlme)
library(MVN)
library(DHARMa)
library(useful)
library(glmmTMB)
library(usethis)
library(devtools)
library(knitr)
library(coda)
library(rjags)
library(mvtnorm)
library(rjags)
library(R2jags)
library(mvtnorm)


global.mu = 0

repeat.experiment = data.frame(matrix(NA,500,6))
colnames(repeat.experiment) = c("rep", "Lambda", "Side", "glmm_Time", "glmm_Lambda", "glmm_Intercept")


dist.matrix <- function(side)
{
  row.coords <- rep(1:side, times=side)
  col.coords <- rep(1:side, each=side)
  row.col <- data.frame(row.coords, col.coords)
  D <- dist(row.col, method="euclidean", diag=TRUE, upper=TRUE)
  D <- as.matrix(D)
  return(D)
}


cor.surface <- function(side, global.mu, lambda)
{
  D <- dist.matrix(side)
  # scaling the distance matrix by the exponential decay
  SIGMA <- exp(-lambda*D)
  mu <- rep(global.mu, times=side*side)
  # sampling from the multivariate normal distribution
  M <- matrix(nrow=side, ncol=side)
  M[] <- rmvnorm(1, mu, SIGMA)
  return(M) # list(...)
}

set.seed=226

counter=1
for (g in 1:5 ){    # repeat experiment 5 times
  time_repeat = 
    system.time({
      
      for (h in 1:10 ){     # use 10 different lambdas form 0.2 zil 2.0
        time_glmmTMB = 
          system.time({
            lambda = 0 + (h*0.2)
            l.r <- lambda
            
            for (j in 4:13){   # use different size ( as a side*side 2d array)
              time_side = 
                system.time({
                  side = 2+ 2*j
                  s.r <- side
                  
                  D <- dist.matrix(side)
                  M <- cor.surface(side = side, lambda = lambda, global.mu = global.mu)
                  y <- as.vector(as.matrix(M))
                  
                  
                  new.data1 = 0
                  #n= side*side
                  new.data1        =  data.frame(resp = new.data$y)
                  new.data1$pos    <- numFactor(new.data$col, new.data$row.col)
                  new.data1$group  <- factor(rep(1, new.data$N))
                  new.data1$x      <- new.data$row
                  new.data1$y      <- new.data$col
                  
                  
                  time_glmmTMB = 
                    system.time({
                      fit.exp <- glmmTMB(resp ~ 1 + exp(pos + 0 | group), data = new.data1)
                    })
                  fit.exp$fit
                  
                  repeat.experiment[counter, 1] <- g       # repetitions
                  repeat.experiment[counter, 2] <- lambda  # real lambda
                  repeat.experiment[counter, 3] <- s.r     # side size
                  
                  repeat.experiment[counter, 4] <- time_glmmTMB[3] 
                  repeat.experiment[counter, 5] <- exp(-fit.exp$fit$par[4])
                  repeat.experiment[counter, 6] <- summary(fit.exp)$coefficients$cond[1]
                  
                  counter = counter + 1
                  
                }
                )} 
            
            lambda = 0.2 
            
          }
          )}
      
      
    }
    )}

repeat.experiment

saveRDS(repeat.experiment, file = "data")
