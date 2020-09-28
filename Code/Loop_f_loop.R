
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
methods = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent")

repeat.experiment = data.frame(matrix(NA,1500,12))
colnames(repeat.experiment) = c("rep", "Lambda", "Side", "glmm_Time", "glmm_Lambda", "glmm_Intercept", "gls_Time", "gls_Lambda", "gls_Intercept", "optim_Time", "optim_Lambda", "optim_Intercept")


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
for (g in 1:15 ){
  time_repeat = 
    system.time({
      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      # Loop, wiederhole Experiment 5 mal für die Statistik
      for (h in 1:10 ){
        time_glmmTMB = 
          system.time({
            lambda = 0 + (h*0.2)
            l.r <- lambda
            
            #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
            # Loop, ändere Lambda von lambda=0.2 in 0.05 Schritten bis Lambda=0.5
            
            for (j in 4:13){
              time_side = 
                system.time({
                  side = 2+ 2*j
                  s.r <- side
                  # Loop, ändere side von 4 in 2er schritten auf 20 (daten ergeben sich aus side*side )
                  #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                  # new.data1$y      <- new.data$row.col
                  # data$x = row.coords
                  # data$y = col.coords
                  
                  #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

                    
                    D <- dist.matrix(side)
                    M <- cor.surface(side = side, lambda = lambda, global.mu = global.mu)
                    y <- as.vector(as.matrix(M))
                  #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                    new.data1 = 0
                    new.data = 0
                    new.data <- list(side=side, lambda = lambda,N = side * side, D = dist.matrix(side), y = y)
                    new.data$row     = row.coords <- rep(1:side, times=side)
                    new.data$col     = col.coords <- rep(1:side, each=side)
                    new.data$row.col = data.frame(new.data$row, new.data$col)
                    new.data$N      = side*side
                    n= side*side
                    # new.data1        = data.frame(resp = my.data$y)
                    new.data$group   <- as.factor(rep(1, new.data$N))
                    new.data$rows    <- new.data$row.col[,1]
                    new.data$cols    <- new.data$row.col[,2]
                    new.data1        =  data.frame(resp = new.data$y)
                    new.data1$pos    <- numFactor(new.data$col, new.data$row.col)
                    new.data1$group  <- factor(rep(1, new.data$N))
                    new.data1$x      <- new.data$row
                    new.data1$y      <- new.data$col
                    

                    time_glmmTMB = 
                      system.time({
                        fit.exp <- glmmTMB(resp ~ 1 + exp(pos + 0 | group), data = new.data1)
                      })
                    
                    time_gls = 
                      system.time({
                        # gls <- gls(y ~ 1, correlation=corExp (form =~ rows + cols), data = new.data)
                        # gls <- lme(y ~ 1, random=~ 1 | group, correlation=corExp(form=~rows+cols), data = new.data)

                        try({
                          gls = gls(y ~ 1, correlation=corExp (form =~ rows + cols), data = new.data)
                          repeat.experiment[counter, 7] <- time_gls[3]
                          repeat.experiment[counter, 8] <- 1/coef(gls$modelStruct$corStruct, unconstrained = F)
                          repeat.experiment[counter, 9] <- summary(gls)$coefficients
                        }, silent=TRUE)
                      })
                    
                    time_optim = 
                      system.time({
                        ll = function(par) {
                          cov = (exp(-par[1]* new.data$D))
                          -mvtnorm::dmvnorm(new.data1$resp, mean = rep(par[2], side*side), sigma = cov ,log = TRUE)
                        }
                        result <- optim(par = c(0.5,10),  fn = ll, gr = NULL, method = methods[1], hessian = FALSE)
                      })
                  
                  repeat.experiment[counter, 1] <- g
                  repeat.experiment[counter, 2] <- lambda
                  repeat.experiment[counter, 3] <- s.r 
                  
                  repeat.experiment[counter, 4] <- time_glmmTMB[3] 
                  repeat.experiment[counter, 5] <- exp(-fit.exp$fit$par[4])
                  repeat.experiment[counter, 6] <- summary(fit.exp)$coefficients$cond[1]
                  
                  #repeat.experiment[counter, 7] <- time_gls[3]
                  #repeat.experiment[counter, 8] <- 1/coef(gls$modelStruct$corStruct, unconstrained = F)
                  #repeat.experiment[counter, 9] <- summary(gls)$coefficients
                  
                  repeat.experiment[counter, 10] <- time_optim[3]
                  repeat.experiment[counter, 11] <- result$par[1] 
                  repeat.experiment[counter, 12] <- result$par[2]
                  
                  counter = counter + 1
                  
                  #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                  
                }
                )} 
            
            lambda = 0.2 
            #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
          }
          )}
      
      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    }
)}

repeat.experiment

saveRDS(repeat.experiment, file = "loop6")
counter=1






