
counter=1

for (g in 1:1 ){
  time_repeat = 
    system.time({
      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      # Loop, wiederhole Experiment 5 mal für die Statistik
      for (h in 1:2 ){
        time_glmmTMB = 
          system.time({
            lambda = 0 + (h*0.2)
            l.r <- lambda
            
            #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
            # Loop, ändere Lambda von lambda=0.2 in 0.05 Schritten bis Lambda=0.5
            
            for (j in 1:2){
              time_side = 
                system.time({
                  side = 2+ 2*j
                  s.r <- side
                  # Loop, ändere side von 4 in 2er schritten auf 20 (daten ergeben sich aus side*side )
                  #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                  D <- cor.surface(side = side, lambda = lambda, global.mu = global.mu)
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
                  
                  new.data1 = data.frame(resp = new.data$y)
                  new.data1$pos    <- numFactor(new.data$col, new.data$row.col)
                  new.data1$group  <- factor(rep(1, new.data$N))
                  new.data1$x      <- new.data$row.col
                  new.data1$y      <- new.data$col
                  #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                  for(i in 1:1){
                    data
                    time_glmmTMB = 
                      system.time({
                        fit.exp <- glmmTMB(resp ~ 1 + exp(pos + 0 | group), data = new.data1)
                      })

                    time_gls = 
                      system.time({
                        gls <- gls(y ~ 1, correlation=corExp (form =~ rows + cols), data = new.data)
                      })
                    
                    time_optim = 
                      system.time({
                        ll = function(par) {
                          cov = (exp(-par[1]* new.data$D))
                          -mvtnorm::dmvnorm(new.data1$y, mean = rep(par[2], side*side), sigma = cov ,log = TRUE)
                        }
                        result <- optim(par = c(0.5,10),  fn = ll, gr = NULL, method = methods[1], hessian = FALSE)
                      })
                  }
                  repeat.experiment[counter, 1] <- g
                  repeat.experiment[counter, 2] <- lambda
                  repeat.experiment[counter, 3] <- s.r 
                  
                  repeat.experiment[counter, 4] <- time_glmmTMB[3] 
                  repeat.experiment[counter, 5] <- exp(-fit.exp$fit$par[4])
                  repeat.experiment[counter, 6] <- summary(fit.exp)$coefficients$cond[1]
                  
                  repeat.experiment[counter, 7] <- time_gls[3]
                  repeat.experiment[counter, 8] <- 1/coef(gls$modelStruct$corStruct, unconstrained = F)
                  repeat.experiment[counter, 9] <- summary(gls)$coefficients
                  
                  repeat.experiment[counter, 10]  <- time_optim[3]
                  repeat.experiment[counter, 11] <- result$value 
                  repeat.experiment[counter, 12] <- result$par[1]
                  
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

saveRDS(repeat.experiment, file = "loop2")
counter=1





updata.data <- function
{
  new.data <- list(side=side, lambda = lambda,N = side * side, D = dist.matrix(side), y = y)
  new.data$row     <- row.coords <- rep(1:side, times=side)
  new.data$col     <- col.coords <- rep(1:side, each=side)
  new.data$row.col <- data.frame(new.data$row, new.data$col)
  new.data$N       <- side*side
  new.data$group   <- as.factor(rep(1, new.data$N))
  new.data$rows    <- new.data$row.col[,1]
  new.data$cols    <- new.data$row.col[,2]
  return(new.data)
}

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

new.data1 = data.frame(resp = new.data$y)
new.data1$pos    <- numFactor(new.data$col, new.data$row.col)
new.data1$group  <- factor(rep(1, new.data$N))
new.data1$x      <- new.data$row.col
new.data1$y      <- new.data$col












