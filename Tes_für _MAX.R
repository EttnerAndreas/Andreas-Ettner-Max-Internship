repeat.experiment = data.frame(matrix(NA,150,11))

counter=1

for (g in 1:5 ){
  time_repeat = 
    system.time({
      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      # Loop, wiederhole Experiment 5 mal für die Statistik
      for (h in 1:5 ){
        time_glmmTMB = 
          system.time({
            lambda[h] = 0.15+h*0.05
            l.r <- lambda
            
            #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
            # Loop, ändere Lambda von lambda=0.2 in 0.05 Schritten bis Lambda=0.5
            counter = 1
            for (j in 1:5){
              time_side = 
                system.time({
                  side = 2+ 2*j
                  s.r <- side
                  # Loop, ändere side von 4 in 2er schritten auf 20 (daten ergeben sich aus side*side )
                  #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                  new_data = simulate(side=side, lambda=lambda)
                  
                  # Hier werden die Daten neu generiert ( hier liegt das Problem, denke ich )
                 # dist.matrix <- function(side)
                 # {
                 #   row.coords <- rep(1:side, times=side)
                 #   col.coords <- rep(1:side, each=side)
                 #   row.col <- data.frame(row.coords, col.coords)
                 #   D <- dist(row.col, method="euclidean", diag=TRUE, upper=TRUE)
                 #   D <- as.matrix(D)
                 #   return(D)
                 # }

                  row <- row.coords <- rep(1:side, times=side)
                  col <- col.coords <- rep(1:side, each=side)
                  row.col <<- data.frame(row, col)
                  D1 <- dist(row.col, method="euclidean", diag=TRUE, upper=TRUE)
                  D <- as.matrix(D1)
                  
                #  #dist.matrix <- function(list(D=D, coords = row.col))
                #  dist.matrix <- function(side)
                #  {row.coords=row.coords
                #  col.coords=col.coords
                #  row.col=row.col
                #  D=D
                #  return(D)
                #  }

                  
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
                  
                  M <- cor.surface(side = side, lambda = lambda, global.mu = global.mu)
                  y <- as.vector(as.matrix(M))
                  my.data <- list(N = side * side, D = dist.matrix(side), y = y)
                  #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


               # Zusatz Variablen für die 3 Modelle
                  n= side*side
                  data = data.frame(resp=my.data$y)
                  data$pos <- numFactor(row.col, row.coords)
                  data$group <- factor(rep(1, nrow(data)))
                  data$x = row.coords
                  data$y = col.coords

                  my.data$group = as.factor(rep(1, my.data$N))
                  my.data$rows = row.col[,1]
                  my.data$cols = row.col[,2]
                  #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                  
                  for(i in 1:5){
                    time_glmmTMB = 
                      system.time({
                        fit.exp <- glmmTMB(resp ~ 1 + exp(pos + 0 | group), data=data)
                      })
                    
                            time_gls = 
                              system.time({
                                gls <- gls(y ~ 1, correlation=corExp (form=~rows+cols), data = my.data)
                              })
                    
                            time_optim = 
                              system.time({
                                ll = function(par) {
                                  cov = (exp(-par[1]* my.data$D))
                                  -mvtnorm::dmvnorm(my.data$y, mean = rep(par[2], side*side), sigma = cov ,log = TRUE)
                                }
                                result <- optim(par = c(0.5,10),  fn = ll, gr = NULL, method = methods[1], hessian = FALSE)
                              })
                  }
                  
                  repeat.experiment[counter, 1] <- lambda[h]
                  repeat.experiment[counter, 2] <- s.r 
                  
                  repeat.experiment[counter, 3] <- time_glmmTMB[3] 
                  repeat.experiment[counter, 4] <- exp(-fit.exp$fit$par[4])
                  repeat.experiment[counter, 5] <- summary(fit.exp)$coefficients$cond[1]
                  
                  repeat.experiment[counter, 6] <- time_gls[3]
                  repeat.experiment[counter, 7] <- 1/coef(gls$modelStruct$corStruct, unconstrained = F)
                  repeat.experiment[counter, 8] <- summary(gls)$coefficients
                  
                  repeat.experiment[counter, 9]  <- time_optim[3]
                  repeat.experiment[counter, 10] <- result$value 
                  repeat.experiment[counter, 11] <- result$par[1]
                  
                  counter = counter + 1
                  
                  #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                  
                }
                )} 
            
            #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
          }
          )}
      
      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    }
    )}


repeat.experiment
saveRDS(repeat.experiment, file = "test.2")
counter=1


