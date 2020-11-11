## PyTorch
library(reticulate)
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

torch = reticulate::import("torch")
#' CAR model
#'
#' exp CAR model implemented in torch
#' @param y response vector
#' @param D distance matrix
#' @param epochs number of iterations
#' @param device which device to use, number == which gpu device (0-2), "cpu" for cpu
#' 
torch_car = function(y, D, epochs = 100L, device = "cpu", learningrate = 0.05)  {
  if(is.character(device)) device = torch$device("cpu")
  else device = torch$device(paste0("cuda:", device))
  dtype = torch$float32
  LT = torch$tensor(0.1, requires_grad = TRUE, device = device, dtype = dtype)$to(device)
  MT = torch$tensor(0.0, requires_grad = TRUE, device = device, dtype = dtype)$to(device)
  YT = torch$tensor(y, device = device, dtype = dtype)$to(device)
  DT = torch$tensor(D, device = device, dtype = dtype)$to(device)
  shape = torch$tensor(ncol(D), dtype=torch$long, device = device)$to(device)
  eps = torch$tensor(0.01, device = device, dtype = dtype)$to(device)
  optim = torch$optim$RMSprop(list(LT, MT), lr = learningrate)
  torch_loss_func = function(LT, MT, YT, DT, eps, shape) {
    LLT = torch$nn$functional$relu(LT)$add(eps)
    COV = DT$mul(LLT$neg())$exp() # exp(  DT * -LT ) torch$exp( torch$mul( DT, torch$neg( LT)  ) )
    loss = torch$distributions$MultivariateNormal(MT$repeat_interleave(shape), covariance_matrix = COV)$log_prob(YT)$neg()$add(MT$pow(2.0)$mul(eps))
    return(loss)
  }
  # torch_loss_func = reticulate::py_func(loss_func)
  #  if(is.character(device)) torch_loss_func = torch$jit$trace(py_loss_func, example_inputs = c(LT, MT, YT, DT, eps, shape))
  for(i in 1:epochs) {
    optim$zero_grad()
    loss = torch_loss_func(LT, MT, YT, DT, eps, shape)
    loss$backward()
    optim$step()
  }
  torch$cuda$empty_cache()
  return(list(lambda = torch$nn$functional$relu(LT)$add(eps)$data$cpu()$numpy(), MT = MT$data$cpu()$numpy()))
}




global.mu = 0
methods = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent")

repeat.experiment = data.frame(matrix(NA,1950,9))
colnames(repeat.experiment) = c("rep", "Lambda", "Side", "GPU_Time", "GPU_Lambda", "GPU_Intercept" , "CPU_Time", "CPU_Lambda", "CPU_Intercept")            # 


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
for (g in 1:5 ){
  time_repeat = 
    system.time({
      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      # Loop, wiederhole Experiment 10 mal für die Statistik
      for (h in 1:15 ){
        time_glmmTMB = 
          system.time({
            lambda = 0 + (h*0.2)
            l.r <- lambda
            
            #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
            # Loop, ändere Lambda von lambda=0.2 in 0.05 Schritten bis Lambda=0.5
            for (j in 4:29){ # change to 29
              time_side = 
                system.time({
                  side = 2+ 2*j
                  s.r <- side
                  # Loop, ändere side von 12 in 2er schritten auf 50 (daten ergeben sich aus dann aus side*side )
                  #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                  
                  D <- dist.matrix(side)
                  M <- cor.surface(side = side, lambda = lambda, global.mu = global.mu)
                  y <- as.vector(as.matrix(M))
                  #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                  ##############################################################
                  #'
                  #'
                  #'
   
                  if (l.r > 0.7 ){
                    Epochs = 300L
                    learn = 0.05
                  } 
                         if (l.r > 1.1 ){
                           Epochs = 400L
                           learn = 0.05
                         }
                                if (l.r > 1.5 ){
                                  Epochs = 500L
                                  learn = 0.05
                                }
                                       if (l.r > 1.9 ){
                                         Epochs = 600L
                                         learn = 0.05
                                       }
                                              if (l.r > 2.3 ){
                                                Epochs = 700L
                                                learn = 0.05
                                              }
                                                     if (l.r > 2.7 ){
                                                       Epochs = 700L
                                                       learn = 0.05
                                                     }
                
                  else {
                    Epochs = 100L
                    learn = 0.05
                  }
                  
      
                  
                  #'                        
                  # cpu         
                   sys.c <- system.time({res.c = torch_car(y,D, device = "CPU" ) })  # device 0,1,2 = GPU else CPU })
                   lam.c <- res.c$lambda
                   int.c <- res.c$MT
                  #'                                                                        
                  #' 
                  #'                                                                                                                                                                                               
                  # gpu  
                  sys.g <- system.time({res.g = torch_car(y,D, device = 0 , epochs = Epochs , learningrate = learn ) })  # device 0,1,2 = GPU else CPU })
                  lam.g <- res.g$lambda
                  int.g <- res.g$MT
                  
                  #'
                  #'
                  #'
                  #  time_glmmTMB = 
                  #    system.time({
                  #      fit.exp <- glmmTMB(resp ~ 1 + exp(pos + 0 | group), data = new.data1)
                  #    })
                  #'
                  #'                  
                  #'                                                      
                  #  time_gls = 
                  #    system.time({
                  #      try({
                  #        gls = gls(y ~ 1, correlation=corExp (form =~ rows + cols), data = new.data)
                  #        repeat.experiment[counter, 7] <- time_gls[3]
                  #        repeat.experiment[counter, 8] <- 1/coef(gls$modelStruct$corStruct, unconstrained = F)
                  #        repeat.experiment[counter, 9] <- summary(gls)$coefficients
                  #      }, silent=TRUE)
                  #    })
                  #'
                  #'                  
                  #'                                                      
                  #  time_optim = 
                  #     system.time({
                  #      ll = function(par) {
                  #        cov = (exp(-par[1]* new.data$D))
                  #        -mvtnorm::dmvnorm(new.data1$resp, mean = rep(par[2], side*side), sigma = cov ,log = TRUE)
                  #      }
                  #      result <- optim(par = c(0.5,10),  fn = ll, gr = NULL, method = methods[1], hessian = FALSE)
                  #   })
                  #'
                  #'                  
                  #'                                                      
                  repeat.experiment[counter, 1] <- g
                  repeat.experiment[counter, 2] <- lambda
                  repeat.experiment[counter, 3] <- s.r 
                  
                  repeat.experiment[counter, 4] <- sys.g[3] 
                  repeat.experiment[counter, 5] <- lam.g
                  repeat.experiment[counter, 6] <- int.g
                  
                  repeat.experiment[counter, 7] <- sys.c[3] 
                  repeat.experiment[counter, 8] <- lam.c
                  repeat.experiment[counter, 9] <- int.c
                  
                  counter = counter + 1
                  
                  #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                  saveRDS(repeat.experiment, file = "rescue.test21")
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

saveRDS(repeat.experiment, file = "rescue.test21")

