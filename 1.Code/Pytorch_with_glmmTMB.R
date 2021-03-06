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
#' 
#' 
#MC_approximation_loss = function(y, D, lambda, mu) {
#  device = torch$device("cpu")
#  dtype = torch$float32
#  LT = torch$tensor(lambda, requires_grad = TRUE, device = device, dtype = dtype)$to(device)
#  MT = torch$tensor(mu, requires_grad = TRUE, device = device, dtype = dtype)$to(device)
#  YT = torch$tensor(y, device = device, dtype = dtype)$to(device)
#  DT = torch$tensor(D, device = device, dtype = dtype)$to(device)
#  eps = torch$tensor(0.01, device = device, dtype = dtype)$to(device)
#  cov = DT$mul(LT$neg())$exp()
#  L = torch$cholesky(cov$add(torch$eye(DT$shape[0])$mul(eps)  )  )
#  noise = torch$randn(size=list(200L, 1L,DT$shape[0]))
#  Eprob = torch$tensordot(noise, L$t(), dims = 1L)$add(MT)
#  logprob = torch$distributions$Normal(Eprob, scale = 1.0)$log_prob(YT)$sum(2L)
#  maxlogprob = logprob$max(dim = 0L)$values
#  Eprob = logprob$sub(maxlogprob)$exp()$mean(dim = 0L)
#  loss = Eprob$log()$neg()$sub(maxlogprob)
#  return(loss$sum()$data$cpu()$numpy())
#//}

MC_approximation_loss = function(y, D, lambda) {
  if(is.vector(y)) y = matrix(y, nrow = 1L)
  device = torch$device("cpu")
  dtype = torch$float32
  LT = torch$tensor(lambda, requires_grad = TRUE, device = device, dtype = dtype)$to(device)
  YT = torch$tensor(y, device = device, dtype = dtype)$to(device)
  DT = torch$tensor(D, device = device, dtype = dtype)$to(device)
  eps = torch$tensor(0.01, device = device, dtype = dtype)$to(device)
  cov = DT$mul(LT$neg())$exp()
  L = torch$cholesky(cov$add(torch$eye(DT$shape[0])$mul(eps)  )  )
  noise = torch$randn(size=list(2000L, YT$shape[0],DT$shape[0]))
  Eprob = torch$tensordot(noise, L$t(), dims = 1L)$sub( torch$tensordot(noise, torch$eye(DT$shape[0]), dims = 1L) )#$add(MT)
  logprob = torch$distributions$Normal(Eprob, scale = 1.0)$log_prob(YT)$sum(2L)
  maxlogprob = logprob$max(dim = 0L)$values
  Eprob = logprob$sub(maxlogprob)$exp()$mean(dim = 0L)
  loss = Eprob$log()$neg()$sub(maxlogprob)
  return(loss$sum()$data$cpu()$numpy())
}

#' 
#' 
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

ll_mvtnorm = function(y, D, lambda) {
  -dmvnorm(y, sigma = exp(-lambda*D), log = TRUE)  # das kennst du ja
}

repeat.experiment = data.frame(matrix(NA,750,18))
colnames(repeat.experiment) = c("rep", "Lambda", "Side", "glmm_Time", "glmm_Lambda", "glmm_Intercept", "gls_Time", "gls_Lambda", "gls_Intercept", "optim_Time", "optim_Lambda", "optim_Intercept", "CPU_Time", "CPU_Lambda","CPU_Intercept", "GPU_Time", "GPU_Lambda", "GPU_Intercept" )            # 



global.mu = 0
methods = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent")



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
g = 1
h = 1
j = 1
counter=1
for (g in 1:5 ){
  time_repeat = 
    system.time({
      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      # Loop, wiederhole Experiment 10 mal für die Statistik
      for (h in 1:10 ){
        time_glmmTMB = 
          system.time({
            lambda = 0 + (h*0.2)
            l.r <- lambda
         
            #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
            # Loop, ändere Lambda von lambda=0.2 in 0.05 Schritten bis Lambda=0.5
            for (j in 2:11){
              time_side = 
                system.time({
                  side = 0+ 5*j
                  s.r <- side
                  # Loop, ändere side von 12 in 2er schritten auf 50 (daten ergeben sich aus dann aus side*side )
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                  
                  D <- dist.matrix(side)
                  M <- cor.surface(side = side, lambda = lambda, global.mu = global.mu)
                  y <- as.vector(as.matrix(M))
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                  new.data1 = 0
                  new.data  = 0
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
                  ##############################################################
                  #'
                  #'
                  #'                        
                  # cpu 
                  if (l.r > 0.6 ){
                    Epochs = 300L
                    learn = 0.05
                  } 
                  else {
                    Epochs = 100L
                    learn = 0.05
                  }
                  if (l.r > 1.2 ){
                    Epochs = 500L
                    
                  } 
                  if (l.r > 1.8 ){
                    Epochs = 750L
           
                  } 
                  
                  if (l.r > 2.4 ){
                    Epochs = 1200L
                    learn = 0.06
                  } 
                  ?gls
                  
            #      sys.c <- system.time({res.c = torch_car(y,D, device = "CPU" , epochs = Epochs , learningrate = learn ) })     # device 0,1,2 = GPU else CPU })
             #     lam.c <- res.c$lambda
              #    int.c <- res.c$MT
                  #' 
                  #' MC loss function
                  #' 
                  #' 

                  lambdas = seq(0.01, 2.0, length.out=100)
                  res = NULL
                  mos = NULL
                  for(i in 1:100){
                    res[i] = MC_approximation_loss(y, D, lambdas[i])
                    mos[i] = ll_mvtnorm(y,D,lambdas[i])
                  }
                  
                  plot(1:100, res, xlab = "PyTorch")
                  plot(2:100, mos[2:100], xlab = "optim")
                  
                  ei <- optim(res = c(0.5,10),  fn = ll, gr = NULL, method = methods[2], hessian = FALSE)

                  #'                                                                                                                                                 
                  #'                                                                                                                                                                                                                      
                  #' 
                  #'                                                                                                                                                                                               
                  # gpu  

                  sys.g <- system.time({res.g = torch_car(y,D, device = 0 , epochs = Epochs , learningrate = learn ) })        # device 0,1,2 = GPU else CPU })
                  lam.g <- res.g$lambda
                  int.g <- res.g$MT
                 
                  #'
                  #'
                  #'
                  time_glmmTMB = 
                    system.time({
                      fit.exp <- glmmTMB(resp ~ 1 + exp(pos + 0 | group), data = new.data1 )
                    })
                  
                  #'
                  #' 
                  time_glmmTMB.new = 
                    system.time({
                      fit.new <- glmmTMB(resp ~ 1 + exp(pos + 0 | group), data = new.data1 , control=glmmTMBControl(optimizer=optim, optArgs=list(method="Nelder-Mead")))
                    })                 
                  #'                                   
                  #'                                                    
                  #'                                                                                      
                  #'                                                      
                  time_gls = 
                    system.time({
                      try({
                        gls = gls(y ~ 1, correlation=corExp (form =~ rows + cols), data = new.data)
                        repeat.experiment[counter, 7] <- time_gls[3]
                        repeat.experiment[counter, 8] <- 1/coef(gls$modelStruct$corStruct, unconstrained = F)
                        repeat.experiment[counter, 9] <- summary(gls)$coefficients
                      }, silent=TRUE)
                    })
                
                
                    
                  #'
                  #'                  
                  #'                                                      
                  time_optim1 = 
                    system.time({
                      ll = function(par) {
                        cov = (exp(-par[1]* new.data$D))
                        -mvtnorm::dmvnorm(new.data1$resp, mean = rep(par[2], side*side), sigma = cov ,log = TRUE)
                      }
                      result1 <- optim(par = c(0.5,10),  fn = ll, gr = NULL, method = methods[1], hessian = FALSE)
                    })
                  time_optim2 = 
                    system.time({
                      ll = function(par) {
                        cov = (exp(-par[1]* new.data$D))
                        -mvtnorm::dmvnorm(new.data1$resp, mean = rep(par[2], side*side), sigma = cov ,log = TRUE)
                      }
                      result2 <- optim(par = c(0.5,10),  fn = ll, gr = NULL, method = methods[2], hessian = FALSE)
                    }) 
                  #'                                   
                  #'                                                      
                  repeat.experiment[counter, 1] <- g
                  repeat.experiment[counter, 2] <- lambda
                  repeat.experiment[counter, 3] <- s.r 
                  
                  repeat.experiment[counter, 4] <-  time_glmmTMB[3] 
                  repeat.experiment[counter, 5] <-  exp(-fit.exp$fit$par[4])
                  repeat.experiment[counter, 6] <-  summary(fit.exp)$coefficients$cond[1]
                  
                  repeat.experiment[counter, 10] <- time_glmmTMB.new[3]
                  repeat.experiment[counter, 11] <- exp(-fit.new$fit$par[4])
                  repeat.experiment[counter, 12] <- summary(fit.new)$coefficients$cond[1]
                  
                  repeat.experiment[counter, 13] <-  sys.c[3]
                  repeat.experiment[counter, 14] <-  lam.c
                  repeat.experiment[counter, 15] <-  int.c
                  
                  repeat.experiment[counter, 16] <- sys.g[3]
                  repeat.experiment[counter, 17] <- lam.g
                  repeat.experiment[counter, 18] <- int.g
                  
                  
                  counter = counter + 1
                  
                  #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                  saveRDS(repeat.experiment, file = "safe.data")
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

saveRDS(repeat.experiment, file = "safe.data")