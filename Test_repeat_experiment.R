repeat.experiment = data.frame(matrix(NA,25,10))
for (h in 1:10){
  time_repeat = 
    system.time({
      
      
      for (h in 1:10){
        time_glmmTMB = 
          system.time({
            lambda[h] =0.15+h*0.05
            l.r <- lambda[h]
            
            
            
            for(i in 1:20){  
              time_glmmTMB = 
                system.time({
                  fit.exp <- glmmTMB(resp ~ 1 + exp(pos + 0 | group), data=data)
                })
              result_glmmTMB[i, 1] = time_glmmTMB[3]                       # Time
              result_glmmTMB[i, 2] = exp(-fit.exp$fit$par[4])              #lambda/theta?
              result_glmmTMB[i, 3] = summary(fit.exp)$coefficients$cond[1] # Intercept
              
              time_gls = 
                system.time({
                  gls <- gls(y ~ 1, correlation=corExp (form=~rows+cols), data = my.data)
                })
              result_gls[i, 1] = time_gls[3]                                            # Time
              result_gls[i, 2] = 1/coef(gls$modelStruct$corStruct, unconstrained = F)   #lambda/theta?
              result_gls[i, 3] = summary(gls)$coefficients                              # Intercept
              
              time_optim = 
                system.time({
                  ll = function(par) {
                    cov = (exp(-par[1]* my.data$D))
                    -mvtnorm::dmvnorm(my.data$y, mean = rep(par[2], 100), sigma = cov ,log = TRUE)
                  }
                  result <- optim(par = c(0.5,10),  fn = ll, gr = NULL, method = methods[1], hessian = FALSE)
                })
              result_optim[i, 1] = time_optim[3]        # Time
              result_optim[i, 2] = result$value         # niedrigster Score...
              result_optim[i, 3] = result$par[1]        # Intercept
            }
            
            repeat.experiment[counter, 1] <- lambda[h]
            #lambda.result[counter, 2] <- l.r # sites?
            
            repeat.experiment[counter, 2] <- time_glmmTMB[3] 
            repeat.experiment[counter, 3] <- exp(-fit.exp$fit$par[4])
            repeat.experiment[counter, 4] <- summary(fit.exp)$coefficients$cond[1]
            
            repeat.experiment[counter, 5] <- time_gls[3]
            repeat.experiment[counter, 6] <- 1/coef(gls$modelStruct$corStruct, unconstrained = F)
            repeat.experiment[counter, 7] <- summary(gls)$coefficients
            
            repeat.experiment[counter, 8]  <- time_optim[3]
            repeat.experiment[counter, 9] <- result$value 
            repeat.experiment[counter, 10] <- result$par[1]
            
            counter = counter + 1
          }
          
          )}
      
    }
    )}
repeat.experiment