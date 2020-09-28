dataset <- readRDS("loop4") 
dataset
RMSE = function(obs, true) sqrt(mean((obs-true)^2))

data.all   = data.frame(matrix(NA,10,31))
data.glmm  = data.frame(matrix(NA,10,11))
data.gls   = data.frame(matrix(NA,10,11))
data.optim  = data.frame(matrix(NA,10,11))


colnames(data.all) = c("Lambda","glmm10","gls10","optim10","glmm12","gls12","optim12","glmm14","gls14","optim14","glmm16","gls16","optim16","glmm18","gls18","optim18","glmm20","gls20","optim20","glmm22","gls22","optim22","glmm24","gls24","optim24","glmm26","gls26","optim26","glmm28","gls28","optim28")
colnames(data.glmm) = c("Lambda","glmm__10","glmm__12","glmm__14","glmm__6","glmm__18","glmm__20","glmm__22","glmm__24","glmm__26","glmm__28")
colnames(data.gls) = c("Lambda","gls__10","gls__12","gls__14","gls__6","gls__18","gls__20","gls__22","gls__24","gls__26","gls__28")
colnames(data.optim) = c("Lambda","optim__10","optim__12","optim__14","optim__6","optim__18","optim__20","optim__22","optim__24","optim__26","optim__28")




zähler = 1
counter = 1
steps = 1
for (a in 1:10){ # Dieser Loop gibt mir den Mean von den Ergebnis(estimated Lambda) - Lambda
  time_lambda = 
    system.time({
      c = 0.2*a
      
      
      for (j in 4:13){
        time_side = 
          system.time({
            side = 2+ 2*j
            
            x1 <- mean(sapply(dataset[dataset$Lambda == 0.2*a & dataset$Side == side, 5], function(value) RMSE(value, 0.2*a))   )
            y1 <- mean(sapply(dataset[dataset$Lambda == 0.2*a & dataset$Side == side, 8], function(value) RMSE(value, 0.2*a))   ) 
            z1 <- mean(sapply(dataset[dataset$Lambda == 0.2*a & dataset$Side == side, 11], function(value) RMSE(value, 0.2*a))    )
            
            data.all[counter, 1] <- c
            data.glmm[counter, 1] <- c
            data.gls[counter, 1] <- c
            data.optim[counter, 1] <- c
            
            
            data.glmm[counter, steps +1] <- x1
            data.gls[counter, steps +1]  <- y1
            data.optim[counter, steps +1] <- z1
 

            data.all[counter, zähler +1]  <- x1
            data.all[counter, zähler +2]  <- y1
            data.all[counter, zähler +3]  <- z1
            
            zähler = zähler + 3
            steps = steps +1 
          })
      }
 
      counter = counter + 1
      zähler = 1
      steps = 1
    }
)}


data.all
data.glmm
data.gls
data.optim



matplot(data.all,type ="o", ylab = "RMSE", xlab = "Lambda")
matplot(data.glmm,type ="o", ylab = "RMSE", xlab = "Lambda")
matplot(data.gls,type ="o", ylab = "RMSE", xlab = "Lambda")
matplot(data.optim,type ="o", ylab = "RMSE", xlab = "Lambda")
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX




time.all   = data.frame(matrix(NA,10,31))
colnames(time.all) = c("Side","t_glmm10","t_gls10","t_optim10","t_glmm12","t_gls12","t_optim12","t_glmm14","t_gls14","t_optim14","t_glmm16","t_gls16","t_optim16","t_glmm18","t_gls18","t_optim18","t_glmm20","t_gls20","t_optim20","t_glmm22","t_gls22","t_optim22","t_glmm24","t_gls24","t_optim24","t_glmm26","t_gls26","t_optim26","t_glmm28","t_gls28","ot_ptim28")



zähler = 1
counter = 1
steps = 1

for (j in 4:13){
  time_lambda = 
    system.time({
      side = 2+ 2*j

        for (a in 1:10){
        time_side = 
          system.time({


            
            x1 <- mean(dataset[dataset$Side == side, 4]   )
            y1 <- mean(dataset[dataset$Side == side, 7]   )
            z1 <- mean(dataset[dataset$Side == side, 10]   )
  
            time.all[counter, 1] <- 2+ 2*j
            
            time.all[counter, zähler +1]  <- x1
            time.all[counter, zähler +2]  <- y1
            time.all[counter, zähler +3]  <- z1
            
            zähler = zähler + 3
          })
      }
     
      counter = counter + 1
      zähler = 1
    }
)}

time.all


matplot(time.all,type ="o", ylab = "Time", xlab = "Sidegröße")
legend(x = "topright",c("glmmTMB"), col = c("red"),text.col = "red4", lty = c(2),merge = TRUE)
legend(x = "bottomright",c( "gls", "optim"), col = c("darkgreen", "darkgreen"),text.col = "green4", lty = c(2, 2),merge = TRUE)



plot(dataset$Lambda ~ dataset$glmm_Lambda)



