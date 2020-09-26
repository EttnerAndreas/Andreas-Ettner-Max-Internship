dataset <- readRDS("loop4") 
dataset

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
  time_side = 
    system.time({
      c = 0.2*a
      
      
      for (j in 4:13){
        time_Lambda = 
          system.time({
            side = 2+ 2*j
            
            x1 <- mean(sapply(dataset[dataset$Lambda == 0.2*a & dataset$Side == side, 5], function(value) RMSE(value, 0.2))   )
            y1 <- mean(sapply(dataset[dataset$Lambda == 0.2*a & dataset$Side == side, 8], function(value) RMSE(value, 0.2))   ) 
            z1 <- mean(sapply(dataset[dataset$Lambda == 0.2*a & dataset$Side == side, 11], function(value) RMSE(value, 0.2))    )
            
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


