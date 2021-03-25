dataset <- repeat.experiment
dataset
RMSE = function(obs, true) sqrt(mean((obs-true)^2))

data.all   = data.frame(matrix(NA,15,6))
data.glmm  = data.frame(matrix(NA,15,11))  #27 davor
data.gls   = data.frame(matrix(NA,15,6))
data.optim  = data.frame(matrix(NA,15,6))
data.cpu  = data.frame(matrix(NA,15,6))
data.cpu.xo  = data.frame(matrix(NA,15,6))
data.trash  = data.frame(matrix(NA,15,6))

#'  > 26*15*10
#'  [1] 3900

colnames(data.all) = c("Lambda","glmm10","gls10","optim10","CPU__10","GPU__10","glmm12","gls12","optim12","CPU__12","GPU__12","glmm14","gls14","optim14","CPU__14","GPU__14","glmm16","gls16","optim16","CPU__16","GPU__16","glmm18","gls18","optim18","CPU__18","GPU__18","glmm20","gls20","optim20","CPU__20","GPU__20","glmm22","gls22","optim22","CPU__22","GPU__22","glmm24","gls24","optim24","CPU__24","GPU__24","glmm26","gls26","optim26","CPU__26","GPU__26","glmm28","gls28","optim28","CPU__28","GPU__28",    "glmm30","gls30","optim30","CPU__30","GPU__30"    ,"glmm32","gls32","optim32","CPU__32","GPU__32",     "glmm34","gls34","optim34","CPU__34","GPU__34",    "glmm34","gls34","optim34","CPU__34","GPU__34",    "glmm36","gls36","optim36","CPU__36","GPU__36",    "glmm38","gls38","optim38","CPU__38","GPU__38"    ,"glmm40","gls40","optim40","CPU__40","GPU__40"   ,"glmm42","gls42","optim42","CPU__42","GPU__42"   ,"glmm44","gls44","optim44","CPU__44","GPU__44"  ,"glmm46","gls46","optim46","CPU__46","GPU__46"   ,"glmm48","gls48","optim48","CPU__48","GPU__48"  ,"glmm50","gls50","optim50","CPU__50","GPU__50"  ,"glmm52","gls52","optim52","CPU__52","GPU__52"   ,"glmm54","gls54","optim54","CPU__54","GPU__54"   ,"glmm56","gls56","optim56","CPU__56","GPU__56"   ,"glmm58","gls58","optim58","CPU__58","GPU__58")      #
colnames(data.glmm) = c("Lambda","glmm__10","glmm__12","glmm__14","glmm__16","glmm__18","glmm__20","glmm__22","glmm__24","glmm__26","glmm__28")
colnames(data.gls) = c("Lambda","gls__10","gls__12","gls__14","gls__16","gls__18","gls__20","gls__22","gls__24","gls__26","gls__28","gls__30","gls__32" ,"gls__34","gls__36","gls__38","gls__40","gls__42","gls__44","gls__46","gls__48","gls__50","gls__52","gls__54","gls__56","gls__58","gls__60")
colnames(data.optim) = c("Lambda","optim__10","optim__12","optim__14","optim__16","optim__18","optim__20","optim__22","optim__24","optim__26","optim__28","optim__30","optim__32","optim__34","optim__36","optim__38","optim__40","optim__42","optim__44","optim__46","optim__48","optim__50","optim__52","optim__54","optim__56","optim__58","optim__60")
colnames(data.cpu) = c("Lambda","CPU__10","CPU__12","CPU__14","CPU__16","CPU__18","CPU__20","CPU__22","CPU__24","CPU__26","CPU__28","CPU__30","CPU__32","CPU__34","CPU__36","CPU__38","CPU__40","CPU__42","CPU__44","CPU__46","CPU__48","CPU__50","CPU__52","CPU__54","CPU__56","CPU__58","CPU__60")
colnames(data.cpu.xo) = c("Lambda","GPU__10","GPU__12","GPU__14","GPU__16","GPU__18","GPU__20","GPU__22","GPU__24","GPU__26","GPU__28","GPU__30","GPU__32","GPU__34","GPU__36","GPU__38","GPU__40","GPU__42","GPU__44","GPU__46","GPU__48","GPU__50","GPU__52","GPU__54","GPU__56","GPU__58","GPU__60")


colnames(data.trash) = c("Lambda","glmm__10","Newton_10", "glmm__15","Newton_15","glmm__20","Newton_20","glmm__25","Newton_25","glmm__30","Newton_30","glmm__35","Newton_35","glmm__40","Newton_40","glmm__45","Newton_45","glmm__50","Newton_50","glmm__55","Newton_55")
colnames(data.optim) = c("Lambda","optim__10","optim__15","optim__20","optim__25","optim__30","optim__35","optim__40","optim__45","optim__50","optim__55")

data.glmm  = data.frame(matrix(NA,15,11)) 
colnames(data.glmm) = c("Lambda","glmm__10","glmm__12","glmm__14","glmm__16","glmm__18","glmm__20","glmm__22","glmm__24","glmm__26","glmm__28")

a=1
j=4
zähler = 1
stip = 1
counter = 1
steps = 1
for (a in 1:10){ # Dieser Loop gibt mir den Mean von den Ergebnis(estimated Lambda) - Lambda
  time_lambda = 
    system.time({
      c = 0.2*a
      
      
      # for (j in 4:29){ # change to 29
      for (j in 4:13){
        time_side = 
          system.time({
            #  side = 2+ 2*j
            side = 2+ 2*j
            
            
            #     dataset[dataset$Lambda == c , ]
            #     dataset[ dataset$Side == side, ]
            dataset[ dataset$Side == side,  ]
            dataset[dataset$Lambda == c & dataset$Side == side, ]
            dataset
            #'
            x1 <- mean(sapply(dataset[dataset$Lambda == c & dataset$Side == side, 5 ], function(value) RMSE(value, 0.2*a)) [1:5]  )
            x1
            #y1 <- mean(sapply(dataset[dataset$Lambda == c & dataset$Side == side, 8 ], function(value) RMSE(value, 0.2*a))  [1:1]  ) 
            #z1 <- mean(sapply(dataset[dataset$Lambda == c & dataset$Side == side, 11], function(value) RMSE(value, 0.2*a))  [1:1]  )
            #w1 <- mean(sapply(dataset[dataset$Lambda == c & dataset$Side == side, 14], function(value) RMSE(value, 0.2*a))  [1:1]  )
            #v1 <- mean(sapply(dataset[dataset$Lambda == c & dataset$Side == side, 17], function(value) RMSE(value, 0.2*a))  [1:1]  )
            #a1 <- mean(sapply(dataset[dataset$Lambda == c , 5], function(value) RMSE(value, 0.2*a)) [1:20] )
            
            
            #'
            #'
            
            # mean() vor sapply und [1] am Ende entfernen 
            #   x1 <- (sapply(dataset[dataset$Lambda == c & dataset$Side == side, 5], function(value) RMSE(value, 0.2*a))  [1] )
            #   y1 <- (sapply(dataset[dataset$Lambda == c & dataset$Side == side, 8], function(value) RMSE(value, 0.2*a))  [1] ) 
            #   z1 <- (sapply(dataset[dataset$Lambda == c & dataset$Side == side, 11], function(value) RMSE(value, 0.2*a))  [1]  )
            #   w1 <- (sapply(dataset[dataset$Lambda == c & dataset$Side == side, 14], function(value) RMSE(value, 0.2*a))  [1]  )
            #   v1 <- (sapply(dataset[dataset$Lambda == c & dataset$Side == side, 17], function(value) RMSE(value, 0.2*a))   [1] )
            #'
            #'
            #'
            #data.all[counter, 1] <- c
            data.glmm[counter, 1] <- c
            #data.gls[counter, 1] <- c
            #data.optim[counter, 1] <- c
            #data.cpu[counter, 1] <- c
            #data.cpu.xo[counter, 1] <- c
            #data.trash[counter, 1] <- c
            #'
            #'
            #'
            data.glmm[counter, steps +1] <- x1
            #data.gls[counter, steps +1]  <- y1
           # data.optim[counter, steps +1] <- z1
           # data.cpu[counter, steps +1] <- w1
            #data.cpu.xo[counter, steps +1] <- v1
            #'
            #'
            #'
            #data.all[counter, zähler +1]  <- x1
            #data.all[counter, zähler +2]  <- y1
            #data.all[counter, zähler +3]  <- z1
            #data.all[counter, zähler +4] <- w1
            #data.all[counter, zähler +5] <- v1
            #data.trash[counter, stip + 1] <- x1
            #data.trash[counter, stip +2 ] <- z1
            
            zähler = zähler + 1
            steps = steps +1 
            stip = stip + 2
          })
      }
      
      counter = counter + 1
      zähler = 1
      steps = 1
      stip = 1
    }
    )}

dataset
data.all
data.glmm
data.gls
data.optim
data.cpu
data.cpu.xo

data.trash
matplot(data.trash,type ="o", ylab = "RMSE score",  pch= c(1,2,3,4,5,6,7,8,9,10),xlab = " glmm estimated lambda", x = data.glmm$Lambda,  cex = c(1,1,1,1,1,1,1,1,1,1,1), lty = 1,lwd= "1", las = 1, bg = "white",col = c("black",col10a,col10b,col11[1:3]))

#'
#'
?legend



#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#   GLMM TMB
matplot(data.glmm,type ="o", ylab = "RMSE score",  pch= c(1,2,3,4,5,6,7,8,9,10),xlab = " glmm estimated lambda", x = data.glmm$Lambda,  cex = c(1,1,1,1,1,1,1,1,1,1,1), lty = 1,lwd= "1", las = 1, bg = "white",col = c("black",col10a,col10b,col11[1:3]))
matplot(data.glmm,type ="o", ylab = "RMSE score",  pch= c(1,2,3,4,5,6,7,8,9,10),xlab = " glmm default estimated lambda", x = data.glmm$Lambda,  cex = c(1,1,1,1,1,1,1,1,1,1,1), lty = 1,lwd= "1", las = 1, bg = "white",col = c("black",col10a,col10b,col11[1:3]), xlim = c(0.2,2), ylim = c(0,2))
legend(x = "topleft", pch= c(1,2,3,4,5,6,7,8,9,10), c("slope = 1", "side 10", "side 15", "side 20", "side 25", "side 30", "side 35", "side 40", "side 45", "side 50","side 55"),box.lty = 0,box.col = 0, box.lwd = 2, cex = 0.7, lwd = 1, lty = c(2,2), col = c("black",col10a,col10b,col11) , text.col = c("black",col10a,col10b,col11), merge = TRUE )
#' legend(x = "topleft", pch= c(1), c("slope = 1"),box.lty = 0,box.col = "white", box.lwd = 2, cex = 0.75, lwd = 2, lty = c(2,2), col = c("black",col10a,col10b,col11) , text.col = c("black",col10a,col10b,col11), merge = TRUE)
abline(0,1, lwd= 1,pch = 1, cex = 1)


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#   GLS
matplot(data.gls,type ="o", ylab = "RMSE score",  pch= c(1,2,3,4,5,6,7,8,9,10),xlab = " gls estimated lambda", x = data.gls$Lambda,  cex = c(1,1,1,1,1,1,1,1,1,1,1), lty = 1,lwd= "0.8", las = 1, bg = "white",col = c("black",col10a,col10b,col11[1:3]))
legend(x = "topleft", pch= c(1,2,3,4,5,6,7,8,9,10), c("slope = 1", "side 10", "side 15", "side 20", "side 25", "side 30", "side 35", "side 40", "side 45", "side 50","side 55"),box.lty = 0,box.col = 0, box.lwd = 2, cex = 0.81, lwd = 1, lty = c(2,2), col = c("black",col10a,col10b,col11) , text.col = c("black",col10a,col10b,col11), merge = TRUE)
#' legend(x = "topleft", pch= c(1), c("slope = 1"),box.lty = 0,box.col = "white", box.lwd = 2, cex = 0.75, lwd = 2, lty = c(2,2), col = c("black",col10a,col10b,col11) , text.col = c("black",col10a,col10b,col11), merge = TRUE)
abline(0,1, lwd= 1,pch = 1, cex = 1)


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#  OPTIM 
matplot(data.optim,type ="o", ylab = "RMSE score",  pch= c(1,2,3,4,5,6,7,8,9,10),xlab = " glmm Nelder mead estimated lambda", x = data.optim$Lambda,  cex = c(1,1,1,1,1,1,1,1,1,1,1), lty = 1,lwd= "0.8", las = 1, bg = "white",col = c("black",col10a,col10b,col11[1:3]), ylim = c(0,2))
legend(x = "topleft", pch= c(1,2,3,4,5,6,7,8,9,10), c("slope = 1", "side 10", "side 15", "side 20", "side 25", "side 30", "side 35", "side 40", "side 45", "side 50","side 55"),box.lty = 0,box.col = 0, box.lwd = 2, cex = 0.81, lwd = 1, lty = c(2,2), col = c("black",col10a,col10b,col11) , text.col = c("black",col10a,col10b,col11), merge = TRUE)
#' legend(x = "topleft", pch= c(1), c("slope = 1"),box.lty = 0,box.col = "white", box.lwd = 2, cex = 0.75, lwd = 2, lty = c(2,2), col = c("black",col10a,col10b,col11) , text.col = c("black",col10a,col10b,col11), merge = TRUE)
abline(0,1, lwd= 1,pch = 1, cex = 1)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#   CPU only 
matplot(data.cpu,type ="o", ylab = "RMSE score",  pch= c(1,2,3,4,5,6,7,8,9,10),xlab = " optim newton estimated lambda", x = data.cpu$Lambda,  cex = c(1,1,1,1,1,1,1,1,1,1,1), lty = 1,lwd= "0.8", las = 1, bg = "white",col = c("black",col10a,col10b,col11[1:3]), ylim = c(0,2))
legend(x = "topleft", pch= c(1,2,3,4,5,6,7,8,9,10), c("slope = 1", "side 10", "side 15", "side 20", "side 25", "side 30", "side 35", "side 40", "side 45", "side 50","side 55"),box.lty = 0,box.col = 0, box.lwd = 2, cex = 0.81, lwd = 1, lty = c(2,2), col = c("black",col10a,col10b,col11) , text.col = c("black",col10a,col10b,col11), merge = TRUE)
#' legend(x = "topleft", pch= c(1), c("slope = 1"),box.lty = 0,box.col = "white", box.lwd = 2, cex = 0.75, lwd = 2, lty = c(2,2), col = c("black",col10a,col10b,col11) , text.col = c("black",col10a,col10b,col11), merge = TRUE)
abline(0,1, lwd= 1,pch = 1, cex = 1)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#   GPU only
matplot(data.cpu.xo,type ="o", ylab = "RMSE score",  pch= c(1,2,3,4,5,6,7,8,9,10),xlab = " GPU estimated lambda", x = data.cpu.xo$Lambda,  cex = c(1,1,1,1,1,1,1,1,1,1,1), lty = 1,lwd= "0.8", las = 1, bg = "white",col = c("black",col10a,col10b,col11[1:3]), ylim = c(0,2))

legend(x = "topleft", pch= c(1,2,3,4,5,6,7,8,9,10), c("slope = 1", "side 10", "side 15", "side 20", "side 25", "side 30", "side 35", "side 40", "side 45", "side 50","side 55"),box.lty = 0,box.col = 0, box.lwd = 2, cex = 0.81, lwd = 1, lty = c(2,2), col = c("black",col10a,col10b,col11) , text.col = c("black",col10a,col10b,col11), merge = TRUE)
#' legend(x = "topleft", pch= c(1), c("slope = 1"),box.lty = 0,box.col = "white", box.lwd = 2, cex = 0.75, lwd = 2, lty = c(2,2), col = c("black",col10a,col10b,col11) , text.col = c("black",col10a,col10b,col11), merge = TRUE)
abline(0,1, lwd= 1,pch = 1, cex = 1)


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#'
#'
#'       GLOBAL MU
#'
#'
#'
#'
#'
#'
#'
RMSE2 = function(obs, true) (mean((obs-true)))

mu.all    = data.frame(matrix(NA,21,7))
mu.glmm   = data.frame(matrix(NA,21,27))  #27 davor
mu.gls    = data.frame(matrix(NA,21,27))
mu.optim  = data.frame(matrix(NA,21,27))
mu.cpu    = data.frame(matrix(NA,21,27))
mu.cpu.xo = data.frame(matrix(NA,21,27))


#colnames(mu.all) = c("side", "glmm mu","gls.mu","optim mu","CPU mu","GPU mu")
#colnames(mu.glmm) = c("side",  "glmm mu")
#colnames(mu.gls) = c("side",  "gls.mu")
#colnames(mu.optim) = c("side",  "optim mu")
#colnames(mu.cpu) = c("side", "CPU mu")
#colnames(mu.cpu.xo) = c("side",  "GPU mu")

#'  3 er
colnames(mu.all) = c("lambda", "global.mu", "glmm mu","gls.mu","optim mu","CPU mu","GPU mu")
colnames(mu.glmm) = c("lambda", "global.mu", "glmm mu")
colnames(mu.gls) = c("lambda", "global.mu", "gls.mu")
colnames(mu.optim) = c("lambda", "global.mu", "optim mu")
colnames(mu.cpu) = c("lambda", "global.mu", "CPU mu")
colnames(mu.cpu.xo) = c("lambda", "global.mu", "GPU mu")



a=1
j=4
zähler = 1
counter = 1
steps = 1
for (a in 1:10){ # Dieser Loop gibt mir den Mean von den Ergebnis(estimated Lambda) - Lambda
  time_lambda = 
    system.time({
      c = 0.2*a
      
      
      for (j in 4:13){ # change to 29
        #for (j in 2:11){
        time_side = 
          system.time({
            side = 2+ 2*j
            # side = 0 + 5*j
            dataset[dataset$Lambda == c & dataset$Side == side,  ]
            
            #     dataset[dataset$Lambda == c , ]
            #     dataset[ dataset$Side == side, ]
            dataset[ dataset$Side == side,  ]
            #'
            x1 <- mean(sapply(dataset[dataset$Lambda == c & dataset$Side == side, 6 ], function(value) RMSE2(value, global.mu))  [1:2]  )
            #y1 <- mean(sapply(dataset[dataset$Lambda == c & dataset$Side == side, 9 ], function(value) RMSE2(value, global.mu))  [1:2]  ) 
            #z1 <- mean(sapply(dataset[dataset$Lambda == c & dataset$Side == side, 12], function(value) RMSE2(value, global.mu))  [1:2]  )
            #w1 <- mean(sapply(dataset[dataset$Lambda == c & dataset$Side == side, 15], function(value) RMSE2(value, global.mu))  [1:2]  )
            #v1 <- mean(sapply(dataset[dataset$Lambda == c & dataset$Side == side, 18], function(value) RMSE2(value, global.mu))  [1:2]  )
            #'
            #'
            mu.all[counter, 1] <- c
            mu.glmm[counter, 1] <- c
            mu.gls[counter, 1] <- c
            mu.optim[counter, 1] <- c
            mu.cpu[counter, 1] <- c
            mu.cpu.xo[counter, 1] <- c
            #'
            #'
            #'
            mu.glmm[counter, steps +1] <- x1
            #mu.gls[counter, steps +1]  <- y1
            #mu.optim[counter, steps +1] <- z1
            #mu.cpu[counter, steps +1] <- w1
           # mu.cpu.xo[counter, steps +1] <- v1
            #'
            #'
            #'
            #mu.all[counter, zähler +1]  <- x1
            #mu.all[counter, zähler +2]  <- y1
            #mu.all[counter, zähler +3]  <- z1
            #mu.all[counter, zähler +4] <- w1
            #mu.all[counter, zähler +5] <- v1
            
            
            
            zähler = zähler + 5
            steps = steps +1 
          })
      }
      
      counter = counter + 1
      zähler = 1
      steps = 1
    }
    )}





mu.all   
mu.glmm  
mu.gls   
mu.optim  
mu.cpu  
mu.cpu.xo
col10b


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#   GLMM TMB
matplot(mu.glmm[2:15] ,type ="o", ylab = "global mu",  pch= 1,xlab = " glmmTMB estimated lambda", x = mu.glmm$lambda   ,  cex = 2.0, lty = 0,lwd= "0.9", las = 1, bg = "white",col = c(col10a,col10b,col11[1:3]),ylim = c(-0.6,0.6))
legend(1.15,0.7, pch= c(1,2,3,4,5,6,7,8,9,10), c( "side 10", "side 15", "side 20", "side 25", "side 30"),box.lty = 0,box.col = 0, box.lwd = 2, cex = 0.79, lwd = 1, lty = c(2,2), col = c(col10a,col10b,col11) , text.col = c(col10a,col10b,col11), merge = TRUE)
legend(1.6,0.7, pch= c(6,7,8,9,10), c( "side 35", "side 40", "side 45", "side 50", "side 55"),box.lty = 0,box.col = 0, box.lwd = 2, cex = 0.79, lwd = 1, lty = c(2,2), col = c(col10b[3:4],col11[1:3]) , text.col = c(col10b[3:4],col11[1:3]), merge = TRUE)


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#   GLS
matplot(mu.gls[2:15] ,type ="o", ylab = "global mu",  pch= c(1,2,3,4,5,6,7,8,9,10),xlab = " gls estimated lambda", x = mu.gls$lambda   ,  cex = c(1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5), lty = 5,lwd= "1", las = 1, bg = "white",col = c(col10a,col10b,col11[1:3]),ylim = c(-0.6,0.6))
legend(1.15,0.7, pch= c(1,2,3,4,5,6,7,8,9,10), c( "side 10", "side 15", "side 20", "side 25", "side 30"),box.lty = 0,box.col = 0, box.lwd = 2, cex = 0.79, lwd = 1, lty = c(2,2), col = c(col10a,col10b,col11) , text.col = c(col10a,col10b,col11), merge = TRUE)
legend(1.6,0.7, pch= c(6,7,8,9,10), c( "side 35", "side 40", "side 45", "side 50", "side 55"),box.lty = 0,box.col = 0, box.lwd = 2, cex = 0.79, lwd = 1, lty = c(2,2), col = c(col10b[3:4],col11[1:3]) , text.col = c(col10b[3:4],col11[1:3]), merge = TRUE)
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#  OPTIM 
matplot(mu.optim[2:15] ,type ="o", ylab = "global mu",  pch= c(1,2,3,4,5,6,7,8,9,10),xlab = " optim estimated lambda", x = mu.optim$lambda   ,  cex = c(1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5), lty = 5,lwd= "1", las = 1, bg = "white",col = c(col10a,col10b,col11[1:3]),ylim = c(-0.6,0.6))
legend(1.15,0.7, pch= c(1,2,3,4,5,6,7,8,9,10), c( "side 10", "side 15", "side 20", "side 25", "side 30"),box.lty = 0,box.col = 0, box.lwd = 2, cex = 0.79, lwd = 1, lty = c(2,2), col = c(col10a,col10b,col11) , text.col = c(col10a,col10b,col11), merge = TRUE)
legend(1.6,0.7, pch= c(6,7,8,9,10), c( "side 35", "side 40", "side 45", "side 50", "side 55"),box.lty = 0,box.col = 0, box.lwd = 2, cex = 0.79, lwd = 1, lty = c(2,2), col = c(col10b[3:4],col11[1:3]) , text.col = c(col10b[3:4],col11[1:3]), merge = TRUE)
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#   CPU only 
matplot(mu.cpu[2:15] ,type ="o", ylab = "global mu",  pch= c(1,2,3,4,5,6,7,8,9,10),xlab = " CPU estimated lambda", x = mu.cpu$lambda   ,  cex = c(1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5), lty = 5,lwd= "1", las = 1, bg = "white",col = c(col10a,col10b,col11[1:3]),ylim = c(-0.6,0.6))
legend(1.15,0.7, pch= c(1,2,3,4,5,6,7,8,9,10), c( "side 10", "side 15", "side 20", "side 25", "side 30"),box.lty = 0,box.col = 0, box.lwd = 2, cex = 0.79, lwd = 1, lty = c(2,2), col = c(col10a,col10b,col11) , text.col = c(col10a,col10b,col11), merge = TRUE)
legend(1.6,0.7, pch= c(6,7,8,9,10), c( "side 35", "side 40", "side 45", "side 50", "side 55"),box.lty = 0,box.col = 0, box.lwd = 2, cex = 0.79, lwd = 1, lty = c(2,2), col = c(col10b[3:4],col11[1:3]) , text.col = c(col10b[3:4],col11[1:3]), merge = TRUE)
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#   GPU only
matplot(mu.cpu.xo[2:15] ,type ="o", ylab = "global mu",  pch= c(1,2,3,4,5,6,7,8,9,10),xlab = " GPU estimated lambda", x = mu.cpu.xo$lambda   ,  cex = c(1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5), lty = 5,lwd= "1", las = 1, bg = "white",col = c(col10a,col10b,col11[1:3]),ylim = c(-0.6,0.6))
legend(1.15,0.7, pch= c(1,2,3,4,5,6,7,8,9,10), c( "side 10", "side 15", "side 20", "side 25", "side 30"),box.lty = 0,box.col = 0, box.lwd = 2, cex = 0.79, lwd = 1, lty = c(2,2), col = c(col10a,col10b,col11) , text.col = c(col10a,col10b,col11), merge = TRUE)
legend(1.6,0.7, pch= c(6,7,8,9,10), c( "side 35", "side 40", "side 45", "side 50", "side 55"),box.lty = 0,box.col = 0, box.lwd = 2, cex = 0.79, lwd = 1, lty = c(2,2), col = c(col10b[3:4],col11[1:3]) , text.col = c(col10b[3:4],col11[1:3]), merge = TRUE)


#'
#'
#'
#'
#'
#'
#'
#'
#'
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
dataset


time.all   = data.frame(matrix(NA,20,6))
colnames(time.all) = c("side", "time of glmmTMB", "time of gls","time of optim","time of CPU","time of GPU")
time.short   = data.frame(matrix(NA,20,4))
colnames(time.short) = c("side", "time of GLS","time of optim","time of GPU")

dataset
counter = 1
steps = 1
j = 5
#for (j in 4:24){
for (j in 2:11){
  time_lambda = 
    system.time({
      # side = 2+ 2*j
      side = 0 + 5*j
      
      for (a in 1:20){
        time_side = 
          system.time({
            
            
            
            x1 <- mean(dataset[dataset$Side == side, 4]   [1:1]) 
            y1 <- mean(dataset[dataset$Side == side, 7]  [1:1] ) 
            z1 <- mean(dataset[dataset$Side == side, 10] [1:1]  ) 
            w1 <- mean(dataset[dataset$Side == side, 13]   [1:1]  )  
            v1 <- mean(dataset[dataset$Side == side, 16]   [1:1] ) 
            
            
            time.all[counter, 1] <- side*side
            
            time.all[counter, 2]  <- v1 /60/60
            time.all[counter, 3]  <- x1 /60/60
            time.all[counter, 4]  <- z1 /60/60
            time.all[counter, 5]  <- w1 /60/60
            time.all[counter, 6]  <- y1 /60/60
            
            
            time.short[counter, 1] <- side*side
            #   time.short[counter, 2]  <- x1
            time.short[counter, 3]  <- y1/60
            time.short[counter, 2]  <- z1/60
            #   time.short[counter, 3]  <- w1
            time.short[counter, 4]  <- v1/60
            #time.all[counter, 5]  <- (j-4)^2 
            
            
          })
      }
      
      counter = counter + 1
      
    }
    )}

time.all
time.short
##     LONG VERSION 
matplot( time.all[2:4] ,type = "o", ylab = "time in hours",pch= c(0,1,2,4,5,8),  cex = 1, lty = c(1),lwd= "1.5", las = 1,   xlab = "side length", x =time.all$side  , bg = "23",col = c(col11))    # x = c(100,225,400,625,900,1225,1600,2025,2500),
legend(x = "topleft",c( "OPTIM", "GLS" , "GPU", "CPU", "GLMM"), lwd = 1.6, lty = c(1), col = c(col11),text.col = c(col11),merge = TRUE,box.lty = 0,box.col = 0, box.lwd = 2, cex = 1,pch = c(1,2,4,5,8))  # c(0,1,2,4,5,8)
legend(x = "topleft",c( "GLS", "GLMM nelder-mead" , "GLMM quasi-newton"), lwd = 1.6, lty = c(1), col = c(col11),text.col = c(col11),merge = TRUE,box.lty = 0,box.col = 0, box.lwd = 2, cex = 1,pch = c(1,2,4,5,8))  # c(0,1,2,4,5,8)
#'
#'

#'
#'
#'##     SHORT VERSION 
matplot( time.short[2:4] ,type = "o", ylab = "time in minutes",pch= c(1,2,4,5,6,7,8,9,10),  cex = 1, lty = c(1),lwd= "1.5", las = 1,   xlab = "side length", x = time.short$side, bg = "23",col = c(col11))
legend(x = "topleft",c( "OPTIM", "GLS" , "GPU"), lwd = 1.6, lty = c(1), col = c(col11),text.col = c(col11),merge = TRUE,box.lty = 0,box.col = 0, box.lwd = 2, cex = 1, pch = c(1,2,4))
matplot( time.short ,type = "o", ylab = "Time",pch= c(1,2,4,5,6,7,8,9,10),  cex = 0.8, lty = c(1),lwd= "1", las = 1,   xlab = "Sidegröße", x = time.short$side, bg = "23",col = c(col10a, col10b))
legend(x = "topleft",c( "OPTIM", "GLMM" , "GLS", "CPU", "CPU"),pch= c(1,2,4,5,6,7,8,9,10), lwd = 2, lty = c(3), col = c(col10a, col10b),text.col = c(col10a, col10b) , merge = TRUE)
#'
#'
#'
#'
##
plot( log(time.all$`time of glmmTMB`) ~ log(time.all$`time of glmmTMB`))

time = c(0,25,50,100,125,150,175,200,225,250,275,300,325,350,375,400,425,450,475,500,525,550,575,600,625)
length(time)
plot( log(time.all$`time of glmmTMB`) ~ log(time))
plot( log(time.all$`time of gls`) ~ log(time))
plot( log(time.all$`time of optim`) ~ log(time))
plot( log(time.all$`time of CPU`) ~ log(time))
plot( log(time.all$`time of GPU`) ~ log(time))
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
s = seq(1, 10, length.out = 100)
plot(log(s), log(s^2))
plot(log(s), log(exp(s)))



#'____________ G L M M__________________________________________________________________________________________
plot(log(dataset$glmm_Time), log(dataset$glmm_Time^2))
plot(log(dataset$glmm_Time), log(exp(dataset$glmm_Time)))
#'
#'
#'____________ G L S__________________________________________________________________________________________
plot(log(dataset$gls_Time), log(dataset$gls_Time^2))
plot(log(dataset$gls_Time), log(exp(dataset$gls_Time)))
#'
#'
#'____________ O P T I M__________________________________________________________________________________________
plot(log(dataset$optim_Time), log(dataset$optim_Time^2))
plot(log(dataset$optim_Time), log(exp(dataset$optim_Time)))
#'
#'
#'____________C P U__________________________________________________________________________________________
plot(log(dataset$CPU_Time), log(dataset$CPU_Time^2))
plot(log(dataset$CPU_Time), log(exp(dataset$CPU_Time)))
#'
#'
#'____________ G P U __________________________________________________________________________________________
plot(log(dataset$GPU_Time), log(dataset$GPU_Time^2))
plot(log(dataset$GPU_Time), log(exp(dataset$GPU_Time)))
#'
#'



#'
#'
#'
################################################
################################################
# Dieser teil zeigt dir den bias und den Error in einem plot an durch normalverteilung und streuung
library(RColorBrewer)
col1 <- brewer.pal(7,"BrBG")
col2 <- brewer.pal(7,"Greens")
col3 <- brewer.pal(7,"PiYG")
col4 <- brewer.pal(7,"PRGn")
col5 <- brewer.pal(7,"PuOr")
col6 <- brewer.pal(11,"RdBu")
col7 <- brewer.pal(11,"RdYlBu")
col8 <- brewer.pal(11,"RdYlGn")
col9 <- brewer.pal(11,"Spectral")
col10 <- brewer.pal(8,"Accent")
col10a <- col10[1:3]
col10b <- col10[5:8]

col11 <- brewer.pal(9,"Set1")
col12 <- brewer.pal(8,"Set2")
col13 <- brewer.pal(9,"Set3")
?brewer.pal


matplot(data.glmm,type ="o", ylab = "RMSE", xlab = "Lambda", x = data.glmm$Lambda,  cex = 0, lty = 2,lwd= "2", las = 1, bg = "23",col = col3)
matplot(data.glmm,type ="o", ylab = "RMSE", xlab = "Lambda", x = data.glmm$Lambda,  cex = 0, lty = 2,lwd= "2", las = 1, bg = "23",col = col4)
matplot(data.glmm,type ="o", ylab = "RMSE", xlab = "Lambda", x = data.glmm$Lambda,  cex = 0, lty = 2,lwd= "2", las = 1, bg = "23",col = col5) # gut
matplot(data.glmm,type ="o", ylab = "RMSE", xlab = "Lambda", x = data.glmm$Lambda,  cex = 0, lty = 2,lwd= "2", las = 1, bg = "23",col = col6) # gut
matplot(data.glmm,type ="o", ylab = "RMSE", xlab = "Lambda", x = data.glmm$Lambda,  cex = 0, lty = 2,lwd= "2", las = 1, bg = "23",col = col7)
matplot(data.glmm,type ="o", ylab = "RMSE", xlab = "Lambda", x = data.glmm$Lambda,  cex = 0, lty = 2,lwd= "2", las = 1, bg = "23",col = col8)
matplot(data.glmm,type ="o", ylab = "RMSE", xlab = "Lambda", x = data.glmm$Lambda,  cex = 0, lty = 2,lwd= "2", las = 1, bg = "23",col = col9)
matplot(data.glmm,type ="o", ylab = "RMSE", xlab = "Lambda", x = data.glmm$Lambda,  cex = 0, lty = 2,lwd= "2", las = 1, bg = "23",col = col10) # sehr gut 
matplot(data.glmm,type ="o", ylab = "RMSE", xlab = "Lambda", x = data.glmm$Lambda,  cex = 0, lty = 2,lwd= "2", las = 1, bg = "23",col = col11) # gut 
matplot(data.glmm,type ="o", ylab = "RMSE", xlab = "Lambda", x = data.glmm$Lambda,  cex = 0, lty = 2,lwd= "2", las = 1, bg = "23",col = col12)
matplot(data.glmm,type ="o", ylab = "RMSE", xlab = "Lambda", x = data.glmm$Lambda,  cex = 0, lty = 2,lwd= "2", las = 1, bg = "23",col = col13)
matplot(data.glmm,type ="o", ylab = "RMSE", xlab = "Lambda", x = data.glmm$Lambda,  cex = 0, lty = 2,lwd= "2", las = 1, bg = "23",col = col3)






dataset

glmm.error.bias   <- plot(dataset$Lambda ~ dataset$glmm_Lambda, xlab= "glmmTMB estimated lambda ", ylab = "real Lambda",  type = "p", col = col10, las = 1, lty = 1 , cex = 2, xlim = c(0,3), ylim = c(0,3))
#abline(a = 0, b = 2, col = 12)
abline(a = 0, b = 1, col = 2)
abline(a = 0, b = 1, col = 2)
legend(1.95,0.35,c( "slope of 1"),box.lty = 0,box.col = 0, box.lwd = 0, col = c( "red"),text.col = c( "red"), lty = c(1),merge = TRUE)
###############################################

gls.error.bias    <- plot(dataset$Lambda ~ dataset$gls_Lambda, xlab= "gls estimated lambda", ylab = "real Lambda",  type = "p", col = col7, las = 1, lty = 1 , cex = 2, xlim = c(0,3), ylim = c(0,3))
#abline(a = 0, b = 2, col = 12)
abline(a = 0, b = 1, col = 2)
abline(a = 0, b = 1, col = 2)
legend(1.95,0.35,c( "slope of 1"),box.lty = 0,box.col = 0, box.lwd = 0, col = c( "red"),text.col = c( "red"), lty = c(1),merge = TRUE)

###############################################
optim.error.bias  <- plot(dataset$Lambda ~ dataset$optim_Lambda, xlab= "optim Nelder mead estimated lambda", ylab = "real Lambda",  type = "p", col = col8, las = 1, lty = 1 , cex = 2, xlim = c(0,3), ylim = c(0,3))
#abline(a = 0, b = 2, col = 12)
abline(a = 0, b = 1, col = 2)
abline(a = 0, b = 1, col = 2)
legend(1.95,0.35,c( "slope of 1"),box.lty = 0,box.col = 0, box.lwd = 0, col = c( "red"),text.col = c( "red"), lty = c(1),merge = TRUE)

###############################################
#'
#'
cpu.error.bias  <- plot(dataset$Lambda ~ dataset$GPU_Lambda, xlab= "optim newton estimated lambda", ylab = "real Lambda",  type = "p", col = col6, las = 1, lty = 1 , cex = 2, xlim = c(0,3), ylim = c(0,3))
#abline(a = 0, b = 2, col = 12)
abline(a = 0, b = 1, col = 2)
abline(a = 0, b = 1, col = 2)
legend(1.95,0.35,c( "slope of 1"),box.lty = 0,box.col = 0, box.lwd = 0, col = c( "red"),text.col = c( "red"), lty = c(1),merge = TRUE)

###############################################
#'
#'
gpu.error.bias  <- plot(dataset$Lambda ~ dataset$GPU_Lambda, xlab= " GPU estimated lambda", ylab = "real Lambda",  type = "p", col = col9, las = 1, lty = 1 , cex = 2,xlim = c(0,3), ylim = c(0,3))
#abline(a = 0, b = 2, col = 12)
abline(a = 0, b = 1, col = 2)
abline(a = 0, b = 1, col = 2)
legend(1.95,0.35,c( "slope of 1"),box.lty = 0,box.col = 0, box.lwd = 0, col = c( "red"),text.col = c( "red"), lty = c(1),merge = TRUE)




















dataset

error.all   = data.frame(matrix(NA,20,7))
colnames(error.all) = c("side","lambda", "glmmTMB", "gls","optim","CPU","GPU")



zähler = 0

for (a in 1:10){ # Dieser Loop gibt mir den Mean von den Ergebnis(estimated Lambda) - Lambda
  time_lambda = 
    system.time({
      c = 0.2*a
      
      
      # for (j in 4:29){ # change to 29
      for (j in 2:11){
        time_side = 
          system.time({
            #  side = 2+ 2*j
            side = 0 + 5*j
            
            error.all[counter, 1] <- (dataset[dataset$rep == 1 & dataset$Lambda == c & dataset$Side == side, 2 ] [1]    )  # lambda
            error.all[counter, 2] <- (dataset[dataset$rep == 1 & dataset$Lambda == c & dataset$Side == side, 3 ] [1])  * (dataset[dataset$rep == 1 & dataset$Lambda == c & dataset$Side == side, 3 ] [1])  # side
            
            error.all[counter, 3] <- (dataset[dataset$rep == 1 & dataset$Lambda == c & dataset$Side == side, 6 ]  [1]  )
            error.all[counter, 4] <- (dataset[dataset$rep == 1 & dataset$Lambda == c & dataset$Side == side, 9 ]  [1]  )
            error.all[counter, 5] <- (dataset[dataset$rep == 1 & dataset$Lambda == c & dataset$Side == side, 12 ] [1]  )
            error.all[counter, 6] <- (dataset[dataset$rep == 1 & dataset$Lambda == c & dataset$Side == side, 15 ] [1]  )
            error.all[counter, 7] <- (dataset[dataset$rep == 1 & dataset$Lambda == c & dataset$Side == side, 18 ] [1]  )
            
            zähler = zähler + 5
            steps = steps +1 
            stip = stip + 2
          })
      }
      
      counter = counter + 1
      zähler = 1
      steps = 1
      stip = 1
    }
    )}


error.all

glmm.er  <- plot(data$Lambda - data$glmm_Lambda,  xlab= "glmmTMB ", ylab = "real Lambda"  ,    type = "p", col = col9,  las = 1, lty = 1 ,  cex = 2)

?plot


#bias und error ?? 
#'____________ G L M M__________________________________________________________________________________________
glmm.er  <- plot(data$Lambda - data$glmm_Lambda, xlab= "glmmTMB", ylab = "real Lambda"  ,    type = "p", col = col9,  las = 1, lty = 1 ,  cex = 2, xlim = c(0,70))
abline(a = 0, b = 0, col = "black")
abline(a = 0, b = 0.005, col = 12)
legend(x = "bottomleft",c("slope 0.005"),box.lty = 0,box.col = 0, box.lwd = 0, col = c(12, "red"),text.col = c(12, "red"), lty = c(1),merge = TRUE)
#'
#'
#'____________ G L S__________________________________________________________________________________________
gl2.er   <- plot(dataset$Lambda - dataset$gls_Lambda, xlab= "GLS", ylab = "real Lambda",     type = "p", col = col9,  las = 1, lty = 1 ,  cex = 2, xlim = c(0,70))
abline(a = 0, b = 0, col = "black")
#'
#'____________ O P T I M __________________________________________________________________________________________
optim.er <- plot(dataset$Lambda - dataset$optim_Lambda, xlab= "OPTIM", ylab = "real Lambda", type = "p", col = col9,  las = 1, lty = 1 ,  cex = 2, xlim = c(0,70))
abline(a = 0, b = 0, col = "black")
#'
#'____________ C P U __________________________________________________________________________________________
cpu.er   <- plot(dataset$Lambda - dataset$CPU_Lambda, xlab= "CPU", ylab = "real Lambda",     type = "p", col = col9,  las = 1, lty = 1 ,  cex = 2, xlim = c(0,70))
abline(a = 0, b = 0, col = "black")
#'
#'____________ G P U __________________________________________________________________________________________
gpu.er   <- plot(dataset$Lambda - dataset$GPU_Lambda, xlab= "GPU", ylab = "real Lambda",     type = "p", col = col9,  las = 1, lty = 1 ,  cex = 2, xlim = c(0,70))
abline(a = 0, b = 0, col = "black")
#'
#'
################################################
#bias und error ?? 
glmm.er  <- plot(dataset$glmm_Lambda - dataset$Lambda , xlab= "glmm_Lambda", ylab = "real Lambda",  type = "p", col = col9,  las = 1, lty = 1 ,  cex = 2, xlim = c(0,60))
gl2.er   <- plot(dataset$gls_Lambda - dataset$Lambda, xlab= "gls_Lambda", ylab = "real Lambda",     type = "p", col = col9,  las = 1, lty = 1 ,  cex = 2, xlim = c(0,60))
optim.er <- plot(dataset$optim_Lambda - dataset$Lambda, xlab= "optim_Lambda", ylab = "real Lambda", type = "p", col = col9,  las = 1, lty = 1 ,  cex = 2, xlim = c(0,60))
cpu.er   <- plot(dataset$CPU_Lambda- dataset$Lambda, xlab= "CPU_Lambda", ylab = "real Lambda",      type = "p", col = col9,  las = 1, lty = 1 ,  cex = 2, xlim = c(0,60))
gpu.er   <- plot(dataset$GPU_Lambda - dataset$Lambda, xlab= "GPU_Lambda", ylab = "real Lambda",     type = "p", col = col9,  las = 1, lty = 1 ,  cex = 2, xlim = c(0,60))


#'
#'
#'
#'
dataset