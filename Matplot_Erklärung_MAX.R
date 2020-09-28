df = data.frame(a = c(rep(1, 10), rep(2, 10)),b = rep(1:4, 5), lambda = rnorm(20))
# b == 1 und iteration == 1 
RMSE = function(obs, true) sqrt(mean((obs-true)^2))
mean(sapply(df[df$a == 1 & df$b == 1, 3], function(value) RMSE(value, 0.2)) )
mean(sapply(df[df$a == 1 & df$b == 2, 3], function(value) RMSE(value, 0.2)) )
mean(sapply(df[df$a == 1 & df$b == 3, 3], function(value) RMSE(value, 0.2)) )

# site = 10, verschiedene lambdas, fuer alle 4 modelle
matplot(data.frame(matrix(rnorm(4*8), 8, 4)), type = "o")
matplot(plot.S4,type ="o")







#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
JJ <- readRDS("loop4")
JJ

plot.S4  = data.frame(matrix(NA,10,31))
# COLNAMES für Side 10x
colnames(plot.S4) = c("Lambda","glmm4","gls4","optim4","glmm6","gls6","optim6","glmm8","gls8","optim8","glmm10","gls10","optim10","glmm12","gls12","optim12","glmm14","gls14","optim14","glmm16","gls16","optim16","glmm18","gls18","optim18","glmm20","gls20","optim20","glmm22","gls22","optim22")

# rownames(plot.S4) = c("0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0")
# xlim = c(0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0)

JJ.data <- data.frame(matrix(data = JJ, 204,31))
JJ.data <-  (JJ$Lambda/ NA)
JJ







counter = 1
for (a in 1:10){ # Dieser Loop gibt mir den Mean von den Ergebnis(estimated Lambda) - Lambda
  time_side = 
    system.time({
      c = 0.2*a
      
     # x <- mean(sapply(df[loop4$Lambda == 0.2 & loop4$side == 4, 5], function(value) RMSE(value, 0.2)) )
      x1 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 10, 5], function(value) RMSE(value, 0.2)) )
      y1 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 10, 8], function(value) RMSE(value, 0.2)) )
      z1 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 10, 11], function(value) RMSE(value, 0.2)) )
      
      x2 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 12, 5], function(value) RMSE(value, 0.2)) )
      y2 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 12, 8], function(value) RMSE(value, 0.2)) )
      z2 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 12, 11], function(value) RMSE(value, 0.2)) )
      
      x3 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 14, 5], function(value) RMSE(value, 0.2)) )
      y3 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 14, 8], function(value) RMSE(value, 0.2)) )
      z3 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 14, 11], function(value) RMSE(value, 0.2)) )
      x4 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 16, 5], function(value) RMSE(value, 0.2)) )
      y4 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 16, 8], function(value) RMSE(value, 0.2)) )
      z4 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 16, 11], function(value) RMSE(value, 0.2)) )
      x5 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 18, 5], function(value) RMSE(value, 0.2)) )
      y5 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 18, 8], function(value) RMSE(value, 0.2)) )
      z5 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 18, 11], function(value) RMSE(value, 0.2)) )
      x6 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 20, 5], function(value) RMSE(value, 0.2)) )
      y6 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 20, 8], function(value) RMSE(value, 0.2)) )
      z6 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 20, 11], function(value) RMSE(value, 0.2)) )
      x7 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 22, 5], function(value) RMSE(value, 0.2)) )
      y7 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 22, 8], function(value) RMSE(value, 0.2)) )
      z7 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 22, 11], function(value) RMSE(value, 0.2)) )
      x8 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 24, 5], function(value) RMSE(value, 0.2)) )
      y8 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 24, 8], function(value) RMSE(value, 0.2)) )
      z8 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 24, 11], function(value) RMSE(value, 0.2)) )
      x9 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 26, 5], function(value) RMSE(value, 0.2)) )
      y9 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 26, 8], function(value) RMSE(value, 0.2)) )
      z9 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 26, 11], function(value) RMSE(value, 0.2)) )
      x10 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 28, 5], function(value) RMSE(value, 0.2)) )
      y10 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 28, 8], function(value) RMSE(value, 0.2)) )
      z10 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 28, 11], function(value) RMSE(value, 0.2)) )
      
        })
  
        plot.S4[counter, 1] <- c
        plot.S4[counter, 2] <- x1
        plot.S4[counter, 3] <- y1
        plot.S4[counter, 4] <- z1
        
        plot.S4[counter, 5] <- x2
        plot.S4[counter, 6] <- y2
        plot.S4[counter, 7] <- z2
        
        plot.S4[counter, 8] <- x3
        plot.S4[counter, 9] <- y3
        plot.S4[counter, 10] <- z3
        
        plot.S4[counter, 11] <- x4
        plot.S4[counter, 12] <- y4
        plot.S4[counter, 13] <- z4
        
        plot.S4[counter, 14] <- x5
        plot.S4[counter, 15] <- y5
        plot.S4[counter, 16] <- z5
        
        plot.S4[counter, 17] <- x6
        plot.S4[counter, 18] <- y6
        plot.S4[counter, 19] <- z6
        
        plot.S4[counter, 20] <- x7
        plot.S4[counter, 21] <- y7
        plot.S4[counter, 22] <- z7
        
        plot.S4[counter, 23] <- x8
        plot.S4[counter, 24] <- y8
        plot.S4[counter, 25] <- z8
        
        plot.S4[counter, 26] <- x9
        plot.S4[counter, 27] <- y9
        plot.S4[counter, 28] <- z9
        
        plot.S4[counter, 29] <- x10
        plot.S4[counter, 30] <- y10
        plot.S4[counter, 31] <- z10
        
        counter = counter + 1

    }
plot.S4



all.RMSE.JJ <- plot.S4
saveRDS( file = all.RMSE.JJ, "all.RMSE")
#matplot(plot.S4,type ="o", ylab = "RMSE", xlab = "Lambda", xlim = c(0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0))
matplot(plot.S4,type ="o", ylab = "RMSE", xlab = "Lambda")



















counter = 1
for (a in 1:10){ # Dieser Loop gibt mir den Mean von den Ergebnis(estimated Lambda) - Lambda
  time_side = 
    system.time({
      c = 0.2*a
      
      # x <- mean(sapply(df[loop4$Lambda == 0.2 & loop4$side == 4, 5], function(value) RMSE(value, 0.2)) )
      x1 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 10, 5][1], function(value) RMSE(value, 0.2)) )
      y1 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 10, 8][1], function(value) RMSE(value, 0.2)) )
      z1 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 10, 11][1], function(value) RMSE(value, 0.2)) )
      
      x2 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 12, 5][1], function(value) RMSE(value, 0.2)) )
      y2 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 12, 8][1], function(value) RMSE(value, 0.2)) )
      z2 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 12, 11][1], function(value) RMSE(value, 0.2)) )
      
      x3 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 14, 5][1], function(value) RMSE(value, 0.2)) )
      y3 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 14, 8][1], function(value) RMSE(value, 0.2)) )
      z3 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 14, 11][1], function(value) RMSE(value, 0.2)) )
      x4 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 16, 5][1], function(value) RMSE(value, 0.2)) )
      y4 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 16, 8][1], function(value) RMSE(value, 0.2)) )
      z4 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 16, 11][1], function(value) RMSE(value, 0.2)) )
      x5 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 18, 5][1], function(value) RMSE(value, 0.2)) )
      y5 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 18, 8][1], function(value) RMSE(value, 0.2)) )
      z5 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 18, 11][1], function(value) RMSE(value, 0.2)) )
      x6 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 20, 5][1], function(value) RMSE(value, 0.2)) )
      y6 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 20, 8][1], function(value) RMSE(value, 0.2)) )
      z6 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 20, 11][1], function(value) RMSE(value, 0.2)) )
      x7 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 22, 5][1], function(value) RMSE(value, 0.2)) )
      y7 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 22, 8][1], function(value) RMSE(value, 0.2)) )
      z7 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 22, 11][1], function(value) RMSE(value, 0.2)) )
      x8 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 24, 5][1], function(value) RMSE(value, 0.2)) )
      y8 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 24, 8][1], function(value) RMSE(value, 0.2)) )
      z8 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 24, 11][1], function(value) RMSE(value, 0.2)) )
      x9 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 26, 5][1], function(value) RMSE(value, 0.2)) )
      y9 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 26, 8][1], function(value) RMSE(value, 0.2)) )
      z9 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 26, 11][1], function(value) RMSE(value, 0.2)) )
      x10 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 28, 5][1], function(value) RMSE(value, 0.2)) )
      y10 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 28, 8][1], function(value) RMSE(value, 0.2)) )
      z10 <- mean(sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == 28, 11][1], function(value) RMSE(value, 0.2)) )
      
    })
  
  plot.S4[counter, 1] <- NA #c
  plot.S4[counter, 2] <- NA#x1
  plot.S4[counter, 3] <- NA#y1
  plot.S4[counter, 4] <- z1
  
  plot.S4[counter, 5] <- NA#x2
  plot.S4[counter, 6] <- NA#y2
  plot.S4[counter, 7] <- z2
  
  plot.S4[counter, 8] <- NA#x3
  plot.S4[counter, 9] <- NA#y3
  plot.S4[counter, 10] <- z3
  
  plot.S4[counter, 11] <- NA#x4
  plot.S4[counter, 12] <- NA#y4
  plot.S4[counter, 13] <- z4
  
  plot.S4[counter, 14] <- NA#x5
  plot.S4[counter, 15] <- NA#y5
  plot.S4[counter, 16] <- z5
  
  plot.S4[counter, 17] <- NA#x6
  plot.S4[counter, 18] <- NA#y6
  plot.S4[counter, 19] <- z6
  
  plot.S4[counter, 20] <- NA#x7
  plot.S4[counter, 21] <- NA#y7
  plot.S4[counter, 22] <- z7
  
  plot.S4[counter, 23] <-NA# x8
  plot.S4[counter, 24] <- NA#y8
  plot.S4[counter, 25] <- z8
  
  plot.S4[counter, 26] <- NA#x9
  plot.S4[counter, 27] <- NA#y9
  plot.S4[counter, 28] <- z9
  
  plot.S4[counter, 29] <- NA#x10
  plot.S4[counter, 30] <- NA#y10
  plot.S4[counter, 31] <- z10
  
  counter = counter + 1
  
}
vier.optim <- plot.S4


all.RMSE.JJ <- plot.S4
saveRDS(all.RMSE.JJ, file= "all.RMSE")
#matplot(plot.S4,type ="o", ylab = "RMSE", xlab = "Lambda", xlim = c(0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0))
matplot(plot.S4,type ="o", ylab = "RMSE", xlab = "Lambda")






















zähler = 1
counter = 1
for (j in 4:13){
  time_side = 
    system.time({
      c = 0.2*a
      

                 for (a in 1:10){ # Dieser Loop gibt mir den Mean von den Ergebnis(estimated Lambda) - Lambda
                   time_Lambda = 
                     system.time({
                       side = 2+ 2*j
      
                       # x <- mean(sapply(df[loop4$Lambda == 0.2 & loop4$side == 4, 5], function(value) RMSE(value, 0.2)) )
                       x1 <- sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == side, 5][1], function(value) RMSE(value, 0.2)) 
                       y1 <- sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == side, 8][1], function(value) RMSE(value, 0.2)) 
                       z1 <- sapply(JJ[JJ$rep ==1 & JJ$Lambda == 0.2*a & JJ$Side == side, 11][1], function(value) RMSE(value, 0.2)) 

                     
                       plot.S4[counter, 1] <- c
                       #plot.S4[counter, 2] <- x1
                       #plot.S4[counter, 3] <- y1
                       #plot.S4[counter, 4] <- NA #z1
                       plot.S4[counter, zähler +1]  <- x1
                       plot.S4[counter, zähler +2]  <- y1
                       plot.S4[counter, zähler +3]  <- z1
                       
                       # plot.S4[counter, 1]  <- s.r
                       zähler = zähler + 3
                       
                     })
                   
                   #counter = counter + 1
                   #zähler = 1
                      
                 }
      # counter = counter + 1  
      counter = counter + 1
      zähler = 1
    }
)}      

plot.S4
best.result <- plot.S4


warnings()
mean(sapply(JJ[JJ$Lambda == 0.2*a & JJ$Side == 22, 5], function(value) RMSE(value, 0.2)) )

dataset <- readRDS("loop4") 
dataset
