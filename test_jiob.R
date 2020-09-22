res = data.frame(matrix(NA, 15, 1))
counter = 1
for(i in 1:5){
  for(j in c("A", "B", "C")){
    # do something
    Sys.sleep(1)
    res[counter, 1] = i
    res[counter, 2] = j
    counter = counter+1
  }
}
saveRDS(res, file = "test.RDS")
res






lambda.result = data.frame(matrix(NA,25,11))
# colnames(lambda.result) = LETTERS[1:11]
colnames(lambda.result) = c("Lambda", "Sides", "TIME(glmmTMB)", "LAMBDA(glmmTMB)", "INTERCEPT(glmmTMB)","TIME(gls)", "LAMBDA(gls)", "INTERCEPT(gls)", "TIME(optim)", "LAMBDA(optim)", "INTERCEPT(optim)")
counter = 1
side.result = data.frame(matrix(NA,25,11))
time.result = data.frame(matrix(NA,25,11))
lambda.result = data.frame(matrix(NA,25,11))
for (h in 1:5){
  time_lambda = 
    system.time({
      lambda[h] =0.15+h*0.05
      l.r <- lambda[h]

      
      
      for (j in 1:16){
        time_side = 
          system.time({
            side = 4 + j
            s.r <- side
            
            {
            
            dist.matrix <- (function(side)
            {
              row.coords <- rep(1:side, times=side)
              col.coords <- rep(1:side, each=side)
              row.col <- data.frame(row.coords, col.coords)
              D <- dist(row.col, method="euclidean", diag=TRUE, upper=TRUE)
              D <- as.matrix(D)
              return(D)
            })
            
            cor.surface <-  function(side, global.mu, lambda)
            {
              D <- dist.matrix(side)
              # scaling the distance matrix by the exponential decay
              SIGMA <- exp(-lambda*D)
              mu <- rep(global.mu, times=side*side)
              # sampling from the multivariate normal distribution
              M <- matrix(nrow=side, ncol=side)
              M[] <- rmvnorm(1, mu, SIGMA)
              return(M)
            }
            
            M <- cor.surface(side = s.r, lambda = lambda, global.mu = global.mu)
            
            
            y <- as.vector(as.matrix(M))
            my.data <- list(N = s.r * s.r, D = dist.matrix(side), y = y)
            }
            
            
            
            side.result[counter,1] <- side
         
           # lambda.result[counter, 1] <- lambda[h]
           # lambda.result[counter, 2] <- side[j]
          })
      } 
      
    })
}


?rmvnorm
D


side

sigma
side

side
times<- c(1:100)
times1<- times[1:100]
times1
times <- side* side
times


lambda.result





res = data.frame(matrix(NA, 15, 1))
counter = 1
for(i in 1:5){
  for(j in c("A", "B", "C")){
    # do something
    res[counter, 1] = i
    res[counter, 2] = j
    counter = counter+1
  }
}



