
# JAGs Code ---------------------------------------------------------------

install.packages("coda")
install.packages("mvtnorm")
install.packages("rjags")
install.packages("boot")
install.packages("knitr")
install.packages("usethis")
install.packages("nlme")
install.packages("glmmTMB")
#install.packages("sjSDM")
install.packages("jSDM")
install.packages("devtools")
#install.packages("R2WinBUGS")
#install.packages("JAGS")
install.packages("R2jags")
install.packages("useful")
install.packages("DHARMa")
install.packages("MVN")
#install.packages("R2OpenBUGS")
install.packages("corpcor")




# Start RUN_CODE ----------------------------------------------------------



library(corpcor)
library(nlme)
library(MVN)
library(DHARMa)
library(useful)
library(glmmTMB)
library(usethis)
library(devtools)
#devtools::install_github("ghislainv/jSDM")
library(knitr)
library(sjSDM)
#library(jSDM)
library(coda)
library(rjags)
library(mvtnorm)
library(rjags)
#library(R2WinBUGS)l
library(JAGS)
library(R2jags)
library(mvtnorm)
#library(R2OpenBUGS)


# <____________ change function
# <____________ dist.matrix <- function(side)
# <____________ {
# <____________  row.coords <- rep(1:side, times=side)
# <____________  col.coords <- rep(1:side, each=side)
# <____________  row.col <<- data.frame(row.coords, col.coords)
# <____________  D <- dist(row.col, method="euclidean", diag=TRUE, upper=TRUE)
# <____________  D <- as.matrix(D)
# <____________  return(D) #  <___________list(D=D, coords = row.col)
# <____________ }

row <- row.coords <- rep(1:side, times=side)
col <- col.coords <- rep(1:side, each=side)
row.col <<- data.frame(row, col)
D1 <- dist(row.col, method="euclidean", diag=TRUE, upper=TRUE)
D <- as.matrix(D1)

#dist.matrix <- function(list(D=D, coords = row.col))
dist.matrix <- function(side)
{
row.coords=row.coords
col.coords=col.coords
row.col=row.col
D=D
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






# parameters (the truth) that I will want to recover by JAGS
side = 10
global.mu = 0
lambda = 0.2  # let's try something new

# simulating the main raster that I will analyze as data
M <- cor.surface(side = side, lambda = lambda, global.mu = global.mu)
image(M)
mean(M)



# simulating the inherent uncertainty of the mean of M: 
test = replicate(1000, mean(cor.surface(side = side, lambda = lambda, global.mu = global.mu)))
hist(test, breaks = 40)
sd(test)
# normal distribution

jag1 <- as.vector(as.matrix(M))
my.data <- list(N = side * side, D = dist.matrix(side), y = jag1)







modelCode = textConnection("
    model
{
    # priors
    lambda ~ dgamma(1, 0.1) 
    global.mu ~ dnorm(0, 0.01)
    for(i in 1:N)
{
    # vector of mvnorm means mu
    mu[i] <- global.mu
}
    
    # derived quantities
    for(i in 1:N)
{
    for(j in 1:N)
{
    # turning the distance matrix to covariance matrix
    D.covar[i,j] <- exp(-lambda*D[i,j])
}
}
    # turning covariances into precisions (that's how I understand it)
    D.tau[1:N,1:N] <- inverse(D.covar[1:N,1:N])
    
    # likelihood
    y[1:N] ~ dmnorm(mu[], D.tau[,])
}
")

fit <- jags(data=my.data, 
            parameters.to.save=c("lambda", "global.mu"),
            model.file=modelCode,
            n.iter=10000,
            n.chains=3,
            n.burnin=5000,
            n.thin=5,
            DIC=FALSE)

?jags
plot(as.mcmc(fit))
summary(fit)
?as.mcmc
pairs(as.matrix(as.mcmc(fit)))









# 13.09 glmmTMB -----------------------------------------------------------

# glmmTMB time variable  --------------------------------------------------
# Spatial correlations ----------------------------------------------------

data = data.frame(resp=my.data$y)
data$pos <- numFactor(row.col, row.coords)
data$group <- factor(rep(1, nrow(data)))
data$x = row.coords
data$y = col.coords


fit.exp <- glmmTMB(resp ~ 1 + exp(pos + 0 | group), data=data)
summary(fit.exp)

### Note:
### from the glmmTMB source:
### exp( -dist(i,j) * exp(-theta(1)) ) );
### which means that to get our lambda parametrization we have to calculate: exp(-theta)

exp(-fit.exp$fit$par[4]) # original
exp(-fit.exp$fit$par[3]) # auch theta
fit.exp$fit$par#[3]

#
#
side= 18
fit.exp <- glmmTMB(resp ~ 1 + exp(pos + 0 | group), data = new.data1)
print(fit.exp)
summary(fit.exp)
18*18
# 13.09 nmle --------------------------------------------------------------

data.1 <- data.frame(my.data$y)
id = rep(letters[1:20],5)
lm1 <- lme(y ~ 1, random=~ 1 | id, correlation=corExp(form=~1|id), data = my.data)
summary(lm1)
my.data$group = as.factor(rep(1, my.data$N))
my.data$rows = row.col[,1]
my.data$cols = row.col[,2]

# <____________ lm2 <- lme(y ~ 1, random=~ 1 | group, correlation=corExp(form=~rows+cols), data = my.data)
lm3 <- gls(y ~ 1, correlation=corExp(form=~rows+cols), data = my.data)
lm3
# <____________ lm2
# <____________ intervals(lm3)
# <____________ summary(lm3)$intervals
# <____________ log(0.2)
range <- coef(gls$modelStruct$corStruct, unconstrained = F)
l = 1 /range           # r = range # ich depp .... 
d <- l
gls.lambda <- exp(-range*D)
gls.lambda

#
#
#
#
# <____________ length(my.data$y)
# <____________ dummy <- rep(1, 100) 
# <____________ spdata <- cbind(my.data$y, dummy) 
# <____________ lme1 <- lme(y ~ 1, data = my.data, random = ~ 1 | dummy, method = "ML") 
# <____________ summary(lme1)
# <____________ ?lme
# <____________ lme2 <- update(lme1, correlation = corGaus(1, form = ~ dummy + 0), method = "ML")
# <____________ summary(lme2)
#
#
#
#
#
#  Eigene Likelihood Function MVN -----------------------------------------

# zweiter Versuch MLE von MVN zu laufen -----------------------------------
# "solve", "qr.solve", "pseudoinverse"
#y <- -mvtnorm::dmvnorm(my.data$y, mean = rep(par[2], 100), sigma = cov,log = TRUE) 

set.seed=26
ll = function(par) {
  cov = (exp(-par[1]* my.data$D))
  -mvtnorm::dmvnorm(my.data$y, mean = rep(par[2], 100), sigma = cov,log = TRUE)
}
methods = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent")
result <- optim(par = c(0.5,10),  fn = ll, gr = NULL, method = methods[1], hessian = FALSE)
result

res = sapply(seq(0.05, 1, by = 0.01),function(i) ll(c(i, 0.0)))
plot(1:96, res)
result$par[1]

# <____________ min(res) # 57.61983 = 0.16 best score
# <____________ optim.minimizer(res)
# <____________ optim.minimum(res)   
# <____________ print(res$minimum)
# <____________ result
# <____________ sapply(seq(0.05, 1, by = 0.01),function(i) ll(c(i, 0.0)))


# <____________ cov = (exp(-5.8* my.data$D))
# <____________ cov


##################################################################################
#'
#'
##################################################################################

#  glmmTMB -----------------------------------------------------------

data = data.frame(resp=my.data$y)
data$pos   <- numFactor(row.col, row.coords)
data$group <- factor(rep(1, nrow(data)))
data$x = row.coords
data$y = col.coords




fit.exp <- glmmTMB(resp ~ 1 + exp(pos + 0 | group), data=data)
summary(fit.exp)

### exp( -dist(i,j) * exp(-theta(1)) ) );
### which means that to get our lambda parametrization we have to calculate: exp(-theta)

exp(-fit.exp$fit$par[4])

    
    
#  nmle4 / GLS--------------------------------------------------------------

my.data$group = as.factor(rep(1, my.data$N))
my.data$rows = row.col[,1]
my.data$cols = row.col[,2]

lm2 <- lme(y ~ 1, random=~ 1 | group, correlation=corExp(form=~rows+cols), data = my.data)
lm3 <- gls(y ~ 1, correlation=corExp(form=~rows+cols), data = my.data)


try({
  
  gls = gls(y ~ 1, correlation=corExp (form =~ rows + cols), data = new.data)

}, silent=TRUE)


range <- coef(gls$modelStruct$corStruct, unconstrained = F)
l = 1 /range           # r = range 
d <- l
gls.lambda <- exp(-range*D)
gls.lambda




#  Eigene Likelihood Function MVN -----------------------------------------

#y <- -mvtnorm::dmvnorm(my.data$y, mean = rep(par[2], 100), sigma = cov,log = TRUE) 
methods = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent")


ll = function(par) { 
  cov = (exp(-par[1]* new.data$D))
  -mvtnorm::dmvnorm(new.data1$resp, mean = rep(par[2], side*side), sigma = cov ,log = TRUE)
}

result <- optim(par = c(0.5,10),  fn = ll, gr = NULL, method = methods[1], hessian = FALSE)
result

res = sapply(seq(0.05, 1, by = 0.01),function(i) ll(c(i, 0.0)))
plot(1:96, res)
result$par[1]

min(res) # 88.61983 = 0.315 best score
sapply(seq(0.05, 1, by = 0.01),function(i) ll(c(i, 0.0)))


cov = (exp(-5.8* my.data$D))
cov
group


#------- For Loop Strucutre, Jags, nmle, glmmTMB (50 times) ---------------

#--------------------------------------------------------------------------

# Loop structur -----------------------------------------------------------

# runtime
## 
for....{
  time_gls = system.time(
    {
      b = 5*3
      gls_model = gls(...)
      
    }
  )
  
  time_glmmTMB = system.time(
    {
      b = 5*3
      glmmTMBs_model = gls(...)
      
    }
  )
  time[3]
  b
}
# time, lambda(theta, range), intercept
# glmmTMB, gls, optim


# 10 sites, 3 ziel values
-------------------------------------------------------------------
  # <____________   result_glmmTMB = matrix(NA, 10, 3)
  # <____________ for(i in 1:10){
  # <____________   time_glmmTMB = 
  # <____________     system.time({
  # <____________      m1 = glmmTMB(y~1, data = my.data)
  # <____________     })
  # <____________   result_glmmTMB[i, 1] = time_glmmTMB[3]
  # <____________   result_glmmTMB[i, 2] = summary(m1)$coefficients$cond[1]
  # <____________  }
  # <____________  b
  ------------------------------------------------------------------
  2+2 
# LOOP__GlmmTMB -----------------------------------------------------------
result_glmmTMB = matrix(NA, 10, 3)
for(i in 1:10){
  time_glmmTMB = 
    system.time({
      fit.exp <- glmmTMB(resp ~ 1 + exp(pos + 0 | group), data=data)
    })
  result_glmmTMB[i, 1] = time_glmmTMB[3]                       # Time
  result_glmmTMB[i, 2] = exp(-fit.exp$fit$par[4])              #lambda/theta?
  result_glmmTMB[i, 3] = summary(fit.exp)$coefficients$cond[1] # Intercept
  
}

result_glmmTMB

# Loop__GLS ---------------------------------------------------------------

result_gls = matrix(NA, 10, 3)
for(i in 1:10){
  time_gls = 
    system.time({
      gls <- gls(y ~ 1, correlation=corExp (form=~rows+cols), data = my.data)
    })
  result_gls[i, 1] = time_gls[3]                                            # Time
  result_gls[i, 2] = 1/coef(gls$modelStruct$corStruct, unconstrained = F)   #lambda/theta?
  result_gls[i, 3] = summary(gls)$coefficients                              # Intercept
}
result_gls

# <____________ range = 2.541579 
# <____________ ?update_labels

# Loop__OPTIM -------------------------------------------------------------


result_optim = matrix(NA, 10, 3)
for(i in 1:10){
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
  result_optim[i, 3] = result$par[1]                          # Intercept
}
result_optim


# Loop__In__Loop_Structure with lambda ------------------------------------
# <____________ lambda
# <____________ h = 0

lambda.result = matrix(NA,50,2)
for (h in 1:50){
  time_glmmTMB = 
    system.time({
      lambda[h] =0.15+h*0.05
      l.r <- lambda[h]
    #  n.lambda = ceiling(lambda[h])
    })
  lambda.result[h, 1] <- lambda[h]
  lambda.result[h, 2] <- l.r
}

lambda.result


# Loop__in__Loop_Combination__Model ---------------------------------------

repeat.experiment = data.frame(matrix(NA,500,12))
colnames(repeat.experiment) = c("rep", "Lambda", "Side", "glmm_Time", "glmm_Lambda", "glmm_Intercept", "gls_Time", "gls_Lambda", "gls_Intercept", "optim_Time", "optim_Lambda", "optim_Intercept")


counter=1

for (g in 1:5 ){
  time_repeat = 
    system.time({
      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      # Loop, wiederhole Experiment 5 mal für die Statistik
      for (h in 1:10 ){
        time_glmmTMB = 
          system.time({
            lambda = 0 + (h*0.2)
            l.r <- lambda
            
            #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
            # Loop, ändere Lambda von lambda=0.2 in 0.05 Schritten bis Lambda=0.5
            
            for (j in 1:10){
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
                  for(i in 1:5){
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

# Einzelfunktionen könnten als 1 Funtion verbunden werden




