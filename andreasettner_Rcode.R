install.packages("git2r")

library(git2r)





# test 
2+2

set.seed(123)
install.packages("knitr")
install.packages("sjSDM")
install.packages("jSDM")
library(knitr)
library(sjSDM)
library(jSDM)


knitr::opts_chunk$set(fig.width=7, fig.height=4.5, fig.align='center', warning=FALSE, message=FALSE, cache = F)

install_sjSDM()
vignette("Dependencies", package = "sjSDM")


com = simulate_SDM(env = 3L, species = 5L, sites = 100L)

model = sjSDM(Y = com$response, env = com$env_weights, iter = 100L, se=TRUE)





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

2+2
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

2+2
?rjags


dist.matrix <- function(side)
{
  row.coords <- rep(1:side, times=side)
  col.coords <- rep(1:side, each=side)
  row.col <<- data.frame(row.coords, col.coords)
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
  return(M)
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


my.data
# liefert mir 1 Wert für Y von 1:100, ergo nehme ich Time als 2. variable von 1:100 oder so siehe
# https://cran.r-project.org/web/packages/glmmTMB/vignettes/covstruct.html

my.data
n= 100
times <- factor(1:n, levels=1:n)
group <- factor(rep(1,n))
View(my.data)

dat0 <- data.frame(my.data,times,group)
dat1 <- data.frame(my.data$y,times,group)


View(dat0)
str(dat0)
?glmmTMB

glm1 <- glmmTMB(my.data$y ~ ar1(times + 0 | group), data=dat0)
glm1 <- glmmTMB(my.data$y ~ ar1(times + 0 | group), data=dat0)
?glmmTMB

dat0
# ---- glm2 <- glmmTMB(y ~ ar1(times + 0 | group), data=dat0)
# ---- glm1 <- glmmTMB(my.data.y ~times, data = dat0) # crazy error
summary(glm1)
plot(glm1)    # Error: 'x' is a list, but does not have components 'x' and 'y'
p <- plot(y, times)

plot(glm1$y, glm1$times) # des functioniert jetzt leider wiederum nicht.. 

# --------   plot (glm$your.x.coordinate, glm$your.y.coordinate)   # Maybe a hint or solution pathway
my.dat1 <- 
  res1 <- simulateResiduals(glm1)
plot(res1)


# Spatial correlations ----------------------------------------------------

# Matern
fit.mat <- glmmTMB(my.data$y ~ mat(times + 0 | group), data=dat0, dispformula=~0)
VarCorr(fit.mat)
plot(fit.mat)

# Gaussian
fit.gau <- glmmTMB(my.data.y ~ gau(times + 0 | group), data=dat0, dispformula=~0)
VarCorr(fit.gau)
plot(fit.gau)
?plot

# Exponential

#### Max:
data = data.frame(resp=my.data$y)
data$pos <- numFactor(row.col$row.coords, row.col$col.coords)
data$group <- factor(rep(1, nrow(data)))
data$x = row.col$row.coords
data$y = row.col$col.coords


fit.exp <- glmmTMB(resp ~ 1 + exp(pos + 0 | group), data=data)
summary(fit.exp)
### Note:
### from the glmmTMB source:
### exp( -dist(i,j) * exp(-theta(1)) ) );
### which means that to get our lambda parametrization we have to calculate: exp(-theta)
res1 <- simulateResiduals(fit.exp)
plot(res1)
fit$par

exp(-fit.exp$fit$par[4]) # original
exp(-fit.exp$fit$par[3]) # auch theta
lambda
global.mu


#
#
#
#
#

# 13.09 nmle --------------------------------------------------------------
View(my.data)
str(my.data)

data.1 <- data.frame(my.data$y)
id = rep(letters[1:20],5)
id
lm1 <- lme(y ~ 1, random=~ 1 | id, correlation=corGaus(form=~1|id), data = my.data)
summary(lm1)
plot(lm1)

exp(-fit.exp$fit$par[3])
exp(-fit.exp$fit$par[3])
exp(-fit.exp$fit$par[3])

#
#
#
#

#  Eigene Likelihood Function MVN -----------------------------------------

?MVN
# bau dir eine eigene Likelihood Funktion mit MVN und optimiere die mit optim, 
# und schau auch wieder dass sie den richtigen Parameter fitten
str(my.data)
?mvn
mvn.data <- as.matrix(my.data$D[,1:100],my.data$N,my.data$y)
mvn.data1 <- as.matrix(my.data$D[1:100,],my.data$N,my.data$y)

mvn.data # not all Arguments have the same lenght!!
mvn.data1
result = mvn(data = mvn.data,  mvnTest = "hz")
result1 = mvn(data = mvn.data1, univariatePlot = "qq" , multivariatePlot = "none",  mvnTest = "hz")
result1

result1 = mvn(data = mvn.data1,  mvnTest = "mardia") # Mardias´s test
result2 = mvn(data = mvn.data1,  mvnTest = "hz") # henze-Zirkler
result3 = mvn(data = mvn.data1,  mvnTest = "royston") # royston´s test
result4 = mvn(data = mvn.data1,  mvnTest = "dh") # Doornik-Hansen's test
result5 = mvn(data = mvn.data1,  mvnTest = "energy") # E statistik

# all seem to have the same Values ... so what is the diffrence and why is there no difference ? 

#---------
corlik <- function(x) {
  
  res <- 0
  if(x >= -1 & x <= 1) res = 1 
  
  return(log(res))
  
}
#----------
varlik <- function(x) {
  
  res <- 0
  if(x > 0) res = 1
  
  return(log(res))
  
}
#--------
#
#
# 
ll = function(par) {
  cov = solve(exp(-par[1]*D))
  mvtnorm::dmvnorm(my.data$y, mean = rep(par[2], 100), sigma = cov,log = TRUE)
}
result <- optim(par = c(0.1,10), fn = ll, gr = NULL, method = "SANN", hessian = FALSE)



# zweiter Versuch MLE von MVN zu laufen -----------------------------------
install.packages("corpcor")
library(corpcor)
# "solve", "qr.solve", "pseudoinverse"
#y <- -mvtnorm::dmvnorm(my.data$y, mean = rep(par[2], 100), sigma = cov,log = TRUE) 


ll = function(par) {
  cov = solve(exp(-par[1]*my.data$y))
  -mvtnorm::dmvnorm(my.data$y, mean = rep(par[2], 100), sigma = cov,log = TRUE)
}
methods = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent")
result <- optim(par = c(0.5,10),  fn = ll, gr = NULL, method = methods[5], hessian = FALSE)

?optim





# > y[1:N] ~ dmnorm(mu[], D.tau[,])
#
# > saveRDS(my.data, "test.RDS")
# > ttt = readRDS("test.RDS")
#
#
#

#------- For Loop Strucutre, Jags, nmle, glmmTMB (50 times) -------------

loopvec <- c(10,20,30,40,50,60)
loop50 <- matrix( 1:50)
for(loopitem in loopvec){
  print(loopitem)
}

#    #1 = fit.exp (glmmTMB)
#    #2 = lm1     (nmle)
#    #3 = fit     (JAGs)
fit.exp
lm1
fit

my.loop <- for (lambda in fit){
  print(lambda)
}



my.loop <- for (lambda in lm1){
  print(lambda[lm1])
  print(j)
  print(k)
}






my.loop <- for(row in 1:50(fit.exp,lm1,)){
  print(row)
}




# --------------OLD STUFF, USELESS or not working but still a backup---------------

# another try to fit glmmTMB ----------------------------------------------

str(my.data)
fit1 <- glmmTMB(y ~ 1, data = my.data)
fit1

?glmmTMB

n = 10000 
times <- factor(1:n, levels=1:n)
levels(times)

group <- factor(rep(1,n))
dat0 <- data.frame(y, times, group)
dat1 <- data.frame(my.data, times, group)

f1 <- glmmTMB(y ~ ar1(times + 0 | group), data = dat1) #ar = fit autregressive time series model to the data
summary(f1)

f2 <- glmmTMB(y ~ 1, data = dat1)
summary(f2)




.



# LET´s try to fit an lme Function # Versuch.6  ---------------------------------------
# --------------------------------- TIPP von MAX:---------------lme(y~1., data=data.frame(y=my.data$y))

# fm0 <- lme(y ~ 1 , data = data.frame (y = my.data$y))

fl1 <- lme(y ~ 1 , correlation = corGaus ,  data=data.frame(y=my.data$y))


?corGaus
?closure
?lme



fm1 <- lmList(y ~ 1 | group, data = my.data)
fm2 <- lme(fm1)

#


fm2 <- lmList(y ~ 1 , my.data)
fm3 <- lmList(y ~ 1 | group, random = 1, data = my.data)
fm4 <- lmList(my.data$y)

fm2 <- lme(fm1)
summary(fm1)
plot(fm1)
?lme
?lmList
# JUHU!!! erster PLOT :D 

fm1 <- lmList(Orthodont)
fm2 <- lme(fm1)
summary(fm1)
summary(fm2)

fm1 <- lme(y ~ 1 | group , random = ~ 1 , data = as.data.frame.data.frame(y = my.data$y))
fm2 <- lme.lmList(y ~1, data = (y = my.data$y))
?lme.lmList
print()
plot()


# Trying to fit an lme and glmmTMB function ~ 1 ----------------------------

?factor
glm1 <- glmmTMB(y ~ 1, data = my.data)
summary(glm1)
plot(glm1)

n=10000

times <- factor(1:n, levels = 1:n)
levels(times)

group <- factor(rep(1,n))
dat0 <- data.frame(y, times, group)
dat1 <- data.frame(my.data, times, group)

glm1 <- glmmTMB(y ~ ar1(times + 0 | group), data = dat0)
glm2 <- glmmTMB(y ~ ar1(times + 0 | group), data = dat1)
summary(glm1)
summary(glm2)
plot(glm1)
plot(glm2)
glm1
glm2


# Copy and paste try ------------------------------------------------------

simGroup <- function(g, n=6, rho=0.7) {
  x <- mvrnorm(mu = rep(0,n),
               Sigma = rho ^ as.matrix(dist(1:n)) )   ## Simulate the process
  y <- x + rnorm(n)                               ## Add measurement noise
  times <- factor(1:n)
  group <- factor(rep(g,n))
  data.frame(y, times, group)
}
simGroup(1)

dat1 <- do.call("rbind", lapply(1:1000, simGroup) )
fit.ar1 <- glmmTMB(y ~ ar1(times + 0 | group), data=dat1)
summary(fit.ar1)

fit.us <- glmmTMB(y ~ us(times + 0 | group), data=dat1, dispformula=~0)
fit.us$sdr$pdHess ## Converged ?
VarCorr(fit.us)

fit.toep <- glmmTMB(y ~ toep(times + 0 | group), data=dat1, dispformula=~0)
fit.toep$sdr$pdHess ## Converged ?
vc.toep <- VarCorr(fit.toep)

vc1 <- vc.toep$cond[[1]] ## first term of var-cov for RE of conditional model
summary(diag(vc1))
summary(vc1[row(vc1)!=col(vc1)])

fit.toep.reml <- update(fit.toep, REML=TRUE)
vc1R <- VarCorr(fit.toep.reml)$cond[[1]]
summary(diag(vc1R))
summary(vc1R[row(vc1R)!=col(vc1R)])


fit.gau <- glmmTMB(y ~ gau(times + 0 | group), data = dat1, dispformula=~0)
fit.gau$sdr$pdHess ## Converged











fit1 <- lme(y ~ 1 , data = data.frame(y = y))
fit2 <- lme(y ~ 1., data=data.frame(y=my.data$y))
# (das ist auch noch nicht ganz richtig, du musst jetzt noch in lme die spatial structure angeben, 
# soweit ich es im Kopf habe geht das mit dem correlation = ... parameter, schau dir die Hilfe an, da ist ne Liste mit verfügbaren correlation structures) 



fit.m <- (as.matrix(as.mcmc(fit)))
fit.m

fit.d <- as.data.frame(fit.m)
fit.d

fit1 <- lme(fit.m ~ 1, data = fit.m)
fit1 <- lme(fit.d ~ 1, data = fit.d)
fit1 <- lme(global.mu ~ 1, data = fit.d)
summary(fit1)
plot(fit1)





# Fitte die Daten mal testweise an mit nlme und glmmTMB und schau ob du die Packages den Parameter korrekt fitten 
# (sollte eigentlich easy funktionieren, einfach resp ~ 1, da wir ja im Moment noch keinen Prediktor haben, 
# aber du kannst natürlich einen einbauen, aber dann musst du ihn auch in a) einbauen)

#  fitx <- as.character(fit)
#  fit1 <- as.vector(as.matrix(fit))
#  resultswithglm <- jags(modelCode, monitor=c('lambda','global.mu'), modules='lme')
#  resultswithglm(fit)

#   install.packages("base")
#   library(base)
#   ??lappy
#   base::lapply(fit.m)
#   lapply(list, function)




