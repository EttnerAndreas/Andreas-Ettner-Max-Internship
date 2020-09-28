## PyTorch
library(reticulate)
torch = import("torch")


?reticulate

X = dist.matrix(side)
Y = side
Z = lambda
# y = beta0 + beta*X 

summary(lm(Y~X))

#c++

# 1. Data -> Tensors
## 1.1 Set constant tensors (e.g. observations)
XT = torch$tensor(X, dtype = torch$float32)   # constant
YT = torch$tensor(Y, dtype = torch$float32) # constant
ZT = torch$tensor(X, dtype = torch$float32)   # constant

## 1.2. Define parameters
lambda = torch$tensor(0.0001, dtype=torch$float32, requires_grad=TRUE)
mu = torch$tensor(0.0001, dtype=torch$float32, requires_grad=TRUE)

# 2. Set optimizer:
optimizer = torch$optim$Adamax( params = list(Beta), lr = 0.1 )
optimizer = torch$optim$Adamax( params = list(lambda), lr = 0.1 )
optimizer = torch$optim$Adamax( params = list(mu), lr = 0.1 )

# 3. Train loop:
for(i in 1:100){
  
  ### calculate likelihood ###
  #dnorm(Y , mean=X*par, log=TRUE)
  dist = torch$distributions$Normal( loc = torch$mul( XT, Beta), scale = 1.0  )
  loss = torch$sum( torch$neg( dist$log_prob( YT ) ))
  
  ### calculate gradient and update parameters
  optimizer$zero_grad()
  
  loss$backward() # calculate gradient
  optimizer$step()
  
  print(Beta)
}
Beta





# back to R:
RBeta = Beta$data$cpu()$numpy()
