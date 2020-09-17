
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
result_glmmTMB = matrix(NA, 10, 3)
for(i in 1:10){
  time_glmmTMB = 
    system.time({
      m1 = glmmTMB(y~1, data = my.data)
    })
  result_glmmTMB[i, 1] = time_glmmTMB[3]
  result_glmmTMB[i, 2] = summary(m1)$coefficients$cond[1]
}
b


