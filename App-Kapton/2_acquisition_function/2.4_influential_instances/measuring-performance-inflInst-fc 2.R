# Profiling 

library(profvis)
profvis()

# Parallelization
# how many cores do I have on my Mac? 8
library(parallel)
system.time(
)

# NO Parallelization: iters = 3, initial.design = 20
# user  system  elapsed 
# 37.846   1.792  39.675 

system.time(
inflInst(res.mbo = res.mbo.cb, iter = 3)
)

# NO Parallelization: iters = 3, initial.design = 100
# user  system    elapsed 
# 499.147  46.857 546.344 

system.time(
  inflInst(res.mbo = res.mbo.cb, iter = 3)
)

