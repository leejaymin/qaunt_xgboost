library(GA)
library(testit)

# Clean measurement data
ga_df <- df_full_1117 
ga_df <- ga_df %>% filter(backend != "VTAInterpreter") %>% filter(precision != "FP32")
ga_df <- ga_df[,c("model","precision","profile","granularity","clipping","schema","accuracy")]
ga_df <- droplevels(ga_df)
ga_df$precision <- recode_factor(ga_df$precision, "8"=0, mixed=1)
ga_df$profile <- recode_factor(ga_df$profile, "1"=0, "1000"=1, "10000"=2)
ga_df$granularity <- recode_factor(ga_df$granularity, "channel"=0, "tensor"=1)
ga_df$clipping <- recode_factor(ga_df$clipping, "KL"=0, "max"=1)
ga_df$schema <- recode_factor(ga_df$schema, "asymmetric"=0, "symmetric"=1, "symmetric_power"=2, "symmetric_unint"=3)

save(ga_df,file="ga_df.Rdata")

# Test code
res <- data.frame()
for (item in model_names){
  for (i in seq(0,47,1)){
    x <- decimal2binary(i,6)
    sch <- binary2decimal(x[1:2])
    cli <- binary2decimal(x[3])
    gran <- binary2decimal(x[4]) 
    calib <- binary2decimal(x[5:6]) 
    
    if(calib > 2){
      calib <- 2
    }
    
    ga_df_item <- ga_df %>% filter(model==item) # select model
    acc <- ga_df_item %>% filter(
        schema == sch &
        clipping == cli &
        granularity == gran &
        profile == calib
    )
    assert(sprintf("bad count %s %d",item,i), {dim(acc)[1] <= 2 })
    res <- rbind.data.frame(res,
                            data.frame("model"=item,"index"=i, "acc"=acc[,"accuracy"]))
  }
}


# define fit_function
fit_function_wo_precision <- function(x){
  sch <- binary2decimal(x[1:2])
  cli <- binary2decimal(x[3])
  gran <- binary2decimal(x[4]) 
  calib <- binary2decimal(x[5:6]) 
  
  if(calib > 2){
    calib <- 2
  }
  
  ga_df_item <- ga_df %>% filter(model==item) # select model
  
  acc <- ga_df_item %>% filter(
    schema == sch &
      clipping == cli &
      granularity == gran &
      profile == calib
  )
  assert(sprintf("bad count %s %d",item,i), {dim(acc)[1] == 1 })
  return(acc[,"accuracy"])
}

fit_function_w_precision <- function(x){
  sch <- binary2decimal(x[1:2])
  cli <- binary2decimal(x[3])
  gran <- binary2decimal(x[4]) 
  calib <- binary2decimal(x[5:6]) 
  pre <- binary2decimal(x[7])
  
  if(calib > 2){
    calib <- 2
  }
  
  ga_df_item <- ga_df %>% filter(model==item)
  
  acc <- ga_df_item %>% filter(
    schema == sch &
      clipping == cli &
      granularity == gran &
      profile == calib &
      precision == pre
  )
  assert(sprintf("bad count %s %d",item,i), {dim(acc)[1] == 1 })
  return(acc[,"accuracy"])
}

# without mixed-precision
for (item in model_names[c(1,3,5,6)]){
  print(item)
  ga_tune <- ga(type = "binary", fitness = fit_function_wo_precision, nBits = 6, seed = 12, popSize = 2, 
                maxiter = 24, run = 24, pmutation=0.1, parallel = F, selection = gabin_lrSelection)  
  summary(ga_tune)
  plot(ga_tune)
}

# with mixed-precision 
for (item in model_names[c(2,4)]){
  print(item)
  ga_tune <- ga(type = "binary", fitness = fit_function_w_precision, nBits = 7, seed = 122, popSize = 2, 
                maxiter = 48, run = 48, pmutation=0.5, parallel = F, selection = gabin_lrSelection)  
  summary(ga_tune)
  plot(ga_tune)
}

genetic_top1_accuracy[[1]] <- c(rep(70.13,4),rep(70.7,44)) #48 MobileNet
genetic_top1_accuracy[[2]] <- c(rep(62.4,10),rep(63.13,54)) # 64 ShuffleNet
genetic_top1_accuracy[[3]] <- c(rep(13.81,4),rep(48.08,22),rep(52.40,22)) # 48 SqueezeNet
genetic_top1_accuracy[[4]] <- c(rep(69.63,2),rep(70.15,8),rep(70.17,12),rep(70.22,41)) # 63 GoogleNet
genetic_top1_accuracy[[5]] <- c(rep(16.70,4),rep(28.05,22),rep(69.15,22)) # Resnet18
genetic_top1_accuracy[[6]] <- c(rep(75.77,4),rep(75.79,44)) # Resnet50

save(genetic_top1_accuracy,file="genetic_top1_accuracy.Rdata")

# Experiment results are as below:
# # Genetic Algorithm
# [1] MobileNet
# GA | iter = 1 | Mean = 63.845 | Best = 70.130
# GA | iter = 2 | Mean = 63.845 | Best = 70.130
# GA | iter = 3 | Mean = 70.415 | Best = 70.700
# GA | iter = 4 | Mean = 70.7 | Best = 70.7
# GA | iter = 5 | Mean = 70.7 | Best = 70.7
# GA | iter = 6 | Mean = 70.7 | Best = 70.7
# GA | iter = 7 | Mean = 70.7 | Best = 70.7
# GA | iter = 8 | Mean = 70.7 | Best = 70.7
# GA | iter = 9 | Mean = 70.7 | Best = 70.7
# GA | iter = 10 | Mean = 70.7 | Best = 70.7
# GA | iter = 11 | Mean = 70.7 | Best = 70.7
# GA | iter = 12 | Mean = 70.7 | Best = 70.7
# GA | iter = 13 | Mean = 70.7 | Best = 70.7
# GA | iter = 14 | Mean = 70.7 | Best = 70.7
# GA | iter = 15 | Mean = 70.7 | Best = 70.7
# GA | iter = 16 | Mean = 70.7 | Best = 70.7
# GA | iter = 17 | Mean = 70.7 | Best = 70.7
# GA | iter = 18 | Mean = 70.7 | Best = 70.7
# GA | iter = 19 | Mean = 70.7 | Best = 70.7
# GA | iter = 20 | Mean = 70.7 | Best = 70.7
# GA | iter = 21 | Mean = 70.7 | Best = 70.7
# GA | iter = 22 | Mean = 70.7 | Best = 70.7
# GA | iter = 23 | Mean = 70.7 | Best = 70.7
# GA | iter = 24 | Mean = 70.7 | Best = 70.7
# 
# [1] ShuffleNet
# GA | iter = 1 | Mean = 55.665 | Best = 62.400
# GA | iter = 2 | Mean = 54.595 | Best = 62.400
# GA | iter = 3 | Mean = 36.48 | Best = 62.40
# GA | iter = 4 | Mean = 62.4 | Best = 62.4
# GA | iter = 5 | Mean = 62.4 | Best = 62.4
# GA | iter = 6 | Mean = 62.765 | Best = 63.130
# GA | iter = 7 | Mean = 63.13 | Best = 63.13
# GA | iter = 8 | Mean = 63.13 | Best = 63.13
# GA | iter = 9 | Mean = 60.75 | Best = 63.13
# GA | iter = 10 | Mean = 62.765 | Best = 63.130
# GA | iter = 11 | Mean = 60.75 | Best = 63.13
# GA | iter = 12 | Mean = 62.425 | Best = 63.130
# GA | iter = 13 | Mean = 62.455 | Best = 63.130
# GA | iter = 14 | Mean = 63.13 | Best = 63.13
# GA | iter = 15 | Mean = 63.13 | Best = 63.13
# GA | iter = 16 | Mean = 63.13 | Best = 63.13
# GA | iter = 17 | Mean = 63.13 | Best = 63.13
# GA | iter = 18 | Mean = 62.455 | Best = 63.130
# GA | iter = 19 | Mean = 62.64 | Best = 63.13
# GA | iter = 20 | Mean = 62.64 | Best = 63.13
# GA | iter = 21 | Mean = 60.75 | Best = 63.13
# GA | iter = 22 | Mean = 60.75 | Best = 63.13
# GA | iter = 23 | Mean = 63.13 | Best = 63.13
# GA | iter = 24 | Mean = 63.13 | Best = 63.13
# GA | iter = 25 | Mean = 60.75 | Best = 63.13
# GA | iter = 26 | Mean = 60.75 | Best = 63.13
# GA | iter = 27 | Mean = 60.75 | Best = 63.13
# GA | iter = 28 | Mean = 57.79 | Best = 63.13
# GA | iter = 29 | Mean = 62.455 | Best = 63.130
# GA | iter = 30 | Mean = 61.81 | Best = 63.13
# GA | iter = 31 | Mean = 61.81 | Best = 63.13
# GA | iter = 32 | Mean = 63.13 | Best = 63.13
# GA | iter = 33 | Mean = 62.455 | Best = 63.130
# GA | iter = 34 | Mean = 62.58 | Best = 63.13
# GA | iter = 35 | Mean = 62.955 | Best = 63.130
# GA | iter = 36 | Mean = 63.13 | Best = 63.13
# GA | iter = 37 | Mean = 63.13 | Best = 63.13
# GA | iter = 38 | Mean = 57.79 | Best = 63.13
# GA | iter = 39 | Mean = 63.36 | Best = 63.59
# GA | iter = 40 | Mean = 36.50 | Best = 63.59
# GA | iter = 41 | Mean = 63.59 | Best = 63.59
# GA | iter = 42 | Mean = 63.075 | Best = 63.590
# GA | iter = 43 | Mean = 62.995 | Best = 63.590
# GA | iter = 44 | Mean = 59.22 | Best = 63.59
# GA | iter = 45 | Mean = 62.995 | Best = 63.590
# GA | iter = 46 | Mean = 63.59 | Best = 63.59
# GA | iter = 47 | Mean = 63.59 | Best = 63.59
# GA | iter = 48 | Mean = 36.50 | Best = 63.59
# 
# [1] SqueezeNet
# GA | iter = 1 | Mean =  6.955 | Best = 13.810
# GA | iter = 2 | Mean =  6.955 | Best = 13.810
# GA | iter = 3 | Mean = 30.945 | Best = 48.080
# GA | iter = 4 | Mean = 48.08 | Best = 48.08
# GA | iter = 5 | Mean = 48.08 | Best = 48.08
# GA | iter = 6 | Mean = 48.08 | Best = 48.08
# GA | iter = 7 | Mean = 48.08 | Best = 48.08
# GA | iter = 8 | Mean = 48.08 | Best = 48.08
# GA | iter = 9 | Mean = 48.08 | Best = 48.08
# GA | iter = 10 | Mean = 48.08 | Best = 48.08
# GA | iter = 11 | Mean = 48.08 | Best = 48.08
# GA | iter = 12 | Mean = 48.08 | Best = 48.08
# GA | iter = 13 | Mean = 48.08 | Best = 48.08
# GA | iter = 14 | Mean = 50.24 | Best = 52.40
# GA | iter = 15 | Mean = 50.24 | Best = 52.40
# GA | iter = 16 | Mean = 52.4 | Best = 52.4
# GA | iter = 17 | Mean = 52.4 | Best = 52.4
# GA | iter = 18 | Mean = 52.4 | Best = 52.4
# GA | iter = 19 | Mean = 52.4 | Best = 52.4
# GA | iter = 20 | Mean = 52.4 | Best = 52.4
# GA | iter = 21 | Mean = 52.4 | Best = 52.4
# GA | iter = 22 | Mean = 52.4 | Best = 52.4
# GA | iter = 23 | Mean = 52.4 | Best = 52.4
# GA | iter = 24 | Mean = 52.4 | Best = 52.4
# 
# [1] googlenet_slim_v4
# GA | iter = 1 | Mean = 69.45 | Best = 69.63
# GA | iter = 2 | Mean = 69.89 | Best = 70.15
# GA | iter = 3 | Mean = 70.12 | Best = 70.15
# GA | iter = 4 | Mean = 70.15 | Best = 70.15
# GA | iter = 5 | Mean = 70.15 | Best = 70.15
# GA | iter = 6 | Mean = 70.16 | Best = 70.17
# GA | iter = 7 | Mean = 69.92 | Best = 70.17
# GA | iter = 8 | Mean = 69.92 | Best = 70.17
# GA | iter = 9 | Mean = 69.185 | Best = 70.170
# GA | iter = 10 | Mean = 70.16 | Best = 70.17
# GA | iter = 11 | Mean = 69.555 | Best = 70.170
# GA | iter = 12 | Mean = 70.195 | Best = 70.220
# GA | iter = 13 | Mean = 45.39 | Best = 70.22
# GA | iter = 14 | Mean = 70.22 | Best = 70.22
# GA | iter = 15 | Mean = 70.22 | Best = 70.22
# GA | iter = 16 | Mean = 70.22 | Best = 70.22
# GA | iter = 17 | Mean = 70.22 | Best = 70.22
# GA | iter = 18 | Mean = 45.39 | Best = 70.22
# GA | iter = 19 | Mean = 70.085 | Best = 70.220
# GA | iter = 20 | Mean = 70.085 | Best = 70.220
# GA | iter = 21 | Mean = 69.645 | Best = 70.220
# GA | iter = 22 | Mean = 69.645 | Best = 70.220
# GA | iter = 23 | Mean = 70.22 | Best = 70.22
# GA | iter = 24 | Mean = 70.22 | Best = 70.22
# GA | iter = 25 | Mean = 69.645 | Best = 70.220
# GA | iter = 26 | Mean = 69.17 | Best = 70.22
# GA | iter = 27 | Mean = 69.17 | Best = 70.22
# GA | iter = 28 | Mean = 69.945 | Best = 70.220
# GA | iter = 29 | Mean = 45.39 | Best = 70.22
# GA | iter = 30 | Mean = 51.635 | Best = 70.220
# GA | iter = 31 | Mean = 51.635 | Best = 70.220
# GA | iter = 32 | Mean = 69.965 | Best = 70.220
# GA | iter = 33 | Mean = 45.39 | Best = 70.22
# GA | iter = 34 | Mean = 67.58 | Best = 70.22
# GA | iter = 35 | Mean = 67.445 | Best = 70.220
# GA | iter = 36 | Mean = 69.965 | Best = 70.220
# GA | iter = 37 | Mean = 70.22 | Best = 70.22
# GA | iter = 38 | Mean = 70.195 | Best = 70.220
# GA | iter = 39 | Mean = 70.155 | Best = 70.220
# GA | iter = 40 | Mean = 51.635 | Best = 70.220
# GA | iter = 41 | Mean = 70.22 | Best = 70.22
# GA | iter = 42 | Mean = 45.39 | Best = 70.22
# GA | iter = 43 | Mean = 69.965 | Best = 70.220
# GA | iter = 44 | Mean = 69.17 | Best = 70.22
# GA | iter = 45 | Mean = 69.965 | Best = 70.220
# GA | iter = 46 | Mean = 70.22 | Best = 70.22
# GA | iter = 47 | Mean = 70.22 | Best = 70.22
# GA | iter = 48 | Mean = 51.635 | Best = 70.220
# 
# [1] resnet18
# GA | iter = 1 | Mean =  9.505 | Best = 16.700
# GA | iter = 2 | Mean =  9.505 | Best = 16.700
# GA | iter = 3 | Mean = 22.375 | Best = 28.050
# GA | iter = 4 | Mean = 28.05 | Best = 28.05
# GA | iter = 5 | Mean = 28.05 | Best = 28.05
# GA | iter = 6 | Mean = 28.05 | Best = 28.05
# GA | iter = 7 | Mean = 28.05 | Best = 28.05
# GA | iter = 8 | Mean = 28.05 | Best = 28.05
# GA | iter = 9 | Mean = 28.05 | Best = 28.05
# GA | iter = 10 | Mean = 28.05 | Best = 28.05
# GA | iter = 11 | Mean = 28.05 | Best = 28.05
# GA | iter = 12 | Mean = 28.05 | Best = 28.05
# GA | iter = 13 | Mean = 28.05 | Best = 28.05
# GA | iter = 14 | Mean = 48.60 | Best = 69.15
# GA | iter = 15 | Mean = 48.60 | Best = 69.15
# GA | iter = 16 | Mean = 69.15 | Best = 69.15
# GA | iter = 17 | Mean = 69.15 | Best = 69.15
# GA | iter = 18 | Mean = 69.15 | Best = 69.15
# GA | iter = 19 | Mean = 69.15 | Best = 69.15
# GA | iter = 20 | Mean = 69.15 | Best = 69.15
# GA | iter = 21 | Mean = 69.15 | Best = 69.15
# GA | iter = 22 | Mean = 69.15 | Best = 69.15
# GA | iter = 23 | Mean = 69.15 | Best = 69.15
# GA | iter = 24 | Mean = 69.15 | Best = 69.15
# 
# [1] resnet50
# GA | iter = 1 | Mean = 75.455 | Best = 75.770
# GA | iter = 2 | Mean = 75.455 | Best = 75.770
# GA | iter = 3 | Mean = 75.78 | Best = 75.79
# GA | iter = 4 | Mean = 75.79 | Best = 75.79
# GA | iter = 5 | Mean = 75.79 | Best = 75.79
# GA | iter = 6 | Mean = 75.79 | Best = 75.79
# GA | iter = 7 | Mean = 75.79 | Best = 75.79
# GA | iter = 8 | Mean = 75.79 | Best = 75.79
# GA | iter = 9 | Mean = 75.79 | Best = 75.79
# GA | iter = 10 | Mean = 75.79 | Best = 75.79
# GA | iter = 11 | Mean = 75.79 | Best = 75.79
# GA | iter = 12 | Mean = 75.79 | Best = 75.79
# GA | iter = 13 | Mean = 75.79 | Best = 75.79
# GA | iter = 14 | Mean = 75.79 | Best = 75.79
# GA | iter = 15 | Mean = 75.79 | Best = 75.79
# GA | iter = 16 | Mean = 75.79 | Best = 75.79
# GA | iter = 17 | Mean = 75.79 | Best = 75.79
# GA | iter = 18 | Mean = 75.79 | Best = 75.79
# GA | iter = 19 | Mean = 75.79 | Best = 75.79
# GA | iter = 20 | Mean = 75.79 | Best = 75.79
# GA | iter = 21 | Mean = 75.79 | Best = 75.79
# GA | iter = 22 | Mean = 75.79 | Best = 75.79
# GA | iter = 23 | Mean = 75.79 | Best = 75.79
# GA | iter = 24 | Mean = 75.79 | Best = 75.79




