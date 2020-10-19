
# 참고사이트: https://apple-rbox.tistory.com/m/6

sapply(c('xgboost','dplyr', #xgboost for modeling, dplyr for data managing 
         'foreach' #parallel packages for grid search
),require,character.only = T)

library(ggplot2)
# Only for Linux
library(doMC) 
registerDoMC(cores=8)


x = iris %>% select(-Species) %>%
  data.matrix #matrix format
y = iris$Species


#==========================================================================
# Cross validation with whole data : multiclass classification
#==========================================================================

#training model
cv_model1 = xgb.cv(data = x, label = as.numeric(y)-1, num_class = levels(y) %>% length, # claiming data to use
                   nfold = 5, nrounds = 200, early_stopping_rounds = 150, # about folds and rounds
                   objective = 'multi:softprob', eval_metric = 'mlogloss', # model options
                   verbose = F, prediction = T # do not print messages while training, make prediction
)

#result : output as list
attributes(cv_model1)

#evaluation log
cv_model1$evaluation_log

#prediction matrix(for softprob) to dataframe
cv_model1$pred
pred_df = cv_model1$pred %>% as.data.frame %>%
  mutate(pred = levels(y)[max.col(.)] %>% as.factor,actual = y)

#table 
pred_df %>% select(pred,actual) %>% table #table
caret::confusionMatrix(pred_df$pred,pred_df$actual) #confusion matrix

#visualizing model

cvplot = function(model){ #visualizing function
  eval.log = model$evaluation_log
  
  std = names(eval.log[,2]) %>% gsub('train_','',.) %>% gsub('_mean','',.)
  
  data.frame(error = c(unlist(eval.log[,2]),unlist(eval.log[,4])),
             class = c(rep('train',nrow(eval.log)),
                       rep('test',nrow(eval.log))),
             nround = rep(1:nrow(eval.log),2)
  ) %>%
    ggplot(aes(nround,error,col = class))+
    geom_point(alpha = 0.2)+
    geom_smooth(alpha = 0.4,se = F)+
    theme_bw()+
    ggtitle("Apple's Rbox : XGBoost Cross-validation Visualization",
            subtitle = paste0('fold : ',length(model$folds),
                              '  iteration : ',model$niter
            )
    )+ylab(std)+theme(axis.title=element_text(size=11))
}

cvplot(cv_model1)


#==========================================================================
# Cross validation with whole data : numeric target regression
#==========================================================================

#which one to be a numeric target in iris??
cor(iris %>% select(-Species)) %>% corrplot::corrplot(method = 'number')

#reclaiming data
x_num = iris %>% model.matrix(~0+Species,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(iris %>% select(-Species,-Sepal.Width)) %>% data.matrix
y_num = iris$Sepal.Width

#training model
cv_model2 = xgb.cv(data = x_num, label = y_num,
                   nfold = 5, nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
                   objective = 'reg:squarederror',verbose = F, prediction = T
)

#check performance by plot
cvplot(cv_model2)

#check performance by prediction (Mean absolute percetage error)
abs((y_num - cv_model2$pred) / y_num)  %>% mean #7%




#==========================================================================
# Hyper parameter tuning : grid search
#==========================================================================

#make grid
grid = expand.grid(eta = seq(0.1,0.4,0.05),gamma = seq(0,5,1))

#make parallel cluster and register
cl = makeCluster(detectCores()-1) #n-1 core cluster
registerDoParallel(cl)

g_list = list(0,1)

model = xgb.cv(data = x_num, label = y_num,
               nfold = 5, nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
               objective = 'reg:squarederror',verbose = F, prediction = T,
               params = list("eta"=0.1,"gamma"=0)
)

#do search by parallel
grid_search = foreach(i = 1:nrow(grid),.combine = rbind,.packages = c('dplyr','xgboost')) %do% {
  model = xgb.cv(data = x_num, label = y_num,
                 nfold = 5, nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
                 objective = 'reg:squarederror',verbose = F, prediction = T,
                 params = list("eta"=grid[i,1],"gamma"=grid[i,2])
  )
  data.frame(train_rmse_last = unlist(model$evaluation_log[,2]) %>% last,
             test_rmse_last = unlist(model$evaluation_log[,4]) %>% last)
  
}

stopCluster(cl) #end parallel cluster

grid_search[which.min(grid_search$test_rmse_last),]

grid[which.min(grid_search$test_rmse_last),]




#==========================================================================
# Variable importance using xgb.importance
#==========================================================================


#xgbooster model using grid search parameter
model = xgboost(data = x_num, label = y_num,
                nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
                objective = 'reg:squarederror',verbose = F,
                params = list("eta"=grid[which.min(grid_search$test_rmse_last),1],
                              "gamma"=grid[which.min(grid_search$test_rmse_last),2]))

#make importance dataframe
imp = xgb.importance(model = model)
imp
xgb.plot.importance(imp)

#customized visualization
data.frame(variable = rep(imp$Feature,3),
           value = c(imp$Gain,imp$Cover,imp$Frequency),
           Type = c(rep('Gain',nrow(imp)),rep('Cover',nrow(imp)),rep('Frequency',nrow(imp)))
) %>% ggplot(aes(variable,value,fill = variable))+
  geom_bar(stat = 'identity')+
  facet_grid(~Type)+
  theme_bw()+
  ggtitle('XGBoost : Customized Importance Plot',
          subtitle = "Author : Apple's R box")

# Gain: featrue를 추가해서 새로운 branch를 생성 했을 때의 정확도 향상
# Cover: measures the relative quantity of observations concerned by a feature
# Frequency: It just counts the number of times a feature is used in all generated trees.


