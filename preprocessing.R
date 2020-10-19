require(xlsx)
library(dplyr)
library(descr)

library(caret)
library(tm)
library(foreach)
library(ggplot2)
library(ROSE)

library(doMC) # Only for Linux
registerDoMC(cores=4)


df_full <- read.xlsx("quant.xlsx",sheetName="full")
df_mf <- read.xlsx("quant.xlsx",sheetName="m_f")

df_mfull <- merge(df_full, df_mf, by="model")

df_mfull <- df_mfull[]

# unrelated index
df_mfull <- df_mfull[-c(1,34,83,116,182,188,184,186,191,193.195,196,197,198,201,203,205,210,238,239,228,229,230,231,232,233,227),]

df_mfull <- df_mfull %>% select(-c("relative_error","relative_mixed","elapsed_time","backend","Fusion","NA."))
df_mfull <- data.frame((df_mfull))

df_mfull <- na.omit(df_mfull)

# Split label and training data 
tr_df <- subset(df_mfull, select=-accuracy)
tr_y <- subset(df_mfull, select=accuracy)

# Categorical data to Factor

x_num = iris %>% model.matrix(~0+Species,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(iris %>% select(-Species,-Sepal.Width)) %>% data.matrix
y_num = iris$Sepal.Width

df_mfull[,"profile"] <- as.factor(df_mfull[,"profile"])

levels(df_mfull[,"linear_type"])<-c("asymmetric","symmetric","symmetric_power","symmetric_unint")

df_mfull[,"linear_type"] <- droplevels(df_mfull[,"linear_type"])

a_num = df_mfull %>% model.matrix(~0+precision,.) %>% as.data.frame %>%  #one-hot encoding matrix
    bind_cols(df_mfull %>% select(-precision))
a_num = a_num %>% model.matrix(~0+profile,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(a_num %>% select(-profile)) 
a_num = a_num %>% model.matrix(~0+granularity,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(a_num %>% select(-granularity)) 
a_num = a_num %>% model.matrix(~0+clipping,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(a_num %>% select(-clipping)) 
a_num = a_num %>% model.matrix(~0+linear_type,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(a_num %>% select(-linear_type,-accuracy)) 

# factor
a <- tr_df %>% data.matrix
b <- tr_y %>% data.matrix

# one-hot encode
a_hot <- a_num %>% data.matrix
b_hot <- tr_y %>% data.matrix

#training model
xgboost_model = xgb.cv(data = a, label = b,
                   nfold = 5, nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
                   objective = 'reg:squarederror',verbose = F, prediction = T
)

xgboost_model_hot = xgb.cv(data = a_hot, label = b_hot,
                       nfold = 5, nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
                       objective = 'reg:squarederror',verbose = F, prediction = T
)

#result : output as list
attributes(xgboost_model)

#evaluation log
xgboost_model$evaluation_log


#check performance by plot
cvplot(xgboost_model)
cvplot(xgboost_model_hot)

#check performance by prediction (Mean absolute percetage error)
abs((b[,1] - xgboost_model$pred) / b[,1])  %>% mean 
abs((b[,1] - xgboost_model_hot$pred) / b[,1])  %>% mean 



# hyper-parameter tuning

#make grid
grid = expand.grid(eta = seq(0.1,0.4,0.05),gamma = seq(0,5,1))

#do search by parallel
grid_search = foreach(i = 1:nrow(grid),.combine = rbind,.packages = c('dplyr','xgboost')) %dopar% {
  model = xgb.cv(data = a_hot, label = b_hot,
                 nfold = 5, nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
                 objective = 'reg:squarederror',verbose = F, prediction = T,
                 params = list("eta"=grid[i,1],"gamma"=grid[i,2])
  )
  data.frame(train_rmse_last = unlist(model$evaluation_log[,2]) %>% last,
             test_rmse_last = unlist(model$evaluation_log[,4]) %>% last)
  
}

grid_search[which.min(grid_search$test_rmse_last),]
grid[which.min(grid_search$test_rmse_last),]


# best modeling and feature importance

#xgbooster model using grid search parameter
model = xgboost(data = a_hot, label = b_hot,
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
          subtitle = "Author : Jemin")


# 6번의 LOOSO Testing, metric은 RMSE



## backup-code
dtrain <- xgb.DMatrix(data = a, label=b)
bstDNatrux <- xgboost(data = dtrain, max.depth =2, eta= 1, nthread=2, nrounds=200, objective = 'reg:squarederror')

predict(bstDNatrux, dtrain)

abs((b[,1] - predict(bstDNatrux, dtrain)) / b[,1])  %>% mean 



