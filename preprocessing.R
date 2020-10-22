require(xlsx)
library(dplyr)
library(descr)

library(caret)
library(tm)
library(foreach)
library(ggplot2)
library(ROSE)
library(xgboost)

library(doMC) # Only for Linux
registerDoMC(cores=4)

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
    ggtitle("XGBoost Cross-validation Visualization",
            subtitle = paste0('fold : ',length(model$folds),
                              '  iteration : ',model$niter
            )
    )+ylab(std)+theme(axis.title=element_text(size=11))
}


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
tr_df <- subset(df_mfull, select=-c(accuracy,model))
tr_y <- subset(df_mfull, select=accuracy)

# Categorical data to Factor

x_num = iris %>% model.matrix(~0+Species,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(iris %>% select(-Species,-Sepal.Width)) %>% data.matrix
y_num = iris$Sepal.Width

df_mfull[,"profile"] <- as.factor(df_mfull[,"profile"])

levels(df_mfull[,"linear_type"])<-c("asymmetric","symmetric","symmetric_power","symmetric_unint")

df_mfull[,"linear_type"] <- droplevels(df_mfull[,"linear_type"])

save(df_mfull,file="./df_mfull.Rdata")


a_num = df_mfull %>% model.matrix(~0+precision,.) %>% as.data.frame %>%  #one-hot encoding matrix
    bind_cols(df_mfull %>% select(-precision))
a_num = a_num %>% model.matrix(~0+profile,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(a_num %>% select(-profile)) 
a_num = a_num %>% model.matrix(~0+granularity,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(a_num %>% select(-granularity)) 
a_num = a_num %>% model.matrix(~0+clipping,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(a_num %>% select(-clipping)) 

a_rank <- a_num %>% model.matrix(~0+linear_type,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(a_num %>% select(-linear_type))
a_num = a_num %>% model.matrix(~0+linear_type,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(a_num %>% select(-linear_type,-accuracy, -model)) 

save(a_num,file="a_num.Rdata")

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
abs((b_hot[,1] - xgboost_model_hot$pred) / b_hot[,1])  %>% mean 



# hyper-parameter tuning

#make grid
grid = expand.grid(eta = seq(0.1,0.4,0.05),gamma = seq(0,5,1))

#do search by parallel
grid_search_hot = foreach(i = 1:nrow(grid),.combine = rbind,.packages = c('dplyr','xgboost')) %do% {
  model = xgb.cv(data = a_hot, label = b_hot,
                 nfold = 5, nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
                 objective = 'reg:squarederror',verbose = F, prediction = T,
                 params = list("eta"=grid[i,1],"gamma"=grid[i,2])
  )
  data.frame(train_rmse_last = unlist(model$evaluation_log[,2]) %>% last,
             test_rmse_last = unlist(model$evaluation_log[,4]) %>% last)
  
}

grid_search_hot[which.min(grid_search_hot$test_rmse_last),]
grid[which.min(grid_search_hot$test_rmse_last),]


# best modeling and feature importance

#xgbooster model using grid search parameter
model_best_hot = xgboost(data = a_hot, label = b_hot,
                nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
                objective = 'reg:squarederror',verbose = F,
                params = list("eta"=grid[which.min(grid_search_hot$test_rmse_last),1],
                              "gamma"=grid[which.min(grid_search_hot$test_rmse_last),2]))

abs((b_hot[,1] - predict(model_best_hot,a_hot)) / b_hot[,1])  %>% mean 


#make importance dataframe (기본 graph)
imp = xgb.importance(model = model_best_hot)
imp
xgb.plot.importance(imp)

# 변형 
importance_matrix <- xgb.importance(colnames(a_num), model = model_best_hot)
# Use `xgb.plot.importance`, which create a _barplot_ or use `xgb.ggplot.importance`
library(Ckmeans.1d.dp) # for xgb.ggplot.importance
xgb.ggplot.importance(importance_matrix, top_n = 15, measure = "Gain")


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

# Print XGBoost Tree
xgb.plot.tree(model = model_best_hot, trees = 1, feature_names = colnames(a_num))

xgb.plot.multi.trees(model = model_best_hot, trees = 1, feature_names = colnames(a_num))

# 6번의 leave one subject out cross validation LOSO, Testing, metric은 RMSE



# incremental learning (online learing)

model_best_hot_new = xgboost(data = a_hot, label = b_hot,
     nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
     objective = 'reg:squarederror',verbose = F,
     params = list("eta"=grid[which.min(grid_search_hot$test_rmse_last),1],
                   "gamma"=grid[which.min(grid_search_hot$test_rmse_last),2]),
     model=model_best_hot)


# Rank model로 다시 수행
# group =  model로 한다. 
#col_name <- colnames(a_rank)
#col_name[1] = "model"
#col_name[17] = "linear_typeasymmetric"
#a_rank <- a_rank[,col_name]

# group based rank 
rank_all_group <- a_rank %>% group_by(model) %>% 
  arrange(desc(accuracy)) %>% mutate(rnk=row_number()) %>% data.frame
rank_all_group <- arrange(rank_all_group, group_by = model)
rank_train_group <- rank_all_group %>% select(-c("model","accuracy","rnk")) %>% data.matrix
rank_label_group <- rank_all_group[,"rnk"]

# single group based rnak
rank_all <- a_rank %>% arrange(desc(accuracy)) %>% mutate(rnk=row_number())
rank_all %>% print(width=Inf, n=30) # 확인
rank_label <- rank_all[,"rnk"]
rank_train <- rank_all %>% select(-c("model","accuracy","rnk")) %>% data.matrix

# group: # of models
groups <- rank_all_group %>% group_by(model) %>% dplyr::summarise(cnt = n()) %>% pull(cnt)
# signle
groups <- 194

xgb_dm_rank_train <- xgb.DMatrix(data = rank_train_group, label = rank_label_group, group = groups)

# "rank:pairwise", "rank:ndcg" rank:map
xgb_predictor_ranking_model4 = xgboost(data = xgb_dm_rank_train,
         nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
         objective = "rank:pairwise",verbose = F,
         params = list("eta"=grid[which.min(grid_search_hot$test_rmse_last),1],
                       "gamma"=grid[which.min(grid_search_hot$test_rmse_last),2]))

predict(xgb_predictor_ranking_model, rank_train)
predict(xgb_predictor_ranking_model, rank_train) > 0 

predict(xgb_predictor_ranking_model2, rank_train)

predict(xgb_predictor_ranking_model3, rank_train)

predict(xgb_predictor_ranking_model4, rank_train_group)
# 그룹이 1개면 뭔가 이상한듯?? -> 전부 상수 값으로 출력됨


# 변형 
importance_matrix <- xgb.importance(colnames(rank_train_group), model = xgb_predictor_ranking_model4)
# Use `xgb.plot.importance`, which create a _barplot_ or use `xgb.ggplot.importance`
library(Ckmeans.1d.dp) # for xgb.ggplot.importance
xgb.ggplot.importance(importance_matrix, top_n = 15, measure = "Gain")


## backup-code
dtrain <- xgb.DMatrix(data = a, label=b)
bstDNatrux <- xgboost(data = dtrain, max.depth =2, eta= 1, nthread=2, nrounds=200, objective = 'reg:squarederror')
predict(bstDNatrux, dtrain)
abs((b[,1] - predict(bstDNatrux, dtrain)) / b[,1])  %>% mean 



