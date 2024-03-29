
# load
load("./df_mfull.Rdata")


# preprocessing effect according to factor and one-hot encoding -----------
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



# hyper-parameter search for XGBosost training ----------------------------
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
save(grid_search_hot,file="./grid_search_hot.Rdata")
grid_search_hot[which.min(grid_search_hot$test_rmse_last),]
grid[which.min(grid_search_hot$test_rmse_last),]



#xgbooster model using grid search parameter
model_best_hot = xgboost(data = a_hot, label = b_hot,
                         nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
                         objective = 'reg:squarederror',verbose = F,
                         params = list("eta"=0.15 ,"gamma"=3))
                         #params = list("eta"=grid[which.min(grid_search_hot$test_rmse_last),1],
                        #               "gamma"=grid[which.min(grid_search_hot$test_rmse_last),2]))
# new, eta 0.3, gamma = 1
# eta 0.15, gamma = 2 
# for paper, eta 0.15, gamma =3 
save(model_best_hot,file="./model_best_hot.Rdata")

abs((b_hot[,1] - predict(model_best_hot,a_hot)) / b_hot[,1])  %>% mean 



# make importance dataframe (기본 graph)
imp = xgb.importance(model = model_best_hot)
imp
xgb.plot.importance(imp)

# 변형 
importance_matrix <- xgb.importance(colnames(a_num), model = model_best_hot)
# Use `xgb.plot.importance`, which create a _barplot_ or use `xgb.ggplot.importance`
library(Ckmeans.1d.dp) # for xgb.ggplot.importance
xgb.ggplot.importance(importance_matrix, top_n = 30, measure = "Gain") + mytheme +
  theme(legend.title = element_blank(), 
        legend.position="none",
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid")) +
  ggtitle("")



#customized visualization
data.frame(variable = rep(imp$Feature,3),
           value = c(imp$Gain,imp$Cover,imp$Frequency),
           Type = c(rep('Gain',nrow(imp)),rep('Cover',nrow(imp)),rep('Frequency',nrow(imp)))
) %>% filter(Type=="Gain") %>% ggplot(aes(variable,value,fill = variable))+
  geom_bar(stat = 'identity')+
  #facet_grid(~Type)+
  mytheme 

# Print XGBoost Tree ------------------------------------------------------
xgb.plot.tree(model = model_best_hot, trees = 1, feature_names = colnames(a_num))
xgb.plot.multi.trees(model = model_best_hot, trees = 1, feature_names = colnames(a_num))



# Leave One Subject Out Cross Validation, LOSO ----------------------------

# Testing sequence
## 1) LOSO, save rds file along with each model 
## 2) load each rds file and retrain with test data.
## 3) batch learning to know effect between batch and online learnings

# 6번의 leave one subject out cross validation LOSO, Testing, metric은 RMSE
model_names <- unique(rank_all_group[,"model"])

result_reg_loso <- data.frame()
reg_loso_m_list <- list()
for(item in model_names){
  reg_train_loso <- rank_all_group %>% filter(model!=item) %>%select(-c("accuracy","rnk","model")) %>% data.matrix
  reg_tr_label_loso <- rank_all_group %>% filter(model!=item) %>% select("accuracy")
  reg_test_loso <- rank_all_group %>% filter(model==item) %>%select(-c("accuracy","rnk","model")) %>% data.matrix
  reg_te_label_loso <- rank_all_group %>% filter(model==item) %>% select("accuracy")
  
  temporal_model = xgboost(data = reg_train_loso, label = reg_tr_label_loso[,1],
  nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
  objective = 'reg:squarederror',verbose = F,
  params = list("eta"=grid[which.min(grid_search_hot$test_rmse_last),1],
               "gamma"=grid[which.min(grid_search_hot$test_rmse_last),2]))
  saveRDS(temporal_model, paste0(item,".rds"))
  
  pred <- predict(temporal_model, reg_test_loso)  
  # mean absolute percentage error 
  temporal_rmse <- abs((reg_te_label_loso[,1] - pred) / reg_te_label_loso[,1])  %>% mean
  
  result_reg_loso <- rbind.data.frame(result_reg_loso,data.frame("model"=item,"rmse"=temporal_rmse))
}


# incremental learning (online learing): initial point from other data and then build model only with current data.
# + check data to use unexplored them.
final_exlored_sequence <- list()
online_top1_seq <- list() 
online_top1_accuracy <- list()

for (sample in (1:1)){
  #print(sprintf("######### Smaple: %d ############",sample))
  prev_pred_accuracy_sum <- -1

  for(item in model_names){
    print(item)
    
    # google lenet v4 implementation 
    reg_test_loso <- rank_all_group %>% filter(model==item) #%>%select(-c("accuracy","rnk","model")) %>% data.matrix
    #reg_te_label_loso <- rank_all_group %>% filter(model==item) %>% select("accuracy","rnk")
    
    online_xgb_models <- readRDS(paste0(item,".rds"))
    #original_xgb_models <- readRDS(paste0(item,".rds"))
    print(1)
    pred <- predict(online_xgb_models, reg_test_loso %>% select(-c("accuracy","rnk","model")) %>% data.matrix)  
        
    df_res_pred <- cbind.data.frame(pred,reg_test_loso)
    
    top1_pred <- which.max(df_res_pred[,"pred"])
    top1_real <- which.max(df_res_pred[,"accuracy"])
    
    df_res_pred <- df_res_pred %>% arrange(desc(pred)) # along with prediction, rearrange pred to make training data.
  
    print(sprintf("Top1 pred: %d, Real: %d",top1_pred,top1_real))
    print(df_res_pred[1:sample,c("rnk")])
    print(df_res_pred[1:sample,c("accuracy")])
    print(df_res_pred[1:sample,c("pred")])
    pred_accuracy_sum <- sum(df_res_pred[1:sample,c("pred")])
    print(sprintf("sum of accuracy: %f",pred_accuracy_sum))
    top1_seq <- c(top1_pred)
    # redundant check (validation check for training)
    #browser()
    print(df_res_pred %>% filter(pred == max(pred)) %>% tally())
    if(df_res_pred %>% filter(pred == max(pred)) %>% tally() %>% as.vector == 1){
      top1_accuracy <- c(df_res_pred[which.max(df_res_pred[,"pred"]), "accuracy"])
    }else{ #중복 있으면 추론 실패로 zero
      top1_accuracy <- c(0)
    }
    #top1_accuracy <- c(0) # simple online learning
    
    print("top1_accuracy vector")
    print(top1_accuracy)
    
    # prepare online learning dataset
    #online_tr_label <- online_tr[1:sample,"accuracy"] # 예측 값의 5개 (top5)의 real rank
    #online_tr <- online_tr[1:sample,] %>% select(-pred, -rnk, -accuracy) %>% data.matrix 
    
    prev_pred_accuracy_sum <- pred_accuracy_sum
    online_xgb_models = xgboost(data = df_res_pred[1:sample,] %>% select(-pred, -rnk, -accuracy, -model) %>% data.matrix, 
                                label = df_res_pred[1:sample,"accuracy"],
                                nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
                                objective = 'reg:squarederror',verbose = F,
                                params = list("eta"=grid[which.min(grid_search_hot$test_rmse_last),1],
                                              "gamma"=grid[which.min(grid_search_hot$test_rmse_last),2]))
    
    explored_rnk <- df_res_pred[1:sample,"rnk"] # 학습에 쓴것들을 기록한다. 다시 재 탐색을 하지 않기 위해서 
    print("explored_rnk")
    print(explored_rnk)
    
    for (i in seq(sample+1,length(df_res_pred[,"accuracy"]),1)){
      print(i)
      #browser()
      pred <- predict(online_xgb_models, reg_test_loso %>% select(-c("accuracy","rnk","model")) %>% data.matrix)  
      df_res_pred <- cbind.data.frame(pred,reg_test_loso)
      # top 1 and print results
      top1_pred <- which.max(df_res_pred[,"pred"])
      top1_real <- which.max(df_res_pred[,"accuracy"])
      df_res_pred <- df_res_pred %>% arrange(desc(pred)) # along with prediction, rearrange pred to make training data.
      print(sprintf("Top1 pred: %d, Real: %d",top1_pred,top1_real))
      print(df_res_pred[1:sample,c("rnk")])
      print(df_res_pred[1:sample,c("accuracy")])
      print(df_res_pred[1:sample,c("pred")])
      pred_accuracy_sum <- sum(df_res_pred[1:sample,c("pred")])
      print(sprintf("sum of accuracy: %f",pred_accuracy_sum))
      print(df_res_pred %>% filter(pred == max(pred)) %>% tally())
      #if(pred_accuracy_sum == prev_pred_accuracy_sum){
      #  break
      #}
      
      # prepare online learning dataset
      #browser()
      explored_dfres <- df_res_pred %>% filter(rnk %in% explored_rnk) # 탐색한것만 data frame 생성 (model 대신 data 사용)
      unexplored_dfres <- df_res_pred %>% filter(!rnk %in% explored_rnk) # 탐색한것 뺴고 data frame 생성
      unexplored_dfres <- unexplored_dfres %>% arrange(desc(pred)) # re-arrange
      explored_rnk <- c(explored_rnk, unexplored_dfres[1:1,"rnk"]) # accumulate explored_rnk (탐색할 것 만큼 누적)
      top1_seq <- c(top1_seq,top1_pred)
      
      # redundant check (validation check for training)
      print(df_res_pred %>% filter(pred == max(pred)) %>% tally())
      if(df_res_pred %>% filter(pred == max(pred)) %>% tally() %>% as.vector == 1){
        top1_accuracy <- c(top1_accuracy,
                           max(c(top1_accuracy, 
                                 df_res_pred[which.max(df_res_pred[,"pred"]), "accuracy"]))
                           )
      }else if (max(top1_accuracy) != 0 ){ #중복 있으면 추론 실패로 zero
        top1_accuracy <- c(top1_accuracy, max(top1_accuracy))
      }else{
        top1_accuracy <- c(top1_accuracy, 0)
      }
      print("top1_accuracy vector")
      print(top1_accuracy)
      
      unexplored_dfres <- unexplored_dfres[1:1,]
      
      # Create train data for online learning w/o pre-model.
      online_train_data <- rbind.data.frame(explored_dfres,unexplored_dfres)
      #print("online train data dim:")
      #print(dim(online_train_data))
      
      print("explored_rnk:")
      print(explored_rnk)
      
      prev_pred_accuracy_sum <- pred_accuracy_sum
      
      online_xgb_models = xgboost(data = online_train_data %>% select(-pred, -rnk, -accuracy, -model) %>% data.matrix, 
                                  label = online_train_data[,"accuracy"],
                                  nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
                                  objective = 'reg:squarederror',verbose = F,
                                  params = list("eta"=grid[which.min(grid_search_hot$test_rmse_last),1],
                                                "gamma"=grid[which.min(grid_search_hot$test_rmse_last),2]))
                                  #xgb_model=online_xgb_models)
    }
    #browser()
    final_exlored_sequence <- c(final_exlored_sequence,list(explored_rnk))
    online_top1_seq <- c(online_top1_seq,list(top1_seq))
    online_top1_accuracy <- c(online_top1_accuracy,list(top1_accuracy))
  }
}
save(online_top1_accuracy,file="./online_top1_accuracy.Rdata")
save(online_top1_accuracy_simple,file="./online_top1_accuracy_simple.Rdata")



# Feasibility Test (upper-bound, if only the data from same model is used)
for(item in model_names){
  print(item)
  reg_test_loso <- rank_all_group %>% filter(model==item) %>%select(-c("accuracy","rnk","model")) %>% data.matrix
  reg_te_label_loso <- rank_all_group %>% filter(model==item) %>% select("accuracy","rnk")
  
  
  online_xgb_models = xgboost(data = reg_test_loso, label = reg_te_label_loso[,"accuracy"],
                              nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
                              objective = 'reg:squarederror',verbose = F,
                              params = list("eta"=grid[which.min(grid_search_hot$test_rmse_last),1],
                                            "gamma"=grid[which.min(grid_search_hot$test_rmse_last),2]))
  
  pred <- predict(online_xgb_models, reg_test_loso) #모델을 바꿔주면 됨
  res_m_alldata <- cbind.data.frame(pred, reg_te_label_loso, reg_test_loso)
  res_m_alldata <- res_m_alldata %>% arrange(desc(pred))
  print(res_m_alldata[1:5,c("rnk")])
  print(res_m_alldata[1:5,c("accuracy")])
  print(res_m_alldata[1:5,c("pred")])
  #print(which.max(res_m_alldata[,"pred"]))
  #print(which.min(res_m_alldata[,"rnk"]))
}  

# transfer + online learning ----------------------------------------------
transfer_online_final_seq <- list()
transfer_online_top1_seq <- list() 
transfer_online_top1_accuracy <- list()
prev_pred_accuracy_sum <- -1

for(item in model_names){
  print(item)
  reg_test_loso <- rank_all_group %>% filter(model==item) #%>%select(-c("accuracy","rnk","model")) %>% data.matrix
  reg_train_data <- rank_all_group %>% filter(model!=item)
  online_xgb_models <- readRDS(paste0(item,".rds"))
  
  explored_rnk <- c()
  top1_seq <- c()
  top1_accuracy <- c()
  
  print(0)
  pred <- predict(online_xgb_models, reg_test_loso %>% select(-c("accuracy","rnk","model")) %>% data.matrix)  
  df_res_pred <- cbind.data.frame(pred,reg_test_loso)
  # Top-1 and Print Results
  top1_pred <- which.max(df_res_pred[,"pred"])
  top1_real <- which.max(df_res_pred[,"accuracy"])
  df_res_pred <- df_res_pred %>% arrange(desc(pred)) # along with prediction, rearrange pred to make training data.
  print(sprintf("Top1 pred: %d, Real: %d",top1_pred,top1_real))
  print(df_res_pred[1:5,c("rnk")])
  print(df_res_pred[1:5,c("accuracy")])
  print(df_res_pred[1:5,c("pred")])
  #pred_accuracy_sum <- df_res_pred[1:1, c("pred")]
  top1_seq <- c(top1_seq,top1_pred)
  
  if (length(top1_accuracy) == 0){
    top1_accuracy <- df_res_pred[1,"accuracy"]
  }else if(df_res_pred[1,"accuracy"] > max(top1_accuracy)){
    top1_accuracy <-c(top1_accuracy,df_res_pred[1,"accuracy"])
  }else{
    top1_accuracy <- c(top1_accuracy,max(top1_accuracy))
  }
  #browser()
  # Prepare online learning dataset
  explored_dfres <- df_res_pred %>% filter(rnk %in% explored_rnk) # 탐색한것만 data frame 생성 (model 대신 data 사용)
  unexplored_dfres <- df_res_pred %>% filter(!rnk %in% explored_rnk) # 탐색한것 뺴고 data frame 생성
  unexplored_dfres <- unexplored_dfres %>% arrange(desc(pred)) # re-arrange
  explored_rnk <- c(explored_rnk, unexplored_dfres[1:1,"rnk"]) # accumulate explored_rnk (탐색할 것 만큼 누적)
  unexplored_dfres <- unexplored_dfres[1:1,]
  
  # Create train data for online learning w/o pre-model
  online_train_data <- rbind.data.frame(reg_train_data,explored_dfres[,2:28],unexplored_dfres[,2:28]) # three data is combined.
  print("online train data dim:")
  print(dim(online_train_data))
  
  print("explored_rnk:")
  print(explored_rnk)
  
  prev_pred_accuracy_sum <- pred_accuracy_sum
  
  for (i in seq(1,length(reg_test_loso[,"accuracy"]),1)){
    print(i)
    
    online_xgb_models = xgboost(data = online_train_data %>% select(-rnk, -accuracy, -model) %>% data.matrix, 
                                label = online_train_data[,"accuracy"],
                                nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
                                objective = 'reg:squarederror',verbose = F,
                                params = list("eta"=grid[which.min(grid_search_hot$test_rmse_last),1],
                                              "gamma"=grid[which.min(grid_search_hot$test_rmse_last),2]))
    
    #browser()
    pred <- predict(online_xgb_models, reg_test_loso %>% select(-c("accuracy","rnk","model")) %>% data.matrix)  
    df_res_pred <- cbind.data.frame(pred,reg_test_loso)
    # top 1 and print results
    top1_pred <- which.max(df_res_pred[,"pred"])
    top1_real <- which.max(df_res_pred[,"accuracy"])
    df_res_pred <- df_res_pred %>% arrange(desc(pred)) # along with prediction, rearrange pred to make training data.
    print(sprintf("Top1 pred: %d, Real: %d",top1_pred,top1_real))
    print(df_res_pred[1:sample,c("rnk")])
    print(df_res_pred[1:sample,c("accuracy")])
    print(df_res_pred[1:sample,c("pred")])
    pred_accuracy_sum <- df_res_pred[1:1,c("pred")]
    top1_seq <- c(top1_seq,top1_pred)
    
    if (length(top1_accuracy) == 0){
      top1_accuracy <- df_res_pred[1,"accuracy"]
    }else if(df_res_pred[1,"accuracy"] > max(top1_accuracy)){
      top1_accuracy <-c(top1_accuracy,df_res_pred[1,"accuracy"])
    }else{
      top1_accuracy <- c(top1_accuracy,max(top1_accuracy))
    }

    #print(sprintf("sum of accuracy: %f",pred_accuracy_sum))
    
    #if(pred_accuracy_sum == prev_pred_accuracy_sum){
    #  break
    #}
    
    # prepare online learning dataset
    explored_dfres <- df_res_pred %>% filter(rnk %in% explored_rnk) # 탐색한것만 data frame 생성 (model 대신 data 사용)
    unexplored_dfres <- df_res_pred %>% filter(!rnk %in% explored_rnk) # 탐색한것 뺴고 data frame 생성
    unexplored_dfres <- unexplored_dfres %>% arrange(desc(pred)) # re-arrange
    explored_rnk <- c(explored_rnk, unexplored_dfres[1:1,"rnk"]) # accumulate explored_rnk (탐색할 것 만큼 누적)
    unexplored_dfres <- unexplored_dfres[1:1,] # unexplored data중에서 top1만 pick 

    # Create train data for online learning w/o pre-model.
    # three data is combined by row.
    online_train_data <- rbind.data.frame(reg_train_data,explored_dfres[,2:28],unexplored_dfres[,2:28]) 
    print("online train data dim:")
    dim(online_train_data)
    
    print("explored_rnk:")
    print(explored_rnk)
    
    prev_pred_accuracy_sum <- pred_accuracy_sum
  }
  saveRDS(online_xgb_models, paste0(item,"_transfer_online_.rds"))
  
  transfer_online_final_seq <- c(transfer_online_final_seq,list(explored_rnk))
  transfer_online_top1_seq <- c(transfer_online_top1_seq,list(top1_seq))
  transfer_online_top1_accuracy <- c(transfer_online_top1_accuracy,list(top1_accuracy))
}
save(transfer_online_final_seq,file="transfer_online_final_seq.Rdata")
save(transfer_online_top1_seq,file="transfer_online_top1_seq.Rdata")
save(transfer_online_top1_accuracy,file="transfer_online_top1_accuracy.Rdata")


# Verify
# 저장은 맞을 수 있나.
# 1 <- 이미
# 48개의 데이터 
# 총 49번 해야 맞는것이다. 1개를 빼먹은 데이터 이므로, 저것과 일치 하지 않는다. 등 
#test <- readRDS(paste0(item,".rds"))
#pred <- predict(online_xgb_models, reg_test_loso %>% 
#                  select(-c("accuracy","rnk","model")) %>% 
#                  data.matrix)  



# LOSO, batch learning and test unkown data from a new model. (under-bound, same bound are in transfer and online)
for(item in model_names){
  print(item)
  reg_test_loso <- rank_all_group %>% filter(model==item) %>%select(-c("accuracy","rnk","model")) %>% data.matrix
  reg_te_label_loso <- rank_all_group %>% filter(model==item) %>% select("accuracy","rnk")
  model_on <- readRDS(paste0(item,".rds"))
  pred <- predict(model_on, reg_test_loso)
  
  res_m_alldata <- cbind.data.frame(pred, reg_te_label_loso, reg_test_loso)
  res_m_alldata <- res_m_alldata %>% arrange(desc(pred))
  print(res_m_alldata[1:5,c("rnk")])
  print(res_m_alldata[1:5,c("accuracy")])
  print(res_m_alldata[1:5,c("pred")])
  #print(which.max(cbind.data.frame(pred,reg_te_label_loso,reg_test_loso)[,"pred"]))
  #print(which.max(cbind.data.frame(pred,reg_te_label_loso,reg_test_loso)[,"accuracy"]))
}

arrange(cbind.data.frame(pred,reg_te_label_loso,reg_test_loso),desc(pred))

#batch learning  - > compute error (mean absolute percentage error)
abs((reg_te_label_loso[,1] - pred) / reg_te_label_loso[,1])  %>% mean
# with online learning data, compute error  (mean absolute percentage error)
pred <- predict(online_xgb_models, reg_test_loso)  
abs((reg_te_label_loso[,1] - pred) / reg_te_label_loso[,1])  %>% mean



# Feasibility Test (upper-bound, it mimics when online learning is done), transfer + online learning
for(item in model_names){
  print(item)
  reg_test_loso <- rank_all_group %>% filter(model==item) %>%select(-c("accuracy","rnk","model")) %>% data.matrix
  reg_te_label_loso <- rank_all_group %>% filter(model==item) %>% select("accuracy","rnk")
  
  pred <- predict(model_best_hot, reg_test_loso) #모델을 바꿔주면 됨
  res_m_alldata <- cbind.data.frame(pred, reg_te_label_loso, reg_test_loso)
  res_m_alldata <- res_m_alldata %>% arrange(desc(pred))
  print(res_m_alldata[1:5,c("rnk")])
  print(res_m_alldata[1:5,c("accuracy")])
  print(res_m_alldata[1:5,c("pred")])
  #print(which.max(res_m_alldata[,"pred"]))
  #print(which.min(res_m_alldata[,"rnk"]))
}  


# grid search
quant_grid_search = expand.grid(schema = c("asymmetric","symmetric","symmetric_power","symmetric_unint"),
                                clipping = c("max","KL"),
                                granularity = c("tensor","channel"),
                                profile = c(1,1000,10000))
                                #precision = c("int8","mixed"))
grid_top1_accuracy <- list()
for(item in model_names){
  print(item)
  top1_accuracy <- c()
  for (i in seq(1,dim(quant_grid_search)[1],1)){
    print(i)
    acc <- df_mfull %>% filter(model==item) %>% 
      filter(schema==as.character(quant_grid_search[i,"schema"]) & 
               clipping==as.character(quant_grid_search[i,"clipping"]) & 
               granularity==as.character(quant_grid_search[i,"granularity"]) & 
               profile==as.character(quant_grid_search[i,"profile"]) & 
               precision=="8") %>%
      select(accuracy)
    top1_accuracy <- c(top1_accuracy,acc[,1])
    top1_accuracy[i] <- max(top1_accuracy[1:i])
  }
  # mixed precision search 
  if(dim(quant_grid_search)[1] < df_mfull %>% filter(model==item) %>% tally() %>% as.vector() )
  {
    #browser()
    mixed_acc <- df_mfull %>% filter(model==item & 
      precision=="mixed") %>% group_by(schema,clipping,granularity,profile) %>% data.frame
    mixed_acc <- mixed_acc[,"accuracy"]  
    for(i in seq(1,length(mixed_acc),1)){
      print(i)
      top1_accuracy <- c(top1_accuracy,max(c(top1_accuracy,mixed_acc[1:i]))) # Max in current mixed version
    }
  }
  grid_top1_accuracy <- c(grid_top1_accuracy,list(top1_accuracy))
}
save(grid_top1_accuracy,file="grid_top1_accuracy.Rdata")



# random search
random_top1_accuracy <- list()
for(item in model_names){
  print(item)
  # not use one-hot encoding data frame.
  reg_test_loso <- df_mfull %>% filter(model==item) #%>%select(-c("accuracy","rnk","model")) %>% data.matrix
  set.seed(1103)
  top1_accuracy <- sample(reg_test_loso[,"accuracy"])
  print("original accuracy sample")
  print(top1_accuracy)
  for (i in seq(2,length(top1_accuracy),1)){
    print(i)
    top1_accuracy[i] <- max(top1_accuracy[1:i])
  }
  print("final accuracy of random search:")
  print(top1_accuracy)
  random_top1_accuracy <- c(random_top1_accuracy,list(top1_accuracy))
}
save(random_top1_accuracy,file="random_top1_accuracy.Rdata")

# Random Search on ResNet50
random_top1_accuracy_resnet50 <-c()
# not use one-hot encoding data frame.
reg_test_loso <- df_mfull %>% filter(model=="resnet50") #%>%select(-c("accuracy","rnk","model")) %>% data.matrix
set.seed(3)
top1_accuracy <- sample(reg_test_loso[,"accuracy"])
print("original accuracy sample")
print(top1_accuracy)
for (i in seq(2,length(top1_accuracy),1)){
  print(i)
  top1_accuracy[i] <- max(top1_accuracy[1:i])
}
print("final accuracy of random search:")
print(table(top1_accuracy))
random_top1_accuracy_resnet50 <- top1_accuracy
random_top1_accuracy[[6]] <- random_top1_accuracy_resnet50
save(random_top1_accuracy,file="random_top1_accuracy.Rdata")


# for graph, extract data from transfer and online learning.
graph_xgb_trials <- data.frame()
for(index in seq(1,6,1)){
  item <- model_names[index]
  graph_xgb_trials <- rbind.data.frame(graph_xgb_trials,
                                       cbind.data.frame("model"=item,"tuner"="XGBoost: transfer & online",
                                       "accuracy"=transfer_online_top1_accuracy[[index]]))
  graph_xgb_trials <- rbind.data.frame(graph_xgb_trials,
                                       cbind.data.frame("model"=item,"tuner"="XGBoost: online",
                                                        "accuracy"=online_top1_accuracy[[index]]))
  graph_xgb_trials <- rbind.data.frame(graph_xgb_trials,
                                       cbind.data.frame("model"=item,"tuner"="XGBoost: individual model",
                                                        "accuracy"=online_top1_accuracy_simple[[index]]))
  graph_xgb_trials <- rbind.data.frame(graph_xgb_trials,
                                      cbind.data.frame("model"=item,"tuner"="Random Search",
                                                       "accuracy"=random_top1_accuracy[[index]]))
  graph_xgb_trials <- rbind.data.frame(graph_xgb_trials,
                                       cbind.data.frame("model"=item,"tuner"="Grid Search",
                                                        "accuracy"=grid_top1_accuracy[[index]]))
  graph_xgb_trials <- rbind.data.frame(graph_xgb_trials,
                                       cbind.data.frame("model"=item,"tuner"="FP32",
                                                        "accuracy"=rep(FP32[index,"accuracy"],
                                                                       length(transfer_online_top1_accuracy[[index]]))))
}
graph_xgb_trials <- graph_xgb_trials %>% group_by(model,tuner) %>% mutate(trials=row_number()) %>% data.frame()



graph_xgb_trials_w_genetic<- data.frame()
for(index in seq(1,6,1)){
  item <- model_names[index]
  graph_xgb_trials_w_genetic <- rbind.data.frame(graph_xgb_trials_w_genetic,
                                       cbind.data.frame("model"=item,"tuner"="XGBoost: transfer & online",
                                                        "accuracy"=transfer_online_top1_accuracy[[index]]))
  graph_xgb_trials_w_genetic <- rbind.data.frame(graph_xgb_trials_w_genetic,
                                       cbind.data.frame("model"=item,"tuner"="XGBoost: online",
                                                        "accuracy"=online_top1_accuracy[[index]]))
  graph_xgb_trials_w_genetic <- rbind.data.frame(graph_xgb_trials_w_genetic,
                                       cbind.data.frame("model"=item,"tuner"="XGBoost: individual model",
                                                        "accuracy"=online_top1_accuracy_simple[[index]]))
  graph_xgb_trials_w_genetic <- rbind.data.frame(graph_xgb_trials_w_genetic,
                                       cbind.data.frame("model"=item,"tuner"="Random Search",
                                                        "accuracy"=random_top1_accuracy[[index]]))
  graph_xgb_trials_w_genetic <- rbind.data.frame(graph_xgb_trials_w_genetic,
                                       cbind.data.frame("model"=item,"tuner"="Grid Search",
                                                        "accuracy"=grid_top1_accuracy[[index]]))
  graph_xgb_trials_w_genetic <- rbind.data.frame(graph_xgb_trials_w_genetic,
                                                 cbind.data.frame("model"=item,"tuner"="Genetic",
                                                                  "accuracy"=genetic_top1_accuracy[[index]]))
  graph_xgb_trials_w_genetic <- rbind.data.frame(graph_xgb_trials_w_genetic,
                                       cbind.data.frame("model"=item,"tuner"="FP32",
                                                        "accuracy"=rep(FP32[index,"accuracy"],
                                                                       length(transfer_online_top1_accuracy[[index]]))))
}
graph_xgb_trials_w_genetic <- graph_xgb_trials_w_genetic %>% 
  group_by(model,tuner) %>% 
  mutate(trials=row_number()) %>% 
  data.frame()



# Finally Draw Plotting (# of trails and Accuracy)
#일단 그래프 부터 그리고 필요한 데이터 구조를 생각 해야할듯 
mydf = data.frame(
  xgboost = c(seq(48,68,2),rep(68,10)),
  random = seq(48,68,1),
  trial = seq(1,21,1)
)
library(reshape2)
mydf_m = melt(mydf,.id=trail)
library(tidyr)
mydf_m = mydf %>% gather(model,accuracy,-trial)

mytheme_wo_dashed <- theme_bw() +
  theme(panel.border = element_rect(colour="black",size=1)) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  #theme(panel.grid.major = element_line(colour="#9a9a9a",size=.3, linetype="dashed")) +
  theme(axis.text.x = element_text(size=rel(1.5))) +
  theme(axis.title.x = element_text(size=rel(1.5))) +
  theme(axis.title.y = element_text(size=rel(1.5))) +
  theme(axis.text.y = element_text(size=rel(1.5))) +
  theme(axis.text = element_text(colour = "black"))


# 통합 그림 - 4개 조건
levels(graph_xgb_trials[,"model"]) <- c("MN","SHN","SQN","GN","RN18","RN50")
levels(graph_xgb_trials[,"tuner"]) <- c("XGB-T","XGB","XGB-simple","Random","Grid","FP32")

save(graph_xgb_trials,file="graph_xgb_trials.Rdata")

graph_xgb_trials %>% filter(tuner != "XGB-simple" & tuner != "FP32") %>%
ggplot(aes(x=trials, y=accuracy, group=tuner, colour=tuner, linetype = tuner)) +
geom_step(size=0.7) +
mytheme_wo_dashed +
theme(legend.title = element_blank(),
      legend.position = c(0.94, 0.1),
      legend.text = element_text(size=9),
      legend.key.size = unit(0.5, 'cm'),
      legend.direction = 'vertical',
      legend.background = element_rect(colour = "black", 
                                       size=0.2, 
                                       linetype="solid")) + 
ylab("Top1 Accuracy(%)") + xlab("# of Trials") +
facet_wrap(.~model, scales = "free") 

# 통합 그림 - 6개의 조건 (not used)
graph_xgb_trials %>%
  ggplot(aes(x=trials, y=accuracy, group=tuner, colour=tuner)) +
  geom_step(size=1) + mytheme +
  theme(legend.title = element_blank(), 
        legend.position="top",
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid")) + 
  ylab("Top1 Accuracy(%)") + xlab("# of Trials") +
  facet_wrap(.~model, scales = "free") 

# 통합 그림 - 5개 (including genetic algorithm (revision FGCS))
levels(graph_xgb_trials_w_genetic[,"model"]) <- c("MN","SHN","SQN","GN","RN18","RN50")
levels(graph_xgb_trials_w_genetic[,"tuner"]) <- c("XGB-T","XGB","XGB-simple","Random","Grid","Genetic","FP32")

save(graph_xgb_trials_w_genetic,file="graph_xgb_trials_w_genetic.Rdata")

graph_xgb_trials_w_genetic %>% filter(tuner != "XGB-simple" & tuner != "FP32") %>%
#graph_xgb_trials_w_genetic %>% filter(tuner == "XGB-T" | tuner == "Genetic") %>%
  ggplot(aes(x=trials, y=accuracy, group=tuner, colour=tuner, linetype = tuner)) +
  geom_step(size=0.7) +
  mytheme_wo_dashed +
  theme(legend.title = element_blank(),
        legend.position = c(0.94, 0.1),
        legend.text = element_text(size=9),
        legend.key.size = unit(0.5, 'cm'),
        legend.direction = 'vertical',
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid")) + 
  ylab("Top1 Accuracy(%)") + xlab("# of Trials") +
  facet_wrap(.~model, scales = "free") 


# Define a Function (각각의 모델 생성을 위한)
trial_plot_func <- function(model_name, zoom_begin, zoom_finish){
  p <- graph_xgb_trials %>% filter(model==model_name) %>%  
    ggplot(aes(x=trials, y=accuracy, group=tuner, colour=tuner)) +
    geom_step(size=1) +
    #geom_hline(aes(yintercept=FP32), colour="#BB0000", linetype="dashed") +
    mytheme +
    theme(legend.title = element_blank(), 
          legend.position="top",
          legend.background = element_rect(colour = "black", 
                                           size=0.2, 
                                           linetype="solid")) +
    ylab("Top1 Accuracy(%)") + xlab("# of Trials") +
    facet_zoom(ylim = c(zoom_begin, zoom_finish), zoom.size = 1, show.area = FALSE )  
  p
}

# idea, debugging 요소 49것 해결 
# 절대적으론 너무 값이 뭐가 잘 안보인다.
# relative spped up처럼, 
# 상대적인 error를 보이는게 어떠한가?
# 그래야 깔끔하게 보일듯 

trial_plot_func("MobileNet", 69, 72)
trial_plot_func("ShuffleNet",59.9, 64)
trial_plot_func("SqueezeNet",52,53.9)
trial_plot_func("googlenet_slim_v4",69.5, 70.6)
trial_plot_func("resnet18",65,71)
trial_plot_func("resnet50",75, 76.1)



# backup ------------------------------------------------------------------
graph_xgb_trials %>% filter(model=="MobileNet") %>%  
  ggplot(aes(x=trials, y=accuracy, group=tuner, colour=tuner)) +
  geom_step(size=1) +
  geom_hline(aes(yintercept=71.81), colour="#BB0000", linetype="dashed") +
  mytheme +
  theme(legend.title = element_blank(), 
        legend.position="top",
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid")) +
  #  coord_cartesian(ylim=c(65,73)) + # real adjust
  #  scale_y_continuous(breaks= seq(65,73, by=4)) +
  ylab("Top1 Accuracy(%)") + xlab("# of Trials") +
  facet_zoom(ylim = c(65, 72), zoom.size = 0.5, show.area = FALSE )
  


graph_xgb_trials %>% filter(model=="ShuffleNet") %>%  
  ggplot(aes(x=trials, y=accuracy, group=tuner, colour=tuner)) +
  geom_line() + mytheme +
  theme(legend.position="top") +
  coord_cartesian(ylim=c(50,65)) + # real adjust
  scale_y_continuous(breaks= seq(50,65, by=3)) +
  geom_hline(aes(yintercept=63.96), colour="#BB0000", linetype="dashed") +
  ylab("Top1 Accuracy(%)") + xlab("# of Trials") 


graph_xgb_trials %>% filter(model=="SqueezeNet") %>%  
  ggplot(aes(x=trials, y=accuracy, group=tuner, colour=tuner)) +
  geom_line() + mytheme +
  theme(legend.position="top") +
  geom_hline(aes(yintercept=53.8), colour="#BB0000", linetype="dashed") +
  ylab("Top1 Accuracy(%)") + xlab("# of Trials")


graph_xgb_trials %>% filter(model=="googlenet_slim_v4") %>%  
  ggplot(aes(x=trials, y=accuracy, group=tuner, colour=tuner)) +
  geom_line() + mytheme +
  theme(legend.position="top") +
  geom_hline(aes(yintercept=70.39), colour="#BB0000", linetype="dashed") +
  ylab("Top1 Accuracy(%)") + xlab("# of Trials")

graph_xgb_trials %>% filter(model=="resnet18") %>%  
  ggplot(aes(x=trials, y=accuracy, group=tuner, colour=tuner)) +
  geom_line() + mytheme +
  theme(legend.position="top") +
  geom_hline(aes(yintercept=70.67), colour="#BB0000", linetype="dashed") +
  ylab("Top1 Accuracy(%)") + xlab("Trials")

graph_xgb_trials %>% filter(model=="resnet50") %>%  
  ggplot(aes(x=trials, y=accuracy, group=tuner, colour=tuner)) +
  geom_line() + mytheme +
  theme(legend.position="top") +
  geom_hline(aes(yintercept=76.08), colour="#BB0000", linetype="dashed") +
  ylab("Top1 Accuracy(%)") + xlab("Trials")

