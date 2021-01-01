
# -------------- Regression Test ------------------

# incremental learning (online learing): initial point from other data and then build model only with current data.
# + check data to use unexplored them.
final_exlored_sequence <- list()
for (sample in (10:10)){
  print(sprintf("######### Smaple: %d ############",sample))
  prev_pred_accuracy_sum <- -1
  
  for(item in model_names){
    print(item)
    #browser()
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
      
      #if(pred_accuracy_sum == prev_pred_accuracy_sum){
      #  break
      #}
      
      # prepare online learning dataset
      #browser()
      df_res_pred <- df_res_pred %>% filter(!rnk %in% explored_rnk) # 탐색한것 뺴고 data frame 생성
      df_res_pred <- df_res_pred %>% arrange(desc(pred)) # re-arrange
      explored_rnk <- c(explored_rnk, df_res_pred[1:1,"rnk"]) # accumulate explored_rnk (탐색할 것 만큼 누적)
      print("explored_rnk")
      print(explored_rnk)
      
      prev_pred_accuracy_sum <- pred_accuracy_sum
      
      online_xgb_models = xgboost(data = df_res_pred[1:1,] %>% select(-pred, -rnk, -accuracy, -model) %>% data.matrix, 
                                  label = df_res_pred[1:1,"accuracy"],
                                  nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
                                  objective = 'reg:squarederror',verbose = F,
                                  params = list("eta"=grid[which.min(grid_search_hot$test_rmse_last),1],
                                                "gamma"=grid[which.min(grid_search_hot$test_rmse_last),2]),
                                  xgb_model=online_xgb_models)
    }
    final_exlored_sequence <- c(final_exlored_sequence,list(explored_rnk))
  }
}



# online learning and transfer learning
for (sample in (1:20)){
  print(sprintf("######### Smaple: %d ############",sample))
  prev_pred_accuracy_sum <- -1  
  for(item in model_names){
    print(item)
    # google lenet v4 implementation 
    reg_test_loso <- rank_all_group %>% filter(model==item) %>%select(-c("accuracy","rnk","model")) %>% data.matrix
    reg_te_label_loso <- rank_all_group %>% filter(model==item) %>% select("accuracy","rnk")
    
    online_xgb_models <- readRDS(paste0(item,".rds"))
    
    for (i in seq(1,length(reg_te_label_loso[,1]),1)){
      print(i)
      #browser()
      pred <- predict(online_xgb_models, reg_test_loso)  
      # top 1
      #top1_pred <- which.max(cbind.data.frame(pred,reg_te_label_loso,reg_test_loso)[,"pred"])
      #top1_real <- which.max(cbind.data.frame(pred,reg_te_label_loso,reg_test_loso)[,"accuracy"])
      
      # prepare online learning dataset
      online_tr <- cbind.data.frame(pred,reg_te_label_loso,reg_test_loso)
      online_tr <- online_tr %>% arrange(desc(pred)) # accroding to prediction, rearrange pred to make training data.
      #print info
      #print(sprintf("Top1 pred: %d, Real: %d",top1_pred,top1_real))
      
      pred_accuracy_sum <- sum(online_tr[1:sample,c("pred")])
      
      if(pred_accuracy_sum == prev_pred_accuracy_sum){
        print(online_tr[1:sample,c("rnk")])
        print(online_tr[1:sample,c("accuracy")])
        print(online_tr[1:sample,c("pred")])
        break
      }
      
      # prepare online learning dataset
      online_tr_label <- online_tr[1:sample,"accuracy"] # 예측 값의 5개 (top5)의 real rank
      online_tr <- online_tr[1:sample,] %>% select(-pred, -rnk, -accuracy) %>% data.matrix 
      
      
      prev_pred_accuracy_sum <- pred_accuracy_sum
      # outdated (pick one based on pred index for online learning)
      #online_tr <- data.frame(reg_test_loso) %>% filter(row_number(size) == top1_pred) %>% data.matrix
      #online_tr_label <- reg_te_label_loso[top1_pred,]
      
      online_xgb_models = xgboost(data = online_tr, label = online_tr_label,
                                  nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
                                  objective = 'reg:squarederror',verbose = F,
                                  params = list("eta"=grid[which.min(grid_search_hot$test_rmse_last),1],
                                                "gamma"=grid[which.min(grid_search_hot$test_rmse_last),2]),
                                  xgb_model=online_xgb_models)
    }
  }
}