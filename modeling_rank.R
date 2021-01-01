
# Rank model로 다시 수행
# group =  model로 한다. 
#col_name <- colnames(a_rank)
#col_name[1] = "model"
#col_name[17] = "linear_typeasymmetric"
#a_rank <- a_rank[,col_name]

# group: # of models
groups <- rank_all_group %>% group_by(model) %>% dplyr::summarise(cnt = n()) %>% pull(cnt)
# signle
#groups <- 194
groups <- 318

xgb_dm_rank_train <- xgb.DMatrix(data = rank_train_group, label = rank_label_group, group = groups)

# "rank:pairwise", "rank:ndcg" rank:map
xgb_predictor_ranking_model4 = xgboost(data = xgb_dm_rank_train,
   nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
   objective = "rank:pairwise",verbose = F,
   params = list("eta"=grid[which.min(grid_search_hot$test_rmse_last),1],
   "gamma"=grid[which.min(grid_search_hot$test_rmse_last),2]))

xgb_predictor_ranking_model5 = xgboost(data = xgb_dm_rank_train,
   nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
   objective = "rank:ndcg",verbose = F,
   params = list("eta"=grid[which.min(grid_search_hot$test_rmse_last),1],
   "gamma"=grid[which.min(grid_search_hot$test_rmse_last),2]))

xgb_predictor_ranking_model6 = xgboost(data = xgb_dm_rank_train,
   nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
   objective = "rank:map",verbose = F,
   params = list("eta"=grid[which.min(grid_search_hot$test_rmse_last),1],
   "gamma"=grid[which.min(grid_search_hot$test_rmse_last),2]))

# no group
predict(xgb_predictor_ranking_model, rank_train)  
predict(xgb_predictor_ranking_model2, rank_train)
predict(xgb_predictor_ranking_model3, rank_train)

# group
predict(xgb_predictor_ranking_model4, rank_train_group)
predict(xgb_predictor_ranking_model5, rank_train_group) 
predict(xgb_predictor_ranking_model6, rank_train_group) 
# 그룹이 1개면 뭔가 이상한듯?? -> 전부 상수 값으로 출력됨

                                     
# 변형 
importance_matrix <- xgb.importance(colnames(rank_train_group), model = xgb_predictor_ranking_model4)
# Use `xgb.plot.importance`, which create a _barplot_ or use `xgb.ggplot.importance`
library(Ckmeans.1d.dp) # for xgb.ggplot.importance
xgb.ggplot.importance(importance_matrix, top_n = 15, measure = "Gain")


# 6번의 leave one subject out cross validation LOSO, Testing, metric은 RMSE
model_names <- unique(rank_all_group[,"model"])
df_group < -cbind.data.frame(model_names,groups) # for xgb.DMatrix, need for groups accroding to each model. 

result_rnk_loso <- data.frame()
for(item in model_names){
  print(item)
  #browser()
  rnk_train_loso <- rank_all_group %>% filter(model!=item) %>%select(-c("accuracy","rnk","model")) %>% data.matrix
  rnk_tr_label_loso <- rank_all_group %>% filter(model!=item) %>% select("rnk") %>% data.matrix
  #rnk_test_loso <- rank_all_group %>% filter(model==item) %>%select(-c("accuracy","rnk","model")) %>% data.matrix
  #rnk_te_label_loso <- rank_all_group %>% filter(model==item) %>% select("rnk")
  
  curr_group_list <- df_group %>% filter(model_names != item)
  
  xgb_rank_train <- xgb.DMatrix(data = rnk_train_loso, label = rnk_tr_label_loso, group = curr_group_list[,"groups"])
  temporal_model = xgboost(data = xgb_rank_train,
                           nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
                           objective = "rank:pairwise", verbose = F,
                           params = list("eta"=grid[which.min(grid_search_hot$test_rmse_last),1],
                                         "gamma"=grid[which.min(grid_search_hot$test_rmse_last),2]))
  saveRDS(temporal_model, paste0(item,"_rnk.rds"))
}

# All with Batch Learning (w/o online learning)
for(item in model_names){
  print(item)
  rnk_test_loso <- rank_all_group %>% filter(model==item) %>%select(-c("accuracy","rnk","model")) %>% data.matrix
  rnk_te_label_loso <- rank_all_group %>% filter(model==item) %>% select("rnk")
  model_on <- readRDS(paste0(item,"_rnk.rds"))
  pred <- predict(model_on, rnk_test_loso)  
  print(which.max(cbind.data.frame(pred, rnk_te_label_loso, rnk_test_loso)[,"pred"]))
  print(which.min(cbind.data.frame(pred, rnk_te_label_loso, rnk_test_loso)[,"rnk"]))
}

# Feasibility Test (upper-bound online learning)
# Model with all data and test it with each data
for(item in model_names){
  print(item)
  rnk_test_loso <- rank_all_group %>% filter(model==item) %>%select(-c("accuracy","rnk","model")) %>% data.matrix
  rnk_te_label_loso <- rank_all_group %>% filter(model==item) %>% select("rnk")

  pred <- predict(xgb_predictor_ranking_model4, rnk_test_loso)  
  res_m_alldata <- cbind.data.frame(pred, rnk_te_label_loso, rnk_test_loso)
  res_m_alldata <- res_m_alldata %>% arrange(desc(pred))
  print(res_m_alldata[1:5,c("rnk")])
  print(res_m_alldata[1:5,c("pred")])
  #print(which.max(res_m_alldata[,"pred"]))
  #print(which.min(res_m_alldata[,"rnk"]))
}  



# online learning
# incremental learning (online learing)
for(item in model_names){
  print(item)
  # google lenet v4 implementation 
  rnk_test_loso <- rank_all_group %>% filter(model==item) %>%select(-c("accuracy","rnk","model")) %>% data.matrix
  rnk_te_label_loso <- rank_all_group %>% filter(model==item) %>% select("rnk")
  
  online_rnk_xgb <- readRDS(paste0(item,"_rnk.rds"))
  for (i in seq(1,length(rnk_te_label_loso[,1]),1)){
    print(i)
    #browser()
    pred <- predict(online_rnk_xgb, rnk_test_loso)  
    top1_pred <- which.max(cbind.data.frame(pred,rnk_te_label_loso,rnk_test_loso)[,"pred"])
    top1_real <- which.min(cbind.data.frame(pred,rnk_te_label_loso,rnk_test_loso)[,"rnk"])
    print(sprintf("Top1 pred: %d, Real: %d",top1_pred,top1_real))
    
    #if(top1_pred == top1_real){
    #  break
    #}
    online_tr <- cbind.data.frame(pred,rnk_te_label_loso,rnk_test_loso)
    online_tr <- online_tr %>% arrange(desc(pred)) # accroding to prediction, rearrange pred to make training data.
    online_tr_label <- online_tr[1:5,"rnk"]
    online_tr <- online_tr[1:5,] %>% select(-pred, -rnk) %>% data.matrix 
    #online_tr <- data.frame(rnk_test_loso) %>% filter(row_number(size) == top1_pred) %>% data.matrix
    #online_tr_label <- rnk_te_label_loso[top1_pred,]
    
    # group 정보 누락됨. group을 넣어야하나 정확도가 너무 낮으므로 시도하지 않음.
    # 왜냐면 전체 데이터로 모델링해서 테스트해도 정확도가 매우 낮다. 
    online_rnk_xgb = xgboost(data = online_tr, label = online_tr_label,
    nrounds = 200, early_stopping_rounds = 150,# about folds and rounds
    objective = "rank:pairwise",verbose = F,
    params = list("eta"=grid[which.min(grid_search_hot$test_rmse_last),1],
                  "gamma"=grid[which.min(grid_search_hot$test_rmse_last),2]),
    xgb_model=online_rnk_xgb)
  }
}


