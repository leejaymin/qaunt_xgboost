# FP32
FP32 %>% select(model,accuracy)

# ----------------- relative error calculation -----------------------
# one hot encoding
rank_all_group %>% filter(rnk<=10) %>% select(model, accuracy)

# human readable 
df_mfull_group_rank <- df_mfull %>% group_by(model) %>% 
  arrange(desc(accuracy)) %>% mutate(rnk=row_number()) %>% data.frame
df_mfull_group_rank <- arrange(df_mfull_group_rank, group_by = model) # 핵심: group으로 재정렬  
df_mfull_group_rank %>% filter(rnk<=5) %>% select(model, accuracy)


df_mfull_group_acc_label <- df_full_0102 %>% select(-relative_error,-relative_mixed,-elapsed_time,-Fusion,-backend)
df_mfull_group_acc_label <- na.omit(df_mfull_group_acc_label) # remove missing data on mixed googlev4

# FP32보다 더 큰값이 있으므로 googlenet은 case_when으로 처리함 
df_mfull_group_acc_label <- df_mfull_group_acc_label %>% group_by(model) %>% 
  mutate(re_error = case_when(model == "googlenet_slim_v4" ~ accuracy - 70.39,
                          TRUE ~ accuracy - max(accuracy)))
df_mfull_group_acc_label <- df_mfull_group_acc_label %>% filter(re_error >= -1.0) %>% data.frame


# ----------------- Entropy: all and individual model -----------------------
entropy_df_mfull <- df_mfull_group_acc_label %>% filter(precision!="FP32")
entropy_df_mfull <- droplevels(entropy_df_mfull)
# all
lapply(entropy_df_mfull[,c(2:6)],table) # check diversity of quantization options. 
# depnding on models 
entropy_df_mfull %>% 
  group_by(model,precision) %>% 
  select(model,precision) %>% tally()

###  Compute entropy 
#setosa_subset <- iris[iris$Species=="setosa",]
#compute Shannon entropy
entropy <- function(target) {
  freq <- table(target)/length(target)
  # vectorize
  vec <- as.data.frame(freq)[,2]
  #drop 0 to avoid NaN resulting from log2
  vec<-vec[vec>0]
  #compute entropy
  -sum(vec * log2(vec))
}
#entropy(setosa_subset$Species)
lapply(entropy_df_mfull[,c(2:6)],entropy) # column entropy across all models

# column entropy depending on models
entropy_df_mfull %>% group_by(model) %>%
  select(model,precision,profile,granularity,clipping,schema) %>% 
  mutate(rnk=row_number()) %>% 
  mutate_at(vars(precision,profile,granularity,clipping,schema), entropy) %>%
  filter(rnk==max(rnk))
    


