
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
lapply(df_mfull_group_acc_label[,c(2:6)],table) # check diversity of quantization options. 


# FP32
FP32 %>% select(model,accuracy)
