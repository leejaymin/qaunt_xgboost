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

#df_full_1107 <- read.xlsx("quant_1107.xlsx",sheetName="full")
#df_full_1201 <- read.xlsx("quant_1201.xlsx",sheetName="full")
df_full_1216 <- read.xlsx("quant_1216.xlsx",sheetName="full")


#res_vta <- df_full_1107 %>% filter(model=="resnet18")
res_vta <- df_full_1216 %>% filter(model=="resnet18" & backend=="VTAInterpreter")
save(res_vta,file="./res_vta.Rdata")


df_mf <- read.xlsx("quant_1216.xlsx",sheetName="m_f")
df_mfull <- merge(df_full_1216, df_mf, by="model")
#df_mfull <- df_mfull[]

# unrelated index
#df_mfull <- df_mfull[-c(1,34,83,116,182,188,184,186,191,193.195,196,197,198,201,203,205,210,238,239,228,229,230,231,232,233,227),]

# remove vta result
df_mfull <- df_mfull %>% filter(backend!="VTAInterpreter")

df_mfull <- df_mfull %>% select(-c("relative_error","relative_mixed","elapsed_time","backend","Fusion"))
# remove FP32 accuracy 
FP32 <- df_mfull %>% filter(precision == "FP32")
df_mfull <- df_mfull %>% filter(precision != "FP32")


df_mfull <- data.frame((df_mfull))
df_mfull <- na.omit(df_mfull) # remove missing data on mixed googlev4

# Split label and training data 
tr_df <- subset(df_mfull, select=-c(accuracy,model))
tr_y <- subset(df_mfull, select=accuracy)

# Categorical data to Factor
#x_num = iris %>% model.matrix(~0+Species,.) %>% as.data.frame %>%  #one-hot encoding matrix
#  bind_cols(iris %>% select(-Species,-Sepal.Width)) %>% data.matrix
#y_num = iris$Sepal.Width

df_mfull[,"profile"] <- as.factor(df_mfull[,"profile"])
df_mfull[,"schema"] <- as.factor(df_mfull[,"schema"])

#levels(df_mfull[,"schema"])<-c("0","asymmetric","symmetric","symmetric_power","symmetric_unint")

df_mfull[,"schema"] <- droplevels(df_mfull[,"schema"]) # remove unused factor

save(df_mfull,file="./df_mfull.Rdata")

# one hot encoding 
a_num = df_mfull %>% model.matrix(~0+precision,.) %>% as.data.frame %>%  #one-hot encoding matrix
    bind_cols(df_mfull %>% select(-precision))
a_num = a_num %>% model.matrix(~0+profile,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(a_num %>% select(-profile)) 
a_num = a_num %>% model.matrix(~0+granularity,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(a_num %>% select(-granularity)) 
a_num = a_num %>% model.matrix(~0+clipping,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(a_num %>% select(-clipping)) 

a_rank <- a_num %>% model.matrix(~0+schema,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(a_num %>% select(-schema))
a_num = a_num %>% model.matrix(~0+schema,.) %>% as.data.frame %>%  #one-hot encoding matrix
  bind_cols(a_num %>% select(-schema,-accuracy, -model)) 

save(a_num,file="a_num.Rdata")

# factor
#a <- tr_df %>% data.matrix
#b <- tr_y %>% data.matrix

# one-hot encode
a_hot <- a_num %>% data.matrix
b_hot <- tr_y %>% data.matrix

# group
# group based rank 
rank_all_group <- a_rank %>% group_by(model) %>% 
  arrange(desc(accuracy)) %>% mutate(rnk=row_number()) %>% data.frame
rank_all_group <- arrange(rank_all_group, group_by = model) # 핵심: group으로 재정렬  
rank_train_group <- rank_all_group %>% select(-c("model","accuracy","rnk")) %>% data.matrix
rank_label_group <- rank_all_group[,"rnk"]

# single group based rnak
rank_all <- a_rank %>% arrange(desc(accuracy)) %>% mutate(rnk=row_number())
rank_all %>% print(width=Inf, n=30) # 확인
rank_label <- rank_all[,"rnk"]
rank_train <- rank_all %>% select(-c("model","accuracy","rnk")) %>% data.matrix




## backup-code
dtrain <- xgb.DMatrix(data = a, label=b)
bstDNatrux <- xgboost(data = dtrain, max.depth =2, eta= 1, nthread=2, nrounds=200, objective = 'reg:squarederror')
predict(bstDNatrux, dtrain)
abs((b[,1] - predict(bstDNatrux, dtrain)) / b[,1])  %>% mean 



