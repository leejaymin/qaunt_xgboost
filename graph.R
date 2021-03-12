mytheme <- theme_bw() +
  theme(panel.border = element_rect(colour="black",size=1)) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_line(colour="#9a9a9a",size=.3, linetype="dashed")) +
  theme(axis.text.x = element_text(size=rel(1.5))) +
  theme(axis.title.x = element_text(size=rel(1.5))) +
  theme(axis.title.y = element_text(size=rel(1.5))) +
  theme(axis.text.y = element_text(size=rel(1.5))) +
  theme(axis.text = element_text(colour = "black"))

load("./rank_all.Rdata")


# Resnet18 (70.67%)
df_full_0102 %>% filter(model=="resnet18") %>% filter(precision!="FP32" & backend!="VTAInterpreter") %>% 
  ggplot(aes(x=clipping, y=accuracy, fill = schema))+
  geom_bar(stat="identity",position="dodge", colour="black")+
  facet_grid(granularity~profile) +
  coord_cartesian(ylim=c(0,75)) + # real adjust
  scale_y_continuous(breaks= seq(0,75, by=10)) +
  geom_text(size=3,aes(label=accuracy,  y = accuracy-0.5),color = "black",position = position_dodge(width=0.89),vjust=1,hjust=0.5 ) +
  geom_text(x=1, y=73, aes(label="70.67%")) +
  geom_hline(aes(yintercept=70.67), colour="#BB0000", linetype="dashed") +
  mytheme + 
  theme(legend.title = element_blank(), 
        legend.position="top",
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid"),
        axis.title.x=element_blank()) +  
  ylab("Top1 Accuracy(%)") 

# MobileNet (71.81 %)
df_full_0102 %>% filter(model=="MobileNet") %>% filter(precision!="FP32") %>% 
  ggplot(aes(x=clipping, y=accuracy, fill = schema))+
  geom_bar(stat="identity",position="dodge", colour="black")+
  facet_grid(granularity~profile) +
  coord_cartesian(ylim=c(0,75)) + # real adjust
  scale_y_continuous(breaks= seq(0,75, by=10)) +
  geom_text(size=3,aes(label=accuracy,  y = accuracy-0.5),color = "black",position = position_dodge(width=0.89),vjust=1.6,hjust=0.5 ) +
  geom_text(x=1, y=73, aes(label="71.81%")) +
  geom_hline(aes(yintercept=71.81), colour="#BB0000", linetype="dashed") +
  mytheme + 
  theme(legend.title = element_blank(), 
        legend.position="top",
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid"),
        axis.title.x=element_blank()) +  
  ylab("Top1 Accuracy(%)")
  

# ResNet50 (76.08 %)
df_full_0102 %>% filter(model=="resnet50") %>% filter(precision!="FP32") %>% 
  ggplot(aes(x=clipping, y=accuracy, fill = schema))+
  geom_bar(stat="identity",position="dodge", colour="black")+
  facet_grid(granularity~profile) +
  coord_cartesian(ylim=c(0,89)) + # real adjust
  scale_y_continuous(breaks= seq(0,89, by=10)) +
  geom_text(size=3,aes(label=accuracy,  y = accuracy-1),color = "black",position = position_dodge(width=0.89),vjust=1.6,hjust=0.5 ) +
  geom_text(x=1, y=78, aes(label="76.08%")) +
  geom_hline(aes(yintercept=76.08), colour="#BB0000", linetype="dashed") +
  mytheme + 
  theme(legend.title = element_blank(), 
        legend.position="top",
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid"),
        axis.title.x=element_blank()) +  
  ylab("Top1 Accuracy(%)")


# SqueezeNet (53.8 %)
df_full_1216 %>% filter(model=="SqueezeNet") %>% filter(precision!="FP32") %>% 
  ggplot(aes(x=clipping, y=accuracy, fill = schema))+
  geom_bar(stat="identity",position="dodge", colour="black")+
  facet_grid(granularity~profile) +
  coord_cartesian(ylim=c(0,60)) + # real adjust
  scale_y_continuous(breaks= seq(0,60, by=10)) +
  geom_text(size=3,aes(label=accuracy,  y = accuracy-1),color = "black",position = position_dodge(width=0.89),vjust=1.6,hjust=0.5 ) +
  geom_text(x=1, y=56, aes(label="53.8%")) +
  geom_hline(aes(yintercept=53.8), colour="#BB0000", linetype="dashed") +
  mytheme + 
  theme(legend.title = element_blank(), 
        legend.position="top",
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid"),
        axis.title.x=element_blank()) +  
  ylab("Top1 Accuracy(%)")


# ShuffleNet (63.96)
df_full_1216 %>% filter(model=="ShuffleNet") %>% filter(precision!="FP32" & precision!="mixed") %>% 
  ggplot(aes(x=clipping, y=accuracy, fill = schema))+
  geom_bar(stat="identity",position="dodge", colour="black")+
  facet_grid(granularity~profile) +
  coord_cartesian(ylim=c(0,79)) + # real adjust
  scale_y_continuous(breaks= seq(0,70, by=10)) +
  geom_text(size=3,aes(label=accuracy,  y = accuracy-1),color = "black",position = position_dodge(width=0.89), vjust=1.6, hjust=0.5) +
  geom_text(x=1, y=66, aes(label="63.96%")) +
  geom_hline(aes(yintercept=63.96), colour="#BB0000", linetype="dashed") +
  mytheme + 
  theme(legend.title = element_blank(), 
        legend.position="top",
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid"),
        axis.title.x=element_blank()) +  
  ylab("Top1 Accuracy(%)")
# mixed precision, size 10.4in x 5.4in 
df_full_0302 %>% filter(model=="ShuffleNet") %>% filter(precision!="FP32" & precision=="mixed") %>%
  ggplot(aes(x=clipping, y=accuracy, fill = schema))+
  geom_bar(stat="identity",position="dodge", colour="black")+
  facet_grid(granularity~profile) +
  coord_cartesian(ylim=c(0,79)) + # real adjust
  scale_y_continuous(breaks= seq(0,70, by=10)) +
  geom_text(size=3,aes(label=accuracy,  y = accuracy-1),color = "black",position = position_dodge(width=0.89), vjust=1.6, hjust=0.5) +
  geom_text(x=1, y=66, aes(label="63.96%")) +
  geom_hline(aes(yintercept=63.96), colour="#BB0000", linetype="dashed") +
  mytheme + 
  theme(legend.title = element_blank(), 
        legend.position="top",
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid"),
        axis.title.x=element_blank()) +  
  ylab("Top1 Accuracy(%)")

# googlenet_slim_v4 (70.39)
df_full_0102 %>% filter(model=="googlenet_slim_v4") %>% filter(precision!="FP32" & precision!="mixed") %>% 
  ggplot(aes(x=clipping, y=accuracy, fill = schema))+
  geom_bar(stat="identity",position="dodge", colour="black")+
  facet_grid(granularity~profile) +
  coord_cartesian(ylim=c(0,80)) + # real adjust
  scale_y_continuous(breaks= seq(0,80, by=10)) +
  geom_text(size=3,aes(label=accuracy,  y = accuracy-1),color = "black",position = position_dodge(width=0.89), vjust=1.6, hjust=0.5) +
  geom_text(x=1, y=75, aes(label="70.39%")) +
  geom_hline(aes(yintercept=70.39), colour="#BB0000", linetype="dashed") +
  mytheme + 
  theme(legend.title = element_blank(), 
        legend.position="top",
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid"),
        axis.title.x=element_blank()) +  
  ylab("Top1 Accuracy(%)")
# mixed precision
df_full_0302 %>% filter(model=="googlenet_slim_v4") %>% filter(precision!="FP32" & precision=="mixed") %>% 
  ggplot(aes(x=clipping, y=accuracy, fill = schema))+
  geom_bar(stat="identity",position="dodge", colour="black")+
  facet_grid(granularity~profile) +
  coord_cartesian(ylim=c(0,80)) + # real adjust
  scale_y_continuous(breaks= seq(0,80, by=10)) +
  geom_text(size=3,aes(label=accuracy,  y = accuracy-1),color = "black",position = position_dodge(width=0.89), vjust=1.6, hjust=0.5) +
  geom_text(x=1, y=75, aes(label="70.39%")) +
  geom_hline(aes(yintercept=70.39), colour="#BB0000", linetype="dashed") +
  mytheme + 
  theme(legend.title = element_blank(), 
        legend.position="top",
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid"),
        axis.title.x=element_blank()) +  
  ylab("Top1 Accuracy(%)")



# The plot for all models. The plot is for the project slides.
# Combine best models and TensorRT results 
# filter를 이용하면 rnk1이 항상 FP32가 아닌 문제 발생. 따라서 직접 입력 
g_best_trt <- data.frame(model=c("MobileNet","MobileNet","MobileNet",
                                 "ShuffleNet","ShuffleNet","ShuffleNet",
                                 "SqueezeNet","SqueezeNet","SqueezeNet",
                                 "GoogleNet-slim-v4","GoogleNet-slim-v4","GoogleNet-slim-v4",
                                 "ResNet18","ResNet18","ResNet18",
                                 "ResNet50","ResNet50","ResNet50"),
                         type=c("FP32","NEST-C(i8)","TensorRT-7.2.2(i8)",
                                "FP32","NEST-C(i8)","TensorRT-7.2.2(i8)",
                                "FP32","NEST-C(i8)","TensorRT-7.2.2(i8)",
                                "FP32","NEST-C(i8)","TensorRT-7.2.2(i8)",
                                "FP32","NEST-C(i8)","TensorRT-7.2.2(i8)",
                                "FP32","NEST-C(i8)","TensorRT-7.2.2(i8)"),
                         accuracy=c(71.81,71.23,NaN,
                                    63.96,63.41,64.77,
                                    53.80,53.15,53.65,
                                    70.39,70.58,69.99,
                                    70.67,70.25,70.44,
                                    76.08,76.01,76.03))
g_best_trt %>%  ggplot(aes(x=model, y=accuracy, fill = type)) +
  geom_bar(stat="identity",position="dodge", colour="black") +
  #facet_grid(granularity~profile) +d
  coord_cartesian(ylim=c(52,77)) + # real adjust
  scale_y_continuous(breaks= seq(52,77, by=3)) +
  geom_text(size=3,aes(label=accuracy,  y = accuracy-0.5),color = "black",position = position_dodge(width=0.89), vjust=1.6, hjust=0.5) +
  #geom_text(x=1, y=75, aes(label="70.39%")) +
  #geom_hline(aes(yintercept=70.39), colour="#BB0000", linetype="dashed") +
  #ggtitle('Googlenet-v4-slim: FP32 Top1 accuracy: 70.39',
  #        subtitle = "Author : Jemin") +
  ylab("Top1 Accuracy(%)") +
  mytheme + 
  theme(legend.title = element_blank(), 
        legend.position="top",
        axis.title.x=element_blank(),
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid")) 


# vta result --------------------------------------------------------------
load("./res_vta.Rdata")
# Top1: 70.67%

res_vta %>% filter(precision!="FP32") %>% ggplot(aes(x=clipping, y=accuracy, fill = Fusion))+
  geom_bar(stat="identity",position="dodge", colour="black")+
  facet_wrap(.~profile) +
  coord_cartesian(ylim=c(20,80)) + # real adjust
  scale_y_continuous(breaks= seq(20,80, by=20)) +
  geom_text(aes(label=accuracy,  y = accuracy-1),color = "black",position = position_dodge(width=0.89),vjust=1.6,hjust=0.5 ) +
  geom_text(x=1.3, y=72, aes(label="FP32 Acc: 70.67%")) +
  geom_text(x=1, y=39, aes(label="VTA Acc: 37%")) +
  geom_hline(aes(yintercept=70.67), colour="#BB0000", linetype="dashed") +
  geom_hline(aes(yintercept=37), colour="Blue", linetype="dashed") +
  #ggtitle('ResNet18: FP32 Top1 accuracy 70.67%',
  #        subtitle = "Author : Jemin") +
  ylab("Top1 Accuracy(%)") +
  mytheme + 
  theme(#legend.title = element_blank(), 
        legend.position="top",
        #axis.title.x=element_blank(),
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid")) 
 

# latency -----------------------------------------------------------------
load("./df_latency.Rdata")
# ARM-a53
df_latency_a53 <- df_latency %>% filter(model!="MobileNetv2_onnx" & 
                                      target == "a53") %>% data.frame
df_latency_a53 <- droplevels(df_latency_a53)
levels(df_latency_a53[,"model"]) <-c ("googlenet_slim_v4","mobilenet","shufflenet","squeezenet","resnet18","resnet50")
vec_level <- levels(df_latency_a53y$schema)
df_latency_a53$schema <- factor(df_latency_a53$schema, levels=c(vec_level[2],vec_level[1],vec_level[3],vec_level[5],vec_level[4]))

df_latency_a53 %>%
  #ggplot(aes(x=schema, y=speedup, fill = schema))+
  #geom_bar(stat="identity", colour="black") +
  ggplot(aes(x=model, y=speedup, fill = schema))+
  geom_bar(stat="identity",position="dodge", colour="black") +
#facet_wrap(.~model, scales = "free") +
  mytheme +
  geom_text(aes(label=round(speedup,2),  y = speedup-.01), size =3 ,color = "black",position = position_dodge(width=0.89),vjust=1.6,hjust=0.5 ) +
  theme(axis.title.x=element_blank(),
        legend.title = element_blank(), 
        legend.position="top",
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid")) + 
  ylab("Normalized Performance")  

# x86 i7-8700
df_latency_i78700 <- df_latency %>% filter(model!="MobileNetv2_onnx" & 
                                             target == "i7-8700") %>% data.frame
df_latency_i78700 <- df_latency_i78700 %>% group_by(model) %>% mutate(up = latency.ms.[1]/latency.ms.) %>% 
  select(model,tool,target,precision,schema,latency.ms.,up) %>% data.frame()
levels(df_latency_i78700[,"model"]) <-c ("googlenet_slim_v4","mobilenet","shufflenet","squeezenet","resnet18","resnet50")
df_latency_i78700 <- droplevels(df_latency_i78700)
vec_level <- levels(df_latency_i78700$schema)
df_latency_i78700$schema <- factor(df_latency_i78700$schema, levels=c(vec_level[2],vec_level[1],vec_level[3],vec_level[5],vec_level[4]))

df_latency_i78700 %>%
  #ggplot(aes(x=schema, y=speedup, fill = schema))+
  #geom_bar(stat="identity", colour="black") +
  ggplot(aes(x=model, y=up, fill = schema))+
  geom_bar(stat="identity",position="dodge", colour="black") +
  #facet_wrap(.~model, scales = "free") +
  mytheme +
  geom_text(aes(label=round(up,2),  y = up-.01),size=3, color = "black",position = position_dodge(width=0.89),vjust=1.6,hjust=0.5 ) +
  theme(axis.title.x=element_blank(),
        legend.title = element_blank(), 
        legend.position="top",
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid")) + 
  ylab("Normalized Performance")


# 2080ti GPU
df_latency_2080ti <- df_latency %>% filter(model!="MobileNetv2_onnx" & 
                                          target == "2080ti") %>% data.frame

df_latency_2080ti <- df_latency_2080ti %>% group_by(model) %>% mutate(up = latency.ms.[1]/latency.ms.) %>% 
  select(model,tool,target,precision,schema,latency.ms.,up) %>% data.frame()

df_latency_2080ti <- droplevels(df_latency_2080ti)
levels(df_latency_2080ti[,"model"]) <-c ("googlenet_slim_v4","mobilenet","shufflenet","squeezenet","resnet18","resnet50")
vec_level <- levels(df_latency_2080ti$schema)
df_latency_2080ti$schema <- factor(df_latency_2080ti$schema, levels=c(vec_level[2],vec_level[1],vec_level[3],vec_level[5],vec_level[4],vec_level[6]))
df_latency_2080ti <- df_latency_2080ti %>%  mutate(type = paste(tool,precision,schema))  # for all tools, nees to generate unique labels
  
# nestc 
df_latency_2080ti %>% filter(schema != "unknown") %>%
  #ggplot(aes(x=schema, y=speedup, fill = schema))+
  #geom_bar(stat="identity", colour="black") +
  ggplot(aes(x=model, y=up, fill = schema))+
  geom_bar(stat="identity",position="dodge", colour="black") +
  #facet_wrap(.~model, scales = "free") +
  mytheme +
  geom_text(aes(label=round(up,2),  y = up-.01),color = "black",position = position_dodge(width=0.89),vjust=1.6,hjust=0.5 ) +
  theme(axis.title.x=element_blank(),
        legend.title = element_blank(), 
        legend.position="top",
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid")) + 
  ylab("Normalized Performance")  
  
## ALL, trt, onnxruntime, nestc
df_latency_2080ti %>% 
  #ggplot(aes(x=schema, y=speedup, fill = schema))+
  #geom_bar(stat="identity", colour="black") +
  ggplot(aes(x=model, y=up, fill = type))+
  geom_bar(stat="identity",position="dodge", colour="black") +
  #facet_wrap(precision~tool, scales = "free") +
  mytheme +
  geom_text(aes(label=round(up,1),  y = up-.01),size=3, color = "black",position = position_dodge(width=0.89),vjust=1.6,hjust=0.5 ) +
  theme(axis.title.x=element_blank(),
        legend.title = element_blank(), 
        legend.position="top",
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid")) + 
  ylab("Normalized Performance")
