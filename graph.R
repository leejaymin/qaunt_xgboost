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

df_full_1216 %>% filter(model=="ShuffleNet") %>% filter(precision!="FP32" & precision=="mixed") %>% 
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

df_full_1216 %>% filter(model=="googlenet_slim_v4") %>% filter(precision!="FP32" & precision=="mixed") %>% 
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
                                    63.96,63.41,NaN,
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
  coord_cartesian(ylim=c(20,75)) + # real adjust
  scale_y_continuous(breaks= seq(20,75, by=5)) +
  geom_text(aes(label=accuracy,  y = accuracy-1),color = "black",position = position_dodge(width=0.89),vjust=1.6,hjust=0.5 ) +
  geom_text(x=1, y=72, aes(label="70.67%")) +
  geom_text(x=1, y=39, aes(label="TVM 37%")) +
  geom_hline(aes(yintercept=70.67), colour="#BB0000", linetype="dashed") +
  geom_hline(aes(yintercept=37), colour="Blue", linetype="dashed") +
  ggtitle('ResNet18: FP32 Top1 accuracy 70.67%',
          subtitle = "Author : Jemin") +
  mytheme
 

# latency -----------------------------------------------------------------
df_latency <- df_latency %>% filter(model!="MobileNetv2_onnx" & 
                                      target != "x86 ") %>% data.frame
df_latency <- droplevels(df_latency)
levels(df_latency[,"model"]) <-c ("googlenet_slim_v4","mobilenet","shufflenet","squeezenet","resnet18","resnet50")

df_latency %>%
  #ggplot(aes(x=schema, y=speedup, fill = schema))+
  #geom_bar(stat="identity", colour="black") +
  ggplot(aes(x=model, y=speedup, fill = schema))+
  geom_bar(stat="identity",position="dodge", colour="black") +
#facet_wrap(.~model, scales = "free") +
  mytheme +
  theme(axis.title.x=element_blank(),
        legend.title = element_blank(), 
        legend.position="top",
        legend.background = element_rect(colour = "black", 
                                         size=0.2, 
                                         linetype="solid")) + 
  ylab("Normalized Performance")  
 
# 2080ti GPU
latency_gpu <- data.frame(model=c("resnet18","b"),
                          target=c("2080ti","2080ti"),
                          tool=c("nestc","nestc"),
                          precision=c("FP32","FP32"),
                          latency=c(100,200))
  
  
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
