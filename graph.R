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
df_full_1216 %>% filter(model=="resnet18") %>% filter(precision!="FP32" & backend!="VTAInterpreter") %>% 
  ggplot(aes(x=clipping, y=accuracy, fill = schema))+
  geom_bar(stat="identity",position="dodge", colour="black")+
  facet_grid(granularity~profile) +
  coord_cartesian(ylim=c(0,75)) + # real adjust
  scale_y_continuous(breaks= seq(0,75, by=10)) +
  geom_text(size=3,aes(label=accuracy,  y = accuracy-0.5),color = "black",position = position_dodge(width=0.89),vjust=1,hjust=0.5 ) +
  geom_text(x=1, y=73, aes(label="70.67%")) +
  geom_hline(aes(yintercept=70.67), colour="#BB0000", linetype="dashed") +
  mytheme + 
  theme(legend.position="top") +
  ylab("Top1 Accuracy(%)") + xlab("Clipping")

# MobileNet (71.81 %)
df_full_1216 %>% filter(model=="MobileNet") %>% filter(precision!="FP32") %>% 
  ggplot(aes(x=clipping, y=accuracy, fill = schema))+
  geom_bar(stat="identity",position="dodge", colour="black")+
  facet_grid(granularity~profile) +
  coord_cartesian(ylim=c(0,75)) + # real adjust
  scale_y_continuous(breaks= seq(0,75, by=10)) +
  geom_text(size=3,aes(label=accuracy,  y = accuracy-1),color = "black",position = position_dodge(width=0.89),vjust=1.6,hjust=0.5 ) +
  geom_text(x=1, y=73, aes(label="71.81%")) +
  geom_hline(aes(yintercept=71.81), colour="#BB0000", linetype="dashed") +
  mytheme + 
  theme(legend.position="top") +
  ylab("Top1 Accuracy(%)") + xlab("Clipping")
  

# ResNet50 (76.08 %)
df_full_1216 %>% filter(model=="resnet50") %>% filter(precision!="FP32") %>% 
  ggplot(aes(x=clipping, y=accuracy, fill = schema))+
  geom_bar(stat="identity",position="dodge", colour="black")+
  facet_grid(granularity~profile) +
  coord_cartesian(ylim=c(0,89)) + # real adjust
  scale_y_continuous(breaks= seq(0,89, by=10)) +
  geom_text(size=3,aes(label=accuracy,  y = accuracy-1),color = "black",position = position_dodge(width=0.89),vjust=1.6,hjust=0.5 ) +
  geom_text(x=1, y=78, aes(label="76.08%")) +
  geom_hline(aes(yintercept=76.08), colour="#BB0000", linetype="dashed") +
  mytheme + 
  theme(legend.position="top") +
  ylab("Top1 Accuracy(%)") + xlab("Clipping")


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
  theme(legend.position="top") +
  ylab("Top1 Accuracy(%)") + xlab("Clipping")


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
  theme(legend.position="top") +
  ylab("Top1 Accuracy(%)") + xlab("Clipping")

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
  theme(legend.position="top") +
  ylab("Top1 Accuracy(%)") + xlab("Clipping")


# googlenet_slim_v4 (70.39)
df_full_1216 %>% filter(model=="googlenet_slim_v4") %>% filter(precision!="FP32" & precision!="mixed") %>% 
  ggplot(aes(x=clipping, y=accuracy, fill = schema))+
  geom_bar(stat="identity",position="dodge", colour="black")+
  facet_grid(granularity~profile) +
  coord_cartesian(ylim=c(0,80)) + # real adjust
  scale_y_continuous(breaks= seq(0,80, by=10)) +
  geom_text(size=3,aes(label=accuracy,  y = accuracy-1),color = "black",position = position_dodge(width=0.89), vjust=1.6, hjust=0.5) +
  geom_text(x=1, y=75, aes(label="70.39%")) +
  geom_hline(aes(yintercept=70.39), colour="#BB0000", linetype="dashed") +
  mytheme + 
  theme(legend.position="top") +
  ylab("Top1 Accuracy(%)") + xlab("Clipping")

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
  theme(legend.position="top") +
  ylab("Top1 Accuracy(%)") + xlab("Clipping")


# The plot for all models. The plot is for the project slides.
rank_graph <- df_full_1201 %>% group_by(model) %>% 
  arrange(desc(accuracy)) %>% mutate(rnk=row_number()) %>% data.frame

rank_graph <- arrange(rank_graph, group_by = model) # 핵심: group으로 재정렬  

rank_graph_best <- rank_graph %>% filter(rnk==1 | rnk ==2)
rank_graph_best[,"rnk"] <- as.factor(rank_graph_best[,"rnk"])
levels(rank_graph_best[,"rnk"]) <- c("FP32","INT8")

rank_graph_best %>%  ggplot(aes(x=model, y=accuracy, fill = rnk)) +
  geom_bar(stat="identity",position="dodge", colour="black") +
  #facet_grid(granularity~profile) +d
  #coord_cartesian(ylim=c(0,80)) + # real adjust
  #scale_y_continuous(breaks= seq(0,80, by=10)) +
  geom_text(size=3,aes(label=accuracy,  y = accuracy-1),color = "black",position = position_dodge(width=0.89), vjust=1.6, hjust=0.5) +
  #geom_text(x=1, y=75, aes(label="70.39%")) +
  #geom_hline(aes(yintercept=70.39), colour="#BB0000", linetype="dashed") +
  #ggtitle('Googlenet-v4-slim: FP32 Top1 accuracy: 70.39',
  #        subtitle = "Author : Jemin") +
  mytheme




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
 


