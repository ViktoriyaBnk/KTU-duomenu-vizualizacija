setwd("D:/FAILAI/2023 rudens semestras/Programavimas duomenu tvarkymui ir vizualizavimui/2 laboras/KTU-duomenu-vizualizacija/laboratorinis/data")
rm(list = ls())
sodra <- read.csv("lab_sodra.csv")
eAC <- 494100

library(tidyverse)
DF <- sodra %>% filter(ecoActCode==eAC)
summary(DF)
hist(DF$avgWage,n=100)

topfive <- DF %>% arrange(desc(avgWage)) %>% group_by(name) %>%  slice(1) %>% ungroup() %>% arrange(desc(avgWage)) %>% top_n(avgWage,n=5) %>% select(name)
topfive <- as.vector(topfive)[[1]]
topfive
ND <- DF %>% filter(name %in% topfive) %>% group_by(name)

library(ggplot2)
ggplot(ND, aes(x=month, y=avgWage,group=name,color=name))+geom_line(size=0.7)+ theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

LF <- ND %>% group_by(name) %>% summarise(apdraustieji=sum(numInsured)) %>%
  ggplot(aes(reorder(name,-apdraustieji),apdraustieji,fill=reorder(name,-apdraustieji))) +
  geom_bar(stat = "identity", position = "dodge") + theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+xlab("name")+labs(fill = "name")
LF
