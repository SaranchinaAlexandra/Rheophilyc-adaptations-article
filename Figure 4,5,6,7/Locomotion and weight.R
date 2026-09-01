library(openxlsx)
library(scales)
library(ggplot2)
library(ggpubr)
library(devtools)
#install.packages("devtools")
#devtools::install_github("thomasp85/patchwork")
library(patchwork)
#devtools::install_github("")
library(ggeasy) ## for easy remove axis
#install.packages("ggeasy")
library(ggbeeswarm)

BookEve <- read.xlsx(xlsxFile = "Book1.xlsx", sheet = "Eve")
BookEvi <- read.xlsx(xlsxFile = "Book1.xlsx", sheet = "Evi")
Bookstill<- read.xlsx(xlsxFile = "Book1.xlsx", sheet = "Still_water")


BookEveEvi <- merge(BookEve, BookEvi, all=TRUE)
BookEveEvi$Species <- factor(BookEveEvi$Species, levels = c("EviW", "EveS", "EveW"))
Bookstill$Species <- factor(Bookstill$Species, levels = c("EviW", "EveS", "EveW"))


pstill <- ggplot(Bookstill, aes(x=Location, y=`cm/min`,  fill=Environment)) + 
  geom_boxplot(show.legend = TRUE, alpha = 0.6) + 
  geom_beeswarm(size=0.7) +
  ylab("Locomotion rate, cm/min") +
  facet_grid(~Species, scales="free_x", space="free") + 
  scale_fill_manual(name = "Enviroment", limits = c("river", "lake"),
                    values=c("#ffcc66", "#6699cc")) + 
  theme(legend.position = "bottom", 
        strip.background = element_rect(fill="white"), 
        strip.text = element_text(size = 12)) +
  geom_pwc(data = Bookstill[Bookstill$Species == "EveS", ],
           method = "wilcox_test", label="{p.adj.signif}", 
           hide.ns = TRUE, label.size = 6, vjust = 0.5) +
  geom_pwc(data = Bookstill[Bookstill$Species == "EviW", ],
           method = "wilcox_test", label="{p.adj.signif}", 
           hide.ns = TRUE, label.size = 6, vjust = 0.5)

pstill

pw <- ggplot(BookEveEvi, aes(x=Location, y=Weight,  fill=Environment)) + 
  geom_boxplot(show.legend = TRUE, alpha = 0.6) + 
  
  ylab("Raw weight, g") +
  facet_grid(~Species, scales="free_x", space="free") + 
  scale_fill_manual(name = "Enviroment", limits = c("river", "lake"),
                    values=c("#ffcc66", "#6699cc")) + 
  theme(legend.position = "bottom", 
        strip.background = element_rect(fill="white"), 
        strip.text = element_text(size = 12)) +
  geom_pwc(data = BookEveEvi[BookEveEvi$Species == "EveS", ],
           method = "wilcox_test", label="{p.adj.signif}", 
           hide.ns = TRUE, label.size = 6, vjust = 0.5) +
  geom_pwc(data = BookEveEvi[BookEveEvi$Species == "EviW", ],
           method = "wilcox_test", label="{p.adj.signif}", 
           hide.ns = TRUE, label.size = 6, vjust = 0.5)

pw


ggarrange(pstill, pw, 
          nrow = 1, labels = c("A", "B"), 
          common.legend = TRUE, legend = "bottom")

ggsave("Locomotion and weight.png", device=png, 
       width=24, height=8, units="cm", 
       bg = "white", res=600)
