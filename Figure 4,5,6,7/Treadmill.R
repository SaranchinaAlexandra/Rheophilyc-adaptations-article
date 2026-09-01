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

# read data
BookEve <- read.xlsx(xlsxFile = "Book1.xlsx", sheet = "Eve")
BookEvi <- read.xlsx(xlsxFile = "Book1.xlsx", sheet = "Evi")

# join Eve and Evi
BookEveEvi <- merge(BookEve, BookEvi, all=TRUE)

# order
BookEveEvi$Species <- factor(BookEveEvi$Species, levels = c("EviW", "EveS", "EveW"))

# Longest run segment plot
pLRS <- ggplot(BookEveEvi, aes(x=Location, y=Longest_run_segment, col=Group,  fill=Environment)) + 
  geom_boxplot(show.legend = TRUE, alpha = 0.6) + 
  ylab("Longest run segment, s") +
  facet_grid(~Species, scales="free_x", space="free") + 
  scale_colour_manual(name = "Group", limits = c("treadmill"), values=c("#cd5550ff")) + 
  scale_fill_manual(name = "Enviroment", limits = c("river", "lake"),
                    values=c("#ffcc66", "#6699cc")) + 
  theme(legend.position = "bottom", 
        strip.background = element_rect(fill="white"), 
        strip.text = element_text(size = 12)) +
  geom_pwc(method = "wilcox_test", label="{p.adj.signif}", 
           hide.ns = TRUE, label.size = 6, vjust = 0.5)
#variable of longest run segnebt
pLRS


pTRT <- ggplot(BookEveEvi, aes(x=Location, y=Total_run_time, col=Group,  fill=Environment)) + 
  geom_boxplot(show.legend = TRUE, alpha = 0.6) + 
  # name y lab
  ylab("Total run time, s") + 
  facet_grid(~Species, scales="free_x", space="free") + 
  scale_colour_manual(name = "Group", limits = c("treadmill"), values=c("#cd5550ff")) + 
  scale_fill_manual(name = "Enviroment", limits = c("river", "lake"),
                    values=c("#ffcc66", "#6699cc")) + 
  theme(legend.position = "bottom", 
        strip.background = element_rect(fill="white"), 
        strip.text = element_text(size = 12)) + 
  geom_pwc(data = BookEveEvi[BookEveEvi$Species != "EveW", ],
           method = "wilcox_test", label="{p.adj.signif}", 
           hide.ns = TRUE, label.size = 6, vjust = 0.5)
pTRT
  
# splitting two plots
ggarrange(pLRS + xlab(""), pTRT, 
          nrow = 1, labels = c("A", "B"), 
          common.legend = TRUE, legend = "bottom")


ggsave("Treadmill_exp.png", device=png, 
       width=24, height=8, units="cm", 
       bg = "white", res=600)

BookEveS <- BookEveEvi[BookEveEvi$Species=="EveS", ]
BookEviW <- BookEveEvi[BookEveEvi$Species=="EviW", ]

wilcox.test(BookEveS$Longest_run_segment ~ BookEveS$Location)
wilcox.test(BookEviW$Longest_run_segment ~ BookEviW$Location)

wilcox.test(BookEveS$Total_run_time ~ BookEveS$Location)
wilcox.test(BookEviW$Total_run_time ~ BookEviW$Location)
