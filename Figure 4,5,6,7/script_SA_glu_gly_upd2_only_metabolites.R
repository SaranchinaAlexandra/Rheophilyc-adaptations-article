library(openxlsx) ## for reading excel files
library(scales) ## for pretty axes
library(ggplot2) ## for plotting
library(ggpubr) ## for advanced plotting
#library(devtools) 
#install.packages("devtools")
#devtools::install_github("thomasp85/patchwork")
library(patchwork) ## for combining plots
#install.packages("ggeasy")
library(ggeasy) ## for easy remove axis
library(rstatix) ## for wilcox test



BookEve <- read.xlsx(xlsxFile = "Book1.xlsx", sheet = "Eve")
BookEvi <- read.xlsx(xlsxFile = "Book1.xlsx", sheet = "Evi")
book_both <- merge(BookEve, BookEvi, all = TRUE)

#795a29ff
## settings for all plots
mytheme <- function(){
  list(scale_colour_manual(name = "Group", limits = c("field", "control", "treadmill"),
                      values=c("#795a29ff", "#24a61fff", "#cd5550ff")),
       scale_fill_manual(name = "Enviroment", limits = c("river", "lake"),
                              values=c("#ffcc66", "#6699cc")),
    expand_limits(y=0))
}

## function to plot one species and one metabolite
plot_field_and_exp <- function(book, metabolite="Lactate", units="PARROTS", remove_xlab=FALSE) {
  message(paste0("Working with ", unique(book$Species)))
  
  ## filter data for field and experiment
  Book_field <- book[book$Group=="field", ]
  Book_exp <- book[book$Group=="treadmill" | book$Group == "control", ]
  
  ## set y limits for metabolite
  maxy <- max(book[, metabolite], na.rm=TRUE) * 1.2 ## changed here, doesn't fit
  ## if we decide on common limits for all species
  #maxy <- max(book_both[, metabolite], na.rm=TRUE) * 1.2

  ## how many locations in this panel? Useful for statistics tests
  nloc <- length(unique(book$Location))

  ## plot with field values
  pField <- ggplot(Book_field, aes(x=Location, y=.data[[metabolite]], col=Group, fill=Environment)) + 
    geom_boxplot(show.legend = TRUE, alpha = 0.6) + 
    ## this is the dotted line we decided not to draw
    #annotate(geom = 'segment', y = -Inf, yend = Inf, 
    #         color = 'black', x = Inf, xend = Inf, 
    #         linetype = "dashed", linewidth = .5) + 
    ylab(paste0(metabolite, ", ", units)) +
    scale_y_continuous(labels = function(x) ifelse(x < 0, "", x), limits = c(-maxy/10, maxy)) +
    mytheme()
  
  ## calculate wilcox test if 3 locations and adjust
  if (nloc > 2) {
    my_formula <- as.formula(paste(metabolite, "~", "Location"))
    stat <- rstatix::wilcox_test(Book_field, my_formula, p.adjust.method = "holm")
    message(paste0("Printing stat comparison for ", metabolite, " field"))
    print(stat)
    pField <- pField + geom_pwc(method = "wilcox_test", label="{p.adj.signif}", 
             hide.ns = TRUE, label.size = 6, vjust = 0.5)
  }
  
  ## calculate wilcox test if exactly 2 locations
  if (nloc == 2) {
    my_formula <- as.formula(paste(metabolite, "~", "Location"))
    stat <- rstatix::wilcox_test(Book_field, my_formula, p.adjust.method = "holm")
    message(paste0("Printing stat comparison for ", metabolite, " field"))
    print(stat)
    pField <- pField + geom_pwc(method = "wilcox_test", label="{p.signif}", 
                                hide.ns = TRUE, label.size = 6, vjust = 0.5)
  }
  
  ## if only one location, don't calculate anything, pass
  
  ## now to laboratory exp data 
  ## base plot
  pExp <- ggplot(Book_exp, aes(x=Location, y=.data[[metabolite]], col=Group,  fill=Environment)) + 
    geom_boxplot(show.legend = TRUE, alpha = 0.6) + 
    mytheme() + 
    scale_y_continuous(labels = function(x) ifelse(x < 0, "", x), limits=c(-maxy/10, maxy)) + 
    #geom_pwc(method = "wilcox_test", label="{p.adj.signif}", 
    #         hide.ns = TRUE, label.size = 6, vjust = 0.5) + 
    ylab(paste0(metabolite, ", ", units)) +
    easy_remove_y_axis()
  pExp
  
  ## add statistics for experiment (control / treadmill) 
  my_formula <- as.formula(paste(metabolite, "~", "Group"))
    Book_exp %>%
    group_by(Location) %>%
    pairwise_wilcox_test(my_formula, p.adjust.method = "holm") %>% 
      add_xy_position(x = "Location") -> stat_exp
  ## now REALLY adjust p-values
  stat_exp$p.adj <- p.adjust(stat_exp$p.adj, method = "holm")
  stat_exp$p.adj.signif <- symnum(x = stat_exp$p.adj, 
                                  cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), 
                                  symbols = c("****", "***", "**", "*", "ns"))
  #### if 1 location only, p.adj == p
  ##if (nloc == 1) stat_exp$p.adj.signif <- stat_exp$p.signif
  message(paste0("Printing stat comparison for ", metabolite, " control/treadmill"))
  print(stat_exp)
  
  ## add statistics for experiment
  pExp <- pExp + stat_pvalue_manual(stat_exp, size = 6, hide.ns = TRUE)
  
  ## and compare controls
  Book_exp_controls <- subset(Book_exp, Group == "control")
  
  ## calculate comparisons across controls between locations
  if (nloc > 1){ 
    my_formula <- as.formula(paste(metabolite, "~", "Location"))
    stat <- rstatix::pairwise_wilcox_test(Book_exp_controls, my_formula, p.adjust.method = "holm")
    message(paste0("printing stat comparison for ", metabolite, " lab controls"))
    print(stat)
    stat <- add_xy_position(stat)
    stat$xmin <- stat$xmin - .2
    stat$xmax <- stat$xmax - .2
    stat$y.position <- stat$y.position * -.1
    pExp <- pExp + 
    stat_pvalue_manual(data = stat, tip.length = 0, color="#24a61fff", size = 6,
                       vjust = 1.2, hide.ns = TRUE) 
  }
  
  if (remove_xlab == TRUE) {
    pField <- pField + xlab("")
    pExp <- pExp + xlab("")
  }
  
  
  ## combine field and exp into one common plot
  pField + pExp  +
    plot_layout(guides = 'collect', widths = c(1, 2.2), axes = 'collect') + 
    plot_annotation(Book_field$Species, 
                    theme = theme(plot.title = element_text(hjust=0.5, vjust=0),
                                  legend.position = 'none')) + 
    theme(plot.margin = margin(0,0,0,0),
          legend.position = "none") 
}


combine_plots_3species <- function(metabolite, units) {
  p_eves <- plot_field_and_exp(BookEve[BookEve$Species == "EveS",], 
                               metabolite = metabolite, units=units)
  p_evew <- plot_field_and_exp(BookEve[BookEve$Species == "EveW",], 
                               metabolite = metabolite, units=units)
  p_evi <- plot_field_and_exp(BookEvi, metabolite = metabolite, units=units)
  
  p3 <- ggarrange(p_evi, p_eves, p_evew, widths=c(3,3,2), nrow=1, common.legend = TRUE, legend = "bottom")
  ggsave(paste0(metabolite, ".png"), p3, device=png, 
         width=24, height=8, units="cm", 
         bg = "white", res=600)
  print(p3)
}


combine_plots_2species <- function(metabolite, units, legend_position, remove_xlab=FALSE) {
  p_eves <- plot_field_and_exp(BookEve[BookEve$Species == "EveS",], 
                               metabolite = metabolite, units=units, remove_xlab)
  p_evew <- plot_field_and_exp(BookEve[BookEve$Species == "EveW",], 
                               metabolite = metabolite, units=units, remove_xlab)
    
  p2 <- ggarrange(p_eves, p_evew, widths=c(3,3,2), nrow=1, common.legend = TRUE, legend = legend_position)
  print(p2)
}

combine_plots_3species("Lactate", "μmol / g wet weight")
combine_plots_3species("Glucose", "μmol / g wet weight")
combine_plots_3species("Glycogen", "μmol / g wet weight")

Lac<-combine_plots_3species("Lactate", "μmol / g wet weight")
Glu<-combine_plots_3species("Glucose", "μmol / g wet weight")
Gly<-combine_plots_3species("Glycogen", "μmol / g wet weight")

ggarrange(Gly + xlab(""), Glu, Lac,
          nrow = 3, labels = c("A", "B", "C"), 
          common.legend = TRUE, legend = "bottom")

a<-combine_plots_2species("LDH_activity", "nKat/mg protein",legend_position = "bottom", remove_xlab=TRUE)
a
b<-combine_plots_2species("Protein", "mg / ml", legend_position = "bottom", remove_xlab=TRUE)
b

ggarrange(a + xlab(""), b, 
          nrow = 1, labels = c("A", "B"), 
          common.legend = TRUE, legend = "bottom")

ggsave("LDH_protein.png", device=png, 
       width=24, height=8, units="cm", 
       bg = "white", res=600)

