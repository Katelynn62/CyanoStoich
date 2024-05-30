#### Publication Plots from R Markdown File. 
library(patchwork)

### Figure 1; MB Nutrients.

fig2_1 <- (MB_C_Fig + SA_C_Fig) + plot_layout(guides = "collect") + theme(legend.position = "right")

fig2_2 <- (MB_N_Fig + SA_N_Fig) + plot_layout(guides = "collect") + theme(legend.position = "right")

fig2_3 <- (MB_P_Fig + SA_P_Fig) + plot_layout(guides = "collect") + theme(legend.position = "right")

fig <- fig2_1 / fig2_2 / fig2_3

tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/Fig1New.tiff", width = 7, height = 7, units = "in", res = 600, bg = "white")

fig

dev.off()

#Figure 2; MB Weather and Water Physical Lake Data

fig1 <- ggarrange(MB_Temp, MB_DO, MB_SS, MB21Wind, MB_SF, ncol=1, nrow=5, align="hv")

fig1 <- annotate_figure(fig1, top = textGrob(paste("Missisquoi Bay"), hjust=1.1, gp=gpar(cex=0.8)))

fig2 <- ggarrange(SA_Temp, SA_DO, SA_SS, SA21Wind, SA_SF, ncol=1, nrow=5, align="hv")

fig2 <- annotate_figure(fig2, top = textGrob(paste("St. Albans Bay"), hjust=1.27, gp=gpar(cex=0.8)))




fig <- ggarrange(fig1, fig2, ncol=2, nrow=1, align="hv")

fig <- annotate_figure(fig, bottom=textGrob(paste("Date"), hjust=0, gp=gpar(cex=0.75)))

tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/Fig2New.tiff", width = 6, height = 8, units = "in", res = 600, bg = "white")

fig

dev.off()

##################################################################################
########################### Toxin Quota vs. PC ###################################
##################################################################################
Phytos_Toxin %>%
  filter(Station == "MB") %>%
  ggplot(aes(x=Date, y=Q, fill=Toxin)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=c("orangered3", "cadetblue4"))+
  theme_classic()

MB_ToxPC <- Toxins_PC %>%
  pivot_longer(cols=c(ATX_Q, MC_Q), values_to = "Q", names_to = "Toxin") %>%
  filter(Station == "MB") %>%
  filter(Date > as.Date("2021-06-01") & Date <= as.Date("2021-11-05")) %>%
  ggplot() + 
  annotate("rect", xmin = as.Date("2021-06-01"), xmax = as.Date("2021-07-26"), ymin = 0, ymax = .00045,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-26"), xmax = as.Date("2021-08-14"), ymin = 0, ymax =.00045,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-14"), xmax = as.Date("2021-09-08"), ymin = 0, ymax = .00045,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-08"), xmax = as.Date("2021-11-08"), ymin = 0, ymax = .00045,
           fill = "grey98") +
  geom_bar(aes(x = Date, y=Q, fill=Toxin), stat="identity", position="dodge", width=4, alpha=.4) +
  geom_line(aes(x = Date, y = PC_RFU/18000), size = 0.5, color="black", group = 1) + 
  scale_fill_manual(values=c("orangered3", "cadetblue4"))+
  scale_y_continuous(sec.axis = sec_axis(~.*18000, name = "")) +
  theme_classic() +
  ggtitle("A")+
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(hjust = 0, size=7), axis.title = element_text(size=7), axis.text=element_text(size=6),
        legend.title = element_text(size = 5), 
        legend.key.size = unit(0.07, 'in'), legend.text = element_text(size=3.8))


# SA

SA_ToxPC <- Toxins_PC %>%
  pivot_longer(cols=c(ATX_Q, MC_Q), values_to = "Q", names_to = "Toxin") %>%
  filter(Station == "SA") %>%
  filter(Date > as.Date("2021-06-01")& Date <= as.Date("2021-11-05")) %>%
  ggplot() + 
  annotate("rect", xmin = as.Date("2021-06-01"), xmax = as.Date("2021-07-08"), ymin = 0, ymax = .0002,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-08"), xmax = as.Date("2021-07-31"), ymin = 0, ymax =.0002,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-07-31"), xmax = as.Date("2021-10-04"), ymin = 0, ymax = .0002,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-10-04"), xmax = as.Date("2021-11-05"), ymin = 0, ymax = .0002,
           fill = "grey98") +
  geom_bar(aes(x = Date, y=Q, fill=Toxin), stat="identity", position="dodge", width=4, alpha=.4) +
  geom_line(aes(x = Date, y = PC_RFU/12000), size = 0.5, color="black", group = 1) + 
  scale_fill_manual(values=c("orangered3", "cadetblue4"))+
  scale_y_continuous(sec.axis = sec_axis(~.*12000, name = "")) +
  theme_classic() +
  ggtitle("B")+
  ylab("")+
  xlab("")+
  theme(plot.title = element_text(hjust = 0, size=7), axis.title = element_text(size=7), axis.text=element_text(size=6),
        legend.title = element_text(size = 5), 
        legend.key.size = unit(0.07, 'in'), legend.text = element_text(size=3.8))

# ggarrange code for publication


fig <- ggarrange(MB_ToxPC, SA_ToxPC, nrow=2, ncol=1, hjust=TRUE, common.legend=TRUE, labels = NULL)

tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/Fig2.1.tiff", width = 3.5, height = 4, units = "in", res = 600, bg = "white")

annotate_figure(fig, left = textGrob(expression(paste("Toxin Quota")), rot = 90, vjust = 1, gp = gpar(cex = 0.6)),
                bottom = textGrob("Date", hjust = .5, gp = gpar(cex = 0.6)),
                right = textGrob("Phycocyanin (RFU)", rot = 90, vjust = .25, gp=gpar(cex=0.6)))

dev.off()

library(factoextra)


################### Clustering ##################

## SA Cluster
SA_BloomPeriod <- fviz_dend(
  SA_PC_clust,
  k = 3,
  horiz = TRUE,
  rect = TRUE,
  rect_fill = TRUE,
  rect_border = "jco",
  k_colors = "jco",
  main = "C",
  cex = 0.35,
  ylab = "") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size=8), axis.title = element_text(size=8), axis.text=element_text(size=8),axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

## MB Cluster
MB_BloomPeriod <- fviz_dend(
  MB_PC_clust,
  k = 4,
  horiz = TRUE,
  rect = TRUE,
  rect_fill = TRUE,
  rect_border = "jco",
  k_colors = "jco",
  main = "A",
  cex = 0.35,
  ylab = "") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size=8), axis.title = element_text(size=8), axis.text=element_text(size=8),axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


fig1 <- ggarrange(MB_BloomPeriod, SA_BloomPeriod, ncol=1, nrow=2, align="hv")

fig1 <- annotate_figure(fig1, bottom = textGrob("Bray-Curtis Dissimilarity", hjust = .5, gp = gpar(cex = 0.6)),
                        left= textGrob("Hierarchical Clusters", rot = 90, vjust = 1, gp = gpar(cex = 0.6)))


MB_Cyano <- Phytos %>%
  filter(Station == "MB") %>%
  filter(Genus %in% MB_Phyto_Keep$Genus) %>%
  filter(Taxonomic_group == "Cyanobacteria") %>%
  group_by(Date) %>%
  summarise(BM = sum(BM_ug.L)) %>%
  unique() %>%
  filter(Date > as.Date("2021-06-01")) %>%
  mutate(Group = if_else(Date == as.Date("2021-08-10") | Date == as.Date("2021-08-19") | Date == as.Date("2021-08-25") | Date == as.Date("2021-09-17") | Date == as.Date("2021-09-03") | Date == as.Date("2021-09-10"), 1, 0),
         Group = if_else(Date == as.Date("2021-06-29") | Date == as.Date("2021-08-03") | Date == as.Date("2021-10-11"), 2, Group)) %>%
  ggplot(aes(x=as.Date(Date), y=BM)) +
  geom_line() + 
  geom_point(aes(color=as.character(Group))) + 
  scale_color_manual(values=c("black", "#0073C2FF", "#EFC000FF")) +
  theme_classic() +
  ylab("") +
  xlab("") +
  ggtitle("B") +
  xlim(as.Date("2021-06-01"), as.Date("2021-10-31")) +
  theme(plot.title = element_text(hjust = 0, size=8), axis.title = element_text(size=8), axis.text=element_text(size=8),
        legend.position="none")



SA_Cyano <- Phytos %>%
  filter(Station == "SA") %>%
  filter(Genus %in% SA_Phyto_Keep$Genus) %>%
  filter(Taxonomic_group == "Cyanobacteria") %>%
  group_by(Date) %>%
  summarise(BM = sum(BM_ug.L)) %>%
  unique() %>%
  filter(Date > as.Date("2021-06-01")) %>%
  mutate(Group = if_else(Date == as.Date("2021-08-13") | Date == as.Date("2021-07-27"), 1, 0),
         Group = if_else(Date == as.Date("2021-08-19") | Date == as.Date("2021-08-03") | Date == as.Date("2021-09-03") | Date == as.Date("2021-08-26") | Date == as.Date("2021-09-17"), 2, Group),
         Group = if_else(Date == as.Date("2021-07-22") | Date == as.Date("2021-07-13") | Date == as.Date("2021-10-11"), 3, Group)) %>%
  ggplot(aes(x=as.Date(Date), y=BM)) +
  geom_line() + 
  geom_point(aes(color=as.character(Group))) + 
  scale_color_manual(values=c("black", "#0073C2FF", "#EFC000FF",  "#868686FF")) +
  theme_classic() +   
  ylab("") +
  xlab("") +
  ggtitle("D") +
  xlim(as.Date("2021-06-01"), as.Date("2021-10-31")) +
  theme(plot.title = element_text(hjust = 0, size=8), axis.title = element_text(size=8), axis.text=element_text(size=8),
        legend.position="none")
fig2 <- ggarrange(MB_Cyano, SA_Cyano, ncol=1, nrow=2, align="hv")

fig2 <- annotate_figure(fig2, bottom = textGrob("Date", hjust = .5, gp = gpar(cex = 0.6)),
                        left=textGrob(expression(paste("Cyanobacteria Biomass (",~ mu, "g/L)")), rot = 90, vjust = 1, gp = gpar(cex = 0.6)))


p1_MB<- fviz_nbclust(as.matrix(MB_PC_Euc), FUN = hcut, method = "wss",  k.max = 10) +
  ggtitle("B") +
  theme(plot.title = element_text(hjust = 0, size=8), axis.title = element_text(size=8), axis.text=element_text(size=8),
        legend.position="none")

p1_SA <- fviz_nbclust(as.matrix(SA_PC_Euc), FUN = hcut, method = "wss",  k.max = 10) +
  ggtitle("D") +
  theme(plot.title = element_text(hjust = 0, size=8), axis.title = element_text(size=8), axis.text=element_text(size=8),
        legend.position="none")


fig3 <- ggarrange(p1_MB, p1_SA, ncol=1, nrow=2, align="hv")

fig <- ggarrange(fig1, fig3, nrow=1, ncol=2, align="hv")
tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/Fig_Clusters.tiff", width = 8, height = 8, units = "in", res = 600, bg = "white")
fig
dev.off()



################################################################################
#Dissolved Nutrient Figures with seston in 6 panel plot. 

# Surface and Bottom: 


DN <- ggarrange(DN_DIN, DN_PO4, DN_DIN.TP, ncol=3, nrow=1, common.legend = TRUE, align="hv")


Ses <- ggarrange(MC_CN, MC_CP, MC_NP, ncol=3, nrow=1, common.legend = TRUE, align="hv")

Fig <- ggarrange(Ses, DN, align="hv", nrow=2, ncol=1)

Fig <- annotate_figure(Fig, bottom=textGrob(paste("Date"), gp=gpar(cex=0.7), hjust=-.45))

tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/Nut_Ses_Tox.tiff", width = 8, height = 5.5, units = "in", res = 600, bg = "white")

Fig

dev.off()

###############################################################################
# RDA

tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/RDA.tiff", width = 7, height = 2.75, units = "in", res = 600, bg = "white")
ggarrange(MB_RDA, SA_RDA, ncol=2, nrow=1, common.legend = TRUE)
dev.off()

################################################################################