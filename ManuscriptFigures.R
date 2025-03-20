#### Publication Plots not included in the "AcceptedManuscriptRevision" Sheet, except for Map, which has it's own R Script.

################################################################################
# Figure 6: Phytoplankton RDA: 
################################################################################


tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/RDA.tiff", width = 7, height = 2.75, units = "in", res = 600, bg = "white")
ggarrange(MB_RDA, SA_RDA, ncol=2, nrow=1, common.legend = TRUE)
dev.off()



###############################################################################
###############################################################################
# SUPPLEMENTAL FIGURES
###############################################################################
###############################################################################

###############################################################################
### Figure 1; Cyanotoxin Quota
###############################################################################

Phytos_Toxin %>%
  filter(Station == "SA") %>%
  ggplot(aes(x=Date, y=Q, fill=Toxin)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=c("orangered3", "cadetblue4"))+
  theme_classic()

MB_ToxPC <- Toxins_PC %>%
  pivot_longer(cols=c(ATX_Q, MC_Q), values_to = "Q", names_to = "Toxin") %>%
  filter(Station == "MB") %>%
  filter(Date > as.Date("2021-06-01") & Date <= as.Date("2021-11-05")) %>% 
  ggplot() + 
  annotate("rect", xmin = as.Date("2021-06-01"), xmax = as.Date("2021-08-07"), ymin = 0, ymax = 0.00045,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-08-07"), xmax = as.Date("2021-08-12"), ymin = 0, ymax =0.00045,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-12"), xmax = as.Date("2021-08-20"), ymin = 0, ymax = 0.00045,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-08-20"), xmax = as.Date("2021-08-24"), ymin = 0, ymax = 0.00045,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-24"), xmax = as.Date("2021-09-03"), ymin = 0, ymax = 0.00045,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-03"), xmax = as.Date("2021-09-15"), ymin = 0, ymax = 0.00045,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-09-15"), xmax = as.Date("2021-09-27"), ymin = 0, ymax = 0.00045,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-27"), xmax = as.Date("2021-10-31"), ymin = 0, ymax = 0.00045,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-06-05"), y=0.00046, label="PrB",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-08-09"), y=0.00044, label="E1",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-08-15"), y=0.00046, label="PB1",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-08-22"), y=0.00044, label="BC1",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-08-28"), y=0.00046, label="E2",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-09-07"), y=0.00044, label="PB2",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-09-18"), y=0.00046, label="BC2",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-09-30"), y=0.00044, label="PoB",
           color="black", size=1.2) +
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
  annotate("rect", xmin = as.Date("2021-06-01"), xmax = as.Date("2021-07-11"), ymin = 0, ymax = 0.0002,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-11"), xmax = as.Date("2021-07-15"), ymin = 0, ymax =0.0002,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-07-15"), xmax = as.Date("2021-07-27"), ymin = 0, ymax = 0.0002,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-27"), xmax = as.Date("2021-08-16"), ymin = 0, ymax = 0.0002,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-16"), xmax = as.Date("2021-08-27"), ymin = 0, ymax = 0.0002,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-08-27"), xmax = as.Date("2021-10-31"), ymin = 0, ymax = 0.0002,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-06-05"), y=0.000206, label="PrB",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-07-12"), y=0.000206, label="BC1",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-07-18"), y=0.000206, label="E",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-07-30"), y=0.000206, label="PB",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-08-20"), y=0.000206, label="BC2",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-09-01"), y=0.000206, label="PoB",
           color="black", size=1.2) +
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

###############################################################################
### Figure 2: Hierarchical Clustering
###############################################################################

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



###############################################################################
### Figure 3: Nutrients
###############################################################################

fig2_1 <- (MB_C_Fig + SA_C_Fig) + plot_layout(guides = "collect") + theme(legend.position = "right")

fig2_2 <- (MB_N_Fig + SA_N_Fig) + plot_layout(guides = "collect") + theme(legend.position = "right")

fig2_3 <- (MB_P_Fig + SA_P_Fig) + plot_layout(guides = "collect") + theme(legend.position = "right")

fig <- fig2_1 / fig2_2 / fig2_3

tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/Fig1New.tiff", width = 7, height = 7, units = "in", res = 600, bg = "white")

fig

dev.off()

################################################################################
### Figure 5: MB chlorophyll HeatMap
################################################################################

tiff(filename = "MB_CHL_Heatmap.tiff", width = 6, height = 4, units = "in", res = 600, bg = "white")

wtr.heat.map.newdepth(MB_CHL_HM,  zlim=c(0,20),
                      key.title=graphics::title((main="Chlorophyll\n(RFU)"), adj=0.1, col="red", cex.main=1),
                      main="Missisquoi Bay")

dev.off()

################################################################################
### Figure 6: SA chlorophyll HeatMap
################################################################################

tiff(filename = "SA_CHL_Heatmap.tiff", width = 6, height = 4, units = "in", res = 600, bg = "white")

wtr.heat.map.newdepth(SA_CHL_HM,  zlim=c(0,10),
                      key.title=graphics::title((main="Chloropyll\n(RFU)"), adj=0.1, col="red", cex.main=1),
                      main="St. Albans Bay")
dev.off()

