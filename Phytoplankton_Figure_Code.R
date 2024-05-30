### Making the Phytoplankton Type Figure

Phytos <- read_csv("./Clean Data/Warner_etal_Disturbance_PhytoplanktonBV.csv") %>%
  #filter(Taxonomic_group == "Cyanobacteria") %>%
  select(Sample_ID, SampleDate, Genus, species, BM_ug.L, RA_BM, Taxonomic_group) %>%
  mutate(SampleDate = if_else(Sample_ID == "IP21_074_SA", as.Date("2021-08-26"), SampleDate),
         SampleDate = if_else(Sample_ID == "IP21_065_SA", as.Date("2021-08-13"), SampleDate)) %>%
  mutate(Date = SampleDate) %>%
  mutate(Station = if_else(grepl("MB", Sample_ID), "MB", "SA"))


## Join dataframes together

SA21_Genus <- Phytos %>%
  filter(Station == "SA") %>%
  select(Genus) %>%
  unique()

MB21_Genus <- Phytos %>%
  filter(Station == "MB") %>%
  select(Genus) %>%
  unique()


BV <- full_join(MB21_Genus, SA21_Genus) %>%
  unique()

write.csv(BV, "SAMB21_Genus.csv")  

## Manually typed in names of group

## Reload excel sheet

SAMB_Algae <- read.csv("SAMB21_Genus_AlgalGroup.csv")

## Join back to the group of Genera

SA21_Phyto <- left_join(SA21_BV, SAMB_Algae, by="Genus") %>%
  mutate(`Algal Group` = Algal_Group)



## Changing this to be the Genera that are at least 5% or greater of the communitity. 


SA_Phyto_Keep <- Phytos %>%
  filter(Station == "SA") %>%
  group_by(Sample_ID, SampleDate, Genus) %>%
  summarize(RA_BM = sum(RA_BM)) %>%
  mutate(Keep = if_else(RA_BM >= 0.10, "Yes", "No")) %>%
  ungroup() %>%
  select(Genus, Keep) %>%
  filter(Keep=="Yes") %>%
  unique()

###############################################################################
## Phyto Taxonomic Groups

SA21_PhytoFam <- Phytos %>%
  filter(Station == "SA") %>%
  select(SampleDate, Taxonomic_group, BM_ug.L) %>%
  group_by(SampleDate, Taxonomic_group) %>%
  summarise(BM = sum(BM_ug.L)) %>%
  filter(SampleDate > as.Date("2021-06-01")) %>%
  rename(Date = SampleDate) %>%
  group_by(Date) %>%
  arrange(desc(BM))

unique(SA21_PhytoFam$Taxonomic_group)

SA_21_Fam <- SA21_PhytoFam %>%
  ggplot(aes(fill = reorder(Taxonomic_group, desc(BM)), y=BM, x=as.Date(Date))) + 
  annotate("rect", xmin = as.Date("2021-06-01"), xmax = as.Date("2021-07-11"), ymin = 0, ymax = 20000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-11"), xmax = as.Date("2021-07-15"), ymin = 0, ymax =20000,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-07-15"), xmax = as.Date("2021-07-27"), ymin = 0, ymax = 20000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-27"), xmax = as.Date("2021-08-16"), ymin = 0, ymax = 20000,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-16"), xmax = as.Date("2021-08-27"), ymin = 0, ymax = 20000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-08-27"), xmax = as.Date("2021-10-31"), ymin = 0, ymax = 20000,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-06-05"), y=20500, label="PrB",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-07-12"), y=20500, label="BC1",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-07-18"), y=20500, label="E",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-07-30"), y=20500, label="PB",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-08-20"), y=20500, label="BC2",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-09-01"), y=20500, label="PoB",
           color="black", size=1.2) +
  geom_bar(stat="identity", position="stack", show.legend = TRUE) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("")+
  ylab("") +
  labs(fill='Taxonomic Group', 
       title='St. Albans Bay',
       subtitle='B') +
  scale_fill_manual(
    values = c("aquamarine4",  "chocolate","darkorange4", "darksalmon","chartreuse3","azure4", "chartreuse2", 
               "bisque4", "gold4", "gold2","darkgoldenrod", "burlywood3"),
    limits = c("Cyanobacteria", "Bacillariophyta", "Cryptophyta", "Dinophyceae", "Chlorophyta", "UNK", "Desmidiaceae", "Euglenophyceae",
               "Ochrophyta", "Chrysophyta", "Xanthophyceae", "Prymnesiaceae"), 
    drop = FALSE) +
  xlim(as.Date("2021-06-01"), as.Date("2021-11-05")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size=7), plot.subtitle = element_text(size=6.5), axis.title = element_text(size=7), axis.text=element_text(size=6),
        legend.title = element_text(size = 5), 
        legend.key.size = unit(0.09, 'in'), legend.text = element_text(size=4.8),
        legend.key.spacing = unit(0, 'cm'), plot.margin = unit(c(0, 0, -.1, 0), 'in')) +
  #scale_x_date(breaks = scales::breaks_pretty(10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

## Phyto Taxonomic Groups

MB21_PhytoFam <- Phytos %>%
  filter(Station == "MB") %>%
  select(SampleDate, Taxonomic_group, BM_ug.L) %>%
  group_by(SampleDate, Taxonomic_group) %>%
  summarise(BM = sum(BM_ug.L)) %>%
  filter(SampleDate > as.Date("2021-06-01")) %>%
  rename(Date = SampleDate) %>%
  group_by(Date) %>%
  arrange(desc(BM))


unique(MB21_PhytoFam$Taxonomic_group)

MB_21_Fam <- MB21_PhytoFam %>%
  ggplot(aes(fill = reorder(Taxonomic_group, desc(BM)), y=BM, x=as.Date(Date))) + 
  annotate("rect", xmin = as.Date("2021-06-01"), xmax = as.Date("2021-08-07"), ymin = 0, ymax = 55000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-08-07"), xmax = as.Date("2021-08-12"), ymin = 0, ymax =55000,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-12"), xmax = as.Date("2021-08-20"), ymin = 0, ymax = 55000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-08-20"), xmax = as.Date("2021-08-24"), ymin = 0, ymax = 55000,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-24"), xmax = as.Date("2021-09-03"), ymin = 0, ymax = 55000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-03"), xmax = as.Date("2021-09-15"), ymin = 0, ymax = 55000,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-09-15"), xmax = as.Date("2021-09-27"), ymin = 0, ymax = 55000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-27"), xmax = as.Date("2021-10-31"), ymin = 0, ymax = 55000,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-06-05"), y=56000, label="PrB",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-08-09"), y=56000, label="E1",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-08-15"), y=56000, label="PB1",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-08-22"), y=56000, label="BC1",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-08-28"), y=56000, label="E2",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-09-07"), y=56000, label="PB2",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-09-18"), y=56000, label="BC2",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-09-30"), y=56000, label="PoB",
           color="black", size=1.2) +
  geom_bar(position="stack", stat="identity", show.legend = TRUE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("")+
  ylab("") +
  labs(fill='Taxonomic Group',
       title='Missisquoi Bay',
       subtitle='A') +
  scale_fill_manual(
    values = c("aquamarine4",  "chocolate","darkorange4", "darksalmon","chartreuse3","azure4", "chartreuse2", 
               "bisque4", "gold4", "gold2","darkgoldenrod", "burlywood3"),
    limits = c("Cyanobacteria", "Bacillariophyta", "Cryptophyta", "Dinophyceae", "Chlorophyta", "UNK", "Desmidiaceae", "Euglenophyceae",
               "Ochrophyta", "Chrysophyta", "Xanthophyceae", "Prymnesiaceae"), 
    drop = FALSE) +
  xlim(as.Date("2021-06-01"), as.Date("2021-11-05")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size=7), plot.subtitle = element_text(size=6.5), axis.title = element_text(size=7), axis.text=element_text(size=6),
        legend.title = element_text(size = 5), 
        legend.key.size = unit(0.1, 'in'), legend.text = element_text(size=4.8),
        legend.key.spacing = unit(0.09, 'cm'), plot.margin = unit(c(0, 0, -.1, 0), 'in')) +
  #scale_x_date(breaks = scales::breaks_pretty(10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  guides(fill = guide_legend(byrow = TRUE))


###############################################################################
## Major Phytoplankton Groups

SA21_PhytoDom <- Phytos %>%
  filter(Station == "SA") %>%
  select(SampleDate, Genus, BM_ug.L, RA_BM) %>%
  group_by(SampleDate, Genus) %>%
  summarise(BM = sum(BM_ug.L),
            RA_BM = sum(RA_BM)) %>%
  mutate(Algal_Group = if_else(RA_BM > .2, Genus, "Other")) %>%
  filter(SampleDate > as.Date("2021-06-01")) %>%
  rename(Date = SampleDate) %>%
  group_by(Date) %>%
  arrange(desc(BM))

unique(SA21_PhytoDom$Algal_Group)

SA21_PhytoDom <- SA21_PhytoDom %>%
  mutate(Genus1 = if_else(Genus == "Microcystis" | Genus == "Dolichospermum" | Genus == "Aphanocapsa" |
                            Genus == "Gyrosigma" | Genus == "Aulacoseira", Genus, "Other"))

unique(SA21_PhytoDom$Genus1)

SA_21_Dom <- SA21_PhytoDom %>%
  ggplot(aes(fill = reorder(Genus1, desc(BM)), y=BM, x=as.Date(Date))) + 
  annotate("rect", xmin = as.Date("2021-06-01"), xmax = as.Date("2021-07-11"), ymin = 0, ymax = 20000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-11"), xmax = as.Date("2021-07-15"), ymin = 0, ymax =20000,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-07-15"), xmax = as.Date("2021-07-27"), ymin = 0, ymax = 20000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-27"), xmax = as.Date("2021-08-16"), ymin = 0, ymax = 20000,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-16"), xmax = as.Date("2021-08-27"), ymin = 0, ymax = 20000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-08-27"), xmax = as.Date("2021-10-31"), ymin = 0, ymax = 20000,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-06-05"), y=20500, label="PrB",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-07-12"), y=20500, label="BC1",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-07-18"), y=20500, label="E",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-07-30"), y=20500, label="PB",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-08-20"), y=20500, label="BC2",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-09-01"), y=20500, label="PoB",
           color="black", size=1.2) +
  geom_bar(stat="identity", position="stack", show.legend=TRUE) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("")+
  ylab("") +
  labs(fill='Dominant Genera',
       subtitle = "D") +
  scale_fill_manual(
    values = c("aquamarine4", "cyan3", "chocolate", "azure4", "turquoise2","darkorange4", "goldenrod2",  "turquoise" ,"darksalmon", 
               "deepskyblue4", "goldenrod4"),
    limits = c("Dolichospermum", "Gloeotrichia", "Aulacoseira", "Other", "Aphanizomenon", "Cryptomonas", "Pinnularia", "Aphanocapsa",
               "Peridiniopsis", "Microcystis", "Gyrosigma"), 
    drop = T) +
  xlim(as.Date("2021-06-01"), as.Date("2021-11-05")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size=7), plot.subtitle = element_text(size=6.5), axis.title = element_text(size=7), axis.text=element_text(size=6),
        legend.title = element_text(size = 5), 
        legend.key.size = unit(0.09, 'in'), legend.text = element_text(size=4.8),
        legend.key.spacing = unit(0, 'cm'), plot.margin = unit(c(-.1, 0, 0, 0), 'in')) +
  #scale_x_date(breaks = scales::breaks_pretty(10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))


## Miss Bay

MB21_PhytoDom <- Phytos %>%
  filter(Station == "MB") %>%
  select(SampleDate, Genus, BM_ug.L, RA_BM) %>%
  group_by(SampleDate, Genus) %>%
  summarise(BM = sum(BM_ug.L),
            RA_BM = sum(RA_BM)) %>%
  mutate(Algal_Group = if_else(RA_BM > .2, Genus, "Other")) %>%
  filter(SampleDate > as.Date("2021-06-01")) %>%
  rename(Date = SampleDate) %>%
  group_by(Date) %>%
  arrange(desc(BM))

unique(MB21_PhytoDom$Genus1)


MB21_PhytoDom <- MB21_PhytoDom %>%
  mutate(Genus1 = if_else(Genus == "Gloeotrichia" | Genus == "Dolichospermum" | Genus == "Aphanizomenon" |
                            Genus == "Pinnularia" | Genus == "Aulacoseira" | Genus == "Peridiniopsis" | Genus == "Cryptomonas",
                          Genus, "Other"))

MB_21_Dom <- MB21_PhytoDom %>%
  ggplot(aes(fill = reorder(Algal_Group, desc(BM)), y=BM, x=as.Date(Date))) + 
  annotate("rect", xmin = as.Date("2021-06-01"), xmax = as.Date("2021-08-07"), ymin = 0, ymax = 55000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-08-07"), xmax = as.Date("2021-08-12"), ymin = 0, ymax =55000,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-12"), xmax = as.Date("2021-08-20"), ymin = 0, ymax = 55000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-08-20"), xmax = as.Date("2021-08-24"), ymin = 0, ymax = 55000,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-24"), xmax = as.Date("2021-09-03"), ymin = 0, ymax = 55000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-03"), xmax = as.Date("2021-09-15"), ymin = 0, ymax = 55000,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-09-15"), xmax = as.Date("2021-09-27"), ymin = 0, ymax = 55000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-27"), xmax = as.Date("2021-10-31"), ymin = 0, ymax = 55000,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-06-05"), y=56000, label="PrB",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-08-09"), y=56000, label="E1",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-08-15"), y=56000, label="PB1",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-08-22"), y=56000, label="BC1",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-08-28"), y=56000, label="E2",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-09-07"), y=56000, label="PB2",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-09-18"), y=56000, label="BC2",
           color="black", size=1.2) +
  annotate(geom="text", x= as.Date("2021-09-30"), y=56000, label="PoB",
           color="black", size=1.2) +
  geom_bar(position="stack", stat="identity", show.legend = TRUE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("")+
  ylab("") +
  labs(fill='Dominant Genera',
       subtitle='C') +
  scale_fill_manual(
    values = c("aquamarine4", "cyan3", "chocolate", "azure4", "turquoise2","darkorange4", "goldenrod2",  "turquoise" ,"darksalmon", 
               "deepskyblue4", "goldenrod4"),
    limits = c("Dolichospermum", "Gloeotrichia", "Aulacoseira", "Other", "Aphanizomenon", "Cryptomonas", "Pinnularia", "Aphanocapsa",
               "Peridiniopsis", "Microcystis", "Gyrosigma"), 
    drop = FALSE) +
  xlim(as.Date("2021-06-01"), as.Date("2021-11-05")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size=7), plot.subtitle = element_text(size=6.5), axis.title = element_text(size=7), axis.text=element_text(size=6),
        legend.title = element_text(size = 5), 
        legend.key.size = unit(0.1, 'in'), legend.text = element_text(size=4.8),
        legend.key.spacing = unit(0.09, 'cm'), plot.margin = unit(c(-.1, 0, 0, 0), 'in')) +
  #scale_x_date(breaks = scales::breaks_pretty(10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  guides(fill = guide_legend(byrow = TRUE))



################################################################################
################################################################################

fig1 <- ggarrange(MB_21_Fam, SA_21_Fam, nrow=1, ncol=2, align="hv", legend="right", common.legend = TRUE, labels=NULL)
fig2 <- ggarrange(MB_21_Dom, SA_21_Dom, nrow=1, ncol=2, align="hv", legend="right", common.legend = TRUE, labels=NULL)


fig <- ggarrange(fig1, fig2, align="hv", nrow=2, ncol=1)

#fig <- ggarrange(MB_21_Fam, SA_21_Fam, MB_21_Dom, SA_21_Dom, nrow=2, ncol=2, align="hv", legend = "right",
#          labels=NULL)

tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/Fig4.2.tiff", width = 7, height = 4.5, units = "in", res = 600, bg = "white")

annotate_figure(fig, left = textGrob(expression(paste("Phytoplankton Biomass (",~ mu, "g/L)")), rot = 90, vjust = 1, hjust= 0.4, gp = gpar(cex=0.6)),
                bottom = textGrob("Date", gp = gpar(cex=0.6), hjust=1.2))


dev.off()

