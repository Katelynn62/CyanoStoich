################################################################################
# Title: Nitrogen Fixation and Heterocyte Figure Script
# Date: 3/29/23
# Author: Katelynn Warner
################################################################################

# Load Libraries
library(tidyverse)
library(lubridate)
library(ggpubr)
library(readxl)


# Load Data for Heterocyte Counts


# Read in Sample MetaData

MetaData <- read_xlsx("./PhytoplanktonData/MetaData2021_Phytos.xlsx")

###### MB 21 

## Read in Biovolume Files

IP21_047 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_047_MB.csv") %>%
  mutate(Sample_ID = "IP21_047_MB")
IP21_064 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_064_MB.csv") %>%
  mutate(Sample_ID = "IP21_064_MB")
IP21_068 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_068_MB.csv")%>%
  mutate(Sample_ID = "IP21_068_MB")
IP21_072 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_072_MB.csv") %>%
  mutate(Sample_ID = "IP21_072_MB")
IP21_075 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_075_MB.csv") %>%
  mutate(Sample_ID = "IP21_075_MB")
IP21_079 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_079_MB.csv") %>%
  mutate(Sample_ID = "IP21_079_MB") 
IP21_083 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_083_MB.csv") %>%
  mutate(Sample_ID = "IP21_083_MB")
IP21_088 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_088_MB.csv") %>%
  mutate(Sample_ID = "IP21_088_MB")
IP21_092 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_092_MB.csv") %>%
  mutate(Sample_ID = "IP21_092_MB")
IP21_095 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_095_MB.csv") %>%
  mutate(Sample_ID = "IP21_095_MB")

## Join Files together for a single file

MB21_BV <- full_join(IP21_047, IP21_064) %>%
  full_join(., IP21_068) %>%
  full_join(., IP21_072) %>%
  full_join(., IP21_075) %>%
  full_join(., IP21_079) %>%
  full_join(., IP21_083)%>%
  full_join(., IP21_088)%>%
  full_join(., IP21_092)%>%
  full_join(., IP21_095)%>%
  left_join(., MetaData, by=c("Sample_ID"))

BM <- MB21_BV %>%
  group_by(Sample_ID, Total_BM) %>%
  summarize(BM = mean(Total_BM))

MB21_BV %>%
  mutate(Genus.x = if_else(RA_BM < 0.03, "Other", Genus.x)) %>%
  ggplot(aes(fill=Genus.x, y=RA_BM, x=Sample_ID))+
  geom_bar(position="stack", stat="identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("")+
  ylab("Relative Abundance by Biomass (%)") 

MB21 <- MB21_BV %>%
  mutate(Genus.x = if_else(RA_BM < 0.03, "Other", Genus.x)) %>%
  ggplot(aes(fill=Genus.x, y=BM_ug.L, x=Date)) +
  geom_bar(position="stack", stat="identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlim(as.POSIXct("2021-06-01"), as.POSIXct("2021-11-01")) +
  xlab("")+
  ylab("Biomass(ug/L)") 


###### SA 21 


IP21_054 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_054_SA.csv") %>%
  mutate(Sample_ID = "IP21_054_SA")
IP21_057 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_057_SA.csv") %>%
mutate(Sample_ID = "IP21_057_SA")
IP21_058_2 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_058_2_SA.csv")%>%
  mutate(Sample_ID = "IP21_058_2_SA")
IP21_061 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_061_SA.csv") %>%
  mutate(Sample_ID = "IP21_061_SA")
IP21_065 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_065_SA.csv") %>%
  mutate(Sample_ID = "IP21_065_SA") 
IP21_070 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_070_SA.csv") %>%
  mutate(Sample_ID = "IP21_070_SA")
IP21_074 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_074_SA.csv") %>%
  mutate(Sample_ID = "IP21_074_SA")
IP21_078 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_078_SA.csv") %>%
  mutate(Sample_ID = "IP21_078_SA")
IP21_085 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_085_SA.csv") %>%
  mutate(Sample_ID = "IP21_085_SA") 
IP21_089 <- read.csv("./PhytoplanktonData/biovolume_final_IP21_089_SA.csv") %>%
  mutate(Sample_ID = "IP21_089_SA")

SA21_BV <- full_join(IP21_054, IP21_057) %>%
  full_join(., IP21_058_2) %>%
  full_join(., IP21_061) %>%
  full_join(., IP21_065) %>%
  full_join(., IP21_070)%>%
  full_join(., IP21_074)%>%
  full_join(., IP21_078)%>%
  full_join(., IP21_085)%>%
  full_join(., IP21_089) %>%
  left_join(., MetaData, by=c("Sample_ID"))

BM <- SA21_BV %>%
  group_by(Sample_ID, Total_BM) %>%
  summarize(BM = mean(Total_BM))


SA21_BV %>%
  mutate(Genus = if_else(RA_BM < 0.03, "Other", Genus)) %>%
  ggplot(aes(fill=Genus, y=RA_BM, x=Date))+
  geom_bar(position="stack", stat="identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("")+
  ylab("Relative Abundance by Biomass (%)") 

SA21 <- SA21_BV %>%
  mutate(Genus = if_else(RA_BM < 0.03, "Other", Genus)) %>%
  ggplot(aes(fill=Genus, y=BM_ug.L, x=Date))+
  geom_bar(position="stack", stat="identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlim(as.POSIXct("2021-06-01"), as.POSIXct("2021-11-01")) +
  xlab("")+
  ylab("Biomass(ug/L)") 


#### Calculate Nitrogen Fixers ####

Nfixer_MB <- MB21_BV %>%
  mutate(Genus = Genus.x) %>%
  mutate(Nfix = if_else(Genus == "Aphanizomenon" | 
                          Genus == "Cuspidothrix" |
                          Genus == "Dolichospermum" | 
                          Genus == "Gloeotrichia", "Yes", "Omit")) %>%
  mutate(Nfix = if_else(Genus == "Aphanocapsa" |
                          Genus == "Aphanothece" |
                          Genus == "Woronichinia" |
                          Genus == "Chroococcus" |
                          Genus == "Merismopedia" |
                          Genus == "Microcystis" |
                          Genus == "Planktolyngbya" |
                          Genus == "Pseudoanabaena" |
                          Genus == "Romeria" |
                          Genus == "Snowella", "No", Nfix))
MB <- Nfixer_MB %>%
  filter(Nfix != "Omit") %>%
  mutate(Date = as.Date(Date)) %>%
  ggplot(aes(fill=Nfix, y=BM_ug.L, x=Date)) +
  annotate("rect", xmin = as.Date("2021-06-01"), xmax = as.Date("2021-07-26"), ymin = 0, ymax = 55000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-26"), xmax = as.Date("2021-08-14"), ymin = 0, ymax =55000,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-14"), xmax = as.Date("2021-09-08"), ymin = 0, ymax = 55000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-10"), xmax = as.Date("2021-11-08"), ymin = 0, ymax = 55000,
           fill = "grey98") +
  geom_bar(position="stack", stat="identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("")+
  ylab("Biomass(ug/L)") +
  scale_fill_brewer(palette="Set1") + 
  xlim(as.Date("2021-06-01"), as.Date("2021-11-01")) +
  theme_classic() +
  theme(axis.text = element_text(size=13), axis.title = element_text(size=15), 
        legend.position="None")

Nfixer_SA <- SA21_BV %>%
  mutate(Nfix = if_else(Genus == "Anabaenopsis" | 
                          Genus == "Aphanizomenon" | 
                          Genus == "Cuspidothrix" |
                          Genus == "Cylindrospermopsis" |
                          Genus == "Dolichospermum" | 
                          Genus == "Gloeotrichia", "Yes", "Omit")) %>%
  mutate(Nfix = if_else(Genus == "Aphanocapsa" |
                          Genus == "Aphanothece" |
                          Genus == "Chamaesiphon" |
                          Genus == "Chroococcus" |
                          Genus == "Merismopedia" |
                          Genus == "Microcystis" |
                          Genus == "Planktolyngbya" |
                          Genus == "Pseudoanabaena" |
                          Genus == "Romeria" |
                          Genus == "Snowella", "No", Nfix))
SA <- Nfixer_SA %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Nfix != "Omit") %>%
  ggplot(aes(fill=Nfix, y=BM_ug.L, x=Date)) + 
  annotate("rect", xmin = as.Date("2021-06-01"), xmax = as.Date("2021-07-11"), ymin = 0, ymax = 11000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-11"), xmax = as.Date("2021-07-31"), ymin = 0, ymax =11000, 
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-07-31"), xmax = as.Date("2021-10-04"), ymin = 0, ymax = 11000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-10-04"), xmax = as.Date("2021-11-05"), ymin = 0, ymax = 11000,
           fill = "grey98") +
  geom_bar(position="stack", stat="identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("Date")+
  ylab("Biomass(ug/L)") +
  scale_fill_brewer(palette="Set1") + 
  xlim(as.Date("2021-06-01"), as.Date("2021-11-05")) +
  theme_classic() +
  theme(axis.text = element_text(size=13), axis.title = element_text(size=15), 
        legend.position="None")
str(Nfixer_SA)

## Nitrogen Fixers
ggarrange(MB, SA, ncol=1, nrow=2, align="hv")

## Total Biomass
ggarrange(MB21, SA21, ncol=1, nrow=2, align="hv")

unique(Nfixer_SA$Date)

#### Count Number of Heterocytes ####

## Load in Counts Pages and load as one file ##

IP21_054_C <- read.csv("./PhytoplanktonData/2021/St.Albans/IP21_054_SA - Counts.csv") %>%
  mutate(Sample_ID = "IP21_054_SA",
         Heterocyte = as.numeric(Heterocyte))
IP21_057_C <- read.csv("./PhytoplanktonData/2021/St.Albans/IP21_057_SA - Counts.csv") %>%
  mutate(Sample_ID = "IP21_057_SA",
         Heterocyte = as.numeric(Heterocyte))
IP21_058_2_C <- read.csv("./PhytoplanktonData/2021/St.Albans/IP21_058_2_SA - Counts.csv")%>%
  mutate(Sample_ID = "IP21_058_2_SA",
         Heterocyte = as.numeric(Heterocyte))
IP21_061_C <- read.csv("./PhytoplanktonData/2021/St.Albans/IP21_061_SA - Counts.csv") %>%
  mutate(Sample_ID = "IP21_061_SA",
         Heterocyte = as.numeric(Heterocyte))
IP21_065_C <- read.csv("./PhytoplanktonData/2021/St.Albans/IP21_065_SA - Counts.csv") %>%
  mutate(Sample_ID = "IP21_065_SA",
         Heterocyte = as.numeric(Heterocyte)) 
IP21_070_C <- read.csv("./PhytoplanktonData/2021/St.Albans/IP21_070_SA - Counts.csv") %>%
  mutate(Sample_ID = "IP21_070_SA",
         Heterocyte = as.numeric(Heterocyte))
IP21_074_C <- read.csv("./PhytoplanktonData/2021/St.Albans/IP21_074_SA - Counts.csv") %>%
  mutate(Sample_ID = "IP21_074_SA",
         Heterocyte = as.numeric(Heterocyte))
IP21_078_C <- read.csv("./PhytoplanktonData/2021/St.Albans/IP21_078_SA - Counts.csv") %>%
  mutate(Sample_ID = "IP21_078_SA",
         Heterocyte = as.numeric(Heterocyte))
IP21_085_C <- read.csv("./PhytoplanktonData/2021/St.Albans/IP21_085_SA - Counts.csv") %>%
  mutate(Sample_ID = "IP21_085_SA",
         Heterocyte = as.numeric(Heterocyte))
IP21_089_C <- read.csv("./PhytoplanktonData/2021/St.Albans/IP21_089_SA - Counts.csv") %>%
  mutate(Sample_ID = "IP21_089_SA",
         Heterocyte = as.numeric(Heterocyte))

SA21_Nfix <- full_join(IP21_054_C, IP21_057_C) %>%
  full_join(., IP21_058_2_C) %>%
  full_join(., IP21_061_C) %>%
  full_join(., IP21_065_C) %>%
  full_join(., IP21_070_C)%>%
  full_join(., IP21_074_C)%>%
  full_join(., IP21_078_C)%>%
  full_join(., IP21_085_C)%>%
  full_join(., IP21_089_C) %>%
  left_join(., MetaData, by=c("Sample_ID")) %>%
  select(Sample_ID, Genus, Quant_in_view) 

SA21_Nfix <- SA21_Nfix %>%
  mutate(Nfix = if_else(Genus == "Anabaenopsis" | 
                          Genus == "Aphanizomenon" | 
                          Genus == "Cuspidothrix" |
                          Genus == "Cylindrospermopsis" |
                          Genus == "Dolichospermum" | 
                          Genus == "Gloeotrichia", "Yes", "Omit")) %>%
  mutate(Nfix = if_else(Genus == "Aphanocapsa" |
                          Genus == "Aphanothece" |
                          Genus == "Chamaesiphon" |
                          Genus == "Chroococcus" |
                          Genus == "Merismopedia" |
                          Genus == "Microcystis" |
                          Genus == "Planktolyngbya" |
                          Genus == "Pseudoanabaena" |
                          Genus == "Romeria" |
                          Genus == "Snowella", "No", Nfix)) %>%
  filter(Nfix=="Yes") %>%
  group_by(Sample_ID) %>%
  summarize(Num_Filament = sum(Quant_in_view))

SA21_Het <- full_join(IP21_054_C, IP21_057_C) %>%
  full_join(., IP21_058_2_C) %>%
  full_join(., IP21_061_C) %>%
  full_join(., IP21_065_C) %>%
  full_join(., IP21_070_C)%>%
  full_join(., IP21_074_C)%>%
  full_join(., IP21_078_C)%>%
  full_join(., IP21_085_C)%>%
  full_join(., IP21_089_C) %>%
  left_join(., MetaData, by=c("Sample_ID")) %>%
  mutate(Heterocyte = replace_na(Heterocyte, 0)) %>%
  select(Sample_ID, Heterocyte) %>%
  group_by(Sample_ID) %>%
  summarize(Het_Count = sum(Heterocyte))

SA21_Het <- left_join(SA21_Het, MetaData, by=c("Sample_ID")) %>%
  filter(Station == "SA") %>%
  mutate(Het_Count = replace_na(Het_Count, 0))

SA21_Nfix_Het <- full_join(SA21_Het, SA21_Nfix, by=c("Sample_ID")) %>%
  left_join(., MetaData, by=c("Sample_ID")) %>%
  mutate(Num_Filament = replace_na(Num_Filament, 0)) %>%
  mutate(Het.Fil = Het_Count/Num_Filament) %>%
  mutate(Het.Fil = replace_na(Het.Fil, 0)) %>%
  mutate(Date = Date.y) %>%
  select(-Date.x, -Date.y)

### Make Heterocyte Biovolume Figure

SA_HetFig <- SA21_Nfix_Het %>%
  mutate(Date = as.Date(Date)) %>%
  ggplot(aes(x=Date, y=Het.Fil))+
  annotate("rect", xmin = as.Date("2021-06-01"), xmax = as.Date("2021-07-11"), ymin = 0, ymax = 1.25,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-11"), xmax = as.Date("2021-07-31"), ymin = 0, ymax =1.25, 
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-07-31"), xmax = as.Date("2021-10-04"), ymin = 0, ymax = 1.25,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-10-04"), xmax = as.Date("2021-11-05"), ymin = 0, ymax = 1.25,
           fill = "grey98") +
  geom_line(size=1.05)+
  geom_point(size=2.0)+
  theme_classic() +
  xlab("")+
  ylab("Heterocytes per Filament") +
  xlim(as.Date("2021-06-01"), as.Date("2021-11-05")) +
  theme_classic() +
  theme(axis.text = element_text(size=13), axis.title = element_text(size=15), 
        legend.position="None")

ggarrange(SA_HetFig, SA, nrow=2, ncol=1, align="hv")


## Missisquoi Bay



IP21_047_C <- read.csv("./PhytoplanktonData/2021/MissBay/IP21_047_MB - Counts.csv") %>%
  mutate(Sample_ID = "IP21_047_MB",
         Heterocyte = as.numeric(Heterocyte))
IP21_064_C <- read.csv("./PhytoplanktonData/2021/MissBay/IP21_064_MB - Counts.csv") %>%
  mutate(Sample_ID = "IP21_064_MB",
         Heterocyte = as.numeric(Heterocyte))
IP21_068_C <- read.csv("./PhytoplanktonData/2021/MissBay/IP21_068_MB - Counts.csv")%>%
  mutate(Sample_ID = "IP21_068_MB",
         Heterocyte = as.numeric(Heterocyte))
IP21_072_C <- read.csv("./PhytoplanktonData/2021/MissBay/IP21_072_MB - Counts.csv") %>%
  mutate(Sample_ID = "IP21_072_MB",
         Heterocyte = as.numeric(Heterocyte))
IP21_075_C <- read.csv("./PhytoplanktonData/2021/MissBay/IP21_075_MB - Counts.csv") %>%
  mutate(Sample_ID = "IP21_075_MB",
         Heterocyte = as.numeric(Heterocyte))
IP21_079_C <- read.csv("./PhytoplanktonData/2021/MissBay/IP21_079_MB - Counts.csv") %>%
  mutate(Sample_ID = "IP21_079_MB",
         Heterocyte = as.numeric(Heterocyte))
IP21_083_C <- read.csv("./PhytoplanktonData/2021/MissBay/IP21_083_MB - Counts.csv") %>%
  mutate(Sample_ID = "IP21_083_MB",
         Heterocyte = as.numeric(Heterocyte))
IP21_088_C <- read.csv("./PhytoplanktonData/2021/MissBay/IP21_088_MB - Counts.csv") %>%
  mutate(Sample_ID = "IP21_088_MB",
         Heterocyte = as.numeric(Heterocyte))
IP21_092_C <- read.csv("./PhytoplanktonData/2021/MissBay/IP21_092_MB - Counts.csv") %>%
  mutate(Sample_ID = "IP21_092_MB",
         Heterocyte = as.numeric(Heterocyte))
IP21_095_C <- read.csv("./PhytoplanktonData/2021/MissBay/IP21_095_MB - Counts.csv") %>%
  mutate(Sample_ID = "IP21_095_MB",
         Heterocyte = as.numeric(Heterocyte))

MB21_Nfix <-  full_join(IP21_047_C, IP21_064_C) %>%
  full_join(., IP21_068_C) %>%
  full_join(., IP21_072_C) %>%
  full_join(., IP21_075_C) %>%
  full_join(., IP21_079_C) %>%
  full_join(., IP21_083_C)%>%
  full_join(., IP21_088_C)%>%
  full_join(., IP21_092_C)%>%
  full_join(., IP21_095_C)%>%
  left_join(., MetaData, by=c("Sample_ID")) %>%
  select(Sample_ID, Genus, Quant_in_view) 

MB21_Nfix <- MB21_Nfix %>%
  mutate(Nfix = if_else(Genus == "Aphanizomenon" | 
                          Genus == "Cuspidothrix" |
                          Genus == "Dolichospermum" | 
                          Genus == "Gloeotrichia", "Yes", "Omit")) %>%
  mutate(Nfix = if_else(Genus == "Aphanocapsa" |
                          Genus == "Aphanothece" |
                          Genus == "Woronichinia" |
                          Genus == "Chroococcus" |
                          Genus == "Merismopedia" |
                          Genus == "Microcystis" |
                          Genus == "Planktolyngbya" |
                          Genus == "Pseudoanabaena" |
                          Genus == "Romeria" |
                          Genus == "Snowella", "No", Nfix)) %>%
  filter(Nfix=="Yes") %>%
  group_by(Sample_ID) %>%
  summarize(Num_Filament = sum(Quant_in_view))

MB21_Het <- full_join(IP21_047_C, IP21_064_C) %>%
  full_join(., IP21_068_C) %>%
  full_join(., IP21_072_C) %>%
  full_join(., IP21_075_C) %>%
  full_join(., IP21_079_C) %>%
  full_join(., IP21_083_C)%>%
  full_join(., IP21_088_C)%>%
  full_join(., IP21_092_C)%>%
  full_join(., IP21_095_C)%>%
  left_join(., MetaData, by=c("Sample_ID")) %>%
  mutate(Heterocyte = replace_na(Heterocyte, 0)) %>%
  select(Sample_ID, Heterocyte) %>%
  group_by(Sample_ID) %>%
  summarize(Het_Count = sum(Heterocyte))

MB21_Het <- left_join(MB21_Het, MetaData, by=c("Sample_ID")) %>%
  filter(Station == "MB") %>%
  mutate(Het_Count = replace_na(Het_Count, 0))

MB21_Nfix_Het <- full_join(MB21_Het, MB21_Nfix, by=c("Sample_ID")) %>%
  left_join(., MetaData, by=c("Sample_ID")) %>%
  mutate(Num_Filament = replace_na(Num_Filament, 0)) %>%
  mutate(Het.Fil = Het_Count/Num_Filament) %>%
  mutate(Het.Fil = replace_na(Het.Fil, 0)) %>%
  mutate(Date = Date.y) %>%
  select(-Date.x, -Date.y)

### Make Heterocyte Biovolume Figure

MB_HetFig <- MB21_Nfix_Het %>%
  mutate(Date = as.Date(Date)) %>%
  ggplot(aes(x=Date, y=Het.Fil))+
  annotate("rect", xmin = as.Date("2021-06-01"), xmax = as.Date("2021-07-26"), ymin = 0, ymax = 1.25,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-26"), xmax = as.Date("2021-08-14"), ymin = 0, ymax =1.25,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-14"), xmax = as.Date("2021-09-08"), ymin = 0, ymax = 1.25,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-10"), xmax = as.Date("2021-11-08"), ymin = 0, ymax = 1.25,
           fill = "grey98") +
  geom_line(size=1.05)+
  geom_point(size=2.0)+
  theme_classic() +
  xlab("")+
  ylab("Heterocytes per Filament") +
  xlim(as.Date("2021-06-01"), as.Date("2021-11-05")) +
  theme_classic() +
  theme(axis.text = element_text(size=13), axis.title = element_text(size=15), 
        legend.position="None")

ggarrange(MB_HetFig, MB, nrow=2, ncol=1, align="hv")
  