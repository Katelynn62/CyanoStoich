###############################################################################
# Title: C:N:P Analysis Cleaned
# Date: 4/12/2023
# Author: Katelynn Warner
###############################################################################

#### Load Libraries ###
library(tidyverse)
library(lubridate)
library(readxl)
library(ggpubr)
require(grid) 
library(strucchange)
library(broom)
library(patchwork)
library(plotrix)

#### Load Excel Sheets ####

Profiler_MB <- read_csv("MissisquoiWinch_PFL_Step_with_headers.csv") %>%
  mutate(datetime = mdy_hm(TIMESTAMP)) %>%
  mutate(Year = year(datetime)) %>%
  mutate(Month = month(datetime)) %>%
  mutate(Day = day(datetime)) %>%
  mutate(Hour = hour(datetime)) %>%
  mutate(Minute = 0) %>%
  mutate(datetime = make_datetime(Year, Month, Day, Hour, Minute)) %>%
  mutate(Date = make_date(Year, Month, Day)) %>%
  mutate(Depth = Depth_meters) %>%
  mutate(Depth = if_else(Depth <=0.8, 0.5, Depth)) %>%
  mutate(Depth = if_else(Depth >0.8 & Depth <=1.3, 1.0, Depth)) %>%
  mutate(Depth = if_else(Depth >1.3 & Depth <=1.8, 1.5, Depth)) %>%
  mutate(Depth = if_else(Depth >1.8 & Depth <=2.3, 2.0, Depth)) %>%
  mutate(Depth = if_else(Depth >2.3 & Depth <=2.8, 2.5, Depth)) %>%
  mutate(Depth = if_else(Depth >2.8 & Depth <=3.3, 3.0, Depth)) %>%
  filter(Depth < 3.0) %>%
  mutate(ODOmgL = `ODO_mg/L`) %>%
  mutate(ODOsat = `ODOsat_%`) %>%
  mutate(PC_RFU = `BGA_PC_RFU`) %>%
  mutate(Chl_RFU = `Chl_RFU`) %>%
  mutate(Turbid = `Turbid_NTU`) %>%
  mutate(Remove = if_else(Month == 9 & PC_RFU > 15, "Yes", "No")) %>%
  filter(Remove == "No") %>%
  select(datetime, Day, Month, Hour, Depth, Temp_C, ODOmgL, ODOsat, PC_RFU,
         Chl_RFU, pH, Month, Day, Year, Turbid) %>%
  mutate(Date = make_date(Year, Month, Day)) %>%
  group_by(Date) %>%
  summarise(PC_RFU = mean(PC_RFU)) %>%
  mutate(Station = "MB") 


Profiler_SA <- read_csv("StAlbansInner_PFL_Step_with_headers.csv") %>%
  mutate(datetime = mdy_hm(TIMESTAMP)) %>%
  mutate(Year = year(datetime)) %>%
  mutate(Month = month(datetime)) %>%
  mutate(Day = day(datetime)) %>%
  mutate (Hour = hour(datetime)) %>%
  mutate(Minute = 0) %>%
  mutate(datetime = make_datetime(Year, Month, Day, Hour, Minute)) %>%
  mutate(Date = make_date(Year, Month, Day)) %>%
  mutate(Depth = Depth_meters) %>%
  mutate(Depth = if_else(Depth <=0.8, 0.5, Depth)) %>%
  mutate(Depth = if_else(Depth >0.8 & Depth <=1.3, 1.0, Depth)) %>%
  mutate(Depth = if_else(Depth >1.3 & Depth <=1.8, 1.5, Depth)) %>%
  mutate(Depth = if_else(Depth >1.8 & Depth <=2.3, 2.0, Depth)) %>%
  mutate(Depth = if_else(Depth >2.3 & Depth <=2.8, 2.5, Depth)) %>%
  mutate(Depth = if_else(Depth >2.8 & Depth <=3.3, 3.0, Depth)) %>%
  mutate(Depth = if_else(Depth >3.3 & Depth <=3.8, 3.5, Depth)) %>%
  mutate(Depth = if_else(Depth >3.8 & Depth <=4.3, 4.0, Depth)) %>%
  mutate(Depth = if_else(Depth >4.3 & Depth <=4.8, 4.5, Depth)) %>%
  mutate(ODOmgL = `ODO_mg/L`) %>%
  mutate(ODOsat = `ODOsat_%`) %>%
  mutate(PC_RFU = `BGA_PC_RFU`) %>%
  mutate(Chl_RFU = `Chl_RFU`) %>%
  mutate(Turbid = `Turbid_NTU`) %>%
  select(datetime, Day, Month, Hour, Depth, Temp_C, ODOmgL, ODOsat, 
         PC_RFU, Chl_RFU, pH, Month, Day, Year, Turbid) %>%
  mutate(Date = make_date(Year, Month, Day)) %>%
  group_by(Date) %>%
  summarise(PC_RFU = mean(PC_RFU)) %>%
  mutate(Station = "SA")


Profiler_PC <- rbind(Profiler_MB, Profiler_SA)


### DOC and DIN from the Integrated water sample.

Water_CN <- read_xlsx("LakeChampWater_DOC_TDN_2021.xlsx") %>%
  mutate(Water_DOC_uML = `DOC (uM/L)`, 
         Water_TDN_uML = `TDN (uM/L)`,
         Water_DOC_mgL = `DOC (mg/L)`,
         Water_TDN_mgL = `TDN (mg/L)`,
         Water_DOC_uML = `DOC (uM/L)`,
         Water_CN = `C:N`,
         Date = date, 
         Station = site,
         Location = "IWC") %>%
  select(Station, Date, Water_DOC_uML, Water_DOC_mgL, Water_TDN_uML, 
         Water_TDN_mgL, Water_CN, Location) %>%
  filter(Water_DOC_mgL < 8)

Water_TNTP <- read_csv("LG21_Summer_TN-TP_BREE_Clean.csv") %>%
  mutate(Station = if_else(Site == "Miss Bay", "MB", "SA"),
         Date = mdy(Date)) %>%
  filter(`TP_(uMol/L)` < 13 & `TP_(uMol/L)` > 0) %>%
  group_by(Date, Station, Location) %>%
  summarize(Water_TN_uML= mean(`TN_(uMol/L)`),
            Water_TP_uML = mean(`TP_(uMol/L)`),
            Water_TN_mgL = mean(`TN_mg/L`), 
            Water_TP_mgL = mean(`TP_mg/L`))
str(Water_TNTP)

### Looking for outliers
Water_TNTP %>%
  ggplot(aes(x=Date, y=Water_TP_uML, group=Station, color=Station)) +
  geom_point()


S_TNTP <- Water_TNTP %>%
  filter(Location == "surface")

B_TNTP <- Water_TNTP %>%
  filter(Location == "bottom")


Seston_CN <- read_xlsx("LakeChamp_Seston_CN_2021_Clean.xlsx") %>%
  mutate(Date= ymd(Date)) %>%
  group_by(Site, Date) %>%
  summarise(N_mgL = mean(`N mg/L`),
            C_mgL = mean(`C mg/L`), 
            CN = mean(`C:N`),
            C_uML = mean(C_uML),
            N_uML = mean(N_uML)) %>%
  mutate(Station = if_else(Site == "Missisquoi", "MB", "SA"),
         Location = "IWC") %>%
  ungroup() %>%
  select(-Site)

str(Seston_CN)

Seston_P <- read_xlsx("ParticulatePhosphorus_Final.xlsx") %>%
  mutate(Date = ymd(Date), 
         Station = Site, 
         SestonP_uML = `PP_uM.L`, 
         SestonP_mgL = `PP_mg.L`,
         SestonP_ugL = `PP_ug.L`,
         Location = "IWC") %>%
  select(Date, Station, SestonP_uML, SestonP_mgL, SestonP_ugL, Location) %>%
  filter(SestonP_uML > 0.03)  #one really low value in SA Phosphorus


##### Removing Outliers from the P


Toxins <- read.csv("Toxins_SAMB2021_Clean.csv") %>%
  mutate(Microcystin = if_else(is.na(Microcystin), 0, Microcystin),
         Anatoxin = if_else(is.na(Anatoxin), 0, Anatoxin),
         Location = "IWC") %>%
  select(-Cylindrospermopsin, -SampleID) %>%
  mutate(Date = mdy(Date))



CN_Toxins <- full_join(Toxins, Seston_CN, by=c("Station", "Date", "Location"))
S_TNTP_Toxins <- full_join(Toxins, S_TNTP, by=c("Station", "Date", "Location"))
B_TNTP_Toxins <- full_join(Toxins, B_TNTP, by=c("Station", "Date", "Location"))
TNTP_Toxins <- rbind(S_TNTP_Toxins, B_TNTP_Toxins)


CN_Combined_IWC <- full_join(CN_Toxins, Water_CN, by=c("Station", "Date", 
                                                       "Location")) %>%
  full_join(., Seston_P, by=c("Station", "Date", "Location"))


### Created an excel sheet with all of the data in one space. 

All_Combined <- full_join(CN_Combined_IWC, TNTP_Toxins, 
                          by=c("Station", "Date", "Location", "Anatoxin", 
                               "Microcystin")) %>%
  mutate(SestonC_uML = C_mgL/12 *1000,
         SestonN_uML = N_mgL/14 *1000)

Toxins_PC <- full_join(Toxins, Profiler_PC, by=c("Date", "Station"))


All_Combined_BloomStages <- All_Combined %>%
  mutate(BloomStages = if_else(Station == "SA" & 
                                 Date <= as.POSIXct("2021-07-08"), "Lag", "Other"),
         BloomStages = if_else(Station == "SA" & 
                                 Date > as.POSIXct("2021-07-08") & 
                                 Date <= as.POSIXct("2021-07-31"), 
                               "Exponential", BloomStages),
         BloomStages = if_else(Station == "SA" & Date > as.POSIXct("2021-07-31") 
                               & Date <= as.POSIXct("2021-10-04"), "Stationary", 
                               BloomStages),
         BloomStages = if_else(Station == "SA" & Date > as.POSIXct("2021-10-04"),
                               "Decline", BloomStages),
         BloomStages = if_else(Station == "MB" & Date <= as.POSIXct("2021-07-26"), 
                               "Lag", BloomStages),
         BloomStages = if_else(Station == "MB" & Date > as.POSIXct("2021-07-26")
                               & Date <= as.POSIXct("2021-08-14"), "Exponential",
                               BloomStages),
         BloomStages = if_else(Station == "MB" & Date > as.POSIXct("2021-08-14") 
                               & Date <= as.POSIXct("2021-09-08"), "Stationary",
                               BloomStages),
         BloomStages = if_else(Station == "MB" & Date > as.POSIXct("2021-09-08"),
                               "Decline", BloomStages))

## Nutrient and Toxin Data ##

Nutrient_Data <- full_join(Water_CN, Water_TNTP, by=c("Station", 
                                                      "Date",
                                                      "Location")) %>%
  full_join(., Seston_CN, by=c("Station", "Date", "Location")) %>%
  full_join(., Seston_P, by=c("Station", "Date", "Location")) %>%
  full_join(., Toxins, by=c("Station", "Date", "Location")) %>%
  mutate(Water_TN_ugL = Water_TN_mgL*1000,
         Water_TP_ugL = Water_TP_mgL*1000)


str(Nutrient_Data)
###############################################################################
####################### Descriptive Statistics ################################
###############################################################################

##### Seston Nutrient Summary

Seston_CNP_Summary <- All_Combined_BloomStages %>%
  select(BloomStages, Station, C_mgL, N_mgL, SestonP_ugL) %>%
  na.omit() %>%
  group_by(BloomStages, Station) %>%
  summarise(cv_seston_c = sd(C_mgL)/mean(C_mgL) * 100,
            cv_c_se = std.error(C_mgL),
            avg_c = mean(C_mgL), 
            cv_seston_n = sd(N_mgL)/mean(N_mgL) * 100,
            cv_n_se = std.error(N_mgL),
            avg_n = mean(N_mgL), 
            cv_seston_p = sd(SestonP_ugL)/mean(SestonP_ugL) * 100,
            cv_p_se = std.error(SestonP_ugL),
            avg_p = mean(SestonP_ugL))

All_Combined_BloomStages %>%
  select(Station, C_mgL) %>%
  na.omit() %>%
  group_by(Station) %>%
  summarise(range = range(C_mgL))

All_Combined_BloomStages %>%
  select(Station, N_mgL) %>%
  na.omit() %>%
  group_by(Station) %>%
  summarise(range = range(N_mgL))

All_Combined_BloomStages %>%
  select(Station, SestonP_ugL) %>%
  na.omit() %>%
  group_by(Station) %>%
  summarise(range = range(SestonP_ugL))


Seston_CNP_Ratios_Summary <- All_Combined_BloomStages %>%
  select(SestonP_uML, SestonC_uML, SestonN_uML, BloomStages, Station) %>%
  mutate(Seston_CP = SestonC_uML/SestonP_uML,
         Seston_CN = SestonC_uML/SestonN_uML, 
         Seston_NP = SestonN_uML/SestonP_uML) %>%
  na.omit() %>%
  group_by(BloomStages, Station) %>%
  summarise(CV_CP = sd(Seston_CP)/mean(Seston_CP)*100,
            avg_CP = mean(Seston_CP),
            se_CP = std.error(Seston_CP),
            CV_CN = sd(Seston_CN)/mean(Seston_CN)*100,
            avg_CN = mean(Seston_CN),
            se_CN = std.error(Seston_CN),
            CV_NP = sd(Seston_NP)/mean(Seston_NP)*100,
            avg_NP = mean(Seston_NP),
            se_NP = std.error(Seston_NP)) 

### Just CV of total variability across the season. 


Seston_CNP_Ratios_CV_BS <- All_Combined_BloomStages %>%
  select(SestonP_uML, SestonC_uML, SestonN_uML, BloomStages, Station) %>%
  mutate(Seston_CP = SestonC_uML/SestonP_uML,
         Seston_CN = SestonC_uML/SestonN_uML, 
         Seston_NP = SestonN_uML/SestonP_uML) %>%
  na.omit() %>%
  group_by(Station, BloomStages) %>%
  summarise(CV_CP = sd(Seston_CP)/mean(Seston_CP)*100,
            range_CP = range(Seston_CP), 
            CV_CN = sd(Seston_CN)/mean(Seston_CN)*100,
            range_CN = range(Seston_CN),
            CV_NP = sd(Seston_NP)/mean(Seston_NP)*100,
            range_NP = range(Seston_NP)) 

Seston_CNP_Ratios_CV <- All_Combined_BloomStages %>%
  select(SestonP_uML, SestonC_uML, SestonN_uML, BloomStages, Station) %>%
  mutate(Seston_CP = SestonC_uML/SestonP_uML,
         Seston_CN = SestonC_uML/SestonN_uML, 
         Seston_NP = SestonN_uML/SestonP_uML) %>%
  na.omit() %>%
  group_by(Station) %>%
  summarise(CV_CP = sd(Seston_CP)/mean(Seston_CP)*100,
            range_CP = range(Seston_CP), 
            CV_CN = sd(Seston_CN)/mean(Seston_CN)*100,
            range_CN = range(Seston_CN),
            CV_NP = sd(Seston_NP)/mean(Seston_NP)*100,
            range_NP = range(Seston_NP)) 



##### Water Column Nutrient Summary
str(All_Combined_BloomStages)

Water_CNP_Summary <- All_Combined_BloomStages %>%
  select(BloomStages, Station, Location, Water_DOC_mgL, Water_TN_mgL, 
         Water_TP_mgL, Water_TDN_mgL) %>%
  mutate(Water_TP_ugL = Water_TP_mgL * 1000) %>%
  select(-Water_TP_mgL) %>%
  group_by(BloomStages, Station, Location) %>%
  summarise(cv_seston_c = sd(Water_DOC_mgL)/mean(Water_DOC_mgL) * 100,
            cv_c_se = std.error(Water_DOC_mgL),
            avg_c = mean(Water_DOC_mgL), 
            cv_seston_n = sd(Water_TN_mgL)/mean(Water_TN_mgL) * 100,
            cv_n_se = std.error(Water_TN_mgL),
            avg_n = mean(Water_TN_mgL), 
            cv_seston_p = sd(Water_TP_ugL)/mean(Water_TP_ugL) * 100,
            cv_p_se = std.error(Water_TP_ugL),
            avg_p = mean(Water_TP_ugL),
            cv_seston_tdn = sd(Water_TDN_mgL)/mean(Water_TDN_mgL) * 100,
            cv_tdn_se = std.error(Water_TDN_mgL),
            avg_tdn = mean(Water_TDN_mgL))

All_Combined_BloomStages %>%
  select(Water_TDN_mgL, Station) %>%
  na.omit() %>%
  group_by(Station) %>%
  summarize(range = range(Water_TDN_mgL))

All_Combined_BloomStages %>%
  filter(Location == "bottom") %>%
  select(Water_TN_mgL, Station) %>%
  na.omit() %>%
  group_by(Station)%>%
  summarize(range = range(Water_TN_mgL))

All_Combined_BloomStages %>%
  filter(Location == "surface") %>%
  select(Water_TN_mgL, Station) %>%
  na.omit() %>%
  group_by(Station)%>%
  summarize(range = range(Water_TN_mgL))

All_Combined_BloomStages %>%
  filter(Location == "bottom") %>%
  select(Water_TP_mgL, Station) %>%
  mutate(Water_TP_ugL = Water_TP_mgL * 1000) %>%
  na.omit() %>%
  group_by(Station)%>%
  summarize(range = range(Water_TP_ugL))

All_Combined_BloomStages %>%
  filter(Location == "surface") %>%
  select(Water_TP_mgL, Station) %>%
  mutate(Water_TP_ugL = Water_TP_mgL * 1000) %>%
  na.omit() %>%
  group_by(Station)%>%
  summarize(range = range(Water_TP_ugL))

All_Combined_BloomStages %>%
  select(Water_TDN_mgL, Station) %>%
  na.omit() %>%
  group_by(Station) %>%
  summarize(range = range(Water_TDN_mgL))


Water_DOC_Summary <- All_Combined_BloomStages %>%
  select(BloomStages, Station, Water_DOC_mgL) %>%
  na.omit() %>%
  group_by(BloomStages, Station) %>%
  summarise(cv_seston_c = sd(Water_DOC_mgL)/mean(Water_DOC_mgL) * 100,
            cv_c_se = std.error(Water_DOC_mgL),
            avg_c = mean(Water_DOC_mgL))

All_Combined_BloomStages %>%
  select(Water_DOC_mgL, Station) %>%
  na.omit() %>%
  group_by(Station) %>%
  summarize(range = range(Water_DOC_mgL))


#### SA DOC is kind of high. Let's look into that outlier. 

All_Combined_BloomStages %>%
  select(Water_DOC_mgL, Station, Date) %>%
  na.omit() 


Water_TDN_Summary <- All_Combined_BloomStages %>%
  select(BloomStages, Station, Water_TDN_mgL) %>%
  na.omit() %>%
  group_by(BloomStages, Station) %>%
  summarise(cv_seston_tdn = sd(Water_TDN_mgL)/mean(Water_TDN_mgL) * 100,
            cv_tdn_se = std.error(Water_TDN_mgL),
            avg_tdn = mean(Water_TDN_mgL))


## DOC:TDN - Integrated water column

All_Combined_BloomStages %>%
  mutate(DOC.TDN = Water_DOC_uML/Water_TDN_uML) %>%
  select(DOC.TDN, Station, Date, BloomStages) %>%
  na.omit() %>%
  group_by(Station, BloomStages) %>%
  summarize(avg_cn = mean(DOC.TDN),
            se_cn = std.error(DOC.TDN),
            cv_cn = sd(DOC.TDN)/mean(DOC.TDN) * 100,
            n_cn = n(), 
            range = range(DOC.TDN))


All_Combined_BloomStages %>%
  mutate(DOC.TDN = Water_DOC_uML/Water_TDN_uML) %>%
  select(DOC.TDN, Station) %>%
  na.omit() %>%
  group_by(Station) %>%
  summarize(range = range(DOC.TDN))

## TN:TP - Surface

All_Combined_BloomStages %>%
  filter(Location == "surface") %>%
  mutate(TNTP_uML = Water_TN_uML/Water_TP_uML) %>%
  select(TNTP_uML, Station, Date, BloomStages) %>%
  group_by(Station, BloomStages) %>%
  summarize(avg_np = mean(TNTP_uML),
            se_np = std.error(TNTP_uML),
            cv_np = sd(TNTP_uML)/mean(TNTP_uML) * 100,
            n_np = n())


All_Combined_BloomStages %>%
  filter(Location == "surface") %>%
  mutate(TNTP_uML = Water_TN_uML/Water_TP_uML) %>%
  select(TNTP_uML, Station, Date) %>%
  group_by(Station) %>%
  summarize(range = range(TNTP_uML))

All_Combined_BloomStages %>%
  filter(Location == "bottom") %>%
  mutate(TNTP_uML = Water_TN_uML/Water_TP_uML) %>%
  select(TNTP_uML, Station, Date) %>%
  group_by(Station) %>%
  summarize(range = range(TNTP_uML))


## TN:TP - Bottom

All_Combined_BloomStages %>%
  filter(Location == "bottom") %>%
  mutate(TNTP_uML = Water_TN_uML/Water_TP_uML) %>%
  select(TNTP_uML, Station, Date, BloomStages) %>%
  na.omit() %>%
  group_by(Station, BloomStages) %>%
  summarize(avg_np = mean(TNTP_uML),
            cv_np = sd(TNTP_uML)/mean(TNTP_uML) * 100,
            se_np = std.error(TNTP_uML),
            n_np = n())

## DOC:TP - Surface

Surface_TP <- All_Combined_BloomStages %>%
  filter(Location != "bottom") %>%
  select(Station, Date, Water_TP_uML) %>%
  na.omit()

IWC_DOC <- All_Combined_BloomStages %>%
  select(Water_DOC_uML, Station, Date, BloomStages) %>%
  na.omit()

SurfaceTP_IWCDOC <- full_join(Surface_TP, IWC_DOC, by=c("Date", "Station")) %>%
  na.omit()

SurfaceTP_IWCDOC %>%
  mutate(DOC.TP = Water_DOC_uML/Water_TP_uML) %>%
  group_by(Station, BloomStages) %>%
  summarize(avg_cp = mean(DOC.TP),
            cv_cp = sd(DOC.TP)/mean(DOC.TP) * 100,
            se_cp = std.error(DOC.TP),
            n_cp = n())


SurfaceTP_IWCDOC %>%
  mutate(DOC.TP = Water_DOC_uML/Water_TP_uML) %>%
  group_by(Station) %>%
  summarize(range = range(DOC.TP))
## We choose the surface because it should hypothetically be the 
## same concentration as the epilimnion.

##### Toxin T - Tests

Anatoxin_summ <- Toxins %>%
  filter(Anatoxin > 0) %>%
  group_by(Station) %>%
  summarize(avg_ana = mean(Anatoxin),
            se_ana = std.error(Anatoxin),
            n_ana = n())


Microcystin_summ <- Toxins %>%
  filter(Microcystin > 0) %>%
  group_by(Station) %>%
  summarize(avg_mc = mean(Microcystin),
            se_mc = std.error(Microcystin),
            n_mc = n())


## Toxins T-test

Toxins %>%
  filter(Station == "MB") %>%
  ggplot(aes(x=Microcystin))+
  geom_histogram()

Toxins %>%
  filter(Microcystin > 0) %>%
  ggplot(aes(x=Station, y=Microcystin, fill=Station, group=Station))+
  geom_boxplot()

Toxins %>%
  filter(Anatoxin > 0) %>%
  ggplot(aes(x=Station, y=Anatoxin, fill=Station, group=Station))+
  geom_boxplot()

### Two Sample T-Test
MC_TTest <- Toxins %>%
  filter(Microcystin > 0)

ATX_TTest <- Toxins %>%
  filter(Anatoxin > 0)

t.test(Microcystin ~ Station, data=MC_TTest)

t.test(Anatoxin ~ Station, data=ATX_TTest)



# Independent T-Test

SA_Toxins <- Toxins %>%
  filter(Station == "SA") %>%
  pivot_longer(cols=c(Microcystin, Anatoxin), names_to="Toxin", 
               values_to = "Concentration") %>%
  filter(Concentration > 0)

MB_Toxins <- Toxins %>%
  filter(Station == "MB") %>%
  pivot_longer(cols=c(Microcystin, Anatoxin), names_to="Toxin", 
               values_to = "Concentration") %>%
  filter(Concentration > 0)

t.test(Concentration ~ Toxin, data=MB_Toxins)



###############################################################################
###################### Break Point Analysis ###################################
###############################################################################

# https://datascienceplus.com/structural-changes-in-global-warming/

# Helpful tutorial, showing me we want to use probably a trend structural change test
# rather than a level. 


## parameters used explained: 
## Breakpoints function will find optimal number of breakpoints behind the scenes
## h= how many points to include in each "segment"
library(strucchange)
install.packages("strucchange")
##### Seston CN

### Missisquoi Bay 

Seston_CN_BP_MB <- Seston_CN %>%
  filter(Station == "MB") %>%
  select(Date, CN) %>%
  arrange(desc(Date))

x=Fstats(Seston_CN_BP_MB$CN ~ Seston_CN_BP_MB$Date, from=0.01)

sctest(x)
MB_CN_brk <- strucchange::breakpoints(Seston_CN_BP_MB$CN ~ Seston_CN_BP_MB$Date, h=5)

#### BIC and RSS Scores to find max breakpoints. 

plot(MB_CN_brk)

### Dates @  08/03


Seston_CN_BP_MB <- Seston_CN_BP_MB %>%
  mutate(Group = if_else(Date <= "2021-08-10", "One", "Two")) %>%
  mutate(Date=as.Date(Date))

# Regression 

CN_MB <- Seston_CN_BP_MB %>%
  group_by(Group)

dat <- do(CN_MB,
          glance(lm(CN ~ Date, data = .)))

dat

## Group 2 is the only significant group. 

### St. Albans ###

# Plot the time series

Seston_CN_BP_SA <- Seston_CN %>%
  filter(Station == "SA") %>%
  select(Date, CN) %>%
  arrange(desc(Date))

x=Fstats(Seston_CN_BP_SA$CN ~ Seston_CN_BP_SA$Date, from=0.01)

sctest(x)
SA_CN_brk <- strucchange::breakpoints(Seston_CN_BP_SA$CN ~ Seston_CN_BP_SA$Date, h=5)

#### BIC and RSS Scores to find max breakpoints. 

plot(SA_CN_brk)

# Breaks @ Dates -  11

# 07/27

Seston_CN_BP_SA <- Seston_CN_BP_SA %>%
  mutate(Group = if_else(Date <= "2021-07-27", "One", "Two")) %>%
  mutate(Date=as.Date(Date))

# Regression 

CN_SA <- Seston_CN_BP_SA %>%
  group_by(Group)

dat <- do(CN_SA,
          glance(lm(CN ~ Date, data = .)))

dat

## Both are significant

###############################################################################

##### Water Column CN

### Missisquoi Bay 

# Plot the time series

Water_CN_BP_MB <- All_Combined %>%
  filter(Station == "MB") %>%
  select(Date, Water_CN) %>%
  na.omit() %>%
  group_by(Date) %>%
  summarize(Water_CN = mean(Water_CN))  %>%
  arrange(desc(Date))


#####################################  
x=Fstats(Water_CN_BP_MB$Water_CN ~ Water_CN_BP_MB$Date, from=0.01)

sctest(x)
MB_CN_brk <- strucchange::breakpoints(Water_CN_BP_MB$Water_CN ~ Water_CN_BP_MB$Date, h=5)

#### BIC and RSS Scores to find max breakpoints. 

plot(MB_CN_brk)

# Breakpoints @ Dates  08/19

###############################################################################

Water_CN_BP_MB <- Water_CN_BP_MB %>%
  mutate(Group = if_else(Date <= "2021-08-19", "One", "Two")) %>%
  mutate(Date=as.Date(Date))

# Regression 

CN_MB <- Water_CN_BP_MB %>%
  group_by(Group)

dat <- do(CN_MB,
          glance(lm(Water_CN ~ Date, data = .)))

dat

# None have significant regressions. 


### St. Albans


Water_CN_BP_SA <- All_Combined %>%
  filter(Station == "SA") %>%
  select(Date, Water_CN) %>%
  na.omit() %>%
  mutate(Date=as.Date(Date)) %>%
  group_by(Date) %>%
  summarize(Water_CN = mean(Water_CN)) %>%
  arrange(desc(Date))

###############################################################################

## Make the time series
x=Fstats(Water_CN_BP_SA$Water_CN ~ Water_CN_BP_SA$Date, from=0.01)

sctest(x)
SA_CN_brk <- strucchange::breakpoints(Water_CN_BP_SA$Water_CN ~ Water_CN_BP_SA$Date, h=5)

#### BIC and RSS Scores to find max breakpoints. 

plot(MB_CN_brk)

# Breakpoints @ Dates  08/19
###############################################################################

Water_CN_BP_SA <- Water_CN_BP_SA %>%
  mutate(Group = if_else(Date <= "2021-08-03", "One", "Two")) %>%
  mutate(Date=as.Date(Date))

# Regression

CN_SA <- Water_CN_BP_SA %>%
  group_by(Group)

# Figure

dat <- do(CN_SA,
          glance(lm(Water_CN ~ Date, data = .)))


dat
## Group 1 is significant.


###############################################################################
##### N:P Data ###

## Cleaning up NP Data 

## Cleaning up and re-organizing the NP Data so that it is only surface vs. Bottom

sestonNP <- All_Combined_BloomStages %>%
  select(-Location) %>%
  group_by(Date) %>%
  mutate(Seston_NP = SestonN_uML/SestonP_uML) %>%
  mutate(Water_NP = Water_TN_uML/Water_TP_uML) %>%
  select(Date,Station, BloomStages, Seston_NP) %>%
  na.omit()

waterNP_surface <- All_Combined_BloomStages %>%
  filter(Location =="surface") %>%
  select(-Location)%>%
  group_by(Date) %>%
  mutate(Seston_NP = SestonN_uML/SestonP_uML) %>%
  mutate(Water_NP = Water_TN_uML/Water_TP_uML) %>%
  select(Date,Station, BloomStages, Water_NP) %>%
  filter(Water_NP > 0) %>%
  na.omit()

waterNP_bottom <- All_Combined_BloomStages %>%
  filter(Location =="bottom") %>%
  select(-Location)%>%
  group_by(Date) %>%
  mutate(Seston_NP = SestonN_uML/SestonP_uML) %>%
  mutate(Water_NP = Water_TN_uML/Water_TP_uML) %>%
  select(Date,Station, BloomStages, Water_NP) %>%
  filter(Water_NP > 0) %>%
  na.omit()


### Join the NP data frames

ALL_NP <- full_join(waterNP_surface, sestonNP, by=c("Date", "Station", "BloomStages"))


## Subset NP by Stations 

MB_NP <- ALL_NP %>%
  filter(Station == "MB") %>%
  na.omit() %>%
  group_by(Date) %>%
  summarize(Seston_NP = mean(Seston_NP),
            Water_NP = mean(Water_NP)) %>%
  arrange(desc(Date))



SA_NP <- ALL_NP %>%
  filter(Station == "SA") %>%
  na.omit() %>%
  group_by(Date) %>%
  summarize(Seston_NP = mean(Seston_NP),
            Water_NP = mean(Water_NP)) %>%
  arrange(desc(Date))


##### Seston NP

### Missisquoi Bay ###

# Create a model
Seston_NP_BP_MB <- MB_NP %>%
  select(Date, Seston_NP)


x=Fstats(Seston_NP_BP_MB$Seston_NP ~ Seston_NP_BP_MB$Date, from=0.01)

sctest(x)

# Break points aren't significant so don't go further.

################################################################################


### St. Albans

# Create a model
Seston_NP_BP_SA <- SA_NP %>%
  select(Date, Seston_NP) %>%
  arrange(desc(Date))


x=Fstats(Seston_NP_BP_SA$Seston_NP ~ Seston_NP_BP_SA$Date, from=0.01)

sctest(x)

SA_NP_wtr_brk <- strucchange::breakpoints(Seston_NP_BP_SA$Seston_NP ~ Seston_NP_BP_SA$Date, h=5)

#### BIC and RSS Scores to find max breakpoints. 

plot(SA_NP_wtr_brk)



################################################################################

Seston_NP_BP_SA <- ALL_NP %>%
  filter(Station == "SA") %>%
  mutate(Date=as.Date(Date)) %>%
  mutate(Group = if_else(Date <= "2021-09-03", "One", "Two"))


NP_SA <- Seston_NP_BP_SA %>%
  group_by(Group)

dat <- do(NP_SA,
          glance(lm(Seston_NP ~ Date, data = .)))

dat

### Two is significant


###############################################################################

##### Water Column NP

### Top

### Missisquoi Bay

# Create a model

Water_NP_BP_MB <- MB_NP %>%
  select(Date, Water_NP) %>%
  arrange(desc(Date))

x=Fstats(Water_NP_BP_MB$Water_NP ~ Water_NP_BP_MB$Date, from=0.01)

sctest(x)
### Break point is not signficant, carry on. 


### St. Albans

# Create a model
Water_NP_BP_SA <- SA_NP %>%
  select(Date, Water_NP) %>%
  arrange(desc(Date))


x=Fstats(Water_NP_BP_SA$Water_NP ~ Water_NP_BP_SA$Date, from=0.01)

sctest(x)


SA_NP_wtr_brk <- strucchange::breakpoints(Water_NP_BP_SA$Water_NP ~ Water_NP_BP_SA$Date, h=5)
plot(SA_NP_wtr_brk)

# No breakpoint needed. 

dat <- lm(Water_NP_BP_SA$Water_NP ~ Water_NP_BP_SA$Date)
glance(dat)


################################################################################

##### Seston CP

## MB

MB_SestonC <- Seston_CN %>%
  select(Date, Station, C_uML) %>%
  filter(Station == "MB") %>%
  na.omit()

MB_SestonP <- Seston_P %>%
  select(Date, Station, SestonP_uML) %>%
  filter(Station == "MB") %>%
  na.omit()

MB_SestonCP <- full_join(MB_SestonC, MB_SestonP, by=c("Station", "Date")) %>%
  na.omit() %>%
  mutate(Seston_CP = C_uML/SestonP_uML) %>%
  select(Date, Seston_CP) %>%
  arrange(desc(Date))

#####################################  

x=Fstats(MB_SestonCP$Seston_CP ~ MB_SestonCP$Date, from=0.01)

sctest(x)

## Not significant


## SA


SA_SestonC <- Seston_CN %>%
  select(Date, Station, C_uML) %>%
  filter(Station == "SA") %>%
  na.omit()

SA_SestonP <- Seston_P %>%
  select(Date, Station, SestonP_uML) %>%
  filter(Station == "SA") %>%
  na.omit()

SA_SestonCP <- full_join(SA_SestonC, SA_SestonP, by=c("Station", "Date")) %>%
  na.omit() %>%
  mutate(Seston_CP = C_uML/SestonP_uML) %>%
  select(Date, Seston_CP) %>%
  arrange(desc(Date))


#####################################  
x=Fstats(SA_SestonCP$Seston_CP ~ SA_SestonCP$Date, from=0.01)

sctest(x)

SA_CP_ses_brk <- strucchange::breakpoints(SA_SestonCP$Seston_CP ~ SA_SestonCP$Date,h=5)
plot(SA_CP_ses_brk)


Seston_C_BP_SA <- SA_SestonCP %>%
  mutate(Date=as.Date(Date)) %>%
  mutate(Group = if_else(Date <= "2021-09-03", "One", "Two"))


CP_SA <- Seston_C_BP_SA %>%
  group_by(Group)

dat <- do(CP_SA,
          glance(lm(Seston_CP ~ Date, data = .)))

dat
## Group 1 is signficant. 

################################################################################
##### Water Column CP

### Top

## MB

MB_DOC <- All_Combined_BloomStages %>%
  filter(Station=="MB" )%>%
  select(Water_DOC_uML, Date) %>%
  na.omit()

MB_P <- All_Combined_BloomStages %>%
  filter(Station=="MB" )%>%
  select(Water_TP_uML, Date) %>%
  na.omit()

MB_CP <- full_join(MB_DOC, MB_P, by=c("Date")) %>%
  mutate(DOC.TP = Water_DOC_uML/Water_TP_uML) %>%
  select(DOC.TP, Date) %>%
  na.omit() %>%
  group_by(Date) %>%
  summarize(DOC.TP = mean(DOC.TP))

# Create a model
Water_CP_BP_MB <- MB_CP %>%
  arrange(desc(Date))


x=Fstats(Water_CP_BP_MB$DOC.TP ~ Water_CP_BP_MB$Date, from=0.01)

sctest(x)

MB_CP_wtr_brk <- strucchange::breakpoints(Water_CP_BP_MB$DOC.TP ~ Water_CP_BP_MB$Date, h=5)
plot(MB_CP_wtr_brk)

## No breakpoints needed. 


## SA


SA_DOC <- All_Combined_BloomStages %>%
  filter(Station=="SA" )%>%
  select(Water_DOC_uML, Date) %>%
  na.omit()

SA_P <- All_Combined_BloomStages %>%
  filter(Station=="SA" )%>%
  select(Water_TP_uML, Date) %>%
  na.omit()

SA_CP <- full_join(SA_DOC, SA_P, by=c("Date")) %>%
  mutate(DOC.TP = Water_DOC_uML/Water_TP_uML) %>%
  select(DOC.TP, Date) %>%
  na.omit() %>%
  group_by(Date) %>%
  summarize(DOC.TP = mean(DOC.TP))

# Create a model
Water_CP_BP_SA <- SA_CP %>%
  arrange(desc(Date))

x=Fstats(Water_CP_BP_SA$DOC.TP ~ Water_CP_BP_SA$Date, from=0.01)

sctest(x)

SA_CP_wtr_brk <- strucchange::breakpoints(Water_CP_BP_SA$DOC.TP ~ Water_CP_BP_SA$Date, h=5)
plot(SA_CP_wtr_brk)


### Breakpoint @ Date 07/13

Water_CP_BP_SA <- Water_CP_BP_SA %>%
  mutate(Date=as.Date(Date)) %>%
  mutate(Group = if_else(Date <= "2021-07-13", "One", "Two"))


CP_SA <- Water_CP_BP_SA %>%
  group_by(Group)

dat <- do(CP_SA,
          glance(lm(DOC.TP ~ Date, data = .)))

dat

# Neither are significant. 

###############################################################################
################ Linear Regression between Bloom Stages #######################
###############################################################################

#CN_MB

MB_CN_lm <- All_Combined_BloomStages %>%
  filter(Station == "MB") %>%
  select(Date, Water_CN, CN, BloomStages) %>%
  na.omit() %>%
  group_by(BloomStages, Date) %>%
  summarize(Water_CN = mean(Water_CN),
            CN = mean(CN))

CN_MB_bloomstages <- MB_CN_lm %>%
  group_by(BloomStages)

dat <- do(CN_MB_bloomstages,
          glance(lm(Water_CN ~ CN, data = .)))

## Decline and Lag significant. 

dat

#CN_SA

SA_CN <- All_Combined_BloomStages %>%
  filter(Station == "SA") %>%
  select(Date, Water_CN, CN, BloomStages) %>%
  na.omit() %>%
  group_by(BloomStages, Date) %>%
  summarize(Water_CN = mean(Water_CN),
            CN = mean(CN))


CN_SA_bloomstages <- SA_CN %>%
  group_by(BloomStages)

dat <- do(CN_SA_bloomstages,
          glance(lm(Water_CN ~ CN, data = .)))
dat

## Nothing is significant. 


# NP_MB Seston

MB_NP <- ALL_NP %>%
  filter(Station == "MB") %>%
  na.omit() %>%
  group_by(BloomStages, Date) %>%
  summarize(Seston_NP = mean(Seston_NP),
            Water_NP = mean(Water_NP))

MB_NP_bloomstages <- MB_NP %>%
  group_by(BloomStages)

dat <- do(MB_NP_bloomstages,
          glance(lm(Seston_NP ~ Water_NP, data = .)))

dat

## NP_SA Seston

SA_NP <- ALL_NP %>%
  filter(Station == "SA") %>%
  na.omit() %>%
  group_by(BloomStages, Date) %>%
  summarize(Seston_NP = mean(Seston_NP),
            Water_NP = mean(Water_NP))


SA_NP_bloomstages <- SA_NP %>%
  group_by(BloomStages)

dat <- do(SA_NP_bloomstages,
          glance(lm(Seston_NP ~ Water_NP, data = .)))

dat

## CP_MB 

MB_DOC <- All_Combined_BloomStages %>%
  filter(Station=="MB" )%>%
  select(Water_DOC_uML, Date, BloomStages) %>%
  na.omit()

MB_P <- All_Combined_BloomStages %>%
  filter(Station=="MB" )%>%
  select(Water_TP_uML, Date, BloomStages) %>%
  na.omit()

MB_CP_Water <- full_join(MB_DOC, MB_P, by=c("Date", "BloomStages")) %>%
  mutate(DOC.TP = Water_DOC_uML/Water_TP_uML) %>%
  select(DOC.TP, Date, BloomStages) %>%
  na.omit() %>%
  group_by(Date, BloomStages) %>%
  summarize(DOC.TP = mean(DOC.TP))

MB_SestonC <- Seston_CN %>%
  filter(Station == "MB") %>%
  select(Date, C_uML) %>%
  na.omit()

MB_SestonP <- Seston_P %>%
  filter(Station == "MB") %>%
  select(Date, SestonP_uML) %>%
  na.omit()

MB_SestonCP <- full_join(MB_SestonC, MB_SestonP, by=c("Date")) %>%
  na.omit() %>%
  mutate(Seston_CP = C_uML/SestonP_uML) %>%
  select(Date, Seston_CP)

MB_CP <- full_join(MB_SestonCP, MB_CP_Water, by=c("Date")) %>%
  na.omit()

MB_CP_bloomstages <- MB_CP %>%
  group_by(BloomStages)

dat <- do(MB_CP_bloomstages,
          glance(lm(Seston_CP ~ DOC.TP, data = .)))

dat

## CP_SA 


SA_DOC <- All_Combined_BloomStages %>%
  filter(Station=="SA" )%>%
  select(Water_DOC_uML, Date, BloomStages) %>%
  na.omit()

SA_P <- All_Combined_BloomStages %>%
  filter(Station=="SA" )%>%
  select(Water_TP_uML, Date, BloomStages) %>%
  na.omit()

SA_CP_Water <- full_join(SA_DOC, SA_P, by=c("Date", "BloomStages")) %>%
  mutate(DOC.TP = Water_DOC_uML/Water_TP_uML) %>%
  select(DOC.TP, Date, BloomStages) %>%
  na.omit() %>%
  group_by(Date, BloomStages) %>%
  summarize(DOC.TP = mean(DOC.TP))

SA_SestonC <- Seston_CN %>%
  filter(Station == "SA") %>%
  select(Date, C_uML) %>%
  na.omit()

SA_SestonP <- Seston_P %>%
  filter(Station == "SA") %>%
  select(Date, SestonP_uML) %>%
  na.omit()

SA_SestonCP <- full_join(SA_SestonC, SA_SestonP, by=c("Date")) %>%
  na.omit() %>%
  mutate(Seston_CP = C_uML/SestonP_uML) %>%
  select(Date, Seston_CP)

SA_CP <- full_join(SA_SestonCP, SA_CP_Water, by=c("Date")) %>%
  na.omit()

SA_CP_bloomstages <- SA_CP %>%
  group_by(BloomStages)

dat <- do(SA_CP_bloomstages,
          glance(lm(Seston_CP ~ DOC.TP, data = .)))

dat
###############################################################################
####################### Manuscript Figures ####################################
###############################################################################

# Figure 1 - PC vs Toxins over time

# MB

MB_ToxPC <- Toxins_PC %>%
  pivot_longer(cols = c("Microcystin", "Anatoxin"), names_to = "Toxin", values_to = "Value") %>%
  filter(Station == "MB") %>%
  filter(Date > as.Date("2021-06-01") & Date <= as.Date("2021-11-05")) %>%
  ggplot() + 
  annotate("rect", xmin = as.Date("2021-06-01"), xmax = as.Date("2021-08-07"), ymin = 0, ymax = 8,
           fill = "grey91") +
  annotate(geom="text", x= as.Date("2021-06-05"), y=8.2, label="PrB",
           color="black", size=1) +
  annotate("rect", xmin = as.Date("2021-08-07"), xmax = as.Date("2021-08-12"), ymin = 0, ymax = 8,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-08-09"), y=8.2, label="E1",
           color="black", size=1) +
  annotate("rect", xmin = as.Date("2021-08-12"), xmax = as.Date("2021-08-20"), ymin = 0, ymax = 8,
           fill = "grey91") +
  annotate(geom="text", x= as.Date("2021-08-15"), y=8.2, label="PB1",
           color="black", size=1) +
  annotate("rect", xmin = as.Date("2021-08-20"), xmax = as.Date("2021-08-24"), ymin = 0, ymax = 8,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-08-22"), y=8.2, label="BC1",
           color="black", size=1) +
  annotate("rect", xmin = as.Date("2021-08-24"), xmax = as.Date("2021-09-03"), ymin = 0, ymax = 8,
           fill = "grey91") +
  annotate(geom="text", x= as.Date("2021-08-28"), y=8.2, label="E2",
           color="black", size=1) +
  annotate("rect", xmin = as.Date("2021-09-03"), xmax = as.Date("2021-09-15"), ymin = 0, ymax = 8,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-09-07"), y=8.2, label="PB2",
           color="black", size=1) +
  annotate("rect", xmin = as.Date("2021-09-15"), xmax = as.Date("2021-09-27"), ymin = 0, ymax = 8,
           fill = "grey91") +
  annotate(geom="text", x= as.Date("2021-09-18"), y=8.2, label="BC2",
           color="black", size=1) +
  annotate("rect", xmin = as.Date("2021-09-27"), xmax = as.Date("2021-11-05"), ymin = 0, ymax = 8,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-09-30"), y=8.2, label="PoB",
           color="black", size=1) +
  geom_line(aes(x = Date, y = PC_RFU), size = 0.5, color="black", group = 1) +
  geom_bar(aes(x = Date, y=Value*40, fill=Toxin), stat="identity", position="dodge", width=4, alpha=.4) +
  scale_fill_manual(values=c("orangered3", "cadetblue4"))+
  scale_y_continuous(sec.axis = sec_axis(~./40, name = "")) +
  theme_classic() +
  ggtitle("Missisquoi Bay")+
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(hjust = 0, size=7), axis.title = element_text(size=7), axis.text=element_text(size=6),
        legend.title = element_text(size = 5), 
        legend.key.size = unit(0.1, 'in'), legend.text = element_text(size=4.2), plot.margin = unit(c(0, 0, 0, 0), 'in'))
# SA

SA_ToxPC <- Toxins_PC %>%
  pivot_longer(cols = c("Microcystin", "Anatoxin"), names_to = "Toxin", values_to = "Value") %>%
  filter(Station == "SA") %>%
  filter(Date > as.Date("2021-06-01")& Date <= as.Date("2021-11-05")) %>%
  ggplot() + 
  annotate("rect", xmin = as.Date("2021-06-01"), xmax = as.Date("2021-07-11"), ymin = 0, ymax = 3,
           fill = "grey91") +
  annotate(geom="text", x= as.Date("2021-06-05"), y=3.1, label="PrB",
           color="black", size=1) +
  annotate("rect", xmin = as.Date("2021-07-11"), xmax = as.Date("2021-07-15"), ymin = 0, ymax = 3,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-07-12"), y=3.1, label="BC1",
           color="black", size=1) +
  annotate("rect", xmin = as.Date("2021-07-15"), xmax = as.Date("2021-07-27"), ymin = 0, ymax = 3,
           fill = "grey91") +
  annotate(geom="text", x= as.Date("2021-07-18"), y=3.1, label="E",
           color="black", size=1) +
  annotate("rect", xmin = as.Date("2021-07-27"), xmax = as.Date("2021-08-16"), ymin = 0, ymax = 3,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-07-30"), y=3.1, label="PB",
           color="black", size=1) +
  annotate("rect", xmin = as.Date("2021-08-16"), xmax = as.Date("2021-08-27"), ymin = 0, ymax = 3,
           fill = "grey91") +
  annotate(geom="text", x= as.Date("2021-08-20"), y=3.1, label="BC2",
           color="black", size=1) +
  annotate("rect", xmin = as.Date("2021-08-27"), xmax = as.Date("2021-11-05"), ymin = 0, ymax = 3,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-09-01"), y=3.1, label="PoB",
           color="black", size=1) +
  geom_line(aes(x = Date, y = PC_RFU), size = 0.5, color="black", group = 1) + 
  geom_bar(aes(x = Date, y=Value*10, fill=Toxin), stat="identity", position="dodge", width=4, alpha=.4) +
  scale_fill_manual(values=c("orangered3", "cadetblue4"))+
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "")) +
  theme_classic() +
  ggtitle("St. Albans Bay")+
  ylab("")+
  xlab("")+
  theme(plot.title = element_text(hjust = 0, size=7), axis.title = element_text(size=7), axis.text=element_text(size=6),
        legend.title = element_text(size = 5), 
        legend.key.size = unit(0.1, 'in'), legend.text = element_text(size=4.2), plot.margin = unit(c(0, 0, 0, 0), 'in'))

# ggarrange code for publication


fig <- ggarrange(MB_ToxPC, SA_ToxPC, nrow=2, ncol=1, align="hv", common.legend=TRUE, labels = NULL)

tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/Fig2.tiff", width = 3.5, height = 4, units = "in", res = 600, bg = "white")

annotate_figure(fig, right = textGrob(expression(paste("Toxin Concentration (",~ mu, "g/L)")), rot = 90, vjust = 1, gp = gpar(cex = 0.6)),
                bottom = textGrob("Date", hjust = .5, gp = gpar(cex = 0.6)),
                left = textGrob("Phycocyanin (RFU)", rot = 90, vjust = .25, gp=gpar(cex=0.6)))

dev.off()
###############################################################################
# Figure 2 - MB C:N Plots

# Seston CN

MB_Seston_CN_Redfield <- Seston_CN_BP_MB %>%
  ggplot(aes(x=Date, y=CN, color=Group, group=Group)) +
  annotate("rect", xmin = as.Date("2021-05-19"), xmax = as.Date("2021-07-26"), 
           ymin = 0, ymax = 15,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-26"), xmax = as.Date("2021-08-14"), 
           ymin = 0, ymax = 15,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-14"), xmax = as.Date("2021-09-08"), 
           ymin = 0, ymax = 15,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-08"), xmax = as.Date("2021-11-08"), 
           ymin = 0, ymax = 15,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-21"), y=14, label="L",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-07-28"), y=14, label="E",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-08-16"), y=14, label="S",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-09-10"), y=14, label="D",
           color="black", size=1.5) +
  geom_hline(yintercept=6.625, linetype=2) +
  geom_point(size=0.75) +
  geom_smooth(data=subset(Seston_CN_BP_MB, Group == "Two"), method = "lm", alpha=0.15, size=0.5) +
  scale_color_brewer(palette="Set1") +
  theme_classic() +
  ylab("Seston C:N")+
  xlab("") +
  ggtitle("A") + 
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none")


MB_Seston_CN_NStress <- Seston_CN_BP_MB %>%
  ggplot(aes(x=Date, y=CN, shape=Group, group=Group)) +
  annotate("rect", xmin = as.Date("2021-05-19"), xmax = as.Date("2021-07-26"), 
           ymin = 0, ymax = 17,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-26"), xmax = as.Date("2021-08-14"), 
           ymin = 0, ymax = 17,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-14"), xmax = as.Date("2021-09-08"), 
           ymin = 0, ymax = 17,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-08"), xmax = as.Date("2021-11-08"), 
           ymin = 0, ymax = 17,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-21"), y=16, label="L",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-07-28"), y=16, label="E",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-08-16"), y=16, label="S",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-09-10"), y=16, label="D",
           color="black", size=1.5) +
  geom_hline(yintercept=8.3, linetype=2, size=0.25, color="dodgerblue4") +
  geom_hline(yintercept=14.6, linetype=2, size=0.25, color="indianred4") +
  geom_point(size=0.75) +
  geom_smooth(data=subset(Seston_CN_BP_MB, Group == "Two"), method = "lm", alpha=0.15, size=0.5, color="black") +
  #scale_color_brewer(palette="Set1") +
  theme_classic() +
  ylab("Seston C:N")+
  xlab("") +
  ggtitle("A") + 
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none")


# Water Column CN

MB_Water_CN <- Water_CN_BP_MB %>%
  ggplot(aes(x=Date, y=Water_CN, group=Group, shape=Group)) +
  annotate("rect", xmin = as.Date("2021-05-19"), xmax = as.Date("2021-07-26"), 
           ymin = 0, ymax = 20,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-26"), xmax = as.Date("2021-08-14"), 
           ymin = 0, ymax = 20,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-14"), xmax = as.Date("2021-09-08"), 
           ymin = 0, ymax = 20,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-08"), xmax = as.Date("2021-11-08"), 
           ymin = 0, ymax = 20,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-21"), y=19, label="L",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-07-28"), y=19, label="E",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-08-16"), y=19, label="S",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-09-10"), y=19, label="D",
           color="black", size=1.5) +
  geom_hline(yintercept=8.3, linetype=2, size=0.25, color="dodgerblue4") +
  geom_hline(yintercept=14.6, linetype=2, size=0.25, color="indianred4") +
  geom_point(size=0.75) +
  geom_smooth(method = "lm", alpha=0.15, size=0.5, color="black") +
  theme_classic() +
  ylab("Water Column DOC:TDN")+
  xlab("Date") +
  ggtitle("B") + 
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none") #,
# legend.title = element_text(size = 7), 
# legend.key.size = unit(0.05, 'in'), legend.text = element_text(size=6))
str()
# Seston vs Water CN


MB_CN_Plot <- All_Combined_BloomStages %>%
  filter(Station == "MB") %>%
  ggplot(aes(x=Water_CN, y=CN, group=BloomStages, color=BloomStages, shape=BloomStages))+
  geom_point(alpha=0.4, size=1) +
  geom_smooth(data=subset(All_Combined_BloomStages, BloomStages =="Lag" | BloomStages == "Decline"), method = "lm", 
              alpha=0.15, size=0.5) +
  scale_color_brewer(palette="Set1") +
  theme_classic() +
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none") +#,
  #legend.title = element_text(size = 7), 
  #legend.key.size = unit(0.05, 'in'), legend.text = element_text(size=6)) +
  ylab("Seston C:N") +
  xlab("Water Column DOC:TDN") +
  ggtitle("C")


# Patchwork Figure for Publication

tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/Fig5.tiff", width = 6, height = 5, units = "in", res = 600, bg = "white")
((MB_Seston_CN_Redfield / MB_Water_CN)| MB_CN_Plot) + plot_layout(widths = c(3, 3), heights = unit(c(2.5, 5), c('in', 'null')))
dev.off()

tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/Fig5.1.tiff", width = 6, height = 5, units = "in", res = 600, bg = "white")
((MB_Seston_CN_NStress / MB_Water_CN)| MB_CN_Plot) + plot_layout(widths = c(3, 3), heights = unit(c(2.5, 5), c('in', 'null')))
dev.off()

###############################################################################

# Figure 3 - MB N:P Plots

# Seston NP

MB_Seston_NP <- Seston_NP_BP_MB %>%
  ggplot(aes(x=as.Date(Date), y=Seston_NP)) +
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-26"), ymin = 0, ymax = 60,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-26"), xmax = as.Date("2021-08-14"), ymin = 0, ymax = 60,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-14"), xmax = as.Date("2021-09-08"), ymin = 0, ymax = 60,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-08"), xmax =as.Date("2021-11-10"), ymin = 0, ymax = 60,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-03"), y=58, label="L",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-07-28"), y=58, label="E",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-08-16"), y=58, label="S",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-09-10"), y=58, label="D",
           color="black", size=1.5) +
  geom_hline(yintercept=22, linetype=2, size=0.25, color="dodgerblue4")+
  geom_point(size=0.75) +
  #scale_color_brewer(palette = "Set1") +
  theme_classic() +
  ylab("Seston N:P")+
  xlab("") +
  ggtitle("A") +
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none") +
  ylim(0,60) +
  scale_x_date(breaks = scales::breaks_pretty(10))

# Water Column NP

### Top

MB_Water_NP <- Water_NP_BP_MB %>%
  ggplot(aes(x=as.Date(Date), y=Water_NP)) +
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-26"), ymin = 0, ymax = 120,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-26"), xmax = as.Date("2021-08-14"), ymin = 0, ymax = 120,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-14"), xmax = as.Date("2021-09-08"), ymin = 0, ymax = 120,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-08"), xmax = as.Date("2021-11-10"), ymin = 0, ymax = 120,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-03"), y=115, label="L",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-07-28"), y=115, label="E",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-08-16"), y=115, label="S",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-09-10"), y=115, label="D",
           color="black", size=1.5) +
  geom_hline(yintercept=22, linetype=2, size=0.25, color="dodgerblue4")+
  geom_point(size=0.75) +
  #scale_color_brewer(palette="Set1") +
  theme_classic() +
  ylab("Surface Water TN:TP")+
  xlab("Date") +
  ggtitle("B") + 
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none") +
  ylim(0,120) +
  scale_x_date(breaks = scales::breaks_pretty(10))

# Seston vs Water NP

MB_NP_Plot <- ALL_NP %>%
  filter(Station == "MB") %>%
  ggplot(aes(x=Water_NP, y=Seston_NP, group=BloomStages, color=BloomStages, shape=BloomStages))+
  geom_point(alpha=0.4, size=1) +
  scale_color_brewer(palette="Set1") +
  theme_classic() +
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none")+ #,
  #legend.title = element_text(size = 7), 
  #legend.key.size = unit(0.05, 'in'), legend.text = element_text(size=6 )) +
  ylab("Seston N:P") +
  xlab("Surface Water TN:TP") +
  ggtitle("C") +
  xlim(0,100)

# Patchwork Figure for Publication
tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/Fig7.tiff", width = 6, height = 5, units = "in", res = 600, bg = "white")
((MB_Seston_NP / MB_Water_NP)| MB_NP_Plot) + plot_layout(widths = c(3, 3), heights = unit(c(2.5, 5), c('in', 'null')))
dev.off()



###############################################################################

# Figure 4 - MB C:P Plots

# Seston CP

MB_Seston_CP <- Seston_CP_BP_MB %>%
  ggplot(aes(x=Date, y=Seston_CP)) +
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-26"), ymin = 0, ymax = 450,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-26"), xmax = as.Date("2021-08-14"), ymin = 0, ymax = 450,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-14"), xmax = as.Date("2021-09-08"), ymin = 0, ymax = 450,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-08"), xmax = as.Date("2021-11-08"), ymin = 0, ymax = 450,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-03"), y=425, label="L",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-07-28"), y=425, label="E",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-08-16"), y=425, label="S",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-09-10"), y=425, label="D",
           color="black", size=1.5) +
  geom_hline(yintercept=129, linetype=2, size=0.25, color="dodgerblue4")+
  geom_hline(yintercept=256, linetype=2, size=0.25, color="indianred4")+
  geom_point(size=0.75) +
  #scale_color_brewer(palette = "Set1") +
  theme_classic() +
  ylab("Seston C:P")+
  xlab("") +
  ggtitle("A") +
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none") +
  ylim(0,450) +
  scale_x_date(breaks = scales::breaks_pretty(10))
# Water Column CP

MB_Water_CP <- Water_CP_BP_MB %>%
  ggplot(aes(x=as.Date(Date), y=DOC.TP)) +
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-26"), ymin = 0, ymax = 1000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-26"), xmax = as.Date("2021-08-14"), ymin = 0, ymax = 1000,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-14"), xmax = as.Date("2021-09-08"), ymin = 0, ymax = 1000,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-08"), xmax = as.Date("2021-11-08"), ymin = 0, ymax = 1000,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-03"), y=950, label="L",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-07-28"), y=950, label="E",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-08-16"), y=950, label="S",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-09-10"), y=950, label="D",
           color="black", size=1.5) +
  geom_hline(yintercept=129, linetype=2, size=0.25, color="dodgerblue4")+
  geom_hline(yintercept=256, linetype=2, size=0.25, color="indianred4")+
  geom_point(size=0.75) +
  #scale_color_brewer(palette="Set1") +
  theme_classic() +
  ylab("Water Column DOC:TP")+
  xlab("Date") +
  ggtitle("B") + 
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none") +
  ylim(0,1000) +
  scale_x_date(breaks = scales::breaks_pretty(10))


# Seston vs Water CP

MB_CP_Plot <- MB_CP %>%
  na.omit() %>%
  ggplot(aes(x=DOC.TP, y=Seston_CP, group=BloomStages, color=BloomStages, shape=BloomStages))+
  geom_point(alpha=0.4, size=1) +
  scale_color_brewer(palette="Set1") +
  theme_classic() +
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none")+ #,
  #legend.title = element_text(size = 7), 
  #legend.key.size = unit(0.05, 'in'), legend.text = element_text(size=6 )) +
  ylab("Seston C:P") +
  xlab("Water Column DOC:TP") +
  ggtitle("C")


# Patchwork Figure for Publication

tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/Fig9.tiff", width = 6, height = 5, units = "in", res = 600, bg = "white")
((MB_Seston_CP / MB_Water_CP)| MB_CP_Plot) + plot_layout(widths = c(3, 3), heights = unit(c(2.5, 5), c('in', 'null')))
dev.off()

###############################################################################

# Figure 5 - Variability in C:N:P Boxplots

# CN

MB_CN_Box <- All_Combined_BloomStages %>%
  filter(Station=="MB") %>%
  select(BloomStages, CN)


MB_CN_Box$BloomStages  <- factor(MB_CN_Box$BloomStages, levels = c("Lag", "Exponential", "Stationary", "Decline"))

MB_CN_Plot <- MB_CN_Box %>%
  ggplot(aes(x=BloomStages, y=CN, group=BloomStages, fill=BloomStages))+
  geom_hline(yintercept=6.625, linetype=2) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set1") +
  theme_classic() +
  ylab("Seston C:N")+
  xlab("") +
  ggtitle("A") + 
  theme(axis.title = element_text(size=18), axis.text=element_text(size=13),
        plot.title = element_text(size=18), legend.text = element_text(size=13),
        legend.title=element_text(size=15),
        legend.position="none")

# NP

MB_NP_Box <- All_Combined_BloomStages %>%
  filter(Station=="MB") %>%
  select(BloomStages, SestonP_uML, SestonN_uML) %>%
  mutate(Seston_NP = SestonN_uML/SestonP_uML)

MB_NP_Box$BloomStages  <- factor(MB_NP_Box$BloomStages, levels = c("Lag", "Exponential", "Stationary", "Decline"))


MB_NP_Plot <- MB_NP_Box %>%
  ggplot(aes(x=BloomStages, y=Seston_NP, group=BloomStages, fill=BloomStages))+
  geom_hline(yintercept=16, linetype=2) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set1") +
  theme_classic() +
  ylab("Seston N:P")+
  xlab("") +
  ggtitle("B") + 
  theme(axis.title = element_text(size=18), axis.text=element_text(size=13),
        plot.title = element_text(size=18), legend.text = element_text(size=13),
        legend.title=element_text(size=15),
        legend.position="none") 

# CP

MB_CP_Box <- All_Combined_BloomStages %>%
  filter(Station=="MB") %>%
  select(BloomStages, SestonC_uML, SestonP_uML) %>%
  mutate(Seston_CP = SestonC_uML/SestonP_uML)


MB_CP_Box$BloomStages  <- factor(MB_CP_Box$BloomStages, levels = c("Lag", "Exponential", "Stationary", "Decline"))

MB_CP_Plot <- MB_CP_Box %>%
  ggplot(aes(x=BloomStages, y=Seston_CP, group=BloomStages, fill=BloomStages))+
  geom_hline(yintercept=106, linetype=2) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set1") +
  theme_classic() +
  ylab("Seston C:P")+
  xlab("") +
  ggtitle("C") + 
  theme(axis.title = element_text(size=18), axis.text=element_text(size=13),
        plot.title = element_text(size=18), legend.text = element_text(size=13),
        legend.title=element_text(size=15),
        legend.position="none")

# ggarrange fig for publication

ggarrange(MB_CN_Plot, MB_NP_Plot, MB_CP_Plot, nrow=1)


###############################################################################

# Figure 6 - SA C:N Plots

# Seston CN

SA_Seston_CN <- Seston_CN_BP_SA %>%
  ggplot(aes(x=Date, y=CN, shape=Group, group=Group))+
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-08"), ymin = 0, ymax = 20,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-08"), xmax = as.Date("2021-07-31"), ymin = 0, ymax =20,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-07-31"), xmax = as.Date("2021-10-04"), ymin = 0, ymax = 20,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-10-04"), xmax = as.Date("2021-11-05"), ymin = 0, ymax = 20,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-03"), y=19, label="L",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-07-10"), y=19, label="E",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-08-02"), y=19, label="S",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-10-06"), y=19, label="D",
           color="black", size=1.5) +
  geom_hline(yintercept=8.3, linetype=2, size=0.25, color="dodgerblue4") +
  geom_hline(yintercept=14.6, linetype=2, size=0.25, color="indianred4") +
  geom_point(size=0.75) +
  geom_smooth(method = "lm", alpha=0.15, size=0.5, color="black") +
  #scale_color_brewer(palette="Set1") +
  theme_classic() +
  ylab("Seston C:N")+
  xlab("") +
  ggtitle("A") + 
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none") + 
  scale_x_date(breaks = scales::breaks_pretty(10))

# Water Column CN

SA_Water_CN <- Water_CN_BP_SA %>%
  ggplot(aes(x=Date, y=Water_CN, group=Group, shape=Group))+
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-08"), ymin = 0, ymax = 30,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-08"), xmax = as.Date("2021-07-31"), ymin = 0, ymax =30,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-07-31"), xmax = as.Date("2021-10-04"), ymin = 0, ymax = 30,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-10-04"), xmax = as.Date("2021-11-05"), ymin = 0, ymax = 30,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-03"), y=29, label="L",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-07-10"), y=29, label="E",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-08-02"), y=29, label="S",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-10-06"), y=29, label="D",
           color="black", size=1.5) +
  geom_point(size=0.75) +
  geom_hline(yintercept=8.3, linetype=2, size=0.25, color="dodgerblue4") +
  geom_hline(yintercept=14.6, linetype=2, size=0.25, color="indianred4") +
  geom_smooth(data=subset(Water_CN_BP_SA, Group=="One"),method = "lm", alpha=0.15, size=0.5, color="black") +
  #scale_color_brewer(palette="Set1") +
  theme_classic() +
  ylab("Water Column DOC:TDN")+
  xlab("Date") +
  ggtitle("B") + 
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none") + #,
  #legend.title = element_text(size = 7), 
  #legend.key.size = unit(0.05, 'in'), legend.text = element_text(size=6)) +
  scale_x_date(breaks = scales::breaks_pretty(10))


# Seston vs Water CN

SA_CN_Plot <- All_Combined_BloomStages %>%
  filter(Station == "SA") %>%
  ggplot(aes(x=Water_CN, y=CN, group=BloomStages, color=BloomStages, shape=BloomStages))+
  geom_point(alpha=0.4, size=1) +
  scale_color_brewer(palette="Set1") +
  theme_classic() +
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none") + #,
  #legend.title = element_text(size = 7), 
  #legend.key.size = unit(0.05, 'in'), legend.text = element_text(size=6 )) +
  ylab("Seston DOC:TDN") +
  xlab("Water Column DOC:TDN") +
  ggtitle("C") +
  xlim(8,20)

# Patchwork Figure for Publication
tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/Fig6.tiff", width = 6, height = 5, units = "in", res = 600, bg = "white")
((SA_Seston_CN / SA_Water_CN)| SA_CN_Plot) + plot_layout(widths = c(3, 3), heights = unit(c(2.5, 5), c('in', 'null')))
dev.off()

###############################################################################

# Figure 7 - SA N:P Plots

# Seston NP

SA_Seston_NP <- Seston_NP_BP_SA %>%
  ggplot(aes(x=Date, y=Seston_NP, group=Group, shape=Group)) +
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-08"), ymin = 0, ymax = 100,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-08"), xmax = as.Date("2021-07-31"), ymin = 0, ymax =100,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-07-31"), xmax = as.Date("2021-10-04"), ymin = 0, ymax = 100,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-10-04"), xmax = as.Date("2021-11-05"), ymin = 0, ymax = 100,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-03"), y=95, label="L",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-07-10"), y=95, label="E",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-08-02"), y=95, label="S",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-10-06"), y=95, label="D",
           color="black", size=1.5) +
  geom_hline(yintercept=22, linetype=2, size=0.25, color="dodgerblue4") +
  geom_point(size=0.75) +
  geom_smooth(data=subset(Seston_NP_BP_SA, Group == "Two"), method = "lm", alpha=0.15, size=0.5, color="black") +
  #scale_color_brewer(palette = "Set1") +
  theme_classic() +
  ylab("Seston N:P")+
  xlab("") +
  ggtitle("A") +
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none") + 
  scale_x_date(breaks = scales::breaks_pretty(10))

# Water Column NP

SA_Water_NP <- Water_NP_BP_SA %>%
  ggplot(aes(x=as.Date(Date), y=Water_NP)) +
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-08"), ymin = 0, ymax = 200,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-08"), xmax = as.Date("2021-07-31"), ymin = 0, ymax =200,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-07-31"), xmax = as.Date("2021-10-04"), ymin = 0, ymax = 200,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-10-04"), xmax = as.Date("2021-11-05"), ymin = 0, ymax = 200,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-03"), y=190, label="L",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-07-10"), y=190, label="E",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-08-02"), y=190, label="S",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-10-06"), y=190, label="D",
           color="black", size=1.5) +
  geom_hline(yintercept=22, linetype=2, size=0.25, color="dodgerblue4") +
  geom_point(size=0.75) +
  #scale_color_brewer(palette="Set1") +
  theme_classic() +
  ylab("Surface Water TN:TP")+
  xlab("Date") +
  ggtitle("B") + 
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none") + 
  scale_x_date(breaks = scales::breaks_pretty(10)) +
  ylim(0,200) 

# Seston vs Water NP

SA_NP_Plot <- ALL_NP %>%
  filter(Station == "SA") %>%
  ggplot(aes(x=Water_NP, y=Seston_NP, group=BloomStages, color=BloomStages, shape=BloomStages))+
  geom_point(alpha=0.4, size=1) +
  scale_color_brewer(palette="Set1") +
  theme_classic() +
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none") + #,
  #legend.title = element_text(size = 7), 
  #legend.key.size = unit(0.05, 'in'), legend.text = element_text(size=6 )) +
  ylab("Seston N:P") +
  xlab("Surface Water TN:TP") +
  ggtitle("C") +
  xlim(0,100)


# Patchwork Figure for Publication

tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/Fig8.tiff", width = 6, height = 5, units = "in", res = 600, bg = "white")
((SA_Seston_NP / SA_Water_NP)| SA_NP_Plot) + plot_layout(widths = c(3, 3), heights = unit(c(2.5, 5), c('in', 'null')))
dev.off()

###############################################################################

# Figure 8  - SA C:P Plots

# Seston CP

SA_Seston_CP <- Seston_C_BP_SA %>%
  mutate(Date=as.Date(Date)) %>%
  ggplot(aes(x=Date, y=Seston_CP, shape=Group, group=Group)) +
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-08"), ymin = 0, ymax = 600,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-08"), xmax = as.Date("2021-07-31"), ymin = 0, ymax =600,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-07-31"), xmax = as.Date("2021-10-04"), ymin = 0, ymax = 600,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-10-04"), xmax = as.Date("2021-11-05"), ymin = 0, ymax = 600,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-03"), y=575, label="L",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-07-10"), y=575, label="E",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-08-02"), y=575, label="S",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-10-06"), y=575, label="D",
           color="black", size=1.5) +
  geom_hline(yintercept=129, linetype=2, size=0.25, color="dodgerblue4")+
  geom_hline(yintercept=258, linetype=2, size=0.25, color="indianred4")+
  geom_point(size=0.75) +
  geom_smooth(data=subset(Seston_C_BP_SA, Group == "One"), method = "lm", alpha=0.15, size=0.5, color="black") +
  #scale_color_brewer(palette="Set1") +
  theme_classic() +
  ylab("Seston C:P")+
  xlab("Date") +
  ggtitle("A") +
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7),
        legend.position="none") +
  scale_x_date(breaks = scales::breaks_pretty(10))
# Water Column CP

SA_Water_CP <- Water_CP_BP_SA %>%
  filter(DOC.TP < 5000) %>%
  ggplot(aes(x=as.Date(Date), y=DOC.TP, group=Group, shape=Group)) +
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-08"), ymin = 0, ymax = 2500,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-08"), xmax = as.Date("2021-07-31"), ymin = 0, ymax =2500,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-07-31"), xmax = as.Date("2021-10-04"), ymin = 0, ymax = 2500,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-10-04"), xmax = as.Date("2021-11-05"), ymin = 0, ymax = 2500,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-03"), y=2400, label="L",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-07-10"), y=2400, label="E",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-08-02"), y=2400, label="S",
           color="black", size=1.5) +
  annotate(geom="text", x= as.Date("2021-10-06"), y=2400, label="D",
           color="black", size=1.5) +
  geom_hline(yintercept=129, linetype=2, size=0.25, color="dodgerblue4")+
  geom_hline(yintercept=258, linetype=2, size=0.25, color="indianred4")+
  geom_point(size=0.75) +
  #scale_color_brewer(palette="Set1") +
  theme_classic() +
  ylab("Water Column DOC:TP")+
  xlab("Date") +
  ggtitle("B") +
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none") +
  scale_x_date(breaks = scales::breaks_pretty(10))


# Seston vs Water CP

SA_CP_Plot <- SA_CP%>%
  na.omit() %>%
  ggplot(aes(x=DOC.TP, y=Seston_CP, group=BloomStages, color=BloomStages, shape=BloomStages))+
  geom_point(alpha=0.4, size=1) +
  scale_color_brewer(palette="Set1") +
  theme_classic() +
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="none")+#,
  #legend.title = element_text(size = 7), 
  #legend.key.size = unit(0.05, 'in'), legend.text = element_text(size=6 )) +
  ylab("Seston C:P") +
  xlab("Water Column DOC:TP") +
  ggtitle("C")


# Patchwork Figure for Publication
tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/Fig10.tiff", width = 6, height = 5, units = "in", res = 600, bg = "white")
((SA_Seston_CP / SA_Water_CP)| SA_CP_Plot) + plot_layout(widths = c(3, 3), heights = unit(c(2.5, 5), c('in', 'null')))
dev.off()




###############################################################################

# Figure 9 - SA Variability in CNP
str(All_Combined_BloomStages)
# CN

SA_CN_Box <- All_Combined_BloomStages %>%
  filter(Station=="SA") %>%
  select(BloomStages, CN)

SA_CN_Box$BloomStages  <- factor(SA_CN_Box$BloomStages, levels = c("Lag", "Exponential", "Stationary", "Decline"))

SA_CN_Plot <- SA_CN_Box %>%
  ggplot(aes(x=BloomStages, y=CN, group=BloomStages, fill=BloomStages))+
  geom_hline(yintercept=6.625, linetype=2) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set1") +
  theme_classic() +
  ylab("Seston C:N")+
  xlab("") +
  ggtitle("A") + 
  theme(axis.title = element_text(size=18), axis.text=element_text(size=13),
        plot.title = element_text(size=18), legend.text = element_text(size=13),
        legend.title=element_text(size=15),
        legend.position="none")

# NP

SA_NP_Box <- All_Combined_BloomStages %>%
  filter(Station=="SA") %>%
  select(BloomStages, SestonP_uML, SestonN_uML) %>%
  mutate(Seston_NP = SestonN_uML/SestonP_uML)

SA_NP_Box$BloomStages  <- factor(SA_NP_Box$BloomStages, levels = c("Lag", "Exponential", "Stationary", "Decline"))


SA_NP_Plot <- SA_NP_Box %>%
  ggplot(aes(x=BloomStages, y=Seston_NP, group=BloomStages, fill=BloomStages))+
  geom_hline(yintercept=16, linetype=2) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set1") +
  theme_classic() +
  ylab("Seston N:P")+
  xlab("") +
  ggtitle("B") + 
  theme(axis.title = element_text(size=18), axis.text=element_text(size=13),
        plot.title = element_text(size=18), legend.text = element_text(size=13),
        legend.title=element_text(size=15),
        legend.position="none") +
  ylim(0,100)

# CP

SA_CP_Box <- All_Combined_BloomStages %>%
  filter(Station=="SA") %>%
  select(BloomStages, SestonC_uML, SestonP_uML) %>%
  mutate(Seston_CP = SestonC_uML/SestonP_uML)

SA_CP_Box$BloomStages  <- factor(SA_CP_Box$BloomStages, levels = c("Lag", "Exponential", "Stationary", "Decline"))
str(MB_NP)

SA_CP_Plot <- SA_CP_Box %>%
  ggplot(aes(x=BloomStages, y=Seston_CP, group=BloomStages, fill=BloomStages))+
  geom_hline(yintercept=106, linetype=2) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set1") +
  theme_classic() +
  ylab("Seston C:P")+
  xlab("") +
  ggtitle("C") + 
  theme(axis.title = element_text(size=18), axis.text=element_text(size=13),
        plot.title = element_text(size=18), legend.text = element_text(size=13),
        legend.title=element_text(size=15),
        legend.position="none") +
  ylim(0,1000)


# ggarrange for publication fig

ggarrange(SA_CN_Plot, SA_NP_Plot, SA_CP_Plot, nrow=1)

################################################################################
############################# SI FIGURES #######################################
################################################################################


### Missisquoi Bay Nutrients


## Missisquoi Bay


# Phosphorus

MB_P <- All_Combined %>%
  filter(Station == "MB") %>%
  select(Location, Date, Water_TP_mgL) %>%
  mutate(Water_TP_ugL = Water_TP_mgL * 1000) %>%
  group_by(Location, Date) %>%
  summarize(Water_TP_ugL = mean(Water_TP_ugL)) %>%
  pivot_wider(names_from="Location", values_from = "Water_TP_ugL") %>%
  select(-IWC) %>%
  na.omit()

str(MB_P)
MB_SesP <- All_Combined %>%
  select(SestonP_ugL, Date, Station) %>%
  filter(Station == "MB") %>%
  na.omit() %>%
  group_by(Date) %>%
  dplyr::summarize(SestonP_ugL = mean(SestonP_ugL))

MB_P <- full_join(MB_P, MB_SesP, by=c("Date")) %>%
  pivot_longer(cols=c("SestonP_ugL", "bottom", "surface"), names_to = "Location", values_to = "TP_ugL") %>%
  mutate(Location = if_else(Location == "bottom", "Water TP - 2.5m", Location),
         Location = if_else(Location == "surface", "Water TP - 0.5m", Location),
         Location = if_else(Location == "SestonP_ugL", "Seston TP - IWC", Location))

MB_P_Fig <- MB_P %>%
  mutate(Date=as.Date(Date))%>%
  filter(Date > as.Date("2021-05-01") & Date <= as.Date("2021-11-05")) %>%
  ggplot() + 
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-26"), ymin = 0, ymax = 200,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-26"), xmax = as.Date("2021-08-14"), ymin = 0, ymax = 200,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-14"), xmax = as.Date("2021-09-08"), ymin = 0, ymax = 200,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-08"), xmax = as.Date("2021-11-08"), ymin = 0, ymax = 200,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-03"), y=195, label="L",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-07-28"), y=195, label="E",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-08-16"), y=195, label="S",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-09-10"), y=195, label="D",
           color="black", size=2) +
  geom_line(aes(x = Date, y = TP_ugL, color=Location), size = 0.75) + 
  geom_point(aes(x=Date, y=TP_ugL, shape=Location, color=Location), fill="white", size=1) +
  scale_color_manual(values=c("indianred", "darkolivegreen4", "darkolivegreen4"))+
  scale_shape_manual(values=c(21, 22, 24))+
  theme_classic() +
  ylab(expression(paste("Phosphorus (",~ mu, "g/L)")))+
  xlab("") +
  theme(axis.title = element_text(size=8), axis.text=element_text(size=7),
        legend.title = element_text(size = 8), 
        legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=7)) +
  scale_x_date(breaks = scales::breaks_pretty(10))




## Nitrogen

MB_N <- All_Combined %>%
  filter(Station == "MB") %>%
  select(Location, Date, Water_TN_mgL) %>%
  group_by(Location, Date) %>%
  summarize(Water_TN_mgL = mean(Water_TN_mgL)) %>%
  pivot_wider(names_from="Location", values_from = "Water_TN_mgL") %>%
  select(-IWC) %>%
  na.omit()

MB_TDN <- All_Combined %>%
  select(Water_TDN_mgL, Date, Station) %>%
  filter(Station == "MB") %>%
  na.omit() %>%
  group_by(Date) %>%
  summarize(Water_TDN_mgL = mean(Water_TDN_mgL))


MB_SesN <- All_Combined %>%
  select(N_mgL, Date, Station) %>%
  filter(Station == "MB") %>%
  na.omit() %>%
  group_by(Date) %>%
  summarize(N_mgL = mean(N_mgL))

MB_N <- full_join(MB_N, MB_SesN, by=c("Date")) %>%
  full_join(., MB_TDN, by=c("Date")) %>%
  pivot_longer(cols=c("N_mgL", "bottom", "surface", "Water_TDN_mgL"), names_to = "Location", values_to = "TN_mgL") %>%
  mutate(Location = if_else(Location == "bottom", "Water TN - 2.5m", Location),
         Location = if_else(Location == "surface", "Water TN - 0.5m", Location),
         Location = if_else(Location == "N_mgL", "Seston TN - IWC", Location),
         Location = if_else(Location == "Water_TDN_mgL", "Water TDN - IWC", Location))

MB_N_Fig <- MB_N %>%
  mutate(Date=as.Date(Date))%>%
  filter(Date > as.Date("2021-06-01") & Date <= as.Date("2021-11-05")) %>%
  ggplot() + 
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-26"), ymin = 0, ymax = 3,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-26"), xmax = as.Date("2021-08-14"), ymin = 0, ymax =3,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-14"), xmax = as.Date("2021-09-08"), ymin = 0, ymax = 3,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-08"), xmax = as.Date("2021-11-05"), ymin = 0, ymax = 3,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-03"), y=2.9, label="L",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-07-28"), y=2.9, label="E",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-08-16"), y=2.9, label="S",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-09-10"), y=2.9, label="D",
           color="black", size=2) +
  geom_line(aes(x = Date, y = TN_mgL, color=Location), size = 0.75) + 
  geom_point(aes(x=Date, y=TN_mgL, shape=Location, color=Location), fill="white", size=1) +
  scale_color_manual(values=c("indianred", "dodgerblue4", "darkolivegreen4", "darkolivegreen4"))+
  scale_shape_manual(values=c(21, 21, 22, 24))+
  theme_classic() +
  ylab("Nitrogen (mg/L)")+
  xlab("") +
  theme(axis.title = element_text(size=8), axis.text=element_text(size=7),
        legend.title = element_text(size = 8), 
        legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=7)) +
  scale_x_date(breaks = scales::breaks_pretty(10))


## Carbon


MB_C <- All_Combined %>%
  filter(Station == "MB") %>%
  select(Date,  Water_DOC_mgL)%>%
  na.omit() %>%
  group_by(Date) %>%
  summarize(Water_DOC_mgL = mean(Water_DOC_mgL))

MB_SesC <- All_Combined %>%
  select(C_mgL, Date, Station) %>%
  filter(Station == "MB") %>%
  na.omit() %>%
  group_by(Date) %>%
  summarize(C_mgL = mean(C_mgL))

MB_C <- full_join(MB_C, MB_SesC, by=c("Date")) %>%
  pivot_longer(cols=c("C_mgL", "Water_DOC_mgL"), names_to = "Location", values_to = "C_mgL") %>%
  mutate(Location = if_else(Location == "Water_DOC_mgL", "Water DOC - IWC", Location),
         Location = if_else(Location == "C_mgL", "Seston C - IWC", Location))

MB_C_Fig <- MB_C %>%
  mutate(Date=as.Date(Date))%>%
  filter(Date > as.Date("2021-06-01") & Date <= as.Date("2021-11-05")) %>%
  ggplot() + 
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-26"), ymin = 0, ymax = 9,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-26"), xmax = as.Date("2021-08-14"), ymin = 0, ymax = 9,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-08-14"), xmax = as.Date("2021-09-08"), ymin = 0, ymax = 9,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-09-08"), xmax = as.Date("2021-11-05"), ymin = 0, ymax = 9,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-03"), y=8.8, label="L",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-07-28"), y=8.8, label="E",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-08-16"), y=8.8, label="S",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-09-10"), y=8.8, label="D",
           color="black", size=2) +
  geom_line(aes(x = Date, y = C_mgL, color=Location), size = 0.75) + 
  geom_point(aes(x=Date, y=C_mgL, shape=Location, color=Location), fill="white", size=1) +
  scale_color_manual(values=c("indianred", "dodgerblue4"))+
  scale_shape_manual(values=c(21, 21))+
  theme_classic() +
  ylab("Carbon (mg/L)")+
  xlab("") +
  theme(axis.title = element_text(size=8), axis.text=element_text(size=7),
        legend.title = element_text(size = 8), 
        legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=7)) +
  scale_x_date(breaks = scales::breaks_pretty(10))



#### MB Figure 

tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/MB_Nutrients_SI.tiff", width = 6, height = 9, units = "in", res = 600, bg = "white")
ggarrange(MB_C_Fig, MB_N_Fig, MB_P_Fig, nrow=3, ncol=1, align="hv", labels = NULL)
dev.off()

###############################################################################


## St. Albans

# Carbon


SA_C <- All_Combined %>%
  filter(Station == "SA") %>%
  select(Date,  Water_DOC_mgL)%>%
  na.omit() %>%
  group_by(Date) %>%
  summarize(Water_DOC_mgL = mean(Water_DOC_mgL))

SA_SesC <- All_Combined %>%
  select(C_mgL, Date, Station) %>%
  filter(Station == "SA") %>%
  na.omit() %>%
  group_by(Date) %>%
  summarize(C_mgL = mean(C_mgL))

SA_C <- full_join(SA_C, SA_SesC, by=c("Date")) %>%
  pivot_longer(cols=c("C_mgL", "Water_DOC_mgL"), names_to = "Location", values_to = "C_mgL") %>%
  mutate(Location = if_else(Location == "Water_DOC_mgL", "Water DOC - IWC", Location),
         Location = if_else(Location == "C_mgL", "Seston C - IWC", Location))

SA_C_Fig <- SA_C %>%
  mutate(Date=as.Date(Date))%>%
  filter(Date > as.Date("2021-06-01") & Date <= as.Date("2021-11-05")) %>%
  ggplot() + 
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-08"), ymin = 0, ymax = 5,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-08"), xmax = as.Date("2021-07-31"), ymin = 0, ymax = 5,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-07-31"), xmax = as.Date("2021-10-04"), ymin = 0, ymax = 5,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-10-04"), xmax = as.Date("2021-11-05"), ymin = 0, ymax = 5,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-03"), y=4.9, label="L",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-07-10"), y=4.9, label="E",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-08-02"), y=4.9, label="S",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-10-06"), y=4.9, label="D",
           color="black", size=2) +
  geom_line(aes(x = Date, y = C_mgL, color=Location), size = 0.75) + 
  geom_point(aes(x=Date, y=C_mgL, shape=Location, color=Location), fill="white", size=1) +
  scale_color_manual(values=c("indianred", "dodgerblue4"))+
  scale_shape_manual(values=c(21, 21))+
  theme_classic() +
  ylab("Carbon (mg/L)")+
  xlab("") +
  theme(axis.title = element_text(size=8), axis.text=element_text(size=7),
        legend.title = element_text(size = 8), 
        legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=7)) +
  scale_x_date(breaks = scales::breaks_pretty(10))



# Nitrogen

SA_N <- All_Combined %>%
  filter(Station == "SA") %>%
  select(Location, Date, Water_TN_mgL) %>%
  group_by(Location, Date) %>%
  summarize(Water_TN_mgL = mean(Water_TN_mgL)) %>%
  pivot_wider(names_from="Location", values_from = "Water_TN_mgL") %>%
  select(-IWC) %>%
  na.omit()


SA_TDN <- All_Combined %>%
  select(Water_TDN_mgL, Date, Station) %>%
  filter(Station == "SA") %>%
  na.omit() %>%
  group_by(Date) %>%
  summarize(Water_TDN_mgL = mean(Water_TDN_mgL))

SA_SesN <- All_Combined %>%
  select(N_mgL, Date, Station) %>%
  filter(Station == "SA") %>%
  na.omit() %>%
  group_by(Date) %>%
  summarize(N_mgL = mean(N_mgL))

SA_N <- full_join(SA_N, SA_SesN, by=c("Date")) %>%
  full_join(., SA_TDN, by=c("Date")) %>%
  pivot_longer(cols=c("N_mgL", "bottom", "surface", "Water_TDN_mgL"), names_to = "Location", values_to = "TN_mgL") %>%
  mutate(Location = if_else(Location == "bottom", "Water TN - 2.5m", Location),
         Location = if_else(Location == "surface", "Water TN - 0.5m", Location),
         Location = if_else(Location == "N_mgL", "Seston TN - IWC", Location),
         Location = if_else(Location == "Water_TDN_mgL", "Water TDN - IWC", Location))
SA_N_Fig <- SA_N %>%
  mutate(Date=as.Date(Date))%>%
  filter(Date > as.Date("2021-06-01") & Date <= as.Date("2021-11-05")) %>%
  ggplot() + 
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-08"), ymin = 0, ymax = 1,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-08"), xmax = as.Date("2021-07-31"), ymin = 0, ymax =1,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-07-31"), xmax = as.Date("2021-10-04"), ymin = 0, ymax = 1,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-10-04"), xmax = as.Date("2021-11-05"), ymin = 0, ymax = 1,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-03"), y=.98, label="L",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-07-10"), y=.98, label="E",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-08-02"), y=.98, label="S",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-10-06"), y=.98, label="D",
           color="black", size=2) +
  geom_line(aes(x = Date, y = TN_mgL, color=Location), size = 0.75) + 
  geom_point(aes(x=Date, y=TN_mgL, shape=Location, color=Location), fill="white", size=1) +
  scale_color_manual(values=c("indianred", "dodgerblue4", "darkolivegreen4", "darkolivegreen4"))+
  scale_shape_manual(values=c(21, 21, 22, 24))+
  theme_classic() +
  ylab("Nitrogen (mg/L)")+
  xlab("")+
  theme(axis.title = element_text(size=8), axis.text=element_text(size=7),
        legend.title = element_text(size = 8), 
        legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=7)) +
  scale_x_date(breaks = scales::breaks_pretty(10))



# Phosphorus

SA_P <- All_Combined %>%
  filter(Station == "SA") %>%
  select(Location, Date, Water_TP_mgL) %>%
  mutate(Water_TP_ugL = Water_TP_mgL * 1000) %>%
  group_by(Location, Date) %>%
  summarize(Water_TP_ugL = mean(Water_TP_ugL)) %>%
  pivot_wider(names_from="Location", values_from = "Water_TP_ugL") %>%
  select( -IWC) %>%
  na.omit()

SA_SesP <- All_Combined %>%
  select(SestonP_ugL, Date, Station) %>%
  filter(Station == "SA") %>%
  na.omit() %>%
  group_by(Date) %>%
  dplyr::summarize(SestonP_ugL = mean(SestonP_ugL))

SA_P <- full_join(SA_P, SA_SesP, by=c("Date")) %>%
  pivot_longer(cols=c("SestonP_ugL", "bottom", "surface"), names_to = "Location", values_to = "TP_ugL") %>%
  mutate(Location = if_else(Location == "bottom", "Water TP - 4.5m", Location),
         Location = if_else(Location == "surface", "Water TP - 0.5m", Location),
         Location = if_else(Location == "SestonP_ugL", "Seston TP - IWC", Location))

SA_P_Fig <- SA_P %>%
  mutate(Date=as.Date(Date))%>%
  filter(Date > as.Date("2021-06-01") & Date <= as.Date("2021-11-05")) %>%
  ggplot() + 
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-08"), ymin = 0, ymax = 100,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-07-08"), xmax = as.Date("2021-07-31"), ymin = 0, ymax =100,
           fill = "grey98") +
  annotate("rect", xmin = as.Date("2021-07-31"), xmax = as.Date("2021-10-04"), ymin = 0, ymax = 100,
           fill = "grey91") +
  annotate("rect", xmin = as.Date("2021-10-04"), xmax = as.Date("2021-11-05"), ymin = 0, ymax = 100,
           fill = "grey98") +
  annotate(geom="text", x= as.Date("2021-05-03"), y=98, label="L",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-07-10"), y=98, label="E",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-08-02"), y=98, label="S",
           color="black", size=2) +
  annotate(geom="text", x= as.Date("2021-10-06"), y=98, label="D",
           color="black", size=2) +
  geom_line(aes(x = Date, y = TP_ugL, color=Location), size = 0.75) + 
  geom_point(aes(x=Date, y=TP_ugL, shape=Location, color=Location), fill="white", size=1) +
  scale_color_manual(values=c("indianred", "darkolivegreen4", "darkolivegreen4"))+
  scale_shape_manual(values=c(21, 22, 24))+
  theme_classic() +
  ylab(expression(paste("Phosphorus (",~ mu, "g/L)")))+
  xlab("") +
  theme(axis.title = element_text(size=8), axis.text=element_text(size=7),
        legend.title = element_text(size = 8), 
        legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=7)) +
  scale_x_date(breaks = scales::breaks_pretty(10))


#### SA Figure 
tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/SA_Nutrients_SI.tiff", width = 6, height = 9, units = "in", res = 600, bg = "white")
ggarrange(SA_C_Fig, SA_N_Fig, SA_P_Fig, nrow=3, ncol=1,  align="hv", labels = NULL)
dev.off()



###############################################################################
####################### Toxin Analysis ########################################
###############################################################################

###### MC compared to C:N per Bay

Toxins <- All_Combined %>%
  select(Anatoxin, Microcystin, Date, Station) %>%
  na.omit()

CN <- All_Combined %>%
  select(CN, Date, Station) %>%
  na.omit()

Toxins_CN <- full_join(Toxins, CN)

MC_CN <- Toxins_CN %>%
  mutate(Toxin = if_else(Microcystin > 0 & Anatoxin == 0, "MC", "None"), 
         Toxin = if_else(Anatoxin > 0 & Microcystin == 0, "ATX", Toxin),
         Toxin = if_else(Anatoxin > 0 & Microcystin > 0, "Both", Toxin)) %>%
  ggplot(aes(x=as.Date(Date), y=CN, group=Station, linetype=Station)) +
  #annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 4.5, ymax = 8.3,
  #       fill = "grey70", alpha=0.2) +
  annotate("text", x = as.Date("2021-05-21"), y = 7.75, label = "N-Replete", size = 1.5, color="dodgerblue4") +
  #annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 8.3, ymax = 14.6,
  #       fill = "grey50", alpha=0.2) +
  annotate("text", x = as.Date("2021-05-22"), y = 13.95, label = "N-Stressed", size = 1.5, color="mediumpurple4") +
  #annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 14.6, ymax = 16,
  #       fill = "grey25", alpha=0.2) +
  annotate("text", x = as.Date("2021-05-21"), y = 15.45, label = "N-Deplete", size = 1.5, color="indianred4") +
  geom_hline(yintercept = 8.3, linetype=3, color="aquamarine4", linewidth=0.25) +
  geom_hline(yintercept = 14.6, linetype=3, color="indianred4", linewidth=0.25) +
  geom_line(size=0.5) +
  geom_point(aes(color=Toxin), size=0.75) +
  theme_classic() +
  scale_color_manual(values=c("orangered3", "darkmagenta", "dodgerblue3", "black")) +
  xlab("") +
  ylab("Seston C:N") +
  ggtitle("A")+
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="top",
        legend.title = element_text(size = 7), 
        legend.key.size = unit(0.05, 'in'), legend.text = element_text(size=6 ),
        plot.margin = unit(c(0, 0.05, 0, 0.05), 'in')) +
  scale_x_date(breaks = scales::breaks_pretty(10))

###### MC compared to C:P per NBay

CP <- All_Combined %>%
  mutate(CP = SestonC_uML/SestonP_uML) %>%
  select(CP, Date, Station) %>%
  na.omit()

Toxins_CP <- full_join(Toxins, CP)

MC_CP <- Toxins_CP %>%
  mutate(Toxin = if_else(Microcystin > 0 & Anatoxin == 0, "MC", "None"), 
         Toxin = if_else(Anatoxin > 0 & Microcystin == 0, "ATX", Toxin),
         Toxin = if_else(Anatoxin > 0 & Microcystin > 0, "Both", Toxin)) %>%
  ggplot(aes(x=as.Date(Date), y=CP, group=Station, linetype=Station)) +
  #annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 0, ymax = 129,
  #         fill = "grey70", alpha=0.2) +
  annotate("text", x = as.Date("2021-05-21"), y = 100, label = "P-Replete", size = 1.5, color="dodgerblue4") +
  #annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 129, ymax = 258,
  #         fill = "grey50", alpha=0.2) +
  annotate("text", x = as.Date("2021-05-22"), y = 230, label = "P-Stressed", size = 1.5, color="mediumpurple4") +
  #annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 258, ymax = 600,
  #         fill = "grey25", alpha=0.2) +
  annotate("text", x = as.Date("2021-05-21"), y = 570, label = "P-Deplete", size = 1.5, color="indianred4") +
  geom_hline(yintercept = 129, linetype=3, color="dodgerblue4", linewidth=0.25) +
  geom_hline(yintercept = 258, linetype=3, color="indianred4", linewidth=0.25) +
  geom_line(size=0.5) +
  geom_point(aes(color=Toxin), size=0.75) +
  scale_color_manual(values=c("orangered3", "darkmagenta", "dodgerblue3", "black")) +
  theme_classic() +
  ylab("Seston C:P") +
  xlab("") +
  ggtitle("B")+
  ylim(0,600)+
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="bottom",
        legend.title = element_text(size = 7), 
        legend.key.size = unit(0.05, 'in'), legend.text = element_text(size=6),
        plot.margin = unit(c(0, 0.05, 0, 0.05), 'in'))+
  scale_x_date(breaks = scales::breaks_pretty(10))


###### MV compared to N:P per NBay

NP <- All_Combined %>%
  mutate(NP = N_uML/SestonP_uML) %>%
  select(NP, Date, Station) %>%
  na.omit()

Toxins_NP <- full_join(Toxins, NP)

MC_NP <- Toxins_NP %>%
  mutate(Toxin = if_else(Microcystin > 0 & Anatoxin == 0, "MC", "None"), 
         Toxin = if_else(Anatoxin > 0 & Microcystin == 0, "ATX", Toxin),
         Toxin = if_else(Anatoxin > 0 & Microcystin > 0, "Both", Toxin)) %>%
  ggplot(aes(x=as.Date(Date), y=NP, group=Station, linetype=Station)) +
  #annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 0, ymax = 20,
  #         fill = "grey70", alpha=0.2) +
  annotate("text", x = as.Date("2021-05-21"), y = 17, label = "N-Deplete", size = 1.5, color="dodgerblue4") +
  #annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 20, ymax = 50,
  #         fill = "grey50", alpha=0.2) +
  #annotate("text", x = as.Date("2021-05-27"), y = 47, label = "N or P Deplete", size = 1.5, color="mediumpurple4") +
  #annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 50, ymax = 70,
  #         fill = "grey25", alpha=0.2) +
  annotate("text", x = as.Date("2021-05-21"), y = 77, label = "P-Deplete", size = 1.5, color="indianred4") +
  geom_hline(yintercept = 22, linetype=3, color="dodgerblue4", linewidth=0.25) +
  geom_line(size=0.5) +
  geom_point(aes(color=Toxin), size=0.75) +
  scale_color_manual(values=c("orangered3", "darkmagenta", "dodgerblue3", "black")) +
  theme_classic() +
  ylab("Seston N:P") +
  xlab("") +
  ggtitle("C")+
  ylim(0,80) +
  theme(axis.title = element_text(size=7), axis.text=element_text(size=6),
        plot.title = element_text(size=7), 
        legend.position="bottom",
        legend.title = element_text(size = 7), 
        legend.key.size = unit(0.05, 'in'), legend.text = element_text(size=6),
        plot.margin = unit(c(0, 0.05, 0, 0.05), 'in'))+
  scale_x_date(breaks = scales::breaks_pretty(10))


tiff("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/Fig4.tiff", width = 7, height = 2.75, units = "in", res = 600, bg = "white")
ggarrange(MC_CN, MC_CP, MC_NP, ncol=3, nrow=1, common.legend = TRUE)
dev.off()



###### SA compared to C:N per Bay

Toxins <- All_Combined %>%
  select(Anatoxin, Microcystin, Date, Station) %>%
  na.omit()

CN <- All_Combined %>%
  select(CN, Date, Station) %>%
  na.omit()

Toxins_CN <- full_join(Toxins, CN)

ATX_CN <- Toxins_CN %>%
  filter(Date > as.Date("2021-07-01")) %>%
  mutate(Toxin = if_else(Anatoxin > 0, "Yes", "No")) %>%
  ggplot(aes(x=as.Date(Date), y=CN, group=Station, linetype=Station)) +
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 4.5, ymax = 8.3,
           fill = "grey70", alpha=0.2) +
  annotate("label", x = as.Date("2021-05-11"), y = 7.95, label = "N-Replete") +
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 8.3, ymax = 14.6,
           fill = "grey50", alpha=0.2) +
  annotate("label", x = as.Date("2021-05-11"), y = 14.15, label = "N-Stressed") +
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 14.6, ymax = 16,
           fill = "grey25", alpha=0.2) +
  annotate("label", x = as.Date("2021-05-11"), y = 15.65, label = "N-Deplete") +
  geom_line(size=1.1) +
  geom_point(aes(color=Toxin), size=4) +
  scale_color_manual(values=c("black", "orangered3")) +
  theme_classic() +
  ylab("Seston C:N") +
  xlab("Date") +
  theme(axis.title = element_text(size=20), axis.text=element_text(size=18), legend.position = "none") +
  scale_x_date(breaks = scales::breaks_pretty(10))


###### MC compared to C:P per NBay

CP <- All_Combined %>%
  mutate(CP = SestonC_uML/SestonP_uML) %>%
  select(CP, Date, Station) %>%
  na.omit()

Toxins_CP <- full_join(Toxins, CP)
?annotate()
ATX_CP <- Toxins_CP %>%
  filter(Date > as.Date("2021-07-01")) %>%
  mutate(Toxin = if_else(Anatoxin > 0, "Yes", "No")) %>%
  ggplot(aes(x=as.Date(Date), y=CP, group=Station, linetype=Station)) +
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 0, ymax = 129,
           fill = "grey70", alpha=0.2) +
  annotate("label", x = as.Date("2021-05-11"), y = 115, label = "P-Replete") +
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 129, ymax = 258,
           fill = "grey50", alpha=0.2) +
  annotate("label", x = as.Date("2021-05-11"), y = 245, label = "P-Stressed") +
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 258, ymax = 600,
           fill = "grey25", alpha=0.2) +
  annotate("label", x = as.Date("2021-05-11"), y = 585, label = "P-Deplete") +
  geom_line(size=1.1) +
  geom_point(aes(color=Toxin), size=4) +
  scale_color_manual(values=c("black", "orangered3")) +
  theme_classic() +
  ylab("Seston C:P") +
  xlab("Date") +
  theme(axis.title = element_text(size=20), axis.text=element_text(size=18), legend.position = "none") +
  scale_x_date(breaks = scales::breaks_pretty(10))

###### MC compared to N:P per NBay

NP <- All_Combined %>%
  mutate(NP = N_uML/SestonP_uML) %>%
  select(NP, Date, Station) %>%
  na.omit()

Toxins_NP <- full_join(Toxins, NP)

ATX_NP<-Toxins_NP %>%
  mutate(Toxin = if_else(Anatoxin > 0, "Yes", "No")) %>%
  ggplot(aes(x=as.Date(Date), y=NP, group=Station, linetype=Station)) +
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 0, ymax = 22,
           fill = "grey70", alpha=0.2) +
  annotate("label", x = as.Date("2021-05-11"), y = 20, label = "N-Deplete") +
  annotate("rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-11-15"), ymin = 22, ymax = 70,
           fill = "grey25", alpha=0.2) +
  annotate("label", x = as.Date("2021-05-11"), y = 68, label = "P-Deplete") +
  geom_line(size=1.1) +
  geom_point(aes(color=Toxin), size=4) +
  scale_color_manual(values=c("black", "orangered3")) +
  theme_classic() +
  ylab("Seston N:P") +
  xlab("Date") +
  theme(axis.title = element_text(size=17), axis.text=element_text(size=15), legend.position = "none") +
  scale_x_date(breaks = scales::breaks_pretty(10))

jpeg("C:/Users/kwarn/Desktop/Stoich Chapter/Cyano_Stoichiometry/Figures/ATX_NutrientLim.jpeg", width = 30, height = 10, units = "in", res = 1000, bg = "white")
ggarrange(ATX_CN, ATX_CP, ATX_NP, ncol=3, nrow=1)
dev.off()
