library(ggplot2)
library(ggpubr)
library(dplyr)
library(trend)
library(ggforce)

### Data ###
e_daily_q<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/East_Q.csv")
no3<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/East_NO3.csv")
slate_syn_no3<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/Slate_summer_2022_synoptic.csv")
snotel<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/CB_snotel_cleaned.csv")
n_dep<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/CB_N_deposition_cleaned.csv")
wrtds_annual<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/WRTDS_annual_cleaned.csv")
wrtds_monthly<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/WRTDS_monthly_cleaned.csv")

### Cleaning ###
e_daily_q$Q<- e_daily_q$X_00060_00003

no3$date<-as.Date(no3$date, format = "%m/%d/%Y")

no3<- no3 %>%
  mutate(month = month(date))

n_dep$NO3[n_dep$NO3 == -9]<- NA

n_dep$month<-substr(n_dep$yrmonth, nchar(n_dep$yrmonth)-1, nchar(n_dep$yrmonth))

peak_swe<-aggregate(swe~wy, data = snotel, FUN = max)
peak_swe<-peak_swe %>% 
  slice(14:43)
wrtds_annual$peak_swe<-peak_swe
wrtds_annual$peak_swe_mm<-wrtds_annual$peak_swe*25.4

avg_dep<-aggregate(NO3~wy, data = n_dep, FUN = mean)
wrtds_annual<-left_join(wrtds_annual, avg_dep, by = "wy")
wrtds_annual<-wrtds_annual[complete.cases(wrtds_annual$NO3.x),]

##convert units
e_daily_q$Q_m<-e_daily_q$Q*.0283168

no3$no3_n = no3$no3
no3$no3 = no3$no3_n*4.427

wrtds_monthly$no3_n = wrtds_monthly$GenConc
wrtds_monthly$no3 = wrtds_monthly$no3_n*4.427

wrtds_annual$no3_n = wrtds_annual$GenConc
wrtds_annual$no3 = wrtds_annual$no3_n*4.427

snotel$swe_mm<-snotel$swe*25.4

slate_syn_no3$no3_mgL<- slate_syn_no3$NO3*0.062

### Plots ###

#no3 plot (S1a)
no3_plot<-
  ggplot(no3, mapping = aes(as.Date(date), no3), col = "black")+
  geom_point(size = 3)+
  theme_classic()+
  labs(title = "NO3 Concentration", x = "Date", y = "NO3 (mg/L)")+
  theme(text = element_text(size = 20))
no3_plot

#no3 deposition plot (S1b)
n_dep_plot<-
  ggplot()+
  geom_point(n_dep, mapping = aes(as.Date(dateOn), NO3), col = "black")+
  theme_classic()+
  labs(title = "Atmospheric NO3 Deposition", x = "Date", y = "NO3 (mg/L)")+
  theme(text = element_text(size = 20))
n_dep_plot

## box plots
#no3 boxplot (S1c)
no3$month<-as.factor(no3$month)
no3_boxplot<- 
  ggplot()+
  geom_boxplot(no3, mapping = aes(month, no3), fill = "grey")+
  labs(x = "Month", y = "NO3 (mg/L)")+
  theme_classic()+
  theme(text = element_text(size = 20))
no3_boxplot

#deposition boxplot (S1d)
dep_boxplot<- 
  ggplot()+
  geom_boxplot(n_dep, mapping = aes(month, NO3), fill = "grey")+
  labs(x = "Month", y = "NO3 (mg/L)")+
  theme_classic()+
  theme(text = element_text(size = 20))
dep_boxplot

pdf('FigureS1.pdf', height = 8, width = 13)
ggarrange(no3_plot, n_dep_plot, no3_boxplot, dep_boxplot, labels = c("a", "b", "c", "d"))
dev.off()

          
## wrtds plots (Figure S2)
#peak swe (S2a)
peak_swe_plot<-
  ggplot()+
  geom_point(wrtds_annual, mapping = aes(peak_swe_mm$swe, no3, col = peak_swe$wy), size = 3)+
  scale_color_gradient(low = "darkgrey", high = "red")+
  theme_classic()+
  labs(title = "Peak SWE vs NO3 Concentration", x = "Peak SWE (mm)", y = "NO3 (mg/L)", color = "Water year")+
  theme(text = element_text(size = 15), legend.position = 'none')
peak_swe_plot
  
#avg deposition (S2b)
#trend line
dep_conc_trend<- lm(no3~NO3, data = wrtds_annual)
dep_conc_r_sq<- summary(dep_conc_trend)$r.squared
summary(dep_conc_trend)

avg_dep_plot<-
  ggplot(wrtds_annual, mapping = aes(NO3, no3, col = wy))+
  geom_smooth(method = "lm", color = "black", linewidth = 1, se = FALSE)+
  geom_point(size = 3)+
  annotate("text", x = Inf, y = Inf, 
           label = paste("R^2 = ", round(dep_conc_r_sq, 2)),
           hjust = 1.1, vjust = 1.5)+
  theme_classic()+
  labs(title = "Average NO3 Deposition vs Concentration", x = "NO3 Deposition (mg/L)", y = "NO3 Concentration (mg/L)", 
       color = "Water year", size = 20)+
  scale_color_gradient(low = "darkgrey", high = "red")+
  theme(text = element_text(size = 15), legend.position = "none")
avg_dep_plot

#avg discharge (S2c)
avg_q_plot<-
  ggplot()+
  geom_point(wrtds_annual, mapping = aes(Q, no3, col = wy), size = 3)+
  theme_classic()+
  labs(title = "Average Q vs NO3 Concentration", x = "Avg Q (m^3/s)", y = "NO3 (mg/L)", color = "Water year")+
  scale_color_gradient(low = "darkgrey", high = "red")+
  theme(text = element_text(size = 15), legend.position = "none")
avg_q_plot

#avg deposition + flux (S2d)
avg_dep_flux_plot<-
  ggplot(wrtds_annual, mapping = aes(NO3, GenFlux, col = wy))+
  geom_point(size = 3)+
  theme_classic()+
  labs(title = "Average NO3 Deposition vs NO3 Flux", x = "Avg NO3 Deposition (mg/L)", y = "NO3 Flux (kg/yr)", 
       color = "Water year")+
  scale_color_gradient(low = "darkgrey", high = "red")+
  theme(text = element_text(size = 15), legend.position = "none")
avg_dep_flux_plot

pdf("FigureS2.pdf", height = 8, width = 13)
ggarrange(peak_swe_plot, avg_dep_plot, avg_q_plot, avg_dep_flux_plot, 
          labels = c("a", "b", "c", "d"))
dev.off()

#slate summer 2022 concentrations by sampling time (S3)
slate_syn_no3$Time<-factor(slate_syn_no3$Time, levels = c("early", "middle", "late"))
pdf("FigureS3.pdf", width = 8, height = 12)
ggplot()+
  geom_boxplot(slate_syn_no3, mapping = aes(Time, no3_mgL, fill=Time))+
  facet_zoom(ylim=c(0, .75))+
  theme_bw()+
  theme(text=element_text(size=15), axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = 'Summer 2022 Slate River Synoptic Samples', x="", y="NO3 (mg/L)", fill="Sampling time")+
  scale_fill_manual(values = c("early"="#3B5026", "middle"="#6A8E43", "late"='#ADCB90'))
dev.off()



