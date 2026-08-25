library(EflowStats)
library(ggplot2)
library(ggpubr)
require(dataRetrieval)
library(EGRET)
library(dplyr)
library(trend)

### Data ###
e_daily_q<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/Data/east_q_wyd.csv")
e_no3<-read.csv("C:/Users/agneh/Desktop/URSA/Data/e_no3/e_no3.csv")
snotel<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/Data/swe_wyd.csv")
n_dep<-read.csv("C:/Users/agneh/Desktop/URSA/Data/CB_N_deposition.csv")
s_no3<-read.csv("C:/Users/agneh/Desktop/URSA/Data/s_no3/s_no3_2.csv")

### Cleaning ###
e_daily_q$q<- e_daily_q$X_00060_00003*0.02832 #convert to cms
snotel$swe_mm<- snotel$swe*25.4 #convert to mm
n_dep$NO3[n_dep$NO3 == -9]<- NA
e_no3$date<-as.Date(e_no3$date, format = "%m/%d/%Y")
s_no3$date<-as.Date(s_no3$date, format = "%m/%d/%Y")
n_dep$date<- as.Date(n_dep$dateOn)

e_no3$no3_n = e_no3$no3
e_no3$no3 = e_no3$no3_n*4.427

s_no3$no3_n = s_no3$no3
s_no3$no3 = s_no3$no3_n*4.427


east2<-e_no3 %>%
  dplyr::mutate(year = year(e_no3$date)) %>%
  dplyr::mutate(month = month(e_no3$date))
  ))

slate2<-s_no3 %>%
  dplyr::mutate(year = year(slate$date))

avg_no3<- east2 %>%
  group_by(year) %>%
  summarise(avg_conc = mean(no3)) %>%
  mutate(year = as.numeric(as.character(year)))

n_dep <- n_dep %>%
  mutate(month = month(dateOn)) %>%
  mutate(year = year(dateOn)) %>%
  filter(complete.cases(NO3))

avg_dep <- n_dep %>%
  group_by(year) %>%
  summarise(avg_dep = mean(NO3))

e_daily_q <- e_daily_q %>%
  mutate(month = month(Date)) %>%
  mutate(year = year(Date))

avg_q <- e_daily_q %>%
  group_by(year) %>%
  summarise(avg_q = mean(q))

snotel <- snotel %>%
  mutate(year = year(date))

### Plots ###

## Figure S1
no3_points<- 
  ggplot()+
  geom_point(e_no3, mapping = aes(date, no3))+
  labs(x = "Date", y = "NO3 concentration") +
  theme_classic() +
  theme(text = element_text(size = 20))
no3_points

dep_points<- 
  ggplot()+
  geom_point(n_dep, mapping = aes(date, NO3))+
  labs(x = "Date", y = "NO3 deposition") +
  theme_classic() +
  theme(text = element_text(size = 20))
dep_points

no3_boxplot<- 
  ggplot(east2, mapping = aes(group = month, x = month, y = no3))+
  geom_boxplot()+
  labs(x = "Month", y = "NO3 concentration")+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10 , 11, 12)) +
  theme_classic()+
  theme(text = element_text(size = 20))
no3_boxplot

dep_boxplot<- 
  ggplot()+
  geom_boxplot(n_dep, mapping = aes(group = month, x = month, y = NO3))+
  labs(x = "Month", y = "NO3 Deposition")+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10 , 11, 12)) +
  theme_classic()+
  theme(text = element_text(size = 20))
dep_boxplot

pdf("FigureS1", height = 8, width = 12)
ggarrange(no3_points, dep_points, no3_boxplot, dep_boxplot, labels = c("a", "b", "c", "d"))
dev.off()

## Figure S2

#peak swe
peak_swe<-aggregate(swe_mm~year, data = snotel, FUN = max)
peak_swe<-peak_swe %>% 
  slice(14:44)
avg_no3$peak_swe<-peak_swe

peak_swe_trend<- lm(avg_no3~peak_swe$swe_mm, data = avg_no3)
peak_swe_r_sq<- summary(peak_swe_trend)$r.squared
summary(peak_swe_trend)
#trend not sig

peak_swe_plot<-
  ggplot()+
  geom_point(data = avg_no3, mapping = aes(x = peak_swe$swe_mm, y = avg_conc, col = year), size = 3)+
  scale_color_gradient(low = "darkgrey", high = "red")+
  theme_classic()+
  labs(title = "Peak SWE vs NO3 Concentration", x = "Peak SWE (mm)", y = "NO3 (mg/L)", color = "Year")+
  theme(text = element_text(size = 15), legend.position = 'none')
peak_swe_plot
  

#avg deposition
avg_no3_ndep<-left_join(avg_no3, avg_dep, by = "year")
avg_no3_ndep<- avg_no3_ndep %>%
  filter(complete.cases(avg_dep))

dep_no3_trend<- lm(avg_conc~avg_dep, data = avg_no3_ndep)
dep_no3_r_sq<- summary(dep_no3_trend)$r.squared
summary(dep_no3_trend)
#not significant

avg_dep_plot<-
  ggplot(avg_no3_ndep, mapping = aes(avg_dep, avg_conc, col = year))+
  geom_point(size = 3)+
  theme_classic()+
  labs(title = "Average NO3 Deposition vs NO3 Concentration", x = "Avg NO3 Deposition (mg/L)", y = "NO3 Concentration (mg/L)", 
       color = "Year", size = 20)+
  scale_color_gradient(low = "darkgrey", high = "red")+
  theme(text = element_text(size = 15), legend.position = "none")
avg_dep_plot

#avg discharge
avg_no3_q<-left_join(avg_no3, avg_q, by = "year")
Q_no3_trend<- lm(avg_conc~avg_q, data = avg_no3_q)
Q_no3_r_sq<- summary(Q_no3_trend)$r.squared
summary(Q_no3_trend)
#not sig

avg_q_plot<-
  ggplot()+
  geom_point(avg_no3_q, mapping = aes(avg_q, avg_conc, col = year), size = 3)+
  theme_classic()+
  labs(title = "Average Q vs NO3 Concentration", x = "Avg Q (m^3/s)", y = "NO3 (mg/L)", color = "Water year")+
  scale_color_gradient(low = "darkgrey", high = "red")+
  theme(text = element_text(size = 15), legend.position = "none")
avg_q_plot

pdf("FigureS2.pdf", height = 12, width = 8)
ggarrange(avg_dep_plot, avg_q_plot, peak_swe_plot,
          labels = c("a", "b", "c"), ncol = 1, nrow = 3)
dev.off()

#Figure S5
east_p1<-east2[east2$year>2006,]
e_no3_box<- east_p1 %>%
  group_by(year) %>%
  summarise(avg_conc = mean(no3)) %>%
  mutate(year = as.numeric(as.character(year)),
         group = case_when(
           year<=2010 ~ "2007 - 2010",
           year>2010 ~ "2019 - 2023"))


slate_p1<-slate2[slate2$year>2000,]
s_no3_box<- slate_p1 %>%
  group_by(year) %>%
  summarise(avg_conc = mean(no3)) %>%
  mutate(year = as.numeric(as.character(year)),
         group = case_when(
           year<=2010 ~ "2007 - 2010",
           year>2010 ~ "2019 - 2023"))

s5<-ggplot()+geom_boxplot(data = e_no3_box, aes(group, avg_conc), fill = "#43648E", width = 0.2, position = position_nudge(x = -.15))+
  geom_boxplot(data = s_no3_box, aes(group, avg_conc), fill = "#526F34", width = 0.2, position = position_nudge(x = .15))+
  theme_classic()+
  labs(x="Year", y="Mean NO3 (mg/L)")+
  theme(text = element_text(size=16), legend.position = 'bottom')
s5

pdf('FigureS5.pdf', height = 8, width = 12)
s5
dev.off()


