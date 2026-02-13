library(ggplot2)
library(ggpubr)
library(dplyr)
library(lubridate)
library(QuantPsyc)
library(segmented)
library(trend)
library(tidyr)

### Data ###
east_q<- read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/Data/east_q_wyd.csv")
swe<- read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/Data/swe_wyd.csv")
dep<- read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/Data/CB_N_deposition.csv")
loads<- read.csv("C:/Users/agneh/Desktop/deposition_loads.csv")

### Cleaning ###
east_q$q<- east_q$X_00060_00003*0.02832 #convert to cms
swe$swe<- swe$swe*25.4 #convert to mm

east_q <- east_q %>%
  mutate(
  month = month(Date))

east_q_sub <- subset(east_q, water_year > 1992)
east_q_sub <- subset(east_q_sub, water_year < 2024)

swe <- swe %>%
  mutate(
  month = month(date))

swe_sub <- subset(swe, water_year > 1992)
swe_sub <- subset (swe_sub, water_year < 2024)

dep$date = dep$dateOn
dep = dep[, c(19, 30)]
dep$NO3[dep$NO3 == -9]<-NA
dep<- na.omit(dep)

dep <- dep %>%
  mutate(
    year = year(date)
  )

avg_dep <- dep %>%
  group_by(year) %>%
  summarise(
    avg_dep = mean(NO3)
  )

loads$NO3[loads$NO3 == -9]<-NA
loads$NH4[loads$NH4 == -9]<-NA
loads$totalN[loads$totalN == -9]<-NA

mk.test(avg_dep$avg_dep)
sens.slope(avg_dep$avg_dep)
dep_lm<- lm(avg_dep~year, data = avg_dep)
summary(dep_lm)

dep_seg<- segmented(dep_lm, seg.Z = ~ Year, psi = list(Year = 2010))
summary(dep_seg)
dep_seg$psi
slope(dep_seg)
dep_fitted<- fitted(dep_seg)
dep_seg_model<- data.frame(year = avg_dep$year, deposition = dep_fitted)
ggplot(dep_seg_model, aes(x = year, y = deposition)) + geom_line()
davies.test(dep_seg,seg.Z = ~ Year, k = 10)

### Plots ###
q_plot<- ggplot() + geom_line(east_q_sub, mapping = aes(water_year_day, q, group = water_year, col = water_year))+
  labs(title = "Discharge",
       x = "Month",
       y = "Q (cms)", 
       color = "Water year",
       tag = "a")+
  scale_color_gradient(low = 'black', high = 'grey')+
  geom_line(data = filter(east_q_sub, water_year == 2022), 
            aes(x = water_year_day, y = q), color = '#008F8F', linewidth = 1.2)+
  theme_classic()+
  scale_x_continuous(breaks = c(1, 93, 152, 244, 336), labels = c("Oct", "Jan", "Mar", "Jun", "Sep")) +
  theme(text = element_text(size = 16), #legend.position.inside = c(0.15, 0.55),
        legend.position = 'none',
        legend.background = element_rect(fill = 'white', color = 'black'))
q_plot
                
swe_plot<-ggplot()+geom_line(swe_sub, mapping=aes(water_year_day, swe, group=water_year, col = water_year))+
  labs(title = "SWE",
       x = "Month",
       y = "SWE (mm)",
       color = "Water year",
       tag = "b")+
  scale_color_gradient(low = 'black', high = 'grey')+
  geom_line(data = filter(swe_sub, water_year == 2022), 
            aes(x = water_year_day, y = swe), color = '#008F8F', linewidth = 1.2)+
  theme_classic()+
  scale_x_continuous(breaks = c(1, 93, 152, 244, 336), labels = c("Oct", "Jan", "Mar", "Jun", "Sep")) +
  theme(text = element_text(size = 16), legend.position = c(0.85, 0.55), 
        legend.background = element_rect(fill = 'white', color = 'black'))
swe_plot

loads_long <- loads |>
  pivot_longer(
    cols = c(totalN, NO3, NH4),
    names_to = "analyte",
    values_to = "value"
  )

dep_plot<-ggplot(loads_long, aes(x = yr)) +
  geom_point(aes(y = ifelse(analyte == "NO3", value / 2, value), 
                 color = analyte, shape = analyte), size = 2.3) +
  geom_smooth(data = subset(loads_long, analyte != "totalN"),
              aes(y = ifelse(analyte == "NO3", value / 2, value), color = analyte), method = "lm", se = FALSE) + 
  scale_color_manual(values = c(totalN = "#877F76",NO3 = "#A87400",NH4 = "#730E0E")) +
  scale_shape_manual(values = c(totalN = 16, NO3 = 17, NH4 = 15)) +
  scale_y_continuous(name = "NH4 and Total N (kg/ha)", sec.axis = sec_axis(~ . * 2, name = "NO3 (kg/ha)")) +
  labs(title = "Deposition", x = "Year", tag = "c") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = c(0.9, 0.9), 
        legend.background = element_rect(fill = 'white', color = 'black'), legend.title = element_blank())
dep_plot


setwd("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/Figures")

pdf('figure2.pdf', width = 8, height = 12)
ggarrange(q_plot, swe_plot, dep_plot, ncol = 1, nrow = 3)
dev.off()

