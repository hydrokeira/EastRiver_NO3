library(ggplot2)
library(ggpmisc)
library(dplyr)
library(lubridate)
library(ggpubr)
library(trend)
library(cowplot)
library(segmented)

### Data ###

east<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/East_NO3.csv")
slate<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/Slate_NO3.csv")


### Cleaning ###

slate$date<-as.Date(slate$date, format = "%m/%d/%Y")
east$date<-as.Date(east$date, format = "%m/%d/%Y")
slate$site_no<-"Slate"
east$site_no<-"East"

east$no3_n = east$no3
east$no3 = east$no3_n*4.427

slate$no3_n = slate$no3
slate$no3 = slate$no3_n*4.427

wanted_years<-c(2007, 2008, 2009, 2010, 2019, 2020, 2021, 2022, 2023)

east1<-east %>%
  dplyr::mutate(year = year(east$date))
east1<-east1[east1$year > 2006,]

slate1<-slate %>%
  dplyr::mutate(year = year(slate$date))
slate1<-slate1[slate1$year >2006,]
slate2<-slate1[slate1$year < 2011,]
slate3<-slate1[slate1$year > 2011,]


both_now<-bind_rows(slate1, east1)

both_now_2<-subset(both_now, both_now$year %in% wanted_years)

both_now_2<-both_now_2 %>%
  mutate(season = case_when(
    month(date) %in% c(11,12,1,2,3) ~ "winter",
    month(date) %in% c(4,5,6) ~ "spring",
    month(date) %in% c(7,8,9,10) ~ "summer/fall"
  ))

east2<-east %>%
  dplyr::mutate(year = year(east$date)) %>%
  mutate(season = case_when(
    month(date) %in% c(11,12,1,2,3) ~ "winter",
    month(date) %in% c(4,5,6) ~ "spring",
    month(date) %in% c(7,8,9,10) ~ "summer/fall"
  ))

east2$season <- factor(east2$season, levels = c("spring", "summer/fall", "winter"))

slate2<-slate %>%
  dplyr::mutate(year = year(slate$date)) %>%
  mutate(season = case_when(
    month(date) %in% c(11,12,1,2,3) ~ "winter",
    month(date) %in% c(4,5,6) ~ "spring",
    month(date) %in% c(7,8,9,10) ~ "summer/fall"
  ))

slate_p1<-slate2[slate2$year>2000,]
slate_p2<- slate2[slate2$year>2018,]

slate_p1<-slate_p1 %>%
  dplyr::mutate(group = case_when(
    year<=2010 ~ "2007 - 2010",
    year>2010 ~ "2019 - 2023"
  ))

slate_p1$season <- factor(slate_p1$season, levels = c("spring", "summer/fall", "winter"))

avg_no3_s<- slate_p1 %>%
  group_by(year) %>%
  summarise(avg_conc = mean(no3)) %>%
  mutate(year = as.numeric(as.character(year)),
         group = case_when(
           year<=2010 ~ "2007 - 2010",
           year>2010 ~ "2019 - 2023"))

avg_no3_e<- east2 %>%
  group_by(year) %>%
  summarise(avg_conc = mean(no3)) %>%
  mutate(year = as.numeric(as.character(year)))

east_p1<-east2[east2$year>2006,]
east_p2<-east2[east2$year>2018,]

east_p1<-east_p1 %>%
  dplyr::mutate(group = case_when(
    year<=2010 ~ "2007 - 2010",
    year>2010 ~ "2019 - 2023"
  ))

east_p1$season <- factor(east_p1$season, levels = c("spring", "summer/fall", "winter"))


### Analyses/tests ###

#east overlap mann kendell
east_overlap<-subset(east, east$wy %in% wanted_years)
east_annual_no3 <- east %>%
  mutate(
    year = year(date)
  ) %>%
  #subset(year>2007) %>%
  group_by(year) %>%
  summarize(
    avg_no3 = mean(no3, na.rm = TRUE)
  )
mk.test(east_annual_no3$avg_no3)
sens.slope(east_annual_no3$avg_no3)
e_lm<- lm(avg_no3~year, data = east_annual_no3)
summary(e_lm)

#Break point analysis
e_seg<- segmented(e_lm, seg.Z = ~ year, psi = list(year = 2010))
summary(e_seg)
davies.test(e_seg,seg.Z = ~ year, k = 10)

#Slate t tests
summer = "summer/fall"
s1_summer = subset(slate_p1, season == summer)
s2_summer = subset(slate_p2, season == summer)
summer_ttest<- t.test(s1_summer$no3, s2_summer$no3, alternative = c('two.sided'), mu = 0, paired = FALSE, 
                      var.equal = FALSE, conf.level = 0.95)
summer_ttest

winter = "winter"
s1_winter = subset(slate_p1, season == winter)
s2_winter = subset(slate_p2, season == winter)
winter_ttest<- t.test(s1_winter$no3, s2_winter$no3, alternative = c('two.sided'), mu = 0, paired = FALSE, 
                      var.equal = FALSE, conf.level = 0.95)
winter_ttest

spring = "spring"
s1_spring = subset(slate_p1, season == spring)
s2_spring = subset(slate_p2, season == spring)
spring_ttest<- t.test(s1_spring$no3, s2_spring$no3, alternative = c('two.sided'), mu = 0, paired = FALSE, 
                      var.equal = FALSE, conf.level = 0.95)
spring_ttest

#East t tests
e1_summer = subset(east_p1, season == summer)
e2_summer = subset(east_p2, season == summer)
summer_ttest_e<- t.test(e1_summer$no3, e2_summer$no3, alternative = c('two.sided'), mu = 0, paired = FALSE, 
                        var.equal = FALSE, conf.level = 0.95)
summer_ttest_e

e1_winter = subset(east_p1, season == winter)
e2_winter = subset(east_p2, season == winter)
winter_ttest_e<- t.test(e1_winter$no3, e2_winter$no3, alternative = c('two.sided'), mu = 0, paired = FALSE, 
                        var.equal = FALSE, conf.level = 0.95)
winter_ttest_e

e1_spring = subset(east_p1, season == spring)
e2_spring = subset(east_p2, season == spring)
spring_ttest_e<- t.test(e1_spring$no3, e2_spring$no3, alternative = c('two.sided'), mu = 0, paired = FALSE, 
                        var.equal = FALSE, conf.level = 0.95)
spring_ttest_e

#East seasonal Mann Kendall
east_winter <- east2 %>%
  filter(season == "winter")

mk.test(east_winter$no3)
sens.slope(east_winter$no3)

east2_overlap <- east2 %>%
  mutate(
    year = year(date)
  ) %>%
  subset(year>2006) %>%
  group_by(year)

east_overlap_winter<- subset(east2_overlap, season == winter)
east_overlap_spring<- subset(east2_overlap, season == spring)
east_overlap_summer<- subset(east2_overlap, season == summer)

mk.test(east_overlap_winter$no3)
sens.slope(east_overlap_winter$no3)

mk.test(east_overlap_spring$no3)
sens.slope(east_overlap_spring$no3)

mk.test(east_overlap_summer$no3)
sens.slope(east_overlap_summer$no3)


### Plots ###

#2003-2023 East seasonal trends
east2.2<-east2[east2$year >2002,]

p1.2<-ggplot(east2.2, aes(date, no3, col=season))+geom_point()+geom_smooth(method = "lm", se=F)+
  stat_poly_eq(use_label(c("R2", "p")), vstep = 0.07, label.x = "right")+
  theme_classic()+
  theme(text = element_text(size=16), legend.position = "null")+
  labs(x="Date", y="NO3 (mg/L)")+ ggtitle('East River')+
  scale_color_manual(values = c("spring"="#314969", "summer/fall"="#5279AD", "winter"="#90A9CB"))
p1.2

#Figure 3
p6<-ggplot(both_now, aes(date, no3, col=site_no))+geom_point()+
  geom_line(lty="dashed")+theme_classic()+
  labs(x="Date", y="NO3 (mg/L)", col="", tag="a")+
  theme(text = element_text(size=14), legend.position= "bottom")+
  scale_color_manual(values = c("Slate"="#526F34", "East"="#43648E"))

p6

p7<-ggplot(both_now_2, aes(x=site_no, y=no3, fill=site_no))+geom_boxplot()+
  theme_classic()+facet_wrap(~season)+
  labs(x="", y="NO3 (mg/L)", tag="b")+
  theme(text = element_text(size=14), legend.position= "null")+
  scale_fill_manual(values = c("Slate"="#526F34", "East"="#43648E"))
p7

p3<-ggplot()+geom_boxplot(data = avg_no3_s, aes(group, avg_conc), fill = "#526F34", width = 0.3)+
  theme_classic()+
  theme(text = element_text(size=16))+
  labs(x="Year", y="Mean NO3 (mg/L)", tag = 'd')+ggtitle("Slate River")
p3

p4<-ggplot(avg_no3_e, mapping = aes(x = year, y = avg_conc)) +
  geom_point(size = 2, col = "#43648E") +
  theme_classic() +
  labs(x = "Year", y = "Mean NO3 (mg/L)", tag = 'c')+ggtitle("East River")+
  theme(text = element_text(size = 16))+
  geom_smooth(method = "lm", col = "black", formula = 'y ~ x', se = FALSE)
p4

pdf("Figure_3.pdf", width = 12, height = 6)
ggarrange(p6, p7, p4, p3, nrow=2, ncol = 2)
dev.off()

#Figure 4
p1<-ggplot(east2, aes(date, no3, col=season))+geom_point()+geom_smooth(method = "lm", se=F)+
  stat_poly_eq(use_label(c("R2", "p")), vstep = 0.07, label.x = c(0.7, 0.7))+
  theme_classic()+
  theme(text = element_text(size=16), legend.position = c(0.85, 0.97), 
        legend.background = element_rect(fill = 'white', color = 'black'), legend.title = element_blank())+
  labs(x="Date", y="NO3 (mg/L)", tag = 'a')+ ggtitle('East River')+
  scale_color_manual(values = c("spring"="#314969", "summer/fall"="#5279AD", "winter"="#90A9CB"))
p1

p2<-ggplot()+geom_boxplot(data = slate_p1, aes(group, no3, fill=season))+
  theme_classic()+facet_wrap(~season)+
  theme(text = element_text(size=16), legend.position = "null", axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="", y="NO3 (mg/L)", tag = 'c')+ ggtitle('Slate River')+
  scale_y_continuous(limits = c(0, 2.0), breaks = seq(0, 2.0, 0.5)) +
  scale_fill_manual(values = c("spring"="#3B5026", "summer/fall"="#6A8E43", "winter"='#ADCB90'))
p2

p8<-ggplot()+geom_boxplot(data = east_p1, aes(group, no3, fill=season))+
  scale_y_continuous(limits = c(0, 2.0), breaks = seq(0, 2.0, 0.5)) +
  scale_fill_manual(values = c("spring"="#314969", "summer/fall"="#5279AD", "winter"="#90A9CB")) +
  labs(x="", y="NO3 (mg/L)", tag = 'b')+ ggtitle('East River') + 
  theme_classic()+facet_wrap(~season)+
  theme(text = element_text(size=16), legend.position = "null", axis.text.x = element_text(angle = 45, hjust = 1))
p8

top<- plot_grid(p1, ncol = 1)
bottom<- plot_grid(p8, p2, ncol = 2)
pdf("Figure_4.pdf", width = 10, height = 8)
plot_grid(top, bottom, ncol = 1, rel_heights = c(1, 1))
dev.off()

