setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/Data")

dat<-read.csv("USGS_physical_params.csv")

dat<-dat %>%
  select(Location_Identifier, Location_Name, Activity_StartDate, Result_CharacteristicUserSupplied,
         Result_Measure, Result_MeasureUnit, Result_MeasureStatusIdentifier)
dat_sc<-dat %>%
  filter(Result_CharacteristicUserSupplied %like% "Specific")

dat_sc$Date<-as.Date(dat_sc$Activity_StartDate, format="%m/%d/%y")
dat_sc$Result_Measure<-as.numeric(dat_sc$Result_Measure)

dat_sc <- dat_sc %>%
  filter(year(Date) > 1992)

ggplot(dat_sc, aes(Date, Result_Measure))+geom_point()+geom_line()

q<-read.csv("east_q_wyd.csv")
q$Date<-as.Date(q$Date)

dat_sc_q<-dat_sc %>%
  left_join(q)

runoff<-quantile(dat_sc_q$Result_Measure, 0.01)
gw<-quantile(dat_sc_q$Result_Measure, 0.99)

dat_sc_q$bf<-((dat_sc_q$Result_Measure-runoff)/(gw-runoff))

dat_sc_q<-dat_sc_q %>%
  mutate(month = month(Date)) %>%
  mutate(season=case_when(
    month %in% c(4,5,6) ~ "spring",
    month %in% c(7,8,9) ~"summer/fall",
    .default = "winter"
  ))

mk_results <- dat_sc_q %>%
  group_by(season) %>%
  summarise(
    p_value = MannKendall(bf)$sl,
    tau = MannKendall(bf)$tau,
    .groups = "drop"
  )

mk_results

# Prepare data
bf_plot <- dat_sc_q

sig_months <- mk_results %>%
  filter(p_value < 0.05 & tau > 0) %>%
  pull(season)

bf_lines <- bf_plot %>%
  filter(season %in% sig_months)

ggplot(dat_sc_q, aes(Date, bf)) +
  geom_point() +
  geom_smooth(
    data = bf_lines,
    method = "lm",
    se = FALSE,
    col="black"
  ) +
  facet_wrap(~season)+theme_classic()+
  theme(text = element_text(size=20))+
  labs(x="", y="Baseflow Proportion")

ggplot(dat_sc_q, aes(month(Date), bf)) +
  geom_boxplot(aes(group=month(Date)))+
  theme_classic()+
  theme(text = element_text(size=20))+
  labs(x="", y="Baseflow Proportion")+
  scale_x_continuous(breaks = seq(1,12,1), labels = seq(1,12,1))
