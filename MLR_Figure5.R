library(ggplot2)
library(ggpubr)
library(dplyr)
library(trend)
library(car)
library(lubridate)
library(QuantPsyc)
library(segmented)
library(pdp)

setwd("C:/Users/agneh/Desktop/URSA")

### Data ###

e_daily_q<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/East_Q.csv")
e_no3<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/East_NO3.csv")
s_daily_q<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/Slate_Q.csv")
s_no3<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/Slate_NO3.csv")
snotel<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/CB_snotel_cleaned.csv")
n_dep<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/CB_N_deposition_cleaned.csv")
soil_moisture<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/CB_snotel_soil_moisture.csv")

### Cleaning ###
n_dep$NO3[n_dep$NO3 == -9]<- NA
soil_moisture$in_2[soil_moisture$in_2 == -99.9]<- NA
soil_moisture$in_8[soil_moisture$in_8 == -99.9]<- NA
soil_moisture$in_20[soil_moisture$in_20 == -99.9]<- NA

#Convert date format
soil_moisture$date<-as.Date(soil_moisture$date, format = "%m/%d/%Y")
soil_moisture$Date<-as.Date(soil_moisture$date, format = "%Y-%m-%d")
n_dep$Date = n_dep$dateOff
snotel$Date = snotel$date

e_no3$date<-as.Date(e_no3$date, format = "%m/%d/%Y")
e_no3$Date<-as.Date(e_no3$date, format = "%Y-%m-%d")
s_no3$date<-as.Date(s_no3$date, format = "%m/%d/%Y")
s_no3$Date<-as.Date(s_no3$date, format = "%Y-%m-%d")

e_daily_q$Q<-e_daily_q$X_00060_00003
s_daily_q$Q<-s_daily_q$X_00060_00003

#Define current and previous seasons

e_no3_seasons <- e_no3 %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(11, 12, 1, 2, 3) ~ "Winter",
      Month %in% c(4, 5, 6) ~ "Spring",
      Month %in% c(7, 8, 9, 10) ~ "Summer"
    )
  ) %>%
  group_by(Year, Season) %>%
  summarize(
    avg_no3 = mean(no3, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Year, Season) %>% 
  mutate(
    prev_season_no3 = lag(avg_no3, 1), 
    prev2_season_no3 = lag(avg_no3, 2)
  ) %>%
  filter(!is.na(prev2_season_no3)) 

s_no3_seasons <- s_no3 %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(11, 12, 1, 2, 3) ~ "Winter",
      Month %in% c(4, 5, 6) ~ "Spring",
      Month %in% c(7, 8, 9, 10) ~ "Summer"
    )
  ) %>%
  group_by(Year, Season) %>%
  summarize(
    avg_no3 = mean(no3, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Year, Season) %>% 
  mutate(
    prev_season_no3 = lag(avg_no3, 1), 
    prev2_season_no3 = lag(avg_no3, 2)
  ) %>%
  filter(!is.na(prev2_season_no3)) 

e_q_seasons <- e_daily_q %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(11, 12, 1, 2, 3) ~ "Winter",
      Month %in% c(4, 5, 6) ~ "Spring",
      Month %in% c(7, 8, 9, 10) ~ "Summer"
    )
  ) %>%
  group_by(Year, Season) %>%
  summarize(
    avg_Q = mean(Q, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Year, Season) %>% 
  mutate(
    prev_season_Q = lag(avg_Q, 1), 
    prev2_season_Q = lag(avg_Q, 2)
  ) %>%
  filter(!is.na(prev2_season_Q)) 

s_q_seasons <- s_daily_q %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(11, 12, 1, 2, 3) ~ "Winter",
      Month %in% c(4, 5, 6) ~ "Spring",
      Month %in% c(7, 8, 9, 10) ~ "Summer"
    )
  ) %>%
  group_by(Year, Season) %>%
  summarize(
    avg_Q = mean(Q, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Year, Season) %>% 
  mutate(
    prev_season_Q = lag(avg_Q, 1), 
    prev2_season_Q = lag(avg_Q, 2)
  ) %>%
  filter(!is.na(prev2_season_Q)) 

dep_seasons <- n_dep %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(11, 12, 1, 2, 3) ~ "Winter",
      Month %in% c(4, 5, 6) ~ "Spring",
      Month %in% c(7, 8, 9, 10) ~ "Summer"
    )
  ) %>%
  group_by(Year, Season) %>%
  summarize(
    avg_dep = mean(NO3, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Year, Season) %>% 
  mutate(
    prev_season_dep = lag(avg_dep, 1), 
    prev2_season_dep = lag(avg_dep, 2)
  ) %>%
  filter(!is.na(prev2_season_dep)) 

temp_seasons <- snotel %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(11, 12, 1, 2, 3) ~ "Winter",
      Month %in% c(4, 5, 6) ~ "Spring",
      Month %in% c(7, 8, 9, 10) ~ "Summer"
    )
  ) %>%
  group_by(Year, Season) %>%
  summarize(
    avg_temp = mean(temp_avg, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Year, Season) %>% 
  mutate(
    prev_season_temp = lag(avg_temp, 1), 
    prev2_season_temp = lag(avg_temp, 2)
  ) %>%
  filter(!is.na(prev2_season_temp))  

precip_seasons <- snotel %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(11, 12, 1, 2, 3) ~ "Winter",
      Month %in% c(4, 5, 6) ~ "Spring",
      Month %in% c(7, 8, 9, 10) ~ "Summer"
    )
  ) %>%
  group_by(Year, Season) %>%
  summarize(
    avg_precip = mean(precip_inc, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Year, Season) %>% 
  mutate(
    prev_season_precip = lag(avg_precip, 1), 
    prev2_season_precip = lag(avg_precip, 2)
  ) %>%
  filter(!is.na(prev2_season_precip))  

in_2_seasons <- soil_moisture %>%
  filter(in_2 < 1000) %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(11, 12, 1, 2, 3) ~ "Winter",
      Month %in% c(4, 5, 6) ~ "Spring",
      Month %in% c(7, 8, 9, 10) ~ "Summer"
    )
  ) %>%
  group_by(Year, Season) %>%
  summarize(
    avg_2in = mean(in_2, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Year, Season) %>% 
  mutate(
    prev_season_2in = lag(avg_2in, 1), 
    prev2_season_2in = lag(avg_2in, 2)
  ) %>%
  filter(!is.na(prev2_season_2in))  

in_8_seasons <- soil_moisture %>%
  filter(in_2 < 1000) %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(11, 12, 1, 2, 3) ~ "Winter",
      Month %in% c(4, 5, 6) ~ "Spring",
      Month %in% c(7, 8, 9, 10) ~ "Summer"
    )
  ) %>%
  group_by(Year, Season) %>%
  summarize(
    avg_8in = mean(in_8, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Year, Season) %>% 
  mutate(
    prev_season_8in = lag(avg_8in, 1), 
    prev2_season_8in = lag(avg_8in, 2)
  ) %>%
  filter(!is.na(prev2_season_8in))

in_20_seasons <- soil_moisture %>%
  filter(in_2 < 1000) %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Season = case_when(
      Month %in% c(11, 12, 1, 2, 3) ~ "Winter",
      Month %in% c(4, 5, 6) ~ "Spring",
      Month %in% c(7, 8, 9, 10) ~ "Summer"
    )
  ) %>%
  group_by(Year, Season) %>%
  summarize(
    avg_20in = mean(in_20, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Year, Season) %>% 
  mutate(
    prev_season_20in = lag(avg_20in, 1), 
    prev2_season_20in = lag(avg_20in, 2)
  ) %>%
  filter(!is.na(prev2_season_20in)) 

##Subset each season

east_list<- list(e_no3_seasons, e_q_seasons, dep_seasons, temp_seasons, precip_seasons, 
                     in_2_seasons, in_8_seasons, in_20_seasons)
east_seasonal<- east_list[[1]]
for (i in 2:length(east_list)) {east_seasonal<- left_join(east_seasonal, east_list[[i]],
                                                          by = c("Year", "Season"))}
east_winter <- east_seasonal %>%
  filter(Season == "Winter") %>%
  filter(complete.cases(.))

east_spring <- east_seasonal %>%
  filter(Season == "Spring") %>%
  filter(complete.cases(.))

east_summer <- east_seasonal %>%
  filter(Season == "Summer") %>%
  filter(complete.cases(.))

east_all <- east_seasonal %>%
  filter(complete.cases(.))


### Multiple linear regressions ###

#East winter
cor(east_winter[, c("avg_no3", "prev_season_no3", "prev2_season_no3", "avg_Q", "prev_season_Q", "prev2_season_Q",
                    "avg_dep", "prev_season_dep", "prev2_season_dep", "avg_temp", "prev_season_temp", "prev2_season_temp",
                    "avg_precip", "prev_season_precip", "prev2_season_precip", 
                    "avg_2in", "prev_season_2in", "prev2_season_2in", "avg_8in", "prev_season_8in", "prev2_season_8in",
                    "avg_20in", "prev_season_20in", "prev2_season_20in")])
no_lag_w<- lm(avg_no3 ~ avg_Q + avg_dep + avg_temp + avg_precip + avg_8in, data = east_winter)
step_no_lag_w<- step(no_lag_w, direction = 'both', trace = 1)
full_e_w <- lm(avg_no3 ~ avg_Q + prev_season_Q + prev2_season_Q +
               avg_dep + prev_season_dep + prev2_season_dep + avg_temp + prev_season_temp + prev2_season_temp +
               avg_precip + prev_season_precip + prev2_season_precip + avg_8in + prev_season_8in +
               +prev2_season_8in, data = east_winter)

step_e_w<- step(full_e_w, direction = "both", trace = 1)
summary(step_e_w)
summary(full_e_w)
summary(step_no_lag_w)
w_summary<- summary(step_e_w)

#East spring
cor(east_spring[, c("avg_no3", "prev_season_no3", "prev2_season_no3", "avg_Q", "prev_season_Q", "prev2_season_Q",
                    "avg_dep", "prev_season_dep", "prev2_season_dep", "avg_temp", "prev_season_temp", "prev2_season_temp",
                    "avg_precip", "prev_season_precip", "prev2_season_precip", 
                    "avg_2in", "prev_season_2in", "prev2_season_2in", "avg_8in", "prev_season_8in", "prev2_season_8in",
                    "avg_20in", "prev_season_20in", "prev2_season_20in")])
no_lag_sp<- lm(avg_no3 ~ avg_Q + avg_dep + avg_temp + avg_precip + avg_8in, data = east_spring)
step_no_lag_sp<- step(no_lag_sp, direction = 'both', trace = 1)
full_e_sp <- lm(avg_no3 ~ avg_Q + prev_season_Q + prev2_season_Q +
                 avg_dep + prev_season_dep + prev2_season_dep + avg_temp + prev_season_temp +
                 avg_precip + prev_season_precip + prev2_season_precip + avg_8in + prev_season_8in +
                  prev2_season_8in, data = east_spring)
step_e_sp<- step(full_e_sp, direction = "both", trace = 1)
summary(step_e_sp)
summary(full_e_sp)
summary(step_no_lag_sp)
sp_summary<- summary(step_e_sp)

#East summer
cor(east_summer[, c("avg_no3", "prev_season_no3", "prev2_season_no3", "avg_Q", "prev_season_Q", "prev2_season_Q",
                    "avg_dep", "prev_season_dep", "prev2_season_dep", "avg_temp", "prev_season_temp", "prev2_season_temp",
                    "avg_precip", "prev_season_precip", "prev2_season_precip", 
                    "avg_2in", "prev_season_2in", "prev2_season_2in", "avg_8in", "prev_season_8in", "prev2_season_8in",
                    "avg_20in", "prev_season_20in", "prev2_season_20in")])
no_lag_su<- lm(avg_no3 ~ avg_Q + avg_dep + avg_temp + avg_precip + avg_8in, data = east_summer)
step_no_lag_su<- step(no_lag_su, direction = 'both', trace = 1)
full_e_su <- lm(avg_no3 ~ avg_Q + prev_season_Q + prev2_season_Q +
                 avg_dep + prev_season_dep + prev2_season_dep + avg_temp + prev_season_temp + prev2_season_temp +
                 avg_precip + prev_season_precip + prev2_season_precip + 
                 avg_8in + prev_season_8in +
                 prev2_season_8in, data = east_summer)
step_e_su<- step(full_e_su, direction = "both", trace = 1)
summary(step_e_su)
summary(full_e_su)
summary(step_no_lag_su)
su_summary<- summary(step_e_su)

#Check VIFs for variables, remove variables with highest VIF (don't want >10)
vif(step_e_w)
vif(full_e_w)
vif(step_e_sp)
vif(full_e_sp)
vif(step_e_su)
vif(full_e_su)

#Beta coefficients; quantpsyc package
w_coeff <- lm.beta(step_e_w)
sp_coeff <- lm.beta(step_e_sp)
su_coeff <- lm.beta(step_e_su)
w_p_val<- w_summary$coefficients[-1,4]
sp_p_val<- sp_summary$coefficients[-1, 4]
su_p_val<- su_summary$coefficients[-1, 4]

significance_label <- function(p_value) {ifelse(p_value < 0.05, "p-value < 0.05", "Not Significant")}

w_betas <- w_coeff  
w_betas_df <- data.frame(
  Variable = names(w_betas),   
  Standardized_Beta = w_betas, 
  p_value = w_p_val,
  Significance = sapply(w_p_val, significance_label),
  Model = "Winter")

sp_betas <- sp_coeff  
sp_betas_df <- data.frame(
  Variable = names(sp_betas),   
  Standardized_Beta = sp_betas, 
  p_value = sp_p_val,
  Significance = sapply(sp_p_val, significance_label),
  Model = "Spring")

su_betas <- su_coeff  
su_betas_df <- data.frame(
  Variable = names(su_betas),   
  Standardized_Beta = su_betas, 
  p_value = su_p_val,
  Significance = sapply(su_p_val, significance_label),
  Model = "Summer")

combined_beta_df<- rbind(w_betas_df, sp_betas_df, su_betas_df)
combined_beta_df$Model<- factor(combined_beta_df$Model, levels = c("Summer", "Spring", "Winter"))

#Partial dependence plots
ggplot(data = east_winter, mapping = aes(prev2_season_temp, avg_no3, col = Year))+
  geom_point(size = 3)+geom_smooth(se=F)

mk.test(east_winter$avg_no3)

#Model equations
predicted_winter <- -7.516e-01 + 1.891e-03*east_winter$avg_Q + -1.834e-04*east_winter$prev_season_Q +
  4.61e-05*east_winter$prev2_season_Q + -2.586e-02*east_winter$prev2_season_dep + -7.837e-03*east_winter$avg_temp +
  2.005e-02*east_winter$prev2_season_temp + -9.631e-01*east_winter$prev_season_precip + 1.849*east_winter$prev2_season_precip

predicted_spring <- 2.412e-01 + 5.202e-05*east_spring$avg_Q + -1.285e-03*east_spring$prev_season_Q + 
  5.412e-02*east_spring$prev_season_dep + -6.282e-01*east_spring$prev_season_precip + 
  -5.775e-01*east_spring$prev2_season_precip + 4.122e-03*east_spring$prev_season_8in + 
  3.760e-05*east_spring$prev2_season_8in + -4.418e-03*east_spring$prev_season_temp
  
predicted_summer <- 0.1748798 + -0.0003478*east_summer$prev2_season_Q + 0.0086321*east_summer$prev_season_dep +
  -0.0340326*east_summer$prev2_season_dep + -0.0035439*east_summer$avg_temp + 0.0014397*east_summer$prev_season_temp +
  0.0020264*east_summer$prev2_season_temp + -0.4187394*east_summer$avg_precip + -0.1362504*east_summer$prev2_season_precip +
  0.0017389*east_summer$prev2_season_8in

### Plots ###

#Plot of beta coefficients (figure 5)
beta_coeff<- ggplot(combined_beta_df, aes(x = Variable, y = Model, fill = Standardized_Beta)) +
  geom_tile() +
  scale_fill_gradient2(low = "orange", high = "purple", mid = "white", midpoint = 0) +
  geom_point(aes(color = Significance), 
             size = 3, 
             shape = 16,
             data = combined_beta_df[combined_beta_df$Significance == "p-value < 0.05", ]) + 
  scale_color_manual(values = c("p-value < 0.05" = "black", "Not Significant" = NA)) + 
  theme_classic() +
  labs(title = "Standardized Beta Coefficients by Season",
       x = "Variables",
       y = "Season",
       fill = "Coefficeint") +
  theme(axis.ticks.y = element_blank(), text = element_text(size = 20), 
        axis.text.x = element_text(angle = 45, hjust = 1))

pdf("beta_coeff_grid.pdf", width = 12, height = 8)
beta_coeff
dev.off()

#Predicted vs actual
winter_df<- data.frame(east_winter$Year, east_winter$avg_no3, predicted_winter)
spring_df<- data.frame(east_spring$Year, east_spring$avg_no3, predicted_spring)
summer_df<- data.frame(east_summer$Year, east_summer$avg_no3, predicted_summer)

avp_w<-ggplot()+
  geom_point(data = winter_df, mapping = aes(x = east_winter.avg_no3, y = predicted_winter), col = "#90A9CB", size = 2.5)+
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(x = 'Actual NO3 concentration (mg/L)', y = 'Predicted NO3 concentration (mg/L)', title = 'Predicted vs. Actual Winter NO3')+
  theme(text = element_text(size = 20))+
  theme_classic()

avp_sp<-ggplot()+
  geom_point(data = spring_df, mapping = aes(x = east_spring.avg_no3, y = predicted_spring), col = "#314969", size = 2.5)+
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(x = 'Actual NO3 concentration (mg/L)', y = 'Predicted NO3 concentration (mg/L)', title = 'Predicted vs. Actual Spring NO3')+
  theme(text = element_text(size = 20))+
  theme_classic()

avp_su<-ggplot()+
  geom_point(data = summer_df, mapping = aes(x = east_summer.avg_no3, y = predicted_summer), col = "#5279AD", size = 2.5)+
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(x = 'Actual NO3 conccentration (mg/L)', y = 'Predicted NO3 concentration (mg/L)', title = 'Predicted vs. Actual Summer NO3')+
  theme(text = element_text(size = 20))+
  theme_classic()

pdf("actual_vs_predicted.pdf", width = 5, height = 10)

ggarrange(avp_w, avp_sp, avp_su, ncol = 1, nrow = 3)

dev.off()
