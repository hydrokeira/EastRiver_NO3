library(dplyr)
library(lubridate)
library(ggplot2)
library(Kendall)
library(purrr)

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/Data")

ndvi<-read.csv("watershed_NDVI_timeseries_V2.csv")
ndvi$date<-as.Date(ndvi$date)
ndvi <- ndvi %>%
  filter(year(date) > 1992 & year(date) < 2024)

#for Slate River
# Run Mann-Kendall test by month
mk_results <- ndvi %>%
  mutate(
    watershed_name = case_when(
      watershed == "CO20251113205900618000" ~ "East River",
      .default = "Slate River"
    )
  ) %>%
  filter(watershed_name == "Slate River") %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarise(
    p_value = MannKendall(mean_ndvi)$sl,
    tau = MannKendall(mean_ndvi)$tau,
    .groups = "drop"
  )

mk_results

# Prepare data
ndvi_plot <- ndvi %>%
  mutate(
    watershed_name = case_when(
      watershed == "CO20251113205900618000" ~ "East River",
      .default = "Slate River"
    ),
    month = month(date)
  ) %>%
  filter(watershed_name == "Slate River")

sig_months <- mk_results %>%
  filter(p_value < 0.05 & tau > 0) %>%
  pull(month)

ndvi_lines <- ndvi_plot %>%
  filter(month %in% sig_months)

# Plot
ggplot(ndvi_plot, aes(date, mean_ndvi)) +
  geom_point() +
  geom_smooth(
    data = ndvi_lines,
    method = "lm",
    se = FALSE,
    col="black"
  ) +
  facet_wrap(~month)+theme_classic()+
  theme(text = element_text(size=20))+
  labs(x="", y="Watershed Avg NDVI")+
  ggtitle("Slate River")

#for East River
# Run Mann-Kendall test by month
mk_results <- ndvi %>%
  mutate(
    watershed_name = case_when(
      watershed == "CO20251113205900618000" ~ "East River",
      .default = "Slate River"
    )
  ) %>%
  filter(watershed_name == "East River") %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarise(
    p_value = MannKendall(mean_ndvi)$sl,
    tau = MannKendall(mean_ndvi)$tau,
    .groups = "drop"
  )

mk_results

# Prepare data
ndvi_plot <- ndvi %>%
  mutate(
    watershed_name = case_when(
      watershed == "CO20251113205900618000" ~ "East River",
      .default = "Slate River"
    ),
    month = month(date)
  ) %>%
  filter(watershed_name == "East River")

sig_months <- mk_results %>%
  filter(p_value < 0.05 & tau > 0) %>%
  pull(month)

ndvi_lines <- ndvi_plot %>%
  filter(month %in% sig_months)

# Plot
ggplot(ndvi_plot, aes(date, mean_ndvi)) +
  geom_point() +
  geom_smooth(
    data = ndvi_lines,
    method = "lm",
    se = FALSE,
    col="black"
  ) +
  facet_wrap(~month)+theme_classic()+
  theme(text = element_text(size=20))+
  labs(x="", y="Watershed Avg NDVI")



