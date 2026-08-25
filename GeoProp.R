####geo proportions
require(readxl)

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/GIS")

conversion<-read_xlsx("Geo_Color_Reference.xlsx")

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/Data")

ER<-read.csv("ER_Geo_Props.csv")

ER<-ER %>%
  left_join(conversion)

ER_sum <- ER %>%
  group_by(`New Name`) %>%
  summarise(tot_prop=sum(geo_prop)*100)

SR<-read.csv("Slate_Geo_Props.csv")

SR<-SR %>%
  left_join(conversion)

SR_sum <- SR %>%
  group_by(`New Name`) %>%
  summarise(tot_prop=sum(geo_prop)*100)
