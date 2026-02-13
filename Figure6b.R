library(ggplot2)

### Data ###
percent.change<-read.csv("C:/Users/agneh/Box/Hydrology_Lab/Undergraduates/URSA/Gracie Neher/ForSubmission/Final_Data/Percent_change_NO3.csv")

### Plot ###
#Percent change plot (Figure 6b)
percent.change$tributary<-factor(percent.change$tributary, levels = c("OBJ","Wash","Coal","Slate"))
pdf("Figure6b.pdf", width = 6, height = 6)
ggplot()+
  geom_col(data=percent.change, mapping=aes(tributary, percent_change, fill = tributary), position="dodge")+
  theme_classic()+
  theme(text=element_text(size = 20), legend.position = 'none')+labs(x="",y="Average percent change")+
  scale_fill_manual(values = c("OBJ"="#b33d93", "Wash"="#9063c0", "Coal"='#d37538', "Slate"="#020b72"))
dev.off()

