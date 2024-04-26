#02_HistoricWRSalvageandLoss.R----

# Nick Bertrand
# Start Date: Thu Mar 21 13:39:14 2024

#About----
#Project: Winter Run Data Package 
#Purpose:This script will generate vizualizations of the historic winter winter run loss and salvage

#Libraries ----
library(tidyverse)
library(readr)
library(ggpubr)
library(patchwork)
library(ggbreak)

# Set working directory ----
#set to location of root object to highest tier directory
getwd()
root <- "C:/Users/nbertrand/OneDrive - DOI/Desktop/Bertrand/GitHub/WR Data Package"
root <- here::here()
setwd(root)
getwd()
#these root object use directories 
data_root<-file.path(root,"Data")
code_root <- file.path(root,"R_scripts")
table_output_root <- file.path(root,"Table_Output")
viz_output_root <- file.path(root,"Viz_Output")

#Import Data ----

#JPE Loss Comparison
jpedata_loss <- read_csv(file.path(data_root,"JPE_Genetic_Loss_Comparison.csv"))
View(jpedata_loss)

#Data Manipulation
loss_long <- jpedata_loss %>% 
  select(WaterYear,WinterRun_LAD_Loss,WinterRun_Genetic_Loss_according_to_geneticdatasheet) %>% 
  rename(`WR LAD Loss`= WinterRun_LAD_Loss,`WR Genetic Loss` = WinterRun_Genetic_Loss_according_to_geneticdatasheet) %>% 
  pivot_longer(cols =`WR LAD Loss`:`WR Genetic Loss`,names_to = "Loss Type", values_to = "Loss")
view(loss_long)

jpe_long <- jpedata_loss %>% 
  select(WaterYear,BroodYear,JuvenileProductionEstimate,PercentJPE_genetic,PercentJPE_LAD) %>% 
  rename(`Juvenile Production Estimate`= JuvenileProductionEstimate,`Percent JPE Genetic Loss` = PercentJPE_genetic, `Percent JPE LAD Loss`= PercentJPE_LAD) %>% 
  pivot_longer(cols =`Percent JPE Genetic Loss`:`Percent JPE LAD Loss`,names_to = "JPE Metric", values_to = "Value")
view(jpe_long)


#Genetic VS LAD Visualizations----

#ggplot pallet
my_palette=c('#003E51','#007396', '#C69214', '#DDCBA4','#FF671F', '#215732',
             '#4C12A1','#9a3324', "#88CCEE","#AA4499")

LAD_plot <- ggplot(jpedata_loss, aes(x=as.factor(WaterYear),y= WinterRun_LAD_Loss)) +
  geom_bar(stat="identity")+
  labs(title = "LAD Loss")+
  #xlab("Water Year")+
  ylab("WR LAD Loss")+
  theme(plot.title = element_text(size=10), axis.text.x=element_blank(), axis.title.x=element_blank(),axis.ticks=element_blank())+
  #theme(axis.text.x = element_text(angle=45))+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = my_palette)
LAD_plot

gen_plot <- ggplot(jpedata_loss, aes(x=as.factor(WaterYear),y= WinterRun_Genetic_Loss_according_to_geneticdatasheet )) +
  geom_bar(stat="identity")+
  labs(title = "Genetic ID Loss")+
  xlab("Water Year")+
  ylab("WR Genetic ID Loss")+
  theme(plot.title = element_text(size=10), axis.text.x = element_text(angle=45))+
  scale_y_continuous(labels = scales::comma)
gen_plot


JPEplot <- ggplot(data=jpedata_loss, aes(x=as.factor(WaterYear), y= JuvenileProductionEstimate)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=BroodYear),hjust = -0.1, angle = 75) +
  labs(title = "Winter-run Chinook Historic Juvenile Production Estimate", size = 10)+
  #xlab("Water Year")+
  ylab("Juvenile Production Estimate")+
  theme(plot.title = element_text(size=10),axis.text.x=element_blank(), axis.title.x=element_blank(),axis.ticks=element_blank())+
  #theme(axis.text.x = element_text(angle=45))+
  scale_y_continuous(limits = c(0, 5000000),labels = scales::comma)+
  scale_fill_manual(values = my_palette)
JPEplot

com_plot1 <- ggarrange(JPEplot, LAD_plot, gen_plot, ncol = 1, nrow = 3)
com_plot1

ggsave(com_plot1,filename = file.path(viz_output_root,"JPE_LAD_Gen_loss20240322.png"), width = 9, height = 8, units = "in")
# Stacked JPE Plots----

#stacked plot 
GenVLADHistoricalLoss <- 
  ggplot(data=loss_long, aes(x=as.factor(WaterYear), y= Loss, fill=`Loss Type`)) +
  geom_bar(stat="identity", position=position_dodge())+
  #geom_bar(stat="identity")+
  labs(title = "Genetic vs LAD Historical Loss Comparison")+
  xlab("Water Year")+
  ylab("WR Loss")+
  theme(axis.text.x = element_text(angle=45))+
  #scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = my_palette)+
  theme(legend.position = "none")+
  scale_y_break(c(7000, 13990))
  

GenVLADHistoricalLoss
ggsave(GenVLADHistoricalLoss,filename = file.path(viz_output_root,"GenVLADHistoricalLoss20240328.png"),
       width = 11, height = 8, units = "in")

# loss_long_fil <- loss_long %>% filter(WaterYear != "2001")
# #view(loss_long_fil)
# GenVLADHistoricalLoss_fil <- 
#   ggplot(data=loss_long_fil, aes(x=as.factor(WaterYear), y= Loss, fill=`Loss Type`)) +
#   geom_bar(stat="identity", position=position_dodge())+
#   #geom_bar(stat="identity")+
#   labs(title = "Genetic vs LAD Historical Loss Comparison")+
#   xlab("Water Year")+
#   ylab("WR Loss")+
#   theme(axis.text.x = element_text(angle=45), legend.position = "bottom")+
#   scale_y_continuous(labels = scales::comma)+
#   scale_fill_manual(values = my_palette)
# 
# GenVLADHistoricalLoss_fil
# 
# GenVLADHistoricalLossCombined <- GenVLADHistoricalLoss_fil + inset_element(GenVLADHistoricalLoss, 
#                                                    left = 0.5, 
#                                                    bottom = 0.57, 
#                                                    right = 0.99, 
#                                                    top = 0.99)
# GenVLADHistoricalLossCombined
# ggsave(GenVLADHistoricalLossCombined,filename = file.path(viz_output_root,"GenVLADHistoricalLossCombined20240327.png"), 
#        width = 11, height = 8, units = "in")

#stacked plot 
GenVLADPercentLoss <- 
  ggplot(data=jpe_long, aes(x=as.factor(BroodYear), y= Value, fill=`JPE Metric`)) +
  geom_bar(stat="identity", position=position_dodge())+
  #geom_bar(stat="identity")+
  labs(title = "Genetic vs LAD Historical Percent Loss of the JPE")+
  xlab("Brood Year")+
  ylab("% of the JPE Lost")+
  theme(axis.text.x = element_text(angle=45),legend.position = "bottom")+
  #scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = my_palette)+
  theme(legend.position = "none")+
  scale_y_break(c(2.01,3.75))

GenVLADPercentLoss

ggsave(GenVLADPercentLoss,filename = file.path(viz_output_root,"GenVLADPercentLoss20240328.png"),
       width = 11, height = 8, units = "in")

# jpe_long_fil <- jpe_long %>% filter(BroodYear != "2000")
# #view(jpe_long)
# GenVLADPercentLoss_fil <- 
#   ggplot(data=jpe_long_fil, aes(x=as.factor(BroodYear), y= Value, fill=`JPE Metric`)) +
#   geom_bar(stat="identity", position=position_dodge())+
#   #geom_bar(stat="identity")+
#   labs(title = "Genetic vs LAD Historical Percent Loss of the JPE")+
#   xlab("Brood Year")+
#   ylab("% of the JPE Lost")+
#   theme(axis.text.x = element_text(angle=45),legend.position = "bottom")+
#   scale_y_continuous(labels = scales::comma)+
#   scale_fill_manual(values = my_palette)
# 
# GenVLADPercentLoss_fil
# 
# GenVLADPercentLossCombined <- GenVLADPercentLoss_fil + inset_element(GenVLADPercentLoss, 
#                                                                            left = 0.5, 
#                                                                            bottom = 0.65, 
#                                                                            right = 0.99, 
#                                                                            top = 0.99)
# GenVLADPercentLossCombined
# ggsave(GenVLADPercentLossCombined ,filename = file.path(viz_output_root,"GenVLADPercentLossCombined20240327.png"), 
#        width = 11, height = 8, units = "in")
