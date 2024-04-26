#01_WRHistoricJPE_Vizualization.R----

# Nick Bertrand
# Start Date: Thu Mar 21 10:46:55 2024

#About----
#Project: Winter Run Data Package 
#Purpose:This script will generate vizualizations of the historic winter winter run JPE

#Libraries ----
library(tidyverse)
library(readr)

# Set working directory ----
#set to location of root object to highest tier directory
getwd()
# root <- "C:/Users/nbertrand/OneDrive - DOI/Desktop/Bertrand/GitHub/WR Data Package"
root <- here::here()
setwd(root)
#getwd()

#these root object use directories 
data_root<-file.path(root,"Data")
code_root <- file.path(root,"Code")
table_output_root <- file.path(root,"Table_Output")
viz_output_root <- file.path(root,"Viz_Output")

#Import Data ----
Winter_run_JPE_LTOData <- read_csv(file.path(data_root,"Winter-run_JPE_LTOData.csv"))
#View(Winter_run_JPE_LTOData)
#BY2022 and BY2023 added

#JPE Visualizations----
  
WR_JPE_plot <- ggplot(Winter_run_JPE_LTOData, aes(x=as.factor(BroodYear),y= JuvenileProductionEstimate)) +
    geom_bar(stat="identity")+
    labs(title = "Winter-run Chinook Historic \nJuvenile Production Estimate")+
    xlab("Brood Year")+
  theme(axis.text.x = element_text(angle=45))+
  scale_y_continuous(labels = scales::comma)
WR_JPE_plot


ggsave(WR_JPE_plot,filename = file.path(viz_output_root,"WR_JPE_plot20240321.png"), width = 8, height = 4)
