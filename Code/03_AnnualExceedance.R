
#DO NOT use this script for Brood year stuff Cat


#03_AnnualExceedance.R----

# Nick Bertrand
# Start Date: Fri Mar 22 16:51:10 2024

#About----
#Project: Winter Run Data Package 
#Purpose:This script creates a table of the historic exceedances under the 2019 PA


#Libraries ----
library(tidyverse)
library(readr)

# Set working directory ----
#set to location of root object to highest tier directory
getwd()
root <- "C:/Users/nbertrand/OneDrive - DOI/Desktop/Bertrand/GitHub/WR Data Package"
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


ex<-jpedata_loss %>%
  mutate(Threshold_100 = 0.0117*JuvenileProductionEstimate) %>%
  mutate(Threshold_75 = Threshold_100*.75, 
         Threshold_50 = Threshold_100*.50) %>% 
  mutate(Exceed100 = if_else(WinterRun_LAD_Loss >= Threshold_100, 1,0), 
         Exceed75 = if_else(WinterRun_LAD_Loss >= Threshold_75, 1,0),
         Exceed50 = if_else(WinterRun_LAD_Loss >= Threshold_50, 1,0)) %>% 
  mutate(ExceedancesbyYearsum = paste(Exceed50, Exceed75, Exceed100)) %>% 
  mutate(Exceedances = case_when(ExceedancesbyYearsum == "1 1 1" ~ "Exceeds All",
                                 ExceedancesbyYearsum=="0 0 0" ~ "No Exceedance",
                                 ExceedancesbyYearsum=="1 1 0" ~ "50 75 Exceedance",
                                 ExceedancesbyYearsum=="1 0 0" ~ "50 Exceedance",
                                 ExceedancesbyYearsum=="1 1 0" ~ "No Exceedance", 
                                 .default = "error"))
 
view(ex)

FinalExceedanceTable <- ex %>% select(WaterYear,Exceedances)

View(FinalExceedanceTable)
write.csv(FinalExceedanceTable,file.path(root,"Table_Output", "HistoricWYExceedancesforWR.csv") )


Exceedance_summary <- FinalExceedanceTable %>% 
  group_by(Exceedances) %>% 
  count() %>% 
  rename(Count = n)
view(Exceedance_summary)
