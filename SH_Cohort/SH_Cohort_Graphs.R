library(tidyverse)
library(busdater)
library(CDECRetrieve)
library(ggridges)

#################### Annual steelhead cumulative loss by period and water year type

#Read in data dependencies
loss <- read.csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=2%3Af&dnaOnly=no&age=no')
waterDay <- readRDS('SH_Cohort/Data/waterDay.rds')
wytype <- read.csv('SH_Cohort/Data/WYtype.csv')

#Process loss data to find cumulative loss by year, biop, and water year type
cumulative <- loss %>% 
  select(Date = 1, 2, 9, 10, ExpSalv = 11, 12) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(WY = get_fy(Date, opt_fy_start = '10-01')) %>%
  mutate(wDay = waterDay(Date)) %>% 
  group_by(WY) %>%
  mutate(cumuloss = cumsum(Loss)) %>%
  mutate(Status = case_when(WY < 2009 ~ 'Pre-2009 BiOp',
                            WY > 2008  ~ '2009 & 2019 BiOps')) %>%
  left_join(wytype, by = 'WY') %>%
  filter(!is.na(Date)) %>%
  mutate(History = if_else(WY == '2024', 'Current', 'Past')) %>%
  mutate(Type2 = if_else(TYPE %in% c('W', 'AN'), 'Wet', 'Dry')) %>%
  mutate(Status = factor(Status, levels = c('Pre-2009 BiOp', '2009 & 2019 BiOps')))

#summarize additional data for graphing
years <- cumulative %>% 
  group_by(WY, Status, TYPE, Type2) %>% 
  summarize(Day = max(wDay), Loss = max(cumuloss))
max2024 <- cumulative %>% 
  filter(WY == 2024 & cumuloss == max(cumuloss)) %>% 
  pull(cumuloss)
loss2024 <- cumulative %>% 
  filter(WY == 2024) %>% 
  select(8,9)

#Create Plot
CumulLoss <- ggplot() +
  geom_line(filter(cumulative, WY < 2024), 
            mapping = aes(x = wDay, y = cumuloss, group = factor(WY)), color = 'grey', linewidth = 1)+
  geom_line(loss2024,
            mapping = aes(x = wDay, y = cumuloss), color = 'steelblue2', linewidth = 1.5) +
  geom_text(filter(years, Loss >= max2024 & WY < 2024), mapping = aes(x = Day+6, y = Loss, label = WY), 
            size = 2.5, fontface = 'bold') +
  facet_grid(Type2 ~ factor(Status, levels = c('Pre-2009 BiOp', '2009 & 2019 BiOps'))) +
  theme(legend.position = 'none',
        plot.margin = ggplot2::margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=ggplot2::margin(t=10)),
        axis.title.y = element_text(margin=ggplot2::margin(r=10)),
        strip.text = element_text(face = 'bold.italic'),
        axis.title = element_text(size = 15)) +
  labs(x = 'Date', y = 'Cumulative Loss', title = 'Current and Historic Steelhead Cumulative Loss') +
  scale_x_continuous(breaks = c(0,100,200,300), labels = c('Oct 1', 'Jan 8', 'Apr 18', 'July 26')) +
  scale_y_continuous(breaks = seq(0,15000,2500))
CumulLoss

#################### Historic natural vs. hatchery Steelhead loss and FL histograms over time

#importing, cleaning, and summarizing data from SacPAS
loss2 <- read.csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=2%3Aall&dnaOnly=no&age=no') %>%
  mutate(WY = get_fy(as.Date(Sample.Time), opt_fy_start = '10-01')) %>%
  filter(Adipose.Clip %in% c('Clipped', 'Unclipped'))


#making graph for historic loss comparison
historic <- loss2 %>%
  group_by(WY, Adipose.Clip) %>%
  summarize(Loss = sum(Loss)) %>%
  ggplot() + 
  geom_col(aes(x = factor(WY), y = Loss, fill = Adipose.Clip), position = 'dodge') +
  labs(x = 'Water Year', y = 'Total Loss') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.title = element_blank(),
        plot.margin = ggplot2::margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=ggplot2::margin(t=10)),
        axis.title.y = element_text(margin=ggplot2::margin(r=10)),
        legend.position = "top")
historic

ggsave(historic,
       filename = file.path(here::here("SH_Cohort/Figures/historic_SH_loss_barplot.png")), 
       width = 8, height = 6, units = "in")

#making graph for hatchery vs wild FL comparison with a few options
size <- loss2 %>% #facets by WY
  filter(Length < 750) %>% 
  filter(WY > 2004) %>%
  ggplot(aes(x = Length, y = Adipose.Clip, fill = Adipose.Clip)) +
  geom_density_ridges(stat = "binline", bins = 30, scale = 1.25, draw_baseline = FALSE) +
  facet_wrap(~WY) +
  labs(x = 'Fork Length (mm)') +
  theme(legend.position = 'none',
        plot.margin = ggplot2::margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=ggplot2::margin(t=10)),
        axis.title.y = element_text(margin=ggplot2::margin(r=10)))
size

size2 <- loss2 %>% #facets by clipped vs unclipped with WY on Y axis
  filter(Length < 750) %>%
  filter(WY > 2004) %>%
  ggplot(aes(x = Length, y = factor(WY), fill = Adipose.Clip)) +
  geom_density_ridges(scale = 1.5, draw_baseline = FALSE) +
  facet_wrap(~Adipose.Clip) +
  labs(x = 'Fork Length (mm)', y = 'Water Year') +
  theme(legend.position = 'none',
        plot.margin = ggplot2::margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=ggplot2::margin(t=10)),
        axis.title.y = element_text(margin=ggplot2::margin(r=10)))
size2

ggsave(size2,filename = file.path(here::here("SH_Cohort/Figures/size_distribution_historical.png")), 
       width = 6, height = 7, units = "in")

size3 <- loss2 %>% #facets by clipped vs unclipped with WY2024 overlaying historic WYs combined
  filter(Length < 750) %>%
  ggplot() +
  geom_density(filter(loss2, WY != 2024), 
               mapping = aes(x = Length), fill = 'darkgrey', color = NA) +
  geom_histogram(filter(loss2, WY == 2024), 
                 mapping = aes(x = Length, y=after_stat(density)), 
                 fill = 'steelblue3',
                 color = '#333333',
                 bins = 50,
                 alpha = 0.6) +
  labs(x = 'Fork Length (mm)', y = 'Proportional Density') +
  facet_wrap(~Adipose.Clip, ncol = 1) +
    theme(legend.position = 'none',
          plot.margin = ggplot2::margin(0.5,0.5,0.25,0.25, unit = 'cm'),
          axis.title.x = element_text(margin=ggplot2::margin(t=10)),
          axis.title.y = element_text(margin=ggplot2::margin(r=10)))
size3

ggsave(size3,filename = file.path(here::here("SH_Cohort/Figures/size_distribution.png")), 
       width = 6, height = 7, units = "in")

#################### Tillotson model hindcast
#Load packages
require(randomForest)
require(quantregForest)
require(zoo)
require(caret)

#Source model setup and training script, requires "ITMData.rda" to be in same folder
source("SH_Cohort/Steelhead_Model_Setup.r")
wyWeeks <- data.frame(Date = seq(as.Date('2024-01-01'), Sys.Date(), 1)) %>%
  mutate(wyWeek = week(as.Date((waterDay(Date)-1), origin = as.Date("2024-01-01")))) %>%
  group_by(wyWeek) %>% summarize(Week = format(min(Date), "%b-%d"))

#reload Loss table from SacPAS and clean up
loss3 <- read.csv(paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year='
                        ,get_fy(Sys.Date(), opt_fy_start = '10-01'),'&species=2%3Af&dnaOnly=no&age=no')) 
SHLoss <- loss3 %>%
  mutate(Date = as.Date(Sample.Time)) %>%
  group_by(Date) %>%
  summarize(Loss = sum(Loss, na.rm = TRUE)) %>%
  mutate(wyWeek = week(as.Date((waterDay(Date)-1), origin = as.Date("2024-01-01")))) %>%
  ungroup() %>%
  group_by(wyWeek) %>% 
  summarize(Loss = sum(Loss)) %>% 
  filter(wyWeek > 13)

#pull environmental factors from CDEC and organize into inputs for Tillotson model
sac <- cdec_query('FPT', '20', 'D', "2024-01-01", Sys.Date()) %>% #Sacramento flows at Freeport
  filter(!is.na(parameter_value)) %>% select(Date = 3, FPT = 5) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(wyWeek = week(as.Date((waterDay(Date)-1), origin = as.Date("2024-01-01")))) %>%
  group_by(wyWeek) %>% summarize(sac = mean(FPT, na.rm = TRUE))

sjr <- cdec_query('VNS', '41', 'D', "2024-01-01", Sys.Date()) %>% #San Joaquin flows at Vernalis
  filter(!is.na(parameter_value)) %>% select(Date = 3, VNS = 5) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(wyWeek = week(as.Date((waterDay(Date)-1), origin = as.Date("2024-01-01")))) %>%
  group_by(wyWeek) %>% summarize(sjr = mean(VNS, na.rm = TRUE))

omrMissing <- read.csv('Data/missingOMR.csv') %>% #missing OMR values from Mar21-Apr1
  mutate(Date = as.Date(Date, "%m/%d/%Y"))
omr <- cdec_query('OMR', '41', 'D', "2024-01-01", Sys.Date()) %>% #Old and Middle River flows
  filter(!is.na(parameter_value)) %>% select(Date = 3, OMR = 5) %>%
  mutate(Date = as.Date(Date)) %>%
  bind_rows(omrMissing) %>%
  mutate(wyWeek = week(as.Date((waterDay(Date)-1), origin = as.Date("2024-01-01")))) %>%
  group_by(wyWeek) %>% summarize(omr = mean(OMR, na.rm = TRUE))

cvp <- cdec_query('TRP', '70', 'D', "2024-01-01", Sys.Date()) %>% #combined CVP and SWP exports
  filter(!is.na(parameter_value)) %>% select(Date = 3, TRP = 5)
exports <- cdec_query('HRO', '70', 'D', "2024-01-01", Sys.Date()) %>% 
  filter(!is.na(parameter_value)) %>% select(Date = 3, HRO = 5) %>%
  left_join(cvp, by = 'Date') %>% 
  mutate(Flow = HRO + TRP) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(wyWeek = week(as.Date((waterDay(Date)-1), origin = as.Date("2024-01-01")))) %>%
  group_by(wyWeek) %>% 
  summarize(exports = mean(Flow))

mal <- cdec_query('MAL', '25', 'H', "2024-01-01", Sys.Date()) %>% #temp from mallard
  mutate(Date = as.Date(datetime)) %>%
  filter(!is.na(parameter_value)) %>% mutate(Date = as.Date(Date)) %>%
  mutate(wyWeek = week(as.Date(waterDay(Date), origin = as.Date("2024-01-01")))) %>%
  group_by(wyWeek) %>% summarize(mal = (mean(parameter_value)-32)*(5/9)) %>% na.omit()

#put covariates into dataframe for Tillotson
tillotson <- exports %>% left_join(SHLoss, by = 'wyWeek') %>% 
  left_join(sac, by = 'wyWeek') %>% 
  left_join(sjr, by = 'wyWeek') %>%
  left_join(omr, by = 'wyWeek') %>%
  left_join(mal, by = 'wyWeek')

#run Tillotson model
tillotsonList <- list() #list for storing outputs

for(i in 1:nrow(tillotson)){
  NewData <- data.frame(
    wy_week = tillotson$wyWeek[i] + 1,
    mal_temp = tillotson$mal[i], 
    precip = 1,
    OMR = tillotson$omr[i],
    sac = tillotson$sac[i],
    sjr = tillotson$sjr[i],
    dcc = "closed",
    daily_exports = tillotson$exports[i],
    stlhd_loss.pw = tillotson$Loss[i]
  )
  predictions <- data.frame(predict(Stlhd_Simple_Combined[[2]], newdata = NewData, what = c(0.25, 0.5, 0.75))) %>%
    mutate(wyWeek = tillotson$wyWeek[i], OMR = tillotson$omr[i], Exports = tillotson$exports[i], 
           SAC = tillotson$sac[i], SJR = tillotson$sjr[i], Loss = tillotson$Loss[i])
  tillotsonList[[i]] <- predictions
}

PredLoss <- bind_rows(tillotsonList) %>% 
  left_join(wyWeeks, by = 'wyWeek') %>%
  rename('Qtl25' = 1, 'median' = 2, 'Qtl75' = 3, 'ObsLoss' = 9) %>%
  mutate(Week = fct_inorder(factor(Week), ordered = NA))

tillGraph <- ggplot(PredLoss, aes(x = Week, group = 1, fill = median, color = median)) +
 # geom_line(aes(y = median), color = 'steelblue2') +
  #geom_ribbon(aes(ymin = Qtl25, ymax = Qtl75), fill = 'steelblue2', alpha = .4) +
  geom_crossbar(aes(y = median, ymin = Qtl25, ymax = Qtl75), color = 'black', alpha = .5) +
  geom_line(aes(y = ObsLoss), color = 'black', linetype = 'dashed',linewidth = 1, alpha = .75) +
  labs(y = 'Weekly Loss') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.margin = ggplot2::margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=ggplot2::margin(t=10)),
        axis.title.y = element_text(margin=ggplot2::margin(r=10)),
        legend.position = 'none') +
  scale_fill_distiller(palette = 'Reds', direction = 1)
tillGraph

ggsave(tillGraph,filename = file.path(here::here("SH_Cohort/Figures/tillGraph.png")), 
       width = 7, height = 6, units = "in")

#################### Daily SH loss graph with export info
cvp2 <- cdec_query('TRP', '70', 'D', "2023-12-25", Sys.Date()) %>%
  filter(!is.na(parameter_value)) %>% select(Date = 3, CVP = 5)
exports2 <- cdec_query('HRO', '70', 'D', "2023-12-25", Sys.Date()) %>% 
  filter(!is.na(parameter_value)) %>% select(Date = 3, SWP = 5) %>%
  left_join(cvp2, by = 'Date') %>%
  gather(key = 'Facility', value = 'Flow', 2:3)
omrValues <- data.frame(label = c('-5000 OMR','-3500','-2000','-5000','-3500','-2500','-500','-1500','-2500','COA 8.17'),
                        x = as.Date(c('2024-01-01', '2024-01-14', '2024-01-23', '2024-02-04', '2024-02-08',
                                      '2024-02-17', '2024-03-11', '2024-02-26', '2024-04-01', '2024-04-09')))
dailyLoss <- loss3 %>% select(Date = 1, 2, 12) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Date > as.Date('2023-12-01')) %>%
  group_by(Date, Facility) %>%
  summarize(Loss = sum(Loss)) %>%
  left_join(exports2, by = c('Date', 'Facility')) %>%
  mutate(Flow = Flow/21) %>%
  mutate(Date = as.Date(Date))

dailyGraph <- ggplot() + 
  geom_line(dailyLoss, mapping = aes(x = Date, y = Flow, linetype = Facility), linewidth = 1, alpha = 0.75) +
  geom_col(dailyLoss, mapping = aes(x = Date, y = Loss, fill = Facility), position = 'dodge', alpha = 0.75) +
  geom_vline(omrValues, mapping = aes(xintercept = x), color = '#999999', alpha =0.7) +
  geom_label(omrValues, mapping = aes(x = x, y = 220, label = label), angle = -90, size = 2.75, fontface = 'bold') +
  scale_y_continuous(sec.axis = sec_axis(~.*21, name = 'Export (cfs)')) +
  theme(legend.position = 'bottom',
        plot.margin = ggplot2::margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=ggplot2::margin(t=10)),
        axis.title.y = element_text(margin=ggplot2::margin(r=10)),
        axis.title.y.right = element_text(margin=ggplot2::margin(l=10)))
dailyGraph


