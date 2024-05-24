library(tidyverse)
library(busdater)
library(ggrepel)
library(patchwork)

url_orig <- 'https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=1%3Af&dnaOnly=no&age=no'
url <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=1%3Af&dnaOnly=no&age=no"
loss <- read.csv(url) %>%
  filter(LAD.Race == 'Winter'| DNA.Race == 'Winter')
waterDay <- readRDS('Code/waterDay.rds')
root <- here::here()
viz_output_root <- file.path(root,"Viz_Output")
wytype <- read.csv('Data/WYtype.csv') %>% filter(Basin == "SacramentoValley") %>%mutate(TYPE = Yr.type)
loss <- loss %>% select(Date = 1, 2, 5, 6, 11, 12, ExpSalv = 13, 14) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(WY = get_fy(Date, opt_fy_start = '10-01')) %>%
  mutate(wDay = waterDay(Date))

cumulativeLAD <- loss %>% filter(LAD.Race == 'Winter') %>% group_by(WY) %>%
  mutate(cumloss = cumsum(Loss), cumsalvage = cumsum(ExpSalv), cumCount = cumsum(nfish)) %>%
  mutate(Status = case_when(WY < 2009 ~ 'Pre-2009 BiOp',
                            WY > 2008 ~ '2009 & 2019 BiOp')) %>%
  left_join(wytype, by = 'WY') %>%
  filter(!is.na(Date)) %>%
  mutate(History = if_else(WY == '2024', 'Current', 'Past')) %>%
  mutate(Type2 = if_else(TYPE %in% c('W', 'AN'), 'Wet', 'Dry')) %>%
  mutate(Status = factor(Status, levels = c('Pre-2009 BiOp', '2009 & 2019 BiOp')))
yearsLAD <- cumulativeLAD %>% group_by(WY, Status, TYPE, Type2) %>% summarize(Day = max(wDay), Loss = max(cumloss))
max2024LAD <- cumulativeLAD %>% filter(WY == 2024 & cumloss == max(cumloss)) %>% pull(cumloss)
loss2024LAD <- cumulativeLAD %>% filter(WY == 2024) %>% select(10, 11)
label2024 <- data.frame(x = 90, y = 1250, label = '2024')

WR <- ggplot() +
  geom_line(filter(cumulativeLAD, WY < 2024), 
            mapping = aes(x = wDay, y = cumloss, group = factor(WY)), color = 'grey', linewidth = 1)+
  geom_line(loss2024LAD, 
            mapping = aes(x = wDay, y = cumloss), color = 'steelblue2', linewidth = 1.5)+
  geom_text_repel(filter(yearsLAD, Loss > max2024LAD), mapping = aes(x = Day+1, y = Loss, label = WY), 
            size = 3, fontface = 'bold', alpha = 0.75, min.segment.length = .1) +
  facet_grid(Status ~ Type2) +
  labs(x = 'Date', y = 'Cumulative Loss', title = 'Winter-run Current and Historic Cumulative Salvage') +
  scale_x_continuous(breaks = c(93,152,213,274), labels = c('Jan', 'Mar', 'May', 'Jul')) +
  theme(plot.margin = margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=margin(t=10)),
        axis.title.y = element_text(margin=margin(r=10)))
WR
ggsave(WR,filename = file.path(viz_output_root,"WR_loss_historic.png"), width = 7, height = 5, units = "in")

JPE <- read.csv('Data/JPE_Genetic_Loss_Comparison.csv') %>% select(WY = 1, JPE = 6)

cumulativeGen <- read.csv('Data/WRgenetic.csv') %>% 
  mutate(SampleDateTime = mdy(SampleDateTime)) %>%
  rename('Date' = 'SampleDateTime', 'Loss' = 'Loss_GeneticData') %>%
  mutate(WY = get_fy(Date, opt_fy_start = '10-01')) %>%
  mutate(wDay = waterDay(Date)) %>%
  left_join(JPE, by = 'WY') %>%
  group_by(WY, wDay) %>%
  summarize(Loss = sum(Loss), JPE = min(JPE)) %>% ungroup() %>%
  group_by(WY) %>% mutate(CumulLoss = cumsum(Loss)) %>%
  mutate(JPEprop = (CumulLoss/JPE)*100)

yearsGen <- cumulativeGen %>% group_by(WY) %>% summarize(Day = max(wDay), JPEprop = max(JPEprop))
max2024Gen <- cumulativeGen %>% filter(WY == 2024 & JPEprop == max(JPEprop)) %>% pull(JPEprop)

WRGen <- ggplot() +
  geom_line(filter(cumulativeGen, WY < 2024), 
            mapping = aes(x = wDay, y = JPEprop, group = factor(WY)), color = 'grey', linewidth = .5)+
  geom_line(filter(cumulativeGen, WY == 2024), 
            mapping = aes(x = wDay, y = JPEprop), color = 'steelblue2', linewidth = 1)+
  geom_rect(aes(xmin = 25, xmax = 213, ymin = -0.2, ymax = 0.6), fill = 'NA', color = 'black', linetype = 'dashed', linewidth = 1) +
  geom_text(filter(yearsGen, WY ==2001), mapping = aes(x = Day+8, y = JPEprop, label = WY), 
           size = 3, fontface = 'bold', alpha = 0.75) +
  #labs(x = 'Date', y = 'Cumulative Proportion of JPE', title = 'Genetic Winter-run Current and Historic Cumulative Proportion of JPE') +
  scale_x_continuous(breaks = c(93,152,213,274), labels = c('Jan', 'Mar', 'May', 'Jul')) +
  theme(plot.margin = margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
WRGen


WRGenZoom <- ggplot() +
  geom_line(filter(cumulativeGen, WY < 2024 & WY !=2001), 
            mapping = aes(x = wDay, y = JPEprop, group = factor(WY)), color = 'grey', linewidth = 1)+
  geom_line(filter(cumulativeGen, WY == 2024), 
            mapping = aes(x = wDay, y = JPEprop), color = 'steelblue2', linewidth = 1.5)+
  geom_text_repel(filter(yearsGen, JPEprop >= max2024Gen), mapping = aes(x = Day, y = JPEprop, label = WY), 
            size = 3, fontface = 'bold', alpha = 0.75, min.segment.length = .05, direction = 'y') +
  scale_x_continuous(breaks = c(30,93,152,213), labels = c('Nov', 'Jan', 'Mar', 'May'), limits = c(min(cumulativeGen$wDay), 213)) +
  labs(x = 'Date', y = 'Cumulative Percentage of JPE', 
       title = 'Genetic Winter-run Current and Historic Cumulative Percentage of JPE') +
  scale_y_continuous(breaks = seq(0,.6,0.1), limits = c(0,.6)) +
  theme(plot.margin = margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=margin(t=10)),
        axis.title.y = element_text(margin=margin(r=10)))
WRGenZoom

graph <- WRGenZoom + inset_element(WRGen, left = 0.01, bottom = 0.65, right = 0.3, top = 0.99)
graph
ggsave(graph, file = 'Viz_Output/WRcumulJPE.png', units = 'px', width = 2750, height = 1750)
ggsave(WR, file = 'Viz_Output/WRcumul.png', units = 'px', width = 2500, height = 1500)
