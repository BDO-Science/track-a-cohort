library(tidyverse)
library(readr)
library(busdater)
library(patchwork)
library(rvest)
#set source folder, destination folder, and read in most recent salmon and steelhead files
#pulling in most recent steelhead salvage
url <- "https://filelib.wildlife.ca.gov/Public/salvage/Salmon%20Monitoring%20Team/" #salvage files url
page <- read_html(url) #read url in
links <- page %>% html_nodes("a") %>% html_attr("href") # Extract all links from the webpage
salmon_links <- links[grep("salmon_", links, ignore.case = TRUE)]  # Select files containing "salmon_"
salmon_links <- salmon_links[!grepl("summary", salmon_links, ignore.case = TRUE)]  # Exclude files containing "summary"
salmon_links <- salmon_links[!grepl("table", salmon_links, ignore.case = TRUE)] # Exclude files containing "table"
file <- max(grep("\\.csv$", salmon_links, value = TRUE, ignore.case = TRUE)) #isolate the most recent salmon salvage table

#read in salmon salvage file
salmon <- read.csv(paste0('https://filelib.wildlife.ca.gov/',file)) %>%
  mutate(SampleDate = as.Date(SampleDate))

#summarize genetic daily limit exceedance. Resulting table is Table 1.
daily_trigger <- salmon %>% filter(DNA_Race == 'W') %>%
  group_by(SampleDate) %>% summarize(Loss = sum(LOSS, na.rm = TRUE)) %>%
  mutate(Perc_JPE = (Loss/234896)*100) %>%
  mutate(Threshold = case_when(month(SampleDate) == 1 ~ .00124, #add in daily trigger limit by month
                              month(SampleDate) == 2 ~ .00231,
                              month(SampleDate) == 3 ~ .00372,
                              month(SampleDate) == 4 ~ .00226,
                              month(SampleDate) == 5 ~ 0)) %>%
  mutate(Trigger = if_else(Perc_JPE > Threshold, 'Y', 'N')) #check if genetic loss exceeds daily trigger


#filtering for LAD WR and Genetic WR and recombining into dataframe for graph
salmonLAD <- salmon %>% 
  filter(ADCLIP == 'N', Size_Race == 'W') %>%
  select(Date = 2,13) %>% 
  replace(is.na(.), 0) %>% 
  mutate(cumlost = cumsum(LOSS)) %>%
  mutate(Type = 'Length at Date')
salmonGen <- salmon %>%
  filter(ADCLIP == 'N', DNA_Race == 'W') %>%
  select(Date = 2,13) %>% 
  replace(is.na(.), 0) %>% 
  mutate(cumlost = cumsum(LOSS)) %>%
  mutate(Type = 'Genetic') %>% 
  bind_rows(tibble(Date = max(salmonLAD$Date), 
                   LOSS = max(.$LOSS),
                   cumlost = max(.$cumlost), 
                   Type = 'Genetic'))
salmonAll <- bind_rows(salmonLAD, salmonGen)

#graph for LAD salmon
SalthresholdsLAD <- data.frame(Thresholds = c('50%', '75%', '100%', 'Single-year ITL (2% of JPE)'), values = c(1374, 2061, 2748, 4698))
salmonmax <- salmonAll %>% group_by(Type) %>% summarize(Date = max(Date), cumlost = max(cumlost))
Sal1 <- ggplot() + 
  geom_line(filter(salmonAll, Type == 'Length at Date'), 
            mapping = aes(x = Date, y = cumlost), 
            linewidth = 1,
            color = 'steelblue3') + 
  geom_hline(SalthresholdsLAD, mapping = aes(yintercept = values, 
                                             linetype = Thresholds), 
             linewidth = 0.5) + 
  geom_text(SalthresholdsLAD, mapping = aes(x = as.Date(min(salmonAll$Date))-7, y = values, 
                                            label = c('50% Annual Loss Threshold (LAD)', '75%', 
                                                      '100%', 'Single-year ITL (2% of JPE)')), 
            fontface = 'bold', vjust = 1.4, size = 3, color = '#333333', hjust=0) +
  geom_vline(aes(xintercept = as.Date('2024-03-11')), linewidth = 1, 
             linetype = 'dashed', color = 'darkgreen', alpha = 0.6) +
  geom_text(aes(x = as.Date('2024-03-10'), y = 4000, label = '-500 flow \n action'), 
            hjust = .95, fontface = 'bold', size = 3) +
  geom_label(filter(salmonmax, Type == 'Length at Date'), mapping = aes(x = Date + 1, y = cumlost+150,
                                                                        label = paste0(cumlost,
                                                                                       ' (',round(((cumlost/4698)*100),1),'%)')), size = 3) +
  labs(y = 'Cumulative Loss', x = 'Date', title = 'Winter-run LAD Loss') + 
  scale_y_continuous(breaks = seq(0,plyr::round_any(max(SalthresholdsLAD$values), 1000, ceiling), 500)) +
  scale_x_date(date_breaks = '3 weeks', date_labels = "%b %d", limits = c(as.Date('2024-01-01'), Sys.Date() + 14)) +
  guides(linetype = FALSE) +
  theme(legend.position = 'none',
        plot.margin = margin(0.2,0.2,0.2,0.2, unit = 'cm'),
        axis.title.y = element_text(margin=margin(r=15), size = 15),
        axis.title.x = element_text(margin=margin(t=15), size = 15))
Sal1

#graph for genetic salmon
SalthresholdsGen <- data.frame(Thresholds = c('50%', '75%', '100%'), values = c(592, 888, 1184))
salmonmax <- salmonAll %>% group_by(Type) %>% summarize(Date = max(Date), cumlost = max(cumlost))
Sal2 <- ggplot() + 
  geom_line(filter(salmonAll, Type == 'Genetic'), 
            mapping = aes(x = Date, y = cumlost), 
            linewidth = 1,
            color = 'steelblue3') + 
  geom_hline(SalthresholdsGen, mapping = aes(yintercept = values, 
                                             linetype = Thresholds), 
             linewidth = 0.5) + 
  geom_text(SalthresholdsGen, mapping = aes(x = as.Date(min(salmonAll$Date))-7, y = values, 
                                            label = c('50% Annual Loss Threshold (Genetic)', '75%', 
                                                      '100%')), 
            fontface = 'bold', vjust = 1.4, color = '#333333', hjust=0, size = 3) +
  geom_vline(aes(xintercept = as.Date('2024-03-11')), linewidth = 1, 
             linetype = 'dashed', color = 'darkgreen', alpha = 0.6) +
  geom_text(aes(x = as.Date('2024-03-10'), y = 4000, label = '-500 flow \n action'), 
            hjust = .95, fontface = 'bold', size = 3) +
  geom_label(filter(salmonmax, Type == 'Genetic'), mapping = aes(x = Date + 1, y = cumlost+150,
                                                                 label = paste0(cumlost,
                                                                                ' (',round(((cumlost/1184)*100),1),'%)')), size = 3) +
  labs(y = 'Cumulative Loss', title = 'Winter-run Genetic Loss') + 
  scale_y_continuous(breaks = seq(0,plyr::round_any(max(SalthresholdsLAD$values), 1000), 100), limits = c(0, 1200)) +
  scale_x_date(date_breaks = '3 weeks', date_labels = "%b %d", limits = c(as.Date('2024-01-01'), Sys.Date() + 14)) +
  guides(linetype = FALSE) +
  theme(legend.position = 'none',
        plot.margin = margin(0.2,0.2,0.2,0.2, unit = 'cm'),
        axis.title.x = element_text(margin=margin(t=15), size = 15))
Sal2

WRGraph <- Sal1|(Sal2 +
                   theme(axis.text.y = element_blank(),
                         axis.title.y = element_blank()) +
                   scale_y_continuous(breaks = seq(0,plyr::round_any(max(SalthresholdsLAD$values), 1000), 500), 
                                      limits = c(0, max(SalthresholdsLAD$values))))
WRGraph #This is figure 7

ggsave(WRGraph,filename = file.path(viz_output_root,"WR_LAD_Genetic_cumloss_thresholds.png"), width = 7.5, height = 5, units = "in")
