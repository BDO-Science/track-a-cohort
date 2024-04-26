library(tidyverse)
library(readr)
library(busdater)
library(patchwork)

files <- list.files(path = here::here('Data/STARS/'))
waterDay <- readRDS('Code/waterDay.rds')

stars <- read_csv(paste0('Data/STARS/',files)) %>% bind_rows() %>%
  select(1, surv = 2, survL80 = 3, survU80 = 4, idsurv = 17, idsurvL80 = 18, idsurvU80 = 19, idRoute = 32, idRouteL80 = 33, idRouteU80 = 34) %>% arrange(Date) %>%
  mutate(WY = get_fy(Date, opt_fy_start = '10-01')) %>%
  mutate(wDay = waterDay(Date))


stars2 <- stars %>% select(12,Group = 11,2:10)


stars_graph_surv <- ggplot(stars, mapping = aes(x = wDay)) + 
  geom_line(stars2, mapping = aes(y = surv, group = Group), color = 'grey', linewidth = .5) + 
  geom_line(stars, mapping = aes(y = surv), color = 'steelblue2', linewidth = 1) +
  geom_ribbon(stars, mapping = aes(ymin = survL80, ymax = survU80), fill = 'steelblue2', alpha = 0.5) + 
  facet_grid(WY~.) +
  labs(x = 'Date', y = 'Probability', title = 'STARS Total Survival') +
  scale_x_continuous(breaks = c(1,62,124,183,244), labels = c('Oct', 'Dec', 'Feb', 'Apr', 'Jun'), limits = c(0,272)) +
  theme(legend.position = 'none',
        plot.margin = margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=margin(t=10)),
        axis.title.y = element_text(margin=margin(r=10)),
        title = element_text(size = 10))
stars_graph_surv

ggsave(stars_graph_surv,filename = file.path(viz_output_root,"stars_total_survival.png"), width = 6, height = 8)


stars_graph_route <- ggplot(stars, mapping = aes(x = wDay)) + 
  geom_line(stars2, mapping = aes(y = idRoute, group = Group), color = 'grey', linewidth = .5) + 
  geom_line(stars, mapping = aes(y = idRoute), color = '#009933', linewidth = 1) +
  geom_ribbon(stars, mapping = aes(ymin = idRouteL80, ymax = idRouteU80), fill = '#009933', alpha = 0.5) + 
  facet_grid(WY~.) +
  labs(x = 'Date', y = 'Routing Probability', title = 'STARS Interior Delta Routing Probability') +
  scale_x_continuous(breaks = c(1,62,124,183,244), labels = c('Oct', 'Dec', 'Feb', 'Apr', 'Jun'), limits = c(0,272)) +
  theme(legend.position = 'none',
        plot.margin = margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        title = element_text(size = 10))
stars_graph_route
ggsave(stars_graph_route,filename = file.path(viz_output_root,"stars_routing.png"), width = 6, height = 8)

stars_graph_IDsurv <- ggplot(stars, mapping = aes(x = wDay)) + 
  geom_line(stars2, mapping = aes(y = idsurv, group = Group), color = 'grey', linewidth = .5) + 
  geom_line(stars, mapping = aes(y = idsurv), color = '#CC6666', linewidth = 1) +
  geom_ribbon(stars, mapping = aes(ymin = idsurvL80, ymax = idsurvU80), fill = '#CC6666', alpha = 0.5) + 
  facet_grid(WY~.) +
  labs(x = 'Date', y = 'Probability', title = 'STARS Interior Delta Survival') +
  scale_x_continuous(breaks = c(1,62,124,183,244), labels = c('Oct', 'Dec', 'Feb', 'Apr', 'Jun'), limits = c(0,272)) +
  theme(legend.position = 'none',
        plot.margin = margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin=margin(r=10)),
        title = element_text(size = 10))
stars_graph_IDsurv

ggsave(stars_graph_IDsurv,filename = file.path(viz_output_root,"stars_deltasurvival.png"), width = 6, height = 8)

graph <- (stars_graph_IDsurv|stars_graph_route)/stars_graph_surv
graph
ggsave(graph, file = file.path(viz_output_root,"stars_graphs.png"), units = 'px', width = 2250, height = 2750)
