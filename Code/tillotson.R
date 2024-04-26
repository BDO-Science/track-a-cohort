library(tidyverse)

data <- read.csv('Data/tillotson.csv')

graph <- ggplot(data, aes(x = OMR)) + geom_line(aes(y = Median), color = 'steelblue2', linewidth = 1.5) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = 'steelblue2', alpha = 0.3) +
  labs(x = 'OMR Scenarios', y = 'Median Weekly Loss', title = 'Predicted winter-run loss') +
  theme(legend.position = 'none',
        plot.margin = margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=margin(t=10)),
        axis.title.y = element_text(margin=margin(r=10))) +
  scale_y_continuous(breaks = seq(0,300,25))
graph

ggsave(graph, file = 'Viz_Output/tillotson.png', units = 'px', width = 2250, height = 1250)
