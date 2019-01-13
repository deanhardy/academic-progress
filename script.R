################################################
## PURPOSE: For tracking progress of manuscripts, proposals, and reviews
## BY: Dean Hardy
################################################
rm(list=ls())

library(tidyverse)
library(lubridate)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/manuscript-timelines')

df <- read.csv(file.path(datadir, "data.csv")) %>%
  mutate_all(as.character) %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  group_by(id) %>%
  filter(status != "published", type == "ms") %>%
  mutate(start_date = date,
         end_date = c(date[-1], 
                      if_else(last(action) == "accepted", last(date), Sys.Date())))

# cols <- c('red', 'blue')

fig <- ggplot(df) +
  # geom_rect(aes(xmin = start_date,
  #               xmax = end_date,
  #               ymin = id, ymax = id,
  #               color = status,
  #               fill = NULL),
  #           filter(df, status != "accepted"),
  #           size = 0.8) +
  geom_linerange(aes(x = id,
                     ymax = end_date,
                     ymin = start_date,
                     color = status),
                 size = 1,
            filter(df, status != 'accepted')) +
  geom_text(aes(label = place, y = start_date, x = id),
           filter(df, action == 'initial submission'),
           hjust = 1.8, vjust = 0.4,
           size = 4) +
  geom_point(aes(y = start_date, x = id),
             filter(df, status == 'accepted'),
             size = 2,
             shape = 1,
             show.legend = F) +
  geom_hline(yintercept = as.Date('2016-08-05'), linetype = 'dashed') +
  geom_text(aes(label = '<<< PhD conferred', y = as.Date('2017-06-05'), x = 'ms02'),
            size = 3) +
  labs(x = "Manuscript (#)") +
  scale_y_date(name = "Year", date_breaks = "1 year", date_labels = "%Y") + 
  scale_color_manual(name = "Status",
                     labels = c("In revision", "Under review"),
                     values = c('black', 'grey55')) +
  # scale_shape_manual(name = '', values = 1, labels = 'Accepted') + 
  # scale_shape_manual(name = 'Status', values = c(45,45,1), labels = c("In revision", "Under review", "Accepted")) +
  ggtitle("Manuscript Timelines") + 
  theme(legend.background = element_rect(color = "black"),
        legend.key = element_rect(fill = 'white'),
        legend.position = c(0.18, 0.8),
        panel.background = element_rect('white'),
        panel.border = element_rect(colour = 'black', fill = "transparent"),
        panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
        axis.text = element_text(color = 'black'),
        plot.margin = margin(0.5,0.5,0.5,0.5, 'cm')) +
  labs(caption = "*Leading number indicates author position.") + 
  coord_flip()
fig

# png('fig_pubd.png', width = 5, height = 5, units = 'in', res = 150)
# fig
# dev.off()

tiff(file.path(datadir, "figure.tiff"), width = 5.5, height = 5, units = "in", 
     res = 300, compression = "lzw")
fig
dev.off()


# date_test <- df %>%
#   mutate(end_status = c(date[-1], Sys.Date()))

         