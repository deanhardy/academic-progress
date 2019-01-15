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
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(id) %>%
  # mutate(start_date = date,
  #        end_date = c(date[-1], 
  #                     if_else(last(action) %in% 
  #                               c("accepted", "review submitted", "proposal awarded", "proposal declined"),
  #                             last(date), Sys.Date()))) %>%
  mutate(type = factor(type, levels = c("ms", "gr", "rv")),
         start_date = if_else(action %in%
                                c("proposal awarded", "proposal declined"), 
                              first(date), date),
         end_date = c(date[-1],
                      if_else(last(action) %in%
                                c("accepted", "review submitted", "proposal awarded", "proposal declined"),
                              last(date), Sys.Date()))) %>%
  arrange(start_date)
  

# cols <- c('red', 'blue')
ms <- df %>%
  filter(status != "published", type == "ms")

fig_ms <- ggplot(ms) +
  geom_linerange(aes(x = id,
                     ymax = end_date,
                     ymin = start_date,
                     color = status),
                 size = 1,
            filter(ms, status != 'accepted')) +
  geom_text(aes(label = place, y = start_date, x = id),
           filter(ms, action == 'initial submission'),
           hjust = 1.8, vjust = 0.4,
           size = 4) +
  geom_point(aes(y = start_date, x = id),
             filter(ms, status == 'accepted'),
             size = 2,
             shape = 1,
             show.legend = F) +
  geom_hline(yintercept = as.Date('2016-08-05'), linetype = 'dashed') +
  geom_text(aes(label = '<<< PhD conferred', y = as.Date('2017-06-05'), x = 'ms02'),
            size = 3) +
  labs(x = "Manuscript (#)") +
  scale_y_date(name = "Year", date_breaks = "1 year", date_labels = "%Y") + 
  scale_color_manual(name = "Status",
                     labels = c("MS in revision", "MS under review"),
                     values = c('black', 'grey55')) + 
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
fig_ms

tiff(file.path(datadir, "fig-ms.tiff"), width = 5.5, height = 5, units = "in", 
     res = 300, compression = "lzw")
fig_ms
dev.off()


##############################
## all combined
##############################
rv <- df %>% filter(type == 'rv')

fig_rv <- ggplot(rv) +
  geom_linerange(aes(x = id,
                     ymax = end_date,
                     ymin = start_date,
                     color = action),
                 size = 1,
                 show.legend = F) +
  geom_hline(yintercept = as.Date('2016-08-05'), linetype = 'dashed') +
  labs(x = "Review ID") +
  scale_y_date(name = "Year", date_breaks = "1 year", date_labels = "%Y") + 
  theme(legend.background = element_rect(color = "black"),
        legend.key = element_rect(fill = 'white'),
        legend.position = c(0.18, 0.8),
        panel.background = element_rect('white'),
        panel.border = element_rect(colour = 'black', fill = "transparent"),
        panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
        axis.text = element_text(color = 'black'),
        plot.margin = margin(0.5,0.5,0.5,0.5, 'cm')) +
  coord_flip()
fig_rv

gr <- filter(df, type == 'gr')

fig_gr <- ggplot(gr) +
  geom_linerange(aes(x = id,
                     ymax = end_date,
                     ymin = start_date,
                     color = action),
                 size = 1,
                 show.legend = F) +
  geom_text(aes(label = place, y = start_date, x = id),
            filter(gr, action %in% c('proposal submitted')),
            hjust = 1.8, vjust = 0.4,
            size = 4) +
  geom_hline(yintercept = as.Date('2016-08-05'), linetype = 'dashed') +
  labs(x = "Grant Proposal ID") +
  scale_y_date(name = "Year", date_breaks = "1 year", date_labels = "%Y") + 
  theme(legend.background = element_rect(color = "black"),
        legend.key = element_rect(fill = 'white'),
        legend.position = c(0.18, 0.8),
        panel.background = element_rect('white'),
        panel.border = element_rect(colour = 'black', fill = "transparent"),
        panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
        axis.text = element_text(color = 'black'),
        plot.margin = margin(0.5,0.5,0.5,0.5, 'cm')) +
  coord_flip()
fig_gr

fig_all <- ggplot(df2) +
  geom_linerange(aes(x = id,
                     ymax = end_date,
                     ymin = start_date,
                     color = status),
                 size = 1,
                 filter(df2, !status %in% c('accepted', 'published')),
                 show.legend = F) +
  geom_text(aes(label = place, y = start_date, x = id),
            filter(df2, action %in% c('initial submission', 'proposal submitted')),
            hjust = 1.8, vjust = 0.4,
             size = 4) +
  geom_point(aes(y = start_date, x = id),
             filter(ms, status == 'accepted'),
             size = 2,
             shape = 1,
             show.legend = F) +
  geom_hline(yintercept = as.Date('2016-08-05'), linetype = 'dashed') +
  geom_text(aes(label = '<<< PhD conferred', y = as.Date('2017-06-05'), x = 'ms05'),
            size = 3) +
  labs(x = "Productivity ID") +
  scale_y_date(name = "Year", date_breaks = "1 year", date_labels = "%Y") + 
  # scale_color_manual(name = "Status",
  #                    labels = c("In revision", "Under review", "Peer Review"),
  #                    values = c('black', 'grey55', 'grey85')) + 
  # ggtitle("Productivity") + 
  theme(legend.background = element_rect(color = "black"),
        legend.key = element_rect(fill = 'white'),
        legend.position = c(0.18, 0.8),
        panel.background = element_rect('white'),
        panel.border = element_rect(colour = 'black', fill = "transparent"),
        panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
        axis.text = element_text(color = 'black'),
        plot.margin = margin(0.5,0.5,0.5,0.5, 'cm')) +
  labs(caption = "*Leading number indicates author/co-PI position.") + 
  coord_flip()
fig_all

tiff(file.path(datadir, "fig-all.tiff"), width = 6, height = 6, units = "in", 
     res = 300, compression = "lzw")
fig_all
dev.off()

facet_grid()
tiff(file.path(datadir, "fig-all-faceted.tiff"), width = 6, height = 6, units = "in", 
     res = 300, compression = "lzw")
fig_all + facet_grid(rows = vars(type))
dev.off()
