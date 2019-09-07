################################################
## PURPOSE: For tracking progress of manuscripts, proposals, and reviews
## BY: Dean Hardy
################################################
rm(list=ls())

library(tidyverse)
library(lubridate)
library(gridExtra)
library(cowplot)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/academic-progress')

## import data
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
  

## manuscripts
ms <- df %>%
  filter(!(status %in% c("published", "terminated")), type == "ms")

fig_ms <- ggplot(ms) +
  geom_linerange(aes(x = id,
                     ymax = end_date,
                     ymin = start_date,
                     color = status),
                 size = 1,
            filter(ms, status != 'accepted'),
            show.legend = F) +
  geom_errorbar(aes(x = id,
                    ymax = as.Date("2000-01-01"),
                    ymin = as.Date('2010-01-01'),
                    color = status),
                size = 1,
                filter(ms, status != 'accepted'),
                show.legend = T) + ## trick for making horizontal legend bars
  geom_text(aes(label = place, y = start_date, x = id),
           filter(ms, action == 'initial submission'),
           hjust = 1.8, vjust = 0.4,
           size = 4) +
  geom_point(aes(y = start_date, x = id),
             filter(ms, status == 'accepted'),
             size = 3,
             shape = 8,
             show.legend = F) +
  geom_hline(yintercept = as.Date('2016-08-05'), linetype = 'dashed') +
  geom_text(aes(label = '<<< PhD conferred', y = as.Date('2017-04-01'), x = 'ms02'),
            size = 3) +
  geom_hline(yintercept = as.Date('2018-12-18'), linetype = 'dashed') +
  geom_text(aes(label = 'UofSC offer accepted >>>', y = as.Date('2018-01-18'), x = 'ms03'),
            size = 3) +
  labs(x = "Manuscript ID") +
  scale_y_date(name = "Year", date_breaks = "1 year", date_labels = "%Y",
               limits = c(first(df$date), Sys.Date())) +  
  scale_color_manual(name = "MS Status",
                     labels = c("In revision", "Under review"),
                     #values = c('dark blue', 'dark red')) + 
                     values = c('black', 'grey55')) + 
  theme(legend.background = element_rect(color = "black"),
        legend.key = element_rect(fill = 'white'),
        legend.position = c(0.1, 0.8),
        panel.background = element_rect('white'),
        panel.border = element_rect(colour = 'black', fill = "transparent"),
        panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
        axis.text = element_text(color = 'black'),
        plot.margin = margin(0.5,0.5,0.5,0.5, 'cm')) +
  labs(caption = "*Leading number indicates author/Co-PI position. Asterisk indicates accepted/awarded.") + 
  coord_flip()
fig_ms

## peer reviews
rv <- df %>% filter(type == 'rv')
fig_rv <- ggplot(rv) +
  geom_linerange(aes(x = id,
                     ymax = end_date,
                     ymin = start_date),
                 size = 1,
                 show.legend = F) +
  geom_hline(yintercept = as.Date('2016-08-05'), linetype = 'dashed') +
  geom_hline(yintercept = as.Date('2018-12-18'), linetype = 'dashed') +
  labs(x = "Peer Review ID") +
  scale_y_date(name = "", date_breaks = "1 year", date_labels = "%Y",
               limits = c(first(df$date), Sys.Date())) + 
  theme(legend.background = element_rect(color = "black"),
        legend.key = element_rect(fill = 'white'),
        legend.position = c(0.18, 0.8),
        panel.background = element_rect('white'),
        panel.border = element_rect(colour = 'black', fill = "transparent"),
        panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
        axis.text = element_text(color = 'black'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, 'cm')) +
  coord_flip()
fig_rv

## grant proposals
gr <- filter(df, type == 'gr')

fig_gr <- ggplot(gr) +
  geom_linerange(aes(x = id,
                     ymax = end_date,
                     ymin = start_date),
                 size = 1,
                 show.legend = F) +
  geom_text(aes(label = place, y = start_date, x = id),
            filter(gr, action %in% c('proposal submitted')),
            hjust = 1.8, vjust = 0.4,
            size = 4) +
  geom_point(aes(y = end_date, x = id),
             filter(gr, status == 'awarded'),
             size = 3,
             shape = 8,
             show.legend = F) +
  geom_hline(yintercept = as.Date('2016-08-05'), linetype = 'dashed') +
  geom_hline(yintercept = as.Date('2018-12-18'), linetype = 'dashed') +
  labs(x = "Grant Proposal ID") +
  scale_y_date(name = "", date_breaks = "1 year", date_labels = "%Y",
               limits = c(first(df$date), Sys.Date())) + 
  theme(legend.background = element_rect(color = "black"),
        legend.key = element_rect(fill = 'white'),
        legend.position = c(0.18, 0.8),
        panel.background = element_rect('white'),
        panel.border = element_rect(colour = 'black', fill = "transparent"),
        panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
        axis.text = element_text(color = 'black'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = margin(0.5,1.5,0.5,0.5, 'cm')) +
  coord_flip()
fig_gr

# legd <- legendGrob(c('Published/Awarded', 1, 5, pch = 2))

## combining all
tiff(file.path(datadir, "fig-faceted.tiff"), width = 7, height = 9, units = "in", 
     res = 300, compression = "lzw")
# grid.arrange(fig_rv, fig_gr, fig_ms, ncol = 1, nrow = 3)
plot_grid(fig_rv, fig_gr, fig_ms, align = 'v', nrow = 3, rel_heights = c(1/3, 1/4, 1/2))
dev.off()


############################
## Google Scholar grab
############################
library(scholar)

ids <- c('id3d88wAAAAJ&hl', 'CM8MRr8AAAAJ&hl')
mss <- 100

compare_scholars(ids, mss)

#  git remote set-url origin https://github.com/deanhardy/academic-progress.git

