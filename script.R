################################################
## PURPOSE: For tracking progress of manuscripts, proposals, and reviews
## BY: Dean Hardy
################################################
rm(list=ls())

library(tidyverse)
library(lubridate)
library(gridExtra)
library(cowplot)

now.date <- Sys.Date()
# now.date <- as.Date('2025-08-15')
  
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
                              last(date), now.date))) %>%
  arrange(start_date)
  

## manuscripts
ms <- df %>%
  filter(!(status %in% c("in press", "terminated")), type == "ms") %>%
  filter(id != 'msXX')
pub <- df %>%
  filter(status == "published", type == "ms")

## number submissions/pubs per year since significant events
evt_dates <- c('2012-07-16', '2015-05-14', '2016-08-05', '2018-12-18', '2019-08-16')
events <- c('First MS', 'First 1st author', 'PhD conferred', 'UofSC offer', 'UofSC start')
evt <- data.frame(evt_dates, events)
ms_rate <- NULL # used in loop appending outputs
x <- c(0, 1, 2, 3) # of years of tenure clock stoppage

# 
for (j in 1:length(x)) {

  for (i in 1:length(evt_dates)) {
    yrs <- interval(evt_dates[[i]], now.date) %>%
    time_length('years')
    
    OUT <- ms %>%
      filter(date >= evt_dates[[i]]) %>%
      group_by(type) %>%
      summarise(submitted = sum(action == 'initial submission'),
                accepted = sum(action == 'accepted'),
                published = sum(action == 'issue assigned')) %>%
      mutate(yrs_tt_stop = x[[j]], 
             s_rate = round(submitted/(yrs-x[[j]]), 2), 
             a_rate = round(accepted/(yrs-x[[j]]), 2),
             p_rate = round(published/(yrs-x[[j]]), 2),
             years = round((yrs-x[[j]]), 2),
             evt_dates = evt_dates[[i]])
    
    ms_rate <- rbind(OUT, ms_rate)
  }
}

ms_rate <- left_join(ms_rate, evt) %>%
  mutate(evt_dates_par = paste('(', evt_dates, ')', sep = '')) %>%
  mutate(event_names = paste(events, evt_dates_par, sep = '\n')) %>%
  select(yrs_tt_stop, events, evt_dates, event_names, years, submitted, accepted, published, s_rate, a_rate, p_rate) %>%
  arrange(yrs_tt_stop, desc(years))

write.csv(ms_rate, file.path(datadir, "ms_rate.csv"))

# lo <- as.character(Sys.Date()) - as.character(as.Date("2014-07-16"))

## number submissions/pubs per year since significant events
cont_dates <- seq(as.Date("2014-02-24", ), by = "day", length.out = 3532)
ms_rate_cont <- NULL # used in loop appending outputs
x <- c(0) # of days of tenure clock stoppage
y <- c('issue assigned', 'accepted') ## 'initial submission'
d <- c("2014-02-24",'2013-06-17')

OUT <- NULL

start.time <- Sys.time()

# ## needs revision of math for rates to be continuous date minus initial date rather than by years and
# ## interval between cont.date initial date.
# for(z in 1:length(y)) {
#   for (w in 1:length(d)) {
#     for (j in 1:length(x)) {
# 
#   for (i in 1:length(cont_dates)) {
#     yrs <- interval(as.Date(d[[w]]), cont_dates[[i]]) %>%
#       time_length('years')
#     
#     OUT <- ms %>%
#       filter(date <= cont_dates[[i]], action == y[[z]]) %>%
#       group_by(type) %>%
#       summarise(sum = sum(action == y[[z]])) %>%
#       mutate(yrs_tt_stop = x[[j]], 
#              action = y[[z]],
#              rate = round(sum/(yrs-x[[j]]), 2), 
#              years = round((yrs-x[[j]]), 2),
#              cont_dates = cont_dates[[i]])
#     
#     ms_rate_cont <- rbind(OUT, ms_rate_cont)
#       }
#     }
#   }
# }
# 
# end.time <- Sys.time()
# end.time - start.time 
# 
# ggplot(filter(ms_rate_cont, yrs_tt_stop == 0, years > 0.99)) +
#   geom_line(aes(cont_dates, rate, color = action))

  
# ms_rate <- left_join(ms_rate, evt) %>%
#   mutate(evt_dates_par = paste('(', evt_dates, ')', sep = '')) %>%
#   mutate(event_names = paste(events, evt_dates_par, sep = '\n')) %>%
#   select(yrs_tt_stop, events, evt_dates, event_names, years, submitted, accepted, published, s_rate, a_rate, p_rate) %>%
#   arrange(yrs_tt_stop, desc(years))

write.csv(ms_rate, file.path(datadir, "ms_rate_continuous.csv"))

## plot submission rates
fig_sr <- ggplot(ms_rate) +
  geom_col(aes(reorder(event_names, -years), s_rate,fill = as.character(yrs_tt_stop)), position = 'dodge2') + 
  xlab("Events") + 
  geom_hline(yintercept = 2, linetype = 'longdash') + 
  scale_y_continuous(name = "Mean Annual Manuscript Submission Rate", breaks = seq(0, 4, 1),
                     minor_breaks = seq(0, 4, 0.1), expand = c(0,0), limits = c(0,4)) + 
  scale_fill_manual(values = c('grey80','grey60', 'grey40', 'grey20')) + 
  labs(fill = 'Tenure Stoppage (years)') + 
  ggtitle(paste("Submission Rates as of", now.date)) +
  theme(legend.background = element_rect(color = "black"),
        legend.key = element_rect(fill = 'white'),
        legend.position = c(0.175, 0.80),
        panel.background = element_rect('whi te'),
        panel.border = element_rect(colour = 'black', fill = "transparent"),
        panel.grid.major.y = element_line('grey', size = 0.5, linetype = "solid"),
        panel.grid.minor = element_line(colour="grey", size=0.1, linetype = 'longdash'),
        axis.text = element_text(color = 'black'),)
fig_sr

## plot acceptance rates
fig_ar <- ggplot(ms_rate) +
  geom_col(aes(reorder(event_names, -years), a_rate,fill = as.character(yrs_tt_stop)), position = 'dodge2') + 
  xlab("Events") + 
  geom_hline(yintercept = 2, linetype = 'longdash') + 
  scale_y_continuous(name = "Mean Annual Manuscript Acceptance Rate", breaks = seq(0, 4, 1),
                     minor_breaks = seq(0, 4, 0.1), expand = c(0,0), limits = c(0,4)) + 
  scale_fill_manual(values = c('grey80','grey60', 'grey40', 'grey20')) + 
  labs(fill = 'Tenure Stoppage (years)') + 
  ggtitle(paste("Acceptance Rates as of", now.date)) +
  theme(legend.background = element_rect(color = "black"),
        legend.key = element_rect(fill = 'white'),
        legend.position = c(0.2, 0.85),
        panel.background = element_rect('white'),
        panel.border = element_rect(colour = 'black', fill = "transparent"),
        panel.grid.major.y = element_line('grey', size = 0.5, linetype = "solid"),
        panel.grid.minor = element_line(colour="grey", size=0.1, linetype = 'longdash'),
        axis.text = element_text(color = 'black'),)
fig_ar

## plot publication rates
fig_pr <- ggplot(ms_rate) +
  geom_col(aes(reorder(event_names, -years), p_rate,fill = as.character(yrs_tt_stop)), position = 'dodge2') + 
  xlab("Events") + 
  geom_hline(yintercept = 2, linetype = 'longdash') + 
  scale_y_continuous(name = "Mean Annual Manuscript Publication Rate", breaks = seq(0, 4, 1),
                     minor_breaks = seq(0, 4, 0.1), expand = c(0,0), limits = c(0,4)) + 
  scale_fill_manual(values = c('grey80','grey60', 'grey40', 'grey20')) + 
  labs(fill = 'Tenure Stoppage (years)') + 
  ggtitle(paste("Publication Rates as of", now.date)) +
  theme(legend.background = element_rect(color = "black"),
        legend.key = element_rect(fill = 'white'),
        legend.position = c(0.175, 0.80),
        panel.background = element_rect('white'),
        panel.border = element_rect(colour = 'black', fill = "transparent"),
        panel.grid.major.y = element_line('grey', size = 0.5, linetype = "solid"),
        panel.grid.minor = element_line(colour="grey", size=0.1, linetype = 'longdash'),
        axis.text = element_text(color = 'black'),)
fig_pr

# ## save pub rates bar graph
# tiff(file.path(datadir, "ms_rates.tiff"), width = 5, height = 5, units = "in", 
#      res = 300, compression = "lzw")
# fig_rt
# dev.off()

ms <- ms %>%
  filter(!(status %in% c("published")), type == "ms")
  
## plot ms timelines
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
             size = 2,
             shape = 8,
             show.legend = F) +
  geom_point(aes(y = start_date, x = id),
             data = pub,
             size = 2,
             shape = 7,
             show.legend = F) +
  geom_hline(yintercept = as.Date('2016-08-05'), linetype = 'dashed') +
  geom_text(aes(label = '<<< PhD conferred', y = as.Date('2016-08-05'), x = 'ms02'),
            size = 3, hjust = 0) +
  geom_hline(yintercept = as.Date('2018-12-18'), linetype = 'dashed') +
  geom_text(aes(label = 'UofSC offer accepted >>>', y = as.Date('2018-12-18'), x = 'ms03'),
            size = 3, hjust = 1) +
  geom_hline(yintercept = as.Date('2019-07-01'), linetype = 'dashed') +
  geom_text(aes(label = '<<< Became dad', y = as.Date('2019-07-01'), x = 'ms04'),
            size = 3, hjust = 0) +
  geom_hline(yintercept = as.Date('2019-08-16'), linetype = 'dashed') +
  geom_text(aes(label = '<<< Start @UofSC', y = as.Date('2019-08-16'), x = 'ms06'),
            size = 3, hjust = 0) +
  geom_hline(yintercept = as.Date('2019-09-02'), linetype = 'dashed') +
  geom_text(aes(label = '<<< EFMLA', y = as.Date('2019-09-02'), x = 'ms08'),
            size = 3, hjust = 0) +
  geom_hline(yintercept = as.Date('2020-03-11'), linetype = 'dashed') +
  geom_text(aes(label = '<<< COVID19', y = as.Date('2020-03-11'), x = 'ms10'),
            size = 3, hjust = 0) +
  geom_hline(yintercept = as.Date('2023-10-30'), linetype = 'dashed') +
  geom_text(aes(label = '<< DoubleTrouble', y = as.Date('2023-10-30'), x = 'ms12'),
            size = 3, hjust = 0) +
  # geom_text(aes(label = paste('Submission rate =', round(ms_rate$s_rate, 2), '/yr'), 
  #               y = as.Date('2013-01-01'), x = 'ms09'),
  #           size = 3, hjust = 0) +
  labs(x = "Manuscript ID") +
  scale_y_date(name = "Year", date_breaks = "1 year", date_labels = "%Y",
               limits = c(first(df$date), now.date)) +  
  scale_color_manual(name = "Status",
                     labels = c("In revision", "Under review"),
                     #values = c('dark blue', 'dark red')) + 
                     values = c('grey55', 'black')) + 
  theme(legend.background = element_rect(color = "black"),
        legend.key = element_rect(fill = 'white'),
        legend.position = c(0.2, 0.8),
        panel.background = element_rect('white'),
        panel.border = element_rect(colour = 'black', fill = "transparent"),
        panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
        axis.text = element_text(color = 'black'),
        plot.margin = margin(0.5,0.5,0.5,0.5, 'cm')) +
  annotate("rect", ymin = as.Date('2019-08-16'), ymax = as.Date('2020-08-15'), xmin = 'ms01', xmax = 'ms20',
           alpha = .2) +
  annotate("rect", ymin = as.Date('2020-03-11'), ymax = as.Date('2021-03-10'), xmin = 'ms01', xmax = 'ms20',
           alpha = .2) +
  annotate("rect", ymin = as.Date('2023-10-31'), ymax = as.Date('2024-10-30'), xmin = 'ms01', xmax = 'ms20',
           alpha = .2) +
  ggtitle("Manuscript Timelines") +
  labs(caption = "Leading number indicates author/Co-PI position.\nAsterisk indicates accepted/awarded.\nBox indicates published in an issue.\nShading indicates tenure clock stoppage.") + 
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
  labs(x = "Peer Review ID") +
  scale_y_date(name = "", date_breaks = "1 year", date_labels = "%Y",
               limits = c(first(df$date), now.date)) + 
  ggtitle(paste("Status as of", now.date)) +
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
gr <- filter(df, type == 'gr' & action != 'pre-proposal submitted')

fig_gr <- ggplot(gr) +
  geom_linerange(aes(x = id,
                     ymax = end_date,
                     ymin = start_date,
                     color = role),
                 size = 1,
                 show.legend = F) +
  # geom_text(aes(label = place, y = start_date, x = id),
  #           filter(gr, action %in% c('proposal submitted')),
  #           hjust = 1.8, vjust = 0.4,
  #           size = 4) +
  geom_point(aes(y = end_date, x = id,color = role),
             filter(gr, status == 'awarded'),
             shape = 8,
             size = 2,
             show.legend = F) +
  geom_errorbar(aes(x = id,
                    ymax = as.Date("2000-01-01"),
                    ymin = as.Date('2010-01-01'),
                    color = role),
                size = 1,
                # filter(ms, status != 'accepted'),
                show.legend = T) + ## trick for making horizontal legend bars
  # geom_hline(yintercept = as.Date('2016-08-05'), linetype = 'dashed') +
  # geom_hline(yintercept = as.Date('2018-12-18'), linetype = 'dashed') +
  labs(x = "Grant Proposal ID") +
  scale_y_date(name = "", date_breaks = "1 year", date_labels = "%Y",
               limits = c(first(df$date), now.date)) + 
  scale_color_manual(name = "Role",
                     labels = c('Co-PI', 'PI', 'Senior Personnel'),
                     #values = c('dark blue', 'dark red')) + 
                     values = c('grey55', 'black', 'grey80')
                     ) + 
  theme(legend.background = element_rect(color = "black"),
        legend.key = element_rect(fill = 'white'),
        legend.position = c(0.3, 0.8),
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

## submission and publication rates
tiff(file.path(datadir, "fig-manuscript-rates.tiff"), width = 6.5, height = 9, units = "in", 
     res = 300, compression = "lzw")
# grid.arrange(fig_rv, fig_gr, fig_ms, ncol = 1, nrow = 3)
plot_grid(fig_sr, fig_pr, align = 'v', nrow = 2, rel_heights = c(1/2, 1/2))
dev.off()

## combining all
tiff(file.path(datadir, "fig-faceted.tiff"), width = 8.5, height = 14, units = "in", 
     res = 300, compression = "lzw")
# grid.arrange(fig_rv, fig_gr, fig_ms, ncol = 1, nrow = 3)
plot_grid(fig_rv, fig_gr, fig_ms, align = 'v', nrow = 3, rel_heights = c(1/3, 1/4, 1/2))
dev.off()

## pub rates plus ms timelines
tiff(file.path(datadir, "fig-ms-pub-rates.tiff"), width = 7, height = 9, units = "in", 
     res = 300, compression = "lzw")
# grid.arrange(fig_rv, fig_gr, fig_ms, ncol = 1, nrow = 3)
plot_grid(fig_pr, fig_ms, align = 'v', nrow = 2, rel_heights = c(1/2, 1/2))
dev.off()

## perspective on initial submission rates and other assocaited rates
ggplot(filter(df, action == 'initial submission'), aes(date, id)) + geom_point() 

df.rates <- df %>%
  mutate(action = ifelse(action == 'initial submission', 'submitted', 
                               ifelse(action == 'issue assigned', 'published', action))) %>%
  filter(type == 'ms' & id != 'msXX' & action %in% c('submitted', 'accepted', 'published'))

df.rates$action <- factor(df.rates$action, levels = c('submitted', 'accepted', 'published'))
  
milestones <- ggplot(df.rates, aes(date, id, color = action)) + 
  geom_point() +
  ggtitle('Manuscript Milestones') +
  theme_bw()
milestones

tiff(file.path(datadir, "fig-ms-milestones.tiff"), width = 9, height = 7, units = "in", 
     res = 300, compression = "lzw")
milestones
dev.off()

## calculate average rate of publication from submission to publication
pubrate <- df %>%
  filter(type == 'ms' & id != 'msXX' & action %in% c('initial submission', 'accepted', 'issue assigned')) %>%
  mutate(action = ifelse(action == 'initial submission', 'submitted', 
                         ifelse(action == 'issue assigned', 'published', action))) %>%
  select(id, date, action, descriptor) %>%
  spread(action, date) %>%
  mutate(avg2pub = accepted - submitted)

mean(pubrate$avg2pub)
median(pubrate$avg2pub)

ggplot(pubrate, aes(avg2pub, descriptor)) + 
  # geom_boxplot()
  geom_point() + 
  geom_vline(xintercept = median(pubrate$avg2pub)) + 
  theme_bw()

## table summary of submission, accepted, published rates
sumy <- filter(df, type == 'ms' & id != 'msXX' & action %in% c('initial submission', 'accepted', 'issue assigned')) %>%
  mutate(yr = year(date)) %>%
  group_by(yr) %>%
  count(action) %>%
  ungroup() %>%
  mutate(action = ifelse(action == 'initial submission', 'submitted', 
                         ifelse(action == 'issue assigned', 'published', action)))

annual.rates <- spread(sumy, action, n, fill = 0) %>%
  relocate(submitted, .before = accepted) %>%
  gather('action', 'value', 2:4 )

ggplot(annual.rates, aes(yr, value, fill = action)) + 
  # geom_col(position = 'dodge', just = 0.8, width = 0.5) +
  geom_line(aes(color = action)) + 
  scale_y_continuous(name = 'Manuscripts (#)') + 
  scale_x_continuous(name = 'Year', breaks = seq(2012, 2023, 1)) + 
  theme_bw()



## want to work on developing daily time series showing rates (of submission, acceptance, publication) over time; 
## i.e., day to day calculation of rates

## grant application rates and amounts
gr <- df %>% filter(type == 'gr' & 
                      status %in% c('declined', 'awarded') &
                      str_detect(target, 'UofSC', negate = T) &
                      amount != 'n/a')

## summarise totals by awarded/declined
ext.sum <- gr %>%
  group_by(status) %>%
  mutate(amount = as.numeric(amount), inst.amount = as.numeric(institution)) %>%
  summarise(amount = sum(amount), inst = sum(inst.amount, na.rm = T)) %>%
  bind_rows(summarise(., across(where(is.numeric), sum),
                      across(where(is.character), ~'total')))
ext.sum[1,1:3] ## awarded
ext.sum$inst[1] - 25779 ## awarded to USC

## awarded to USC as PI
gr %>%
  group_by(status) %>%
  filter(role == 'pi' | subrole == 'pi') %>%
  mutate(amount = as.numeric(amount), inst.amount = as.numeric(institution)) %>%
  summarise(amount = sum(amount), inst = sum(inst.amount, na.rm = T))%>%
  bind_rows(summarise(., across(where(is.numeric), sum),
                      across(where(is.character), ~'total')))
