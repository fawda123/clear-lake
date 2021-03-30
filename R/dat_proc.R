library(tidyverse)
library(lubridate)
library(patchwork)

# format data
dat <- read.csv('data/ClearLake WT.csv', stringsAsFactors = F) %>% 
  separate(date, c('mo', 'dy', 'yr'),  sep = '/') %>% 
  mutate(
    yr = case_when(
      yr >= 0 & yr <= 21 ~ paste0('20', yr),
      T ~ paste0('19', yr)
    )
  ) %>% 
  unite('date', yr, mo, dy, sep = '/') %>% 
  mutate(
    date = ymd(date), 
    yr = year(date), 
    date = floor_date(date, 'month')
  ) %>% 
  filter(yr < 2020) %>% 
  group_by(date) %>% 
  summarise(
    val = mean(val, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  mutate(
    avg = stats::filter(val, rep(1, 12)/12, sides = 1, method = 'convolution'),
    avg = as.numeric(avg),
    dev = val - avg, 
    mo = month(date, abbr = T)
  )

# long-term trend plots
thm <- theme(
  axis.title.x = element_blank(), 
  legend.position = 'top', 
  panel.grid.minor = element_blank()
)

p1 <- ggplot(dat, aes(x = date, y = val)) + 
  # geom_point() + 
  geom_line() + 
  labs(y = 'Temp. (deg C)', subtitle = 'Monthly averages across all stations') + 
  theme_minimal() + 
  thm

p2 <- ggplot(dat, aes(x = date))  +
  geom_segment(aes(xend = date, yend = avg, y = avg + dev, colour = dev), size = 0.8) +
  geom_line(aes(y = avg), colour = 'black') +
  labs(y = 'Temp. (deg C)', subtitle = '12 month running averages across all stations') + 
  theme_minimal() + 
  thm +
  scale_colour_gradient2('Devation from avg. (+/-)', low = 'tomato1', mid = 'grey90', high = 'lightgreen', midpoint = 0)

p <- p1 + p2 + plot_layout(ncol = 1, heights = c(0.8, 1))

png('figure/tempsmooth.png', height = 4, width = 10, units = 'in', res = 300)
print(p)
dev.off()

# monthly changes
p3 <- ggplot(dat, aes(x = date, y = val)) + 
  geom_point() +
  geom_smooth(method = 'lm') + 
  facet_grid( ~ mo) + 
  labs(y = 'Temp. (deg C)') + 
  theme_minimal() + 
  thm + 
  theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1))

png('figure/monthlychg.png', height = 6, width = 8, units = 'in', res = 300)
print(p3)
dev.off()
