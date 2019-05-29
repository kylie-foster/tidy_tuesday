library(tidyverse)
library(ggridges)

wine_ratings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv") %>%
  mutate_if(is.character, as.factor) %>%
  mutate(value = points/price) 

wine_ratings_group <- group_by(wine_ratings, taster_name)
delay <- summarise(wine_ratings_group,
                   count = n(),
                   med_value = median(value, na.rm = TRUE)
                   )
delay <- filter(delay, count > 100) %>%
  na.omit() 

name_order <- levels(fct_reorder(factor(delay$taster_name), delay$med_value)) # extra factor() is required to remove 
# extra levels that are no longer present

# remove data points for reviewers with a small number of ratings
ggplot(data = filter(wine_ratings, taster_name %in% name_order), aes(x = value, y = fct_relevel(taster_name, name_order))) + 
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, scale = 2, rel_min_height = 0.01)+ 
  theme_ridges() +
  xlim(0, 15)

