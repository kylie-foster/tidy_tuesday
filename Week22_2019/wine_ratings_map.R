# TidyTuesday 2019 - Week 22
# Kylie Foster

# Loading libraries
library(tidyverse)
library(magrittr)
library(ggmap)
library(maps)
library(RColorBrewer)
library(viridis)
library(ggrepel)

# I found last weeks TidyTuesday code by Dewi Koning very useful: 
# https://github.com/KoningD/TidyTuesday/blob/master/2019_Week20/TidyTuesday_2019_Week20.R

# Loading data
wine_ratings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv") %>%
  mutate_if(is.character, as.factor) %>%
  mutate(value = points/price) %>% #  creating new variable that represents value for money
  drop_na(country) #  removing any rows containing missing countries

# Calculating median value for each country
wine_ratings_country <- group_by(wine_ratings, country) %>%
  summarise(med_value = median(value, na.rm = TRUE)) %>%
  drop_na(med_value) %>% #  removing any rows containing missing values
  mutate(country = case_when(country == "US" ~ "USA", #  changing US to USA to match map_data
                             country == "England" ~ "UK", #  changing to match map_data (not quite equivalent)
                             TRUE ~ as.character(country))) 
  
# Loading world map and removing Antarctica
world_map <- map_data("world") %>% 
  filter(region != "Antarctica")

# Checking that all country names in wine_ratings_country and world_map match
test <- inner_join(wine_ratings_country, world_map, by = c("country" = "region"))
length(unique(test$country))
length(unique(wine_ratings_country$country))

# Joining wine and world map data
wine_map <- right_join(wine_ratings_country, world_map, by = c("country" = "region"))

# Make a dataframe with the country with the highest value for money in order to plot these as geom_label
wine_ratings_country_top <- wine_ratings_country %>% 
  top_n(1, med_value) %>%
  left_join(world_map, by = c("country" = "region")) %>% 
  group_by(country) %>% 
  top_n(1, lat) %>% 
  mutate(label_text = paste0(country, ": ", round(med_value, 1)))


# Make a dataframe with the country with the lowest value for money in order to plot these as geom_label
wine_ratings_country_bottom <- wine_ratings_country %>% 
  top_n(-1, med_value) %>%
  left_join(world_map, by = c("country" = "region")) %>% 
  group_by(country) %>% 
  top_n(1, lat) %>% 
  mutate(label_text = paste0("England: ", round(med_value, 1)))

# Combining above two dataframes to that we can use ggrepel later
wine_ratings_country_both <- rbind(wine_ratings_country_top, wine_ratings_country_bottom)

# Manually binning and labelling data into discrete bins for different value ranges
wine_map$med_value_split <- factor(
  cut(wine_map$med_value, c(0, 3, 4, 5, 10)),
  labels = c("Under 3", "3 to 4", "4 to 5", "5 to 10")
)

# Choosing nice colors
col_purple <- c("#b3cde3", "#8c96c6", "#8856a7", "#810f7c", "white")

set.seed(1)
# Generate world map plot 
wine_map %>%
  ggplot(aes(x=long, y = lat, group = group, fill = med_value_split)) + 
  geom_polygon(color = "grey10", size = 0.01) +
  coord_fixed() +
  scale_fill_manual(values = col_purple) +
  theme_void() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.margin = unit(c(0, 0.25, 0.0, 0.25), "in"),
        panel.border = element_rect(fill = NA, colour = "#cccccc"),
        legend.text = element_text(size = 8),
        legend.position = "bottom",
        plot.subtitle = element_text(lineheight = 0.5)) +
  labs(title = "Which country of origin makes wine with the highest value for money?",
     subtitle = "This map shows the median value for money for wine from each country of origin calculated as the 
     \nnumber of points WineEnthusiast rated the wine divided by the price of the wine. 
     \nRomania makes wine with the highest value for money, while England makes wine with the lowest.",
     caption = "By Kylie Foster. Source of data: Kaggle", 
     fill = "Median Value for Money \n(Wine Rating per $ Price)") +
  geom_label_repel(data = wine_ratings_country_both, 
                   aes(x=long, y = lat, label = label_text, group = group), 
                   fill = c(col_purple[4], col_purple[1]), 
                   fontface = "bold", force = 10, size = 3,
                   nudge_y = 15)

ggsave("./Week22_2019/wine_ratings_map.png", dpi = 300, width = 20, height = 12, units = "cm")
 

