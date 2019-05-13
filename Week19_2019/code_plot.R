library(tidyverse)
library(ggalt) # for ggalt
library(ggthemes)

student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv") %>%
  mutate_if(is.character, as.factor) %>%# converting characters to factors
  filter(grepl('income', country)) %>% # selecting only those observations that include "income"
  filter(year %in% c(2017, 2012)) %>% # selecting the highest and lowest years to use as endpoints of dumbbell plot
  rename(value = student_ratio) %>% # renaming student_ratio variable to prevent confusion with dataframe name
  select(indicator, country, year, value) %>%
  spread(key = year, value = value) %>%
  rename(year_2012 = "2012", year_2017 = "2017") %>%
  mutate(country = str_remove(country, "income countries"))

student_ratio$indicator <- factor(student_ratio$indicator, 
                                  levels = c("Pre-Primary Education", "Primary Education",
                                             "Lower Secondary Education", "Secondary Education",
                                             "Upper Secondary Education")) # reordering 

student_ratio_long <- gather(student_ratio, year_2012, year_2017, key = year, value = value) %>%
  mutate_if(is.character, as.factor) # converting dataframe back to long form (required for legend)


# choosing colours:
col_line <- "gray" #line colour
col_2012 <- "#a3c4dc" #light blue, 2012 
col_2017 <- "#0e668b" # dark blue, 2017

# Plotting:
ggplot() +
  geom_dumbbell(data = student_ratio, 
                aes(y = fct_rev(fct_reorder(country, year_2012)), x = year_2012, xend = year_2017),
                colour = col_line, colour_x = col_2012, colour_xend = col_2017, size = 1.5) +
  geom_point(data = student_ratio_long, aes(x = value, y = country, colour = year), size = 3.5) + # required because 
  # geom_dumbell does not provide a legend option
  facet_wrap( ~ indicator, ncol = 2) +
  labs(title = "Change in student to teacher ratios from 2012 to 2017",
       subtitle = "Some ratios increased between 2012 and 2017",
       x = "Student to Teacher Ratio (lower = fewer students per teacher)",
       y = "Country Income Level",
       caption = "Source of data: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-05-07") +
  theme_bw(base_size = 15) +
  theme(legend.position = c(0.75, 0.15)) +
  scale_color_manual(values = c("#a3c4dc", "#0e668b"), label = c("2012", "2017"))

ggsave("Week19_2019/student_teacher_ratios.png", height = 10, width = 8)



