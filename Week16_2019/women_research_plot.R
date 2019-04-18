# Loading packages:
library(tidyverse)
library(ggimage)

# getting data
women_research <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")

women_research <- mutate_if(women_research, is.character, factor) # converting characters to factors

#women_research <- arrange(women_research, desc(x))

women_research <- mutate(women_research, image = case_when(field == "Health sciences" ~ sample("~/Data_Science_Resources/tidy_tuesday/Week16_2019/health_colour.png"),
                                                           field == "Computer science, maths" ~ sample("~/Data_Science_Resources/tidy_tuesday/Week16_2019/computer_colour.png"),
                                                           field == "Physical sciences" ~ sample("~/Data_Science_Resources/tidy_tuesday/Week16_2019/physical_colour.png"),
                                                           field == "Engineering" ~ sample("~/Data_Science_Resources/tidy_tuesday/Week16_2019/engineer_colour.png"),
                                                           field == "Women inventores" ~ sample("~/Data_Science_Resources/tidy_tuesday/Week16_2019/invent_colour.png")))


# Creating plot with symbols representing each field of study:
ggplot(data = women_research, aes(y = country, x = 100*percent_women)) + 
  geom_image(aes(image=image), size=.05, alpha = 0.5) +
  labs(x = "Women among researchers with papers published 2011-15 (as % of total by field of study)",
       y = "Country",
       caption = "Source of data:") +
  geom_vline(xintercept = 50, linetype="dashed", color = "red", size = 2)

# Need to find a nice theme, work out how to add legend, could sort by Health Sciences, need to add 
# source and link to https://fontawesome.com/icons/rocket?style=solid
