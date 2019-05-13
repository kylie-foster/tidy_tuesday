# Loading packages:
library(tidyverse)
library(ggimage)
library(ggthemes)
# getting data
women_research <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")

women_research <- mutate_if(women_research, is.character, factor) # converting characters to factors

#women_research <- arrange(women_research, desc(x)) group_by(Country?)
women_research <- mutate(women_research, country = fct_reorder(country, percent_women, .fun=first))

women_research <- mutate(women_research, image = case_when(field == "Health sciences" ~ sample("~/Data_Science_Resources/tidy_tuesday/Week16_2019/images/health_colour.png"),
                                                           field == "Computer science, maths" ~ sample("~/Data_Science_Resources/tidy_tuesday/Week16_2019/images/computer_colour.png"),
                                                           field == "Physical sciences" ~ sample("~/Data_Science_Resources/tidy_tuesday/Week16_2019/images/physical_colour.png"),
                                                           field == "Engineering" ~ sample("~/Data_Science_Resources/tidy_tuesday/Week16_2019/images/engineer_colour.png"),
                                                           field == "Women inventores" ~ sample("~/Data_Science_Resources/tidy_tuesday/Week16_2019/images/invent_colour.png")))

scale_country <- function(..., guide = "legend") {
  sc <- discrete_scale("country", "identity", scales::identity_pal(), ..., guide = guide)
  
  sc$super <- ScaleDiscreteIdentity
  class(sc) <- class(ScaleDiscreteIdentity)
  sc
}

# Creating plot with symbols representing each field of study:
ggplot(data = women_research, aes(y = country, x = 100*percent_women)) + 
  geom_image(aes(image=image), size=.05, alpha = 0.5) +
  labs(x = "% of women among researchers with papers published 2011-15 \n(as % of total by field of study)",
       y = "",
       caption = "Source of data: TidyTuesday Wk 16 https://github.com/rfordatascience/tidytuesday \n
       Source of symbols: Font Awesome https://fontawesome.com/license",
       title = "Still a man's world") +
  geom_vline(xintercept = 50, linetype="solid", color = "red", size = 1) +
  theme_bw()

ggsave("Week16_2019/women_research.png")

# Need to find work out how to add legend
