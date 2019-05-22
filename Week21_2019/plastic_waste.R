library(tidyverse)
library(countrycode)
library(cowplot)
#### Importing and transforming data ####

# reading in file containing data for mismanaged plastic waste:
mismanaged_vs_gdp <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv") %>%
  # there are a lot of missing values and the variables need renaming
  set_names(c("country", "code", "year", "mismanaged_waste", "GDP", "population")) %>% # renaming variables
  mutate_if(is.character, as.factor) # converting characters to factors
mismanaged_vs_gdp <- mismanaged_vs_gdp[complete.cases(mismanaged_vs_gdp[ , 4]),] # removing any observations that contain N/A for mismanaged_waste


# reading in file containing data for all plastic waste:
waste_vs_gdp <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv") %>%
  set_names(c("country", "code", "year", "waste", "GDP", "population")) %>% # renaming variables
  mutate_if(is.character, as.factor) # converting characters to factors
waste_vs_gdp <- waste_vs_gdp[complete.cases(waste_vs_gdp[ , 4]),] # removing any observations that contain N/A for waste
# There is really only waste data available for 2010.


#### Transforming plastic waste data for lollipop chart ####
waste_vs_gdp <- filter(waste_vs_gdp, country != "Trinidad and Tobago") # removing Trinidad and Tobago because this outlier seems too large to be correct.

mean_country <- mean(waste_vs_gdp$waste) # mean value of plastic waste for all countries

waste_vs_gdp <- mutate(waste_vs_gdp, region = countrycode(country, origin = "country.name", 
                                                          destination = "continent")) # adding geographical regions
# Grouping and calculating average waste per region:
group_waste_vs_gdp <- group_by(waste_vs_gdp, region) %>%
  summarise(mean_region = mean(waste)) %>% # calculating mean waste per region 
  mutate(region = fct_reorder(region, mean_region)) %>% # sorting regions by mean waste
  mutate(region_cat = as.factor(case_when(mean_region < mean_country ~ "below",
                                          TRUE ~ "above"))) # add category specifying if waste level is above or below average

# adding a category specifying if waste level is above or below average to the country level data
waste_vs_gdp <- mutate(waste_vs_gdp, country_cat = as.factor(case_when(waste < mean_country ~ "below", 
                                                                       TRUE ~ "above")))

#### Transforming mismanaged plastic waste data for lollipop chart ####
mismanaged_vs_gdp <- filter(mismanaged_vs_gdp, country != "Trinidad and Tobago") # removing Trinidad and Tobago

mean_mismanaged_country <- mean(mismanaged_vs_gdp$mismanaged_waste)# mean value of mismanaged plastic waste for all countries

mismanaged_vs_gdp <- mutate(mismanaged_vs_gdp, region = countrycode(country, origin = "country.name", 
                                                                    destination = "continent")) # adding geographical regions

# Grouping and calculating average mismanaged waste per region:
group_mismanaged_vs_gdp <- group_by(mismanaged_vs_gdp, region) %>%
  summarise(mean_mismanaged_region = mean(mismanaged_waste)) %>% # calculating mean mismanaged waste per region
  mutate(region = fct_reorder(region, mean_mismanaged_region)) %>% # sorting regions by mean mismanaged waste
  mutate(mis_region_cat = as.factor(case_when(mean_mismanaged_region < mean_mismanaged_country ~ "below",
                                              TRUE ~ "above"))) # add category specifying if waste level is above or below average

# adding a category specifying if waste level is above or below average to the country level data
mismanaged_vs_gdp <- mutate(mismanaged_vs_gdp, mis_country_cat = as.factor(case_when(mismanaged_waste < mean_mismanaged_country ~ "below", 
                                                                                     TRUE ~ "above")))

#### Coordinates for arrow labels ####
arrows <- tibble(
  y1 = c(0.4, 0.35, 0.5),
  y2 = c(mean_country, 0.2, 0.38),
  x1 = c(0.8, 4.3, 2.7),
  x2 = c(0.6, 4, 2)
)

#### Lollipop chart of plastic waste ####
set.seed(1) # for jitter
plot_waste <- ggplot()+
  geom_point(data = filter(group_waste_vs_gdp, region != "NA"),
             aes(x = region, y = mean_region, colour = region_cat), stat='identity', size=5)  +
  geom_segment(data = filter(group_waste_vs_gdp, region != "NA"),
               aes(y = mean_country, 
                   x = region, 
                   yend = mean_region, 
                   xend = region,
                   color = region_cat),
               size = 1) +
  geom_jitter(data = filter(waste_vs_gdp, region != "NA"),
              mapping = aes(x = region, y = waste, color = country_cat),
              stat = "identity",
              width = 0.2, size = 2, alpha = 0.25) + 
  scale_color_manual(name="Plastic Waste", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("above"="tan4", "below"="green4")) + 
  labs(title="Per capita plastic waste", 
       x = "",
       y = "Mean regional per capita plastic waste \nminus mean for all countries \n(kg per person per day)",
       caption = "\n") + 
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        panel.grid = element_blank()) +
  geom_hline(yintercept = mean_country, linetype = "dashed", colour = "black") +
  annotate("text", y = 0.4, x = 0.9, size = 2.7, color = "grey20",
           label = "Global average") +
  annotate("text", y = 0.35, x = 4.5, size = 2.7, color = "grey20",
           label = "Regional average") +
  annotate("text", y = 0.5, x = 2.8, size = 2.7, color = "grey20",
           label = "Individual countries")+ 
  geom_curve(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
             color = "grey20", curvature = -0.3)

#### Lollipop chart of mismanaged plastic waste ####
set.seed(1) # for jitter
plot_mis_waste <- ggplot() + 
  geom_point(data = filter(group_mismanaged_vs_gdp, region != "NA"), 
             aes(x = region, y = mean_mismanaged_region, colour = mis_region_cat), 
             stat='identity', size=5)  +
  geom_segment(data = filter(group_mismanaged_vs_gdp, region != "NA"),
               aes(y = mean_mismanaged_country, 
                   x = region, 
                   yend = mean_mismanaged_region, 
                   xend = region,
                   color = mis_region_cat),
               size = 1) +
  geom_jitter(data = filter(mismanaged_vs_gdp, region != "NA"),
              mapping = aes(x = region, y = mismanaged_waste, color = mis_country_cat),
              stat = "identity",
              width = 0.2, size = 2, alpha = 0.25) +
  scale_color_manual(name="Plastic Waste", 
                     labels = c("Above \nAverage", "Below \nAverage"), 
                     values = c("above"="tan4", "below"="green4")) + 
  labs(title="Per capita mismanaged plastic waste", 
       x = "",
       y = "Mean regional per capita mismanaged plastic \nwaste minus mean for all countries \n(kg per person per day)",
       caption = "Mismanaged waste is material that is littered or inadequately disposed of. \nSource of data: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-05-21") + 
  coord_flip() +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        panel.grid = element_blank()) +
  geom_hline(yintercept = mean_mismanaged_country, linetype = "dashed", colour = "black") +
  annotate("text", y = 0.23, x = 4.35, size = 2.7, color = "grey20",
           label = "Regions with low overall \nplastic waste have high \nlevels of mismanaged \nplastic waste") 

comb <- plot_grid(plot_waste, plot_mis_waste, rel_widths = c(1, 1.35))
ggsave("Week21_2019/plastic_waste.png", comb, width = 20, height = 12, units = "cm")

