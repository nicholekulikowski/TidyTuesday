############################################################
# code to load, clean, and vizualize Tidy Tuesday 8/2/2022 #
############################################################
# Nichole Kulikowski, 8/2/2022 #############################
############################################################

##################
# load libraries #
##################
library(tidyverse)
library(janitor)
library(scales)
library(ggtext)
library(viridis)

#############
# load data #
#############
usgs_frogs <- read_csv("../data/tidy_tuesday_8_2_2022.csv")

##############
# clean data #
##############
usgs_frogs_clean <- usgs_frogs %>%
  row_to_names(row_number = 2,
               remove_row = TRUE,
               remove_rows_above = TRUE)


##################
# plot the data! #
##################
ggfrog <- usgs_frogs_clean %>%
  group_by(HabType) %>%
  summarise(total = n()) %>%
  ggplot(aes(x = HabType,
             y = total,
             color = HabType)) +
  geom_hline(yintercept = 0, color = "grey80", size = .4) +
  stat_summary(
    geom = "point", fun = "sum", size = 12
  ) +
  stat_summary(
    geom = "linerange", ymin = 0, fun.max = function(y) sum(y),
    size = 2, show.legend = FALSE
  ) +
  coord_flip(ylim = c(0, NA), clip = "off")  +
  scale_colour_manual(values = c("chocolate4", "firebrick3", "olivedrab")) +
  labs(y = "Number of Frogs",
       x = "Habitat Type",
       title = "**Where Are These Frogs Spotted?**",
       subtitle = "_Number of Oregon Spotted Frogs Tracked by Habitat Type between September-November 2018_",
       caption = "Visualization by Nichole Kulikowski. Data is publicly available from United States Geological Survey (USGS).") +
  theme(
    axis.line.y = element_blank(),
    axis.text.y = element_text(family = "Pally", color = "grey50", face = "bold",
                               margin = margin(r = 15), lineheight = .9),
    plot.title = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown(),
    plot.title.position = "plot",
    plot.caption = element_text(color = "gray60",
                                face = "italic",
                                hjust = 0),
    legend.position = "none")
  

ggfrog  

###############
# export viz! #
###############
ggsave("usgs_frogs.png", path = "../images/",
       height = 3.29*2, width = 6.67*2, units = "in", dpi = 96)
