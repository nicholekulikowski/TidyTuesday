#####################################################################
# code to load, clean, and vizualize Tidy Tuesday Week 31, 8/9/2022 #
#####################################################################
# Nichole Kulikowski, 8/9/2022 ######################################
#####################################################################

##################
# load libraries #
##################
library(tidyverse)
library(emoGG)
library(LaCroixColoR)
library(extrafont)

#############
# load data #
#############
wheels <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv'
  ) # Data this week is taken from ferriswheels package by Emil Hvitfeldt.

######################
# clean/wrangle data #
######################
wheels_plot <- wheels %>%
  mutate(country = str_replace_all(string = country, pattern = "Tailand", replacement = "Thailand"),
         country = str_replace_all(country, "Phillippines", "Philippines")) %>%
  filter(status == "Operating") %>%
  group_by(country) %>%
  mutate(country_total = n())

#########
# plot! #
#########
theme_set(theme_minimal(base_size = 14, base_family = "Segoe Print"))

wheels_plot %>%
  group_by(country) %>%
  ggplot() +
  geom_bar(aes(x = reorder(country, country_total), alpha = 0.01, fill = country), show.legend = FALSE,
           width = .3) +
  geom_emoji(aes(x = country, y = country_total), emoji = "1f3a1", ) +
  labs(title = "__FERRIS WHEELS AROUND THE WORLD__",
       subtitle = "The largest 46 ferris wheels in the world, plotted by country totals. Note only ferris wheels<br>that are currently operating were included.",
       x = "Country",
       y = "Total",
       caption = "Data | @Emil_Hvitfeldt via TidyTuesday\nVisualization | Nichole Kulikowski @nkulikow") +
  scale_color_manual(values = lacroix_palette("PassionFruit", n = 17, type = "continuous")) +
  coord_flip() +
  theme(
    axis.text.y = element_text(color = "grey60", face = "bold",
                               margin = margin(r = 15), lineheight = .9),
    axis.text.x = element_text(color = "grey60", face = "bold"),
    plot.title = ggtext::element_markdown(colour = "White"),
    plot.subtitle = ggtext::element_markdown(colour = "White", size = 12),
    plot.title.position = "plot",
    plot.caption = element_text(color = "gray60",
                                face = "italic"),
    axis.title = element_text(color = "White"),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.line = element_line(colour = "gray90")) +
  scale_y_continuous(breaks = 1:11)

###############
# save output #
###############
ggsave("../TidyTuesday/images/ferris_wheel.png")