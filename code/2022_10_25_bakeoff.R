#######################################################################
# code to load, clean, and vizualize Tidy Tuesday Week 43, 10/25/2022 #
#######################################################################
# Nichole Kulikowski, 8/9/2022 ########################################
#######################################################################

##################
# load libraries #
##################
library(tidyverse)
library(bakeoff)

############
# get data #
############
bakers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/bakers.csv')
challenges <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/challenges.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/episodes.csv')

##################
# summarize data #
##################
bakers_new <- bakers %>%
  mutate(age_group = case_when(age <= 18 ~ "18 and Under",
                               age > 18 & age <= 25 ~ "19-25",
                               age > 25 & age <= 29 ~ "26-29",
                               age >= 30 & age < 40 ~ "30's",
                               age >= 40 & age < 50 ~ "40's",
                               age >= 50 & age < 60 ~ "50's",
                               age >= 60 ~ "60 and Over",
                               TRUE ~ "na"))

#########
# plot! #
#########
bakers_new %>%
  ggplot(aes(x = age_group, fill = age_group)) +
  geom_bar() +
  theme_minimal() +
  scale_color_bakeoff() +
  theme(plot.background = element_rect(fill = "Black"),
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
  labs(title = "__Are All Ages Represented on the Great British Bakeoff?__",
       subtitle = "_Number of contestants by age group for Seasons 1-10_",
       x = "Age Group",
       y = "Number of Contestants",
       caption = "Data from bakeoff package by Alison Hill, Chester Ismay, and Richard Iannone | Visualization by Nichole Kulikowski @nkulikow")

##########
# export #
##########
ggsave("../TidyTuesday/images/bakeoff.png")
