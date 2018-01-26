library(tidyverse)
library(ggthemes)
library(ggrepel)


# Build dummy data --------------------------------------------------------

# this is the result data from RWA analysis.. + topbox values + attribute items
df <- data_frame(country = c(rep("SE", 10), rep("NO", 10)),
                 importance = runif(20, 0, 1),
                 performance = runif(20, .3, .9),
                 itemtext = rep(c('statement1','statement2','statement3','statement4',
                              'statement5','statement6','statement7','statement8',
                              'statement9','statement10'),2))


# comment.. all below down to plot should be possible to split on country etc. 
# So for each run we get the cross df (to place the cross on plot)


# A leaner plot ------------------------------------------------
ggplot(data = df) +
    theme_tufte() +
    geom_point(aes(x = importance, y = performance * 100)) +
    geom_text_repel(aes(x = importance, y = performance * 100, 
                             label = itemtext, family = "serif")) +
    guides(color = FALSE) +
    scale_color_gradient() +
    theme(axis.line.x = element_line(color = "grey40"),
          axis.line.y = element_line(color = "grey40"),
          axis.text.x=element_blank(),
          axis.ticks.x = element_blank()) +
    scale_x_continuous(limits = c(0,1)) +
    scale_y_continuous(limits = c(0,100)) +
    xlab(label = "") + ylab(label = "") + # Timmy don't want labels....
    geom_vline(aes(xintercept = mean(importance, na.rm = TRUE)), 
               color = "grey", linetype = 2, size = 0.3) +
    geom_hline(aes(yintercept = mean(performance, na.rm = TRUE) * 100), 
               color = "grey", linetype = 2, size = 0.3)

