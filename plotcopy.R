library(tidyverse)

# Build dummy data --------------------------------------------------------

# comment.. all this down to plot, should be possible to split on country etc. 
# So for each run we get the cross df (to place the cross on plot)

df <- data_frame(country = c(rep("SE", 10), rep("NO", 10)),
                 importance = runif(20, 0, 1),
                 performance = runif(20, .3, .9),
                 itemtext = rep(c('statement1','statement2','statement3','statement4',
                              'statement5','statement6','statement7','statement8',
                              'statement9','statement10'),2))



# Make df for cross placement on plot -------------------------------------

cross <- df %>% 
    summarise(xmean = mean(importance, na.rm = TRUE),
              ymean = mean(performance, na.rm = TRUE),
              xmin = mean(importance, na.rm = TRUE) - 0.015,
              xmax = xmin + 0.023, # yikes... dunno if this will make cross x as thick as y?
              ymin = mean(performance, na.rm = TRUE) * 100 - 2,
              ymax = ymin +4)



# Add colorgroup on perf/imp ----------------------------------------------

pdata <- df %>% 
    cbind(., cross) %>% # get values from cross duplicated to make colorgroup
    mutate(colorgroup = case_when(importance < xmean & performance < ymean ~ "ll",
                                  importance < xmean & performance > ymean ~ "ul",
                                  importance > xmean & performance < ymean ~ "lr",
                                  importance > xmean & performance > ymean ~ "ur")) %>% 
    filter(country == "SE")

# Try to mimic Timmys plot ------------------------------------------------

ggplot() +
    geom_point(data = pdata, aes(x = importance, y = performance * 100, color = colorgroup)) +
    geom_text(data = pdata, aes(x = importance, y = performance * 100, 
                             label = itemtext, color = colorgroup), vjust = -.8) +
    guides(color = FALSE) +
    theme_minimal() +
    theme(plot.margin = unit(c(1,1,1,1), "cm"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y = element_text(color="grey", size="10"),
          axis.title.x = element_text(color="#4c7a9f", size="10", face="bold"),
          axis.title.y = element_text(color="#4c7a9f", size="10", face="bold"),
          axis.ticks = element_blank(),
          plot.title = element_text(color="#2e6e9c", face="bold", size="14"),
          legend.position="bottom", legend.title = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(color="#2e6e9c", size="10")) +
    scale_x_continuous(limits = c(0,1)) +
    scale_y_continuous(limits = c(0,100)) +
    xlab(label = "") + # Timmy don't want labels....
    ylab(label = "") +
    annotate(geom = "text", x = 0.02, y = 2, label = "Low importance", 
             color = "#4c7a9f", size = 3, fontface = "italic", hjust = 0) +
    annotate(geom = "text", x = 0.98, y = 2, label = "High importance", 
             color = "#4c7a9f", size = 3, fontface = "italic", hjust = 1) +
    annotate("segment", x=-Inf, xend = Inf, y = 0, yend = 0, arrow = arrow(), color = "grey") +
    # geom_rect(data = cross, mapping=aes(xmin=xmin, xmax=xmax, ymin=0, ymax=100), color = NA, fill = "grey70", alpha=0.3) +
    # geom_rect(data = cross, mapping=aes(xmin=0, xmax=1, ymin=ymin, ymax=ymax), color = NA, fill = "grey70", alpha=0.3) +
    geom_vline(data = cross, aes(xintercept = xmean)) +
    geom_hline(data = cross, aes(yintercept = ymean * 100))
