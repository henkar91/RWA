library(tidyverse)

# Build dummy data --------------------------------------------------------

df <- data_frame(country = c(rep("SE", 10), rep("NO", 10)),
                 importance = runif(20, 0, 1),
                 performance = runif(20, .3, .9),
                 itemtext = rep(c('statement1','statement2','statement3','statement4',
                              'statement5','statement6','statement7','statement8',
                              'statement9','statement10'),2))


# Add colorgroup on perf/imp ----------------------------------------------

df <- df %>% 
    mutate(colorgroup = case_when(importance < 0.5 & performance < 0.5 ~ "ll",
                                  importance < 0.5 & performance > 0.5 ~ "ul",
                                  importance > 0.5 & performance < 0.5 ~ "lr",
                                  importance > 0.5 & performance > 0.5 ~ "ur"))


# Try to mimic Timmys plot ------------------------------------------------

ggplot(df, aes(x = importance, y = performance * 100, color = colorgroup)) + geom_point() +
    geom_text(aes(x = importance, y = performance * 100, label = itemtext, color = colorgroup), 
                  vjust = -.8) +
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
    xlab(label = "IMPORTANCE (IMPACT ON OVERALL STORE SATISFACTION)") +
    ylab(label = "PERFORMANCE (% SCORE 4-5)") +
    annotate(geom = "text", x = 0.02, y = 2, label = "Low importance", 
             color = "#4c7a9f", size = 3, fontface = "italic", hjust = 0) +
    annotate(geom = "text", x = 0.98, y = 2, label = "High importance", 
             color = "#4c7a9f", size = 3, fontface = "italic", hjust = 1) +
    annotate("segment", x=-Inf, xend = Inf, y = 0, yend = 0, arrow = arrow(), color = "grey")+
    # scale_y_continuous(limits=c(-0.5, 1)) +
    geom_rect(mapping=aes(xmin=0.49, xmax=0.51, ymin=0, ymax=100), color = NA, fill = "grey98", alpha=0.01) +
    geom_rect(mapping=aes(xmin=0, xmax=1, ymin=48, ymax=52), color = NA, fill = "grey90", alpha=0.01)

