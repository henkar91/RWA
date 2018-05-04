
library(rwa)

data(exdata)

formula <- "dependant ~ q1+q2+q3+q4+q5+q6+q7+q8+q9"

q <- rwa(formula = formula, 
         data = df, 
         split_var = "split", 
         weights = "weight", 
         tb_limit = 4)


plotrwa <- function(rwa) {
  require(tidyverse)
  
    rwa$rwa %>% 
    rownames_to_column %>% 
    gather(split, imp, -rowname) %>% 
    left_join(., rwa$topbox %>% 
                rownames_to_column %>% 
                gather(split, perf, -rowname), 
              by = c("rowname", "split")) %>% 
    ggplot() +
    geom_text(aes(x = imp, y = perf, label = rowname), size = 3) +
    facet_wrap(~split)

}

rwa_to_plot(q)
