
library(devtools)
install_github("henkar91/RWA/rwa")

library(rwa)
data("exdata")

# Test with split
res <- rwa(formula = "dependant ~ q1+q2+q3+q4+q5+q6+q7+q8+q9", data = df, split_var = "split", weights = "weight", tb_limit = 4)
res
rwa_plot(res)

# Test without split
rwa(formula = "dependant ~ q1+q2+q3+q4+q5+q6+q7+q8+q9", data = df, weights = "weight", tb_limit = 4)

# Test without split without weight
# rwa(formula = "dependant ~ q1+q2+q3+q4+q5+q6+q7+q8+q9", data = df, tb_limit = 4)

data2 <- data
data2[c(1,2,10), 4] <- NA

rwa(formula = "dependant ~ q1+q2+q3+q4+q5+q6+q7+q8+q9", data = data2, split_var = "split", weights = "weight", tb_limit = 4)
