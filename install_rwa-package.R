
library(devtools)
install_github("henkar91/RWA/rwa")

library(rwa)
data("exdata")

# Test with split
rwa(formula = "dependant ~ q1+q2+q3+q4+q5+q6+q7+q8+q9", data = df, split_var = "split", weights = "weight", tb_limit = 4)

# Test without split
rwa(formula = "dependant ~ q1+q2+q3+q4+q5+q6+q7+q8+q9", data = df, weights = "weight", tb_limit = 4)

