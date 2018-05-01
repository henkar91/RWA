
library(devtools)
install_github("henkar91/RWA/rwa", auth_token = "a67c7fa421f5b8783b136139661b7750978749bd")

library(rwa)
data("exdata")

# Test with split
rwa(formula = "dependant ~ q1+q2+q3+q4+q5+q6+q7+q8+q9", data = df, split_var = "split", weights = "weight", tb_limit = 4)

# Test without split
rwa(formula = "dependant ~ q1+q2+q3+q4+q5+q6+q7+q8+q9", data = df, weights = "weight", tb_limit = 4)

