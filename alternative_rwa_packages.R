install.packages("iopsych")

library(iopsych)

X <- rwadata$dependant

r <- cor(rwadata[2:17])

relWt(r_mat = r, y_col = 1, 2:16)

library(devtools)
install_github("Displayr/flipRegression")
library(flipRegression)

