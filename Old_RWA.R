rm(list=ls())

# ==================================================================================
# Data preparation and cleaning
# ==================================================================================

# read raw data
library(haven)
r <- read_spss("")
names(r) <- tolower(names(r))
colnames <- data.frame(names(r)) # get a data.frame with column names for overview

# ----------------------------------------------------------------------------------
# structure data so that weight is last column, and dependant lies just before 
# explanatory variables in data.frame
# ----------------------------------------------------------------------------------
library(tidyverse)

d <- r %>% 
    dplyr::select(index1:Cons, ID1, Telia_Loyal, weight, Segment, Est_Seg) %>% 
    filter(ae != 0 & index1 == 1) %>% 
    na.omit

split_var <- d$d0 # variable to use as split

# get split labels in order, take from raw data
library(sjmisc)
splits <- as.character(sort(unique(to_label(r$d0))))


# ==================================================================================
# Drivers and Corr
# ==================================================================================
library(relaimpo)
library(weights)

x <- split(d, d$d0)

y <- lapply(seq_along(x), function(f) as.data.frame(x[[f]])[, 6:(length(x[[1]]) )]) #removes col that isn't dependent or explanatory

l = lapply(seq_along(y), function(f) {
    assign(splits[f], y[[f]])
}
)

corr = matrix(NA, ncol=length(1:ncol(l[[1]]))-1, nrow=length(l)) #creates an empty matrix for correlation
driver = matrix(NA, ncol=length(2:ncol(l[[1]]))-1, nrow=length(l)) #creates an empty matrix for drivers
r2 = matrix(NA, ncol=length(2:ncol(l[[1]]))-1, nrow=length(l)) #creates an empty matrix for drivers

for(i in 1:length(l)){
    corr[i,] = wtd.cors(l[[i]][,'ae'], l[[i]][,2:ncol(l[[i]])-1], weight=l[[i]][,"weight"])
    driver[i,] =  calc.relimp(l[[i]][,1:ncol(l[[i]])-1], weights = l[[i]][,"weight"], type = "genizi", rank = FALSE, rela = TRUE)@genizi
    r2[i,] = calc.relimp(l[[i]][,1:ncol(l[[i]])-1], weights = l[[i]][,"weight"], type = "genizi", rank = FALSE, rela = FALSE)@genizi
}        

colnames(driver) =  names(l[[1]][3:length(l[[i]])-1])
colnames(r2) = names(l[[1]][3:length(l[[i]])-1])
row.names(corr) = row.names(driver) = splits

driver <- t(driver)
corr <- t(corr)
corr <- cbind(names(l[[1]][,2:ncol(l[[1]])-1]), corr)
r2 <- t(r2)
r2sums <- t(colSums(r2[,1:ncol(r2)]))
rm(r2)



# ==================================================================================
# Write output to excel
# ==================================================================================

# ToDo, also add f?rklaringsgrad to output, maybe in cells below the matrix output?

wb <- createWorkbook()

addWorksheet(wb, "driver")
addWorksheet(wb, "corr")

writeData(wb, "driver", driver, colNames = T, rowNames = T)
writeData(wb, "driver", r2sums, colNames = F, rowNames = F, startRow = nrow(driver)+3, startCol = 2)
writeData(wb, "corr", corr, colNames = T, rowNames = F)

saveWorkbook(wb, "[PATH_TO_OUTFILE].xlsx", overwrite = T)
