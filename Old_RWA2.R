rm(list=ls())

# read raw data
library(xlsx)
library(openxlsx)
library(haven)
r <- read_spss("SE17-039436_DRIVERS_R.sav")
names(r) <- tolower(names(r))

colnames <- data.frame(names(r)) # get a data.frame with column names for overview

# ----------------------------------------------------------------------------------
# structure data so that weight is last column, and dependant lies just before 
# explanatory variables in data.frame
# ----------------------------------------------------------------------------------
library(tidyverse)


d <- r %>% 
    dplyr::select(index1:weight)
    

# library(questionr)
# freq(d$d0)

split_var <- d$diabetestyp_num # variable to use as split



splits <- c("1",
            "2",
            "3",
            "4",
            "5",
            "6",
            "7")

# =========================================================
# Drivers and Corr
# =========================================================
library(relaimpo)
library(weights)

x <- split(d, d$diabetestyp_num)

y <- lapply(seq_along(x), function(f) as.data.frame(x[[f]])[, 4:(length(x[[1]]) )]) #removes col that isn't dependent or explanatory

l = lapply(seq_along(y), function(f) {
    assign(splits[f], y[[f]])
}
)

corr = matrix(NA, ncol=length(1:ncol(l[[1]]))-1, nrow=length(l)) #creates an empty matrix for correlation
driver = matrix(NA, ncol=length(2:ncol(l[[1]]))-1, nrow=length(l)) #creates an empty matrix for drivers
r2 = matrix(NA, ncol=length(2:ncol(l[[1]]))-1, nrow=length(l)) #creates an empty matrix for drivers

for(i in 1:length(l)){
    corr[i,] = wtd.cors(l[[i]][,'index_1'], l[[i]][,2:ncol(l[[i]])-1], weight=l[[i]][,"weight"])
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



# =========================================================
# Write to excel
# =========================================================

wb <- createWorkbook()

sum_style <- createStyle(fontColour = "#000000", textDecoration = "bold")

addWorksheet(wb, "driver")
addWorksheet(wb, "corr")

writeData(wb, "driver", driver, colNames = T, rowNames = T)
writeData(wb, "driver", r2sums, colNames = F, rowNames = F, startRow = nrow(driver)+3, startCol = 2, headerStyle = sum_style)
writeData(wb, "corr", corr, colNames = T, rowNames = F)

saveWorkbook(wb, "//emeasestofile01/Projects/17/17-039436 Diabetesf?rbundet Medlemsunders?kning/06.Data management/Analys - Drivkrafter/driv1.xlsx", overwrite = T)
