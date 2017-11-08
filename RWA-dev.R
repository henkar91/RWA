
# Pre stuff to test.. -----------------------------------------------------
library(tidyverse)
library(relaimpo) #kills dplyr::select
library(weights)
library(sjmisc)

load("data.Rdata")

# You must order dataset as, split_var, dep_var, ... ,  weight_var (last)
data <- r %>% 
    dplyr::select(d0, q9_tot, q27a_1:q27a_2_8, wt = global_weight_10coun_salegdp)






# rwa_function, returns list of 3 -----------------------------------------

rwa <- function(data, dep_var, split_var = NULL, weight_var = NULL) {

    if (length(split_var > 0)) {
    
    x <- split(data, data[, split_var])

    y <- lapply(seq_along(x), function(f) as.data.frame(x[[f]])[, 2:(length(x[[1]]))]) #removes col that isn't dependent or explanatory

    l = lapply(seq_along(y), function(f) {
        assign(split_var[f], y[[f]])
    }
    )
    corr = matrix(NA, ncol=length(1:ncol(l[[1]]))-1, nrow=length(l)) #creates an empty matrix for correlation
    driver = matrix(NA, ncol=length(2:ncol(l[[1]]))-1, nrow=length(l)) #creates an empty matrix for drivers
    r2 = matrix(NA, ncol=length(2:ncol(l[[1]]))-1, nrow=length(l)) #creates an empty matrix for drivers

    for(i in 1:length(l)){
        corr[i,] = wtd.cors(l[[i]][, dep_var], l[[i]][,2:ncol(l[[i]])-1], weight=l[[i]][, weight_var])
        driver[i,] =  calc.relimp(l[[i]][,1:ncol(l[[i]])-1], weights = l[[i]][, weight_var], type = "genizi", rank = FALSE, rela = TRUE)@genizi
        r2[i,] = calc.relimp(l[[i]][,1:ncol(l[[i]])-1], weights = l[[i]][, weight_var], type = "genizi", rank = FALSE, rela = FALSE)@genizi
    }

    colnames(driver) = names(l[[1]][3:length(l[[i]])-1])
    colnames(r2) = names(l[[1]][3:length(l[[i]])-1])
    row.names(corr) = row.names(driver)

    driver <- t(driver)
    corr <- t(corr)
    corr <- cbind(names(l[[1]][,2:ncol(l[[1]])-1]), corr)
    r2 <- t(r2)
    r2sums <- t(colSums(r2[,1:ncol(r2)]))
    rm(r2)

    return(list(driver = driver, corr = corr, r2 = r2sums))
    
    } else {
        
        return("No Splits")
    }
    

}

# test it
test <- rwa(data, dep_var = "q9_tot", split_var = "d0", weight_var = "wt")

corr <- as.data.frame(test[3])
