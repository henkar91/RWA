#' Split Relative Weight Analysis
#'
#' This function uses the relaimpo package to produce a list of corr, driver and r2 elements.
#' These are in matrix form for further analysis. Optionally these list elements can be 
#' exported to a spreadsheet file with package openxlsx
#' @param data A data frame object with variable to split on first, then the dependant variable, 
#' then any independant variables, and lastly, a weight variable (if any). We are presuming 
#' TRUE/FALSE type of data in testing variables.
#' @param split_var The name of the column on which to split, within quotes.
#' @param dep_var The name of the dependant variable, within quotes.
#' @param weight_var The name of a column containing weights, within quotes. Optional.
#' @param filename A string declaring filename (full path) of .xlsx file to be produced.
#' @keywords split_rwa RWA
#' @export

split_rwa <- function(data, 
                      split_var, 
                      dep_var, 
                      weight_var = NULL,
                      filename = NULL) {
    
    x <- split(data, data[, split_var])
    
    y <- lapply(seq_along(x), function(f) as.data.frame(x[[f]])[, 2:(length(x[[1]]))]) 
    
    l = lapply(seq_along(y), function(f) {
        assign(split_var[f], y[[f]])
    }
    )
    
    corr = matrix(NA, ncol=length(1:ncol(l[[1]]))-1, nrow=length(l)) #creates an empty matrix for correlation
    driver = matrix(NA, ncol=length(2:ncol(l[[1]]))-1, nrow=length(l)) #creates an empty matrix for drivers
    r2 = matrix(NA, ncol=length(2:ncol(l[[1]]))-1, nrow=length(l)) #creates an empty matrix for drivers
    
    # Check whether weight_var is given, if not, run unweighted
    if (length(weight_var > 0)) {
        for(i in 1:length(l)) {
            corr[i,] = weights::wtd.cors(l[[i]][, dep_var], l[[i]][,2:ncol(l[[i]])-1], weight=l[[i]][, weight_var])
            driver[i,] =  relaimpo::calc.relimp(l[[i]][,1:ncol(l[[i]])-1], weights = l[[i]][, weight_var], type = "genizi", rank = FALSE, rela = TRUE)@genizi
            r2[i,] = relaimpo::calc.relimp(l[[i]][,1:ncol(l[[i]])-1], weights = l[[i]][, weight_var], type = "genizi", rank = FALSE, rela = FALSE)@genizi
        } 
    } else {
        for(i in 1:length(l)) {
            corr[i,] = cor(l[[i]][, dep_var], l[[i]][,2:ncol(l[[i]])-1])
            driver[i,] =  relaimpo::calc.relimp(l[[i]][,1:ncol(l[[i]])-1], type = "genizi", rank = FALSE, rela = TRUE)@genizi
            r2[i,] = relaimpo::calc.relimp(l[[i]][,1:ncol(l[[i]])-1], type = "genizi", rank = FALSE, rela = FALSE)@genizi
        }
    }
    
    # ToDo, get split as character names to corr, driver and r2 !!!!
    
    colnames(driver) = names(l[[1]][3:length(l[[i]])-1])
    colnames(r2) = names(l[[1]][3:length(l[[i]])-1])
    row.names(corr) = row.names(driver)
    
    driver <- t(driver)
    corr <- t(corr)
    corr <- cbind(names(l[[1]][,2:ncol(l[[1]])-1]), corr)
    r2 <- t(r2)
    r2sums <- t(colSums(r2[,1:ncol(r2)]))
    rm(r2)
    
    
    # if file > 0 write output
    if (length(filename > 0)) {
        wb <- openxlsx::createWorkbook()
        
        openxlsx::addWorksheet(wb, "driver")
        openxlsx::addWorksheet(wb, "corr")
        
        openxlsx::writeData(wb, "driver", driver, colNames = TRUE, rowNames = TRUE)
        openxlsx::writeData(wb, "driver", r2sums, colNames = FALSE, rowNames = FALSE, 
                            startRow = nrow(driver) + 3, startCol = 2)
        openxlsx::writeData(wb, "corr", corr, colNames = TRUE, rowNames = FALSE)
        
        openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
    }
    
    return(list(driver = driver, corr = corr, r2 = r2sums))
    
}

