library(relaimpo)

load("rwadata.rda")

rwa <- function(formula, data, split_var){
    # Store results
    res <- list()
    j <- 1
   
    # Total
    res[[j]] <- calc.relimp(
        eval(parse(text = formula)),
        type = "genizi",
        rank = FALSE,
        rela = TRUE,
        data = data,
        weights = data$weight)@genizi
    
    # Per split
    # Tibbles in tidyverse forces unique to tibble if package is loaded
    # Therefore this ugly work around
    if("tibble" %in% .packages()){
        uniq <- as_vector(unique(data[,split_var]))
    } else{
        uniq <- as.vector(unique(data[,split_var]))
        }
    
    for(i in 1:length(uniq)) {
            df <- subset(data, data[, split_var] == uniq[i])
            res[[j+1]] <- calc.relimp(
                eval(parse(text = formula)),
                type = "genizi",
                rank = FALSE,
                rela = TRUE,
                data = df,
                weights = df$weight)@genizi
            j <- j + 1
            }
    
    # Insert result in data frame
    rwa_output <- NULL
    for (k in 1:length(res)) {
        rwa_output <- as.data.frame(cbind(rwa_output, res[[k]]))
    }
    
    colnames(rwa_output) <- append("Total", uniq)
    return(rwa_output)
}

form <- "dependant ~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10 + q11 + q12 + q13 + q14 + q15"
rwa(form, data = rwadata, split_var = "splitter")


