
# needed functions
library(relaimpo)

f_topbox <- function(formula, data, weights, tb_limit){
    w <- data[,weights]
    
    # Select vars
    form <- strsplit(x = formula, split = "~")
    vars <- trimws(strsplit(form[[1]][2], split = "\\+")[[1]], which = "both")
    sub_data <- subset(data, select = vars)
    
    d <- apply(sub_data, 2, function(x){
        ifelse(x >= tb_limit, 1, 0) * w
        })
    return(apply(d, 2, function(x){sum(x, na.rm = TRUE)/length(x)}))
}


rwa <- function(formula, data, split_var = FALSE, weights, tb_limit){
    data <- as.data.frame(data)
    # Store results
    res <- list()
    tb <- list()
    j <- 1
    
        # Total
        res[[j]] <- calc.relimp(
            eval(parse(text = formula)),
            type = "genizi",
            rank = FALSE,
            rela = TRUE,
            data = data,
            weights = as.vector(data[,weights]))@genizi
        
        tb[[j]] <- f_topbox(formula = formula, data = data, weights = weights, tb_limit = tb_limit)
        
        uniq <- as.vector(unique(data[,split_var]))
        
        # If function for splitting
        if(split_var != FALSE){
            # Per split
            for(i in 1:length(uniq)) {
                sub_df <- subset(data, data[, split_var] == uniq[i])
                res[[j+1]] <- calc.relimp(
                    eval(parse(text = formula)),
                    type = "genizi",
                    rank = FALSE,
                    rela = TRUE,
                    data = sub_df,
                    weights = as.vector(sub_df[,weights]))@genizi
                
                tb[[j+1]] <- f_topbox(formula = formula, data = sub_df, weights = weights, tb_limit = tb_limit)
                
                j <- j + 1
            }
        }
    
    # Insert result in data frame
    rwa_output <- NULL
    tb_output <- NULL
    for (k in 1:length(res)) {
        rwa_output <- as.data.frame(cbind(rwa_output, res[[k]]))
        tb_output <- as.data.frame(cbind(tb_output, tb[[k]]))
    }
    
    colnames(rwa_output) <- append("Total", uniq)
    colnames(tb_output) <- append("Total", uniq)
    
    return(list(rwa = rwa_output,
                topbox = tb_output))
}

# load("rwadata.rda")
# formula <- "dependant ~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10 + q11 + q12 + q13 + q14 + q15"

# test total with split and weight
total_split <- rwa(formula, data = rwadata, split_var = "splitter", weights = "weight", tb_limit = 1)

# Test total without split and weight
total <- rwa(formula, data = rwadata, weight = "weight", tb_limit = 1)


df <- data.frame(dependant = sample(1:7, 200, replace = TRUE),
                 q1 = sample(1:5, 200, replace = TRUE),
                 q2 = sample(1:5, 200, replace = TRUE),
                 q3 = sample(1:5, 200, replace = TRUE),
                 q4 = sample(1:5, 200, replace = TRUE),
                 q5 = sample(1:5, 200, replace = TRUE),
                 q6 = sample(1:5, 200, replace = TRUE),
                 q7 = sample(1:5, 200, replace = TRUE),
                 q8 = sample(1:5, 200, replace = TRUE),
                 q9 = sample(1:5, 200, replace = TRUE),
                 weight = runif(n = 200, min = .5, max = 2.5),
                 split = c(rep("SE", 50),
                           rep("NO", 50),
                           rep("DK", 50),
                           rep("FI", 50)))

save(df, file = "rwa/data/exdata.rda")

form <- c("dependant ~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 +q9")
total_split <- rwa(form, data = df, split_var = "split", weights = "weight", tb_limit = 4)
