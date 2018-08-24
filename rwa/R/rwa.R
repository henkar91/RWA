#' @title Relative Weight Analysis
#' @name rwa
#' @param formula a text string with a formula, format "dependent ~ image1 + image2 + ... + imageN"
#' @param data a data frame containing data, split variable (optional) and weight
#' @param split_var (Optional), specify variable to for sub-computations, a string
#' @param  weights A string with column name of weight variable
#' @param tb_limit Numeric, what is the lowest value included in top box 
#' @return A list containing two data frames, rwa and top box
#' @description Computes relative weight analysis and top boxes with subsets from a data frame
#' @export


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
        weights = as.vector(data[,weights]))
    
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
                weights = as.vector(sub_df[,weights]))
            
            tb[[j+1]] <- f_topbox(formula = formula, data = sub_df, weights = weights, tb_limit = tb_limit)
            
            j <- j + 1
        }
    }
    
    # Insert result in data frame
    rwa_output <- NULL
    r2_output <- NULL
    tb_output <- NULL
    for (k in 1:length(res)) {
        rwa_output <- as.data.frame(cbind(rwa_output, res[[k]]@genizi))
        r2_output <- c(r2_output, res[[k]]@R2)
        tb_output <- as.data.frame(cbind(tb_output, tb[[k]]))
    }
    
    colnames(rwa_output) <- append("Total", uniq)
    names(r2_output) <- append("Total", uniq)
    colnames(tb_output) <- append("Total", uniq)
    
    return(list(rwa = rwa_output,
                topbox = tb_output,
                r2 = r2_output))
}
