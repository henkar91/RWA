#' @title Top Box
#' @name f_topbox
#' @param formula a text string with a formula, format "dependent ~ image1 + image2 + ... + imageN"
#' @param data a data frame containing data, split variable (optional) and weight
#' @param  weights A string with column name of weight variable
#' @param tb_limit Numeric, what is the lowest value included in top box 
#' @return A vector containing top box values
#' @description Computes top boxes, function is being called from rwa()
#' @export

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