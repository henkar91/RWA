#' @title RWA plot
#' @name rwa_plot
#' @param rwa_object list, output from function rwa()
#' @param label_vec Vector of labels, inserted in same order as in formula
#' @return A list of plots
#' @description Makes a scatterplot of relative weight analysis for each split
#' @export

rwa_plot <- function(rwa_object, label_vec = NULL, hjust=-.5, vjust=0){
    nIter <- ncol(rwa_object$rwa)
    if(is.null(label_vec)){
        label_vec <- row.names(rwa_object[[1]])
    }
    
    output <- list()
    for(i in 1:nIter){
        output[[i]] <- eval(substitute(ggplot() +
            geom_point(aes(x = rwa_object[[1]][, i], y = rwa_object[[2]][, i])) +
                geom_text(aes(x = rwa_object[[1]][, i], y = rwa_object[[2]][, i], 
                              label = label_vec, hjust = hjust, vjust = vjust)) +
            geom_vline(aes(xintercept = mean(rwa_object[[1]][, i]))) +
            geom_hline(aes(yintercept = mean(rwa_object[[2]][, i]))) +
            theme_minimal() +
            ggtitle(label = paste("Relative Weight Analysis", colnames(rwa_object[[1]][i]), sep = " "),
                    subtitle = paste(expression(R2), ": ", round(rwa_object[[3]][i], 2), "\n",
                    "Base: ", rwa_object[[4]][i], sep = "")) +
            labs(x = "Relative Weight", y = "Share of Top Box") +
            scale_y_continuous(labels = scales::percent), list(i=i)))
    }
    return(output)
}
