#' @title Double Index
#' @name double_index
#' @description Computes double index from Data Frame or Matrix, first column contain labels
#' @param X a Data Frame or Matrix
#' @param labels_in_first_col boolean if labels is in first col
#' @keywords double_index
#' @examples double_index(data_double_index)
#' @return A a matrix with double index
#' @export

double_index <- function(X, labels_in_first_col = TRUE){
  # set missing to 0
  X[is.na(X)] <- 0
  
  if(labels_in_first_col == TRUE){
    num <- X[,2:ncol(X)]
  } else{
  num <- X
  }
  
  col_mean <- colMeans(num)
  avg_col_mean <- mean(col_mean)
  row_mean <- rowMeans(num)
  
  step1 <- matrix(NA, ncol = ncol(num), nrow = nrow(num))
  for(i in 1:ncol(num)){
    step1[,i] <- num[,i]/col_mean[i]
  }
  
  step2 <- c()
  for(j in 1:nrow(num)){
    step2[j] <- row_mean[j]/avg_col_mean
  }
  
  DI <- matrix(NA, ncol = ncol(num), nrow = nrow(num))
  for(k in 1:ncol(num)){
    DI[,k] <- (step1[,k]/step2)*100
  }
  
  colnames(DI) <- colnames(num)
  if(labels_in_first_col == TRUE){
    rownames(DI) <- X[,1]
  }
  return(DI)
}
