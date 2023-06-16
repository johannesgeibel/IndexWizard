# print -----------------------------------------------------------------------
#' Function to nicely print a SelInd object
#'
#' @param x An object of class SelInd
#' @param ... does nothing, only for compatibility with the generic function
#' 
#' @return No return value, only prints formatted output.
#' 
#' @export
#' @examples
#' tn <- c("RZM", "RZN", "RZEo")
#' G <- matrix(
#'     c(1.0,0.13,0.13,
#'     0.13,1.0,0.23,
#'     0.13,0.23,1.0),
#'     3, 3, dimnames = list(tn,tn)
#'     ) * 144
#' w <- c(0.7, 0.3, 0)
#' names(w) <- tn
#' r2 <- c(0.743, 0.673)
#' names(r2) <- tn[1:2]
#' res <- SelInd(
#'   w = w,
#'   G = G,
#'   r2 = r2
#' )
#' print(res)
#'
print.SelInd <- function(x, ...){
  cat(
    "An object of class SelInd. The index is based on:\n",
    "  - n =",length(x$w),"breeding goal traits\n",
    "  -",sum(x$w!=0),"traits with economic weight != 0\n",
    "  - m =",length(x$r2),"index traits\n")

  # message missing print of matrizes ------------------------------------------
  cat("\n--------------------------------------------------------------------\n")
  mats <- c("G","E","H")
  mats <- mats[!sapply(x[mats],is.null)]
  cat("\nMatrices",paste0(mats,collapse = ", "),"present in SelInd object, but not printed to reduce complexity.\n")
  cat("Extract them by the use of the `$` operator.\n")

  # print variance of index and total excpected gain ---------------------------
  cat("\n--------------------------------------------------------------------\n")
  cat("\nVaiance of the index (`var_I`), expected genetic gain (`dG`)\nand assumed selection intensity (`i`):\n")
  cat("\n$var_I:\n")
  print(round(x$var_I,2))
  if(!is.null(x$dG)){
    cat("\n$dG:\n")
    print(round(x$dG,2))
  }
  if(!is.null(x$i)){
    cat("\n$i:\n")
    print(round(x$i,2))
  }
  # print weights --------------------------------------------------------------
  cat("\n--------------------------------------------------------------------\n")
  cat("\nEconomic (`w`) and index weights (`b`). Potentially they are rescaled (`scaled`)\nso that sum(abs()) = 1. The weights might represent realized (`real`) weights\nbased on an observed composition of the genetic trend:\n")
  cat("\n$w:\n")
  print(round(x$w,2))
  cat("\n$b:\n")
  print(round(x$b,2))
  cat("\n$b_scaled:\n")
  print(round(x$b_scaled,2))
  if(!is.null(x$w_real)){
    cat("\n$w_real:\n")
    print(round(x$w_real,2))
  }
  if(!is.null(x$b_real)){
    cat("\n$b_real:\n")
    print(round(x$b_real,2))
  }


  # print r2 and h2 ------------------------------------------------------------
  cat("\n--------------------------------------------------------------------\n")
  cat("\nreliabilities (`r2`) and heritabilities (`h2`) of the traits:\n")
  cat("\n$r2:\n")
  print(round(x$r2,2))
  if(!is.null(x$h2)){
    cat("\n$h2:\n")
    print(round(x$h2,2))
  }

  # print compositions ---------------------------------------------------------
  cat("\n--------------------------------------------------------------------\n")
  cat("\nComposition (`d`) of genetic (`G`) / phenotypic (`P`) trend.\n")
  cat("The composition might be observed (`obs`) or expected (`exp`).\n")
  cat("The composition might be scaled (`scaled`) so that sum(abs()) = 1:\n")
  if(!is.null(x$d_G_obs)){
    cat("\n$d_G_obs:\n")
    print(round(x$d_G_obs,2))
  }
  if(!is.null(x$d_G_exp)){
    cat("\n$d_G_exp:\n")
    print(round(x$d_G_exp,2))
  }
  if(!is.null(x$d_P_exp)){
    cat("\n$d_P_exp:\n")
    print(round(x$d_P_exp,2))
  }
  if(!is.null(x$d_G_exp_scaled)){
    cat("\n$d_G_exp_scaled:\n")
    print(round(x$d_G_exp_scaled,2))
  }
  if(!is.null(x$d_P_exp_scaled)){
    cat("\n$d_P_exp_scaled:\n")
    print(round(x$d_P_exp_scaled,2))
  }

  # print analytic measures ----------------------------------------------------
  cat("\n--------------------------------------------------------------------\n")
  cat("\nAnalytic measures:\n")
  cat("Correlation between the overall index and the phenotype of trait j (`r_IP`) and\n")
  cat("the loss in prediction accuracy when omitting trait j from the index (`r_IH`).\n")
  cat("Further the approximate first derivative of d_G_exp with respect to w (`del_d_scaled`) with rows\nbeing scaled  so that sum(abs()) = 1.\n")
  if(!is.null(x$r_IP)){
    cat("\n$r_IP:\n")
    print(round(x$r_IP,2))
  }
  if(!is.null(x$r_IH)){
    cat("\n$r_IH:\n")
    print(round(x$r_IH,2))
  }
  if(!is.null(x$del_d_scaled)){
    cat("\n$del_d_scaled:\n")
    print(round(x$del_d_scaled,2))
  }

  #print.default(x)
}

# summary ---------------------------------------------------------------------
#' Function to summarize the content of a SelInd object.
#'
#' @param object An object of class SelInd
#' @param ... does nothing, only for compatibility with the generic function
#'
#' @return No return value, only prints a summary of the SelInd object.
#'
#' @export
#' @examples
#' tn <- c("RZM", "RZN", "RZEo")
#' G <- matrix(
#'     c(1.0,0.13,0.13,
#'     0.13,1.0,0.23,
#'     0.13,0.23,1.0),
#'     3, 3, dimnames = list(tn,tn)
#'     ) * 144
#' w <- c(0.7, 0.3, 0)
#' names(w) <- tn
#' r2 <- c(0.743, 0.673)
#' names(r2) <- tn[1:2]
#' res <- SelInd(
#'   w = w,
#'   G = G,
#'   r2 = r2
#' )
#' summary(res)
#'
summary.SelInd <- function(object, ...){
  cat(
    "An object of class SelInd. The index is based on:\n",
    "  - n =",length(object$w),"breeding goal traits\n",
    "  -",sum(object$w!=0),"traits with economic weight != 0\n",
    "  - m =",length(object$r2),"index traits\n")
  # check whether E is uncorrelated
  check <- any(object$E[upper.tri(object$E)] != 0)
  if(check){
    check <- "correlated"
  }else{
    check <- "uncorrelated"
  }
  cat("\nResidual errors were assumed to be ",check,".\n", sep = "")
  # print non-calculated objects
  obj_in <- paste(names(object)[!sapply(object,is.null)], collapse = ", ")
  obj_out <- paste(names(object)[sapply(object,is.null)], collapse = ", ")
  cat("\nThe SelInd object contains the entries ",obj_in,".\n", sep = "")
  if(any(sapply(object,is.null))){
    obj_out <- paste(names(object)[sapply(object,is.null)], collapse = ", ")
    cat("The SelInd object does not contain the entries ",obj_out,".\n", sep = "")
  }

}

# # as.data.frame ----------------------------------------------------------------
# #' Function to coerce the results of a SelInd object to a data.frame for easy access.
# #'
# #' @param x An object of class SelInd
# #' @param row.names does nothing, only for compatibility with the generic function
# #' @param optional does nothing, only for compatibility with the generic function
# #' @param ... does nothing, only for compatibility with the generic function
# #' @param long logical indicating, whether resulting data.frame shall be in long format.
# #'
# #' @return a data.frame with traits in rows and result vectors in columns. if long = TRUE, a data.frame in long format with three columns (variable, trait and value)
# #' @export
# #'
# as.data.frame.SelInd <- function(x, row.names, optional, ..., long = FALSE){
#   x <- t(x$results)
#   x <- as.data.frame(x)
#   if(long){
#     out <- list()
#     for(i in 1:length(x)){
#       out[[i]] <- data.frame(
#         variable = colnames(x)[i],
#         trait = rownames(x),
#         value = x[[i]]
#       )
#     }
#     out <- do.call(rbind,out)
#     rownames(out) <- NULL
#     return(out)
#   }else{
#     return(x)
#   }
# }

