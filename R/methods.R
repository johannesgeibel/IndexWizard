# print -----------------------------------------------------------------------
#' Function to nicely print a SelInd object
#'
#' @param x An object of class SelInd
#'
#' @return
#' @export
#'
#' @examples
print.SelInd <- function(x){
  print.default(x)
}

# summary ---------------------------------------------------------------------
#' Function to summarize the content of a SelInd object.
#'
#' @param object An object of class SelInd
#'
#' @return
#' @export
#'
#' @examples
summary.SelInd <- function(object){
  cat(
    "An object of class SelInd. The index is based on:\n",
    "  - n =",length(object$w),"breeding goal traits\n",
    "  -",sum(object$w!=0),"traits with economic weight != 0\n",
    "  - m =",length(object$r),"index traits\n")
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
  cat("The SelInd object does not contain the entries ",obj_out,".\n", sep = "")

}

# as.data.frame ----------------------------------------------------------------
#' Function to coerce the results of a SelInd object to a data.frame for easy access.
#'
#' @param x An object of class SelInd
#' @param long logical indicating, whether resulting data.frame shall be in long format.
#'
#' @return a data.frame with traits in rows and result vectors in columns. if long = TRUE, a data.frame in long format with three columns (variable, trait and value)
#' @export
#'
#' @examples
as.data.frame.SelInd <- function(x, long = FALSE){
  x <- t(x$results)
  x <- as.data.frame(x)
  if(long){
    out <- list()
    for(i in 1:length(x)){
      out[[i]] <- data.frame(
        variable = colnames(x)[i],
        trait = rownames(x),
        value = x[[i]]
      )
    }
    out <- do.call(rbind,out)
    rownames(out) <- NULL
    return(out)
  }else{
    return(x)
  }
}
