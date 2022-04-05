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
    "an object of class SelInd. The index is based on:\n",
    "  -",length(object$w),"breeding goal traits\n",
    "  -",sum(object$w!=0),"traits with economic weight != 0\n",
    "  -",length(object$r),"index traits\n")
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
