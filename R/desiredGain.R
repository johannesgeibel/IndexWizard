#' Function to calculate selection index
#'
#' @param w Numeric vector of n economic weights. Traits present in G, but not part of the index need to be coded as 0.
#' @param G n*n genetic variance- covariance matrix.
#' @param E m*m (m <= n) residual variance- covariance matrix. If only a numeric vector is supplied, residuals will be assumed to be uncorrelated.
#' @param traits Character vector of trait names.
#'
#' @return
#' @export
#'
#' @examples
SelInd <- function(
    w,
    G,
    E,
    traits = NULL
){
  # check input ----------------------------------------------------------------
  ## w
  if(!exists(w, inherits = FALSE)){
    stop("No economic weights supplied")
  }
  ## derive trait names
  if(is.null(traits)){
    if(!is.null(names(w))){
      traits <- names(w)
    }else{
      traits <- paste0("trait_",1:length(w))
      warning("no trait names specified - numbering them automatically")
    }
  }else{
    if(length(traits) == length(w)){
      names(w) <- traits
    }else{
      stop("Length of w and traits differ")
    }
  }
  ## G
  if(!exists(G, inherits = FALSE)){
    stop("No Genetic VCV matrix supplied")
  }
  if(any(!dim(G) != c(length(w),length(w)))){
    stop("dimensions of w and G do not match")
  }

  ## E
  if(!exists(E, inherits = FALSE)){
    stop("No environmental VCV matrix supplied")
  }


  # initialize central object --------------------------------------------------
  object <- list(
    traits = traits,
    w = w,
    G = G,
    E = E
  )
  class(object) <- "SelInd"
}
