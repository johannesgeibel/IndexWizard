#' Function to calculate selection index
#'
#' @param w Numeric vector of n economic weights. Traits present in G, but not part of the index need to be coded as 0.
#' @param G Named n*n genetic variance- covariance matrix. Dimnames of G need to match E/r to ensure correct sorting.
#' @param E Named m*m (m <= n) residual variance- covariance matrix. If only a numeric vector is supplied, residuals will be assumed to be uncorrelated.
#' @param r Named numeric vector of reliabilites with length m. If E != NULL, calculated from G and E.
#' @param i Selection intensity
#'
#' @return
#' @export
#'
#' @examples
#'
#' SelInd(
#' w = c(A=0.2,B=1,C=0,D=3),
#' G = matrix(c(1,0,0,0,
#'              0,2,0,0,
#'              0,0,1,0.5,
#'              0,0,0.5,1),4,4,
#'            dimnames = list(c('A','C','B','D'),c('A','C','B','D'))),
#' r = c(D=0.2,A=0.7),
#' i = 0.02
#' )
#'
SelInd <- function(
    w,
    G,
    E = NULL,
    r = NULL,
    i = NULL
){
  # initialize central object --------------------------------------------------
  out <- list(
    w = NULL,
    G = NULL,
    E = E, # to be included
    r = r,
    b = NULL,
    i = i,
    d = NULL,
    dG = NULL
  )
  class(out) <- "SelInd"

  # check input ----------------------------------------------------------------
  ## w
  if(!exists("w", inherits = FALSE)){
    stop("No economic weights (w) supplied")
  }else if(is.null(names(w))){
    stop("w is not named")
  }else{
    out$w <- w
  }
  ## derive trait names
  # if(is.null(traits)){
  #   if(!is.null(names(w))){
  #     traits <- names(w)
  #   }else{
  #     traits <- paste0("trait_",1:length(w))
  #     warning("no trait names specified - numbering them automatically")
  #   }
  # }else{
  #   if(length(traits) == length(w)){
  #     names(w) <- traits
  #   }else{
  #     stop("Length of w and traits differ")
  #   }
  # }
  ## G
  if(!exists("G", inherits = FALSE)){
    stop("No Genetic VCV matrix supplied")
  }else if(any(dim(G) != c(length(w),length(w)))){
    stop("dimensions of w and G do not match")
  }else if(any(!names(w) %in% colnames(G))){
    stop("Trait names of G and w do not match")
  }else{
    G <- G[names(w),names(w)]
    out$G <- G
  }

  ## E and r
  if(is.null(out$E)){
    if(is.null(out$r)){
      stop("Neither environmental VCV matrix nor reliabilites supplied")
    }else{
      if(any(!names(out$r) %in% colnames(out$G))){
        stop("r containes traits not in G")
      }
      # set up D
      out$D <- matrix(0,length(out$r),ncol(out$G),dimnames = list(names(out$r),colnames(out$G)))
      for(i in 1:length(r)){
        out$D[names(out$r)[i],names(out$r)[i]] <- 1
      }
      # set up R
      R <- diag(out$r)
      dimnames(R) <- list(names(out$r), names(out$r))
      #derive E from r and G
      g <- diag(out$D %*% out$G %*% t(out$D))
      out$E <- diag((1-r)/r*g)
      dimnames(out$E) <- list(names(r),names(r))
      warning("only reliabilities given\n --> setting up E based on r and G\n --> residual errors are assumed to be uncorrelated!")
    }
  }else if(!is.null(E) & !is.null(r)){
    # check whether E is diagonal
    # check length of r
    # calc difference between E and r
  }

  # calc index weights ---------------------------------------------------------
  out$b <- solve(R %*% (out$D %*% out$G %*% t(out$D)) %*% R) %*% R %*% out$D %*% out$G %*% out$w

  # calc variance of index -----------------------------------------------------
  out$var_I <- t(out$b) %*% R %*% (out$D %*% out$G %*% t(out$D) + out$E) %*% R %*% out$b

  # calc expected composition of genetic trend ---------------------------------
  if(!is.null(out$i)){
    out$d <- (out$i / sqrt(out$var_I)[1,1] ) * (out$G %*% t(out$D) %*% R %*% out$b)
    out$dG <- t(out$d) %*% out$w
  }else{
    warning("no selection intensity provided, cannot compute the expected genetic trend")
  }

  # return output --------------------------------------------------------------
  out$b <- out$b[,1]
  out$var_I <- out$var_I[1,1]
  if(!is.null(out$d)) out$d <- out$d[,1]
  if(!is.null(out$dG)) out$dG <- out$dG[1,1]

  return(out)
}


