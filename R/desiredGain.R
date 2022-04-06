#' Function to calculate selection index
#'
#' @param w Numeric vector of n economic weights. Traits present in G, but not part of the index need to be coded as 0. If traits of G are missing, they will be added automatically with zero weight. Required.
#' @param G Named n*n genetic variance- covariance matrix. Dimnames of G need to match E/r to ensure correct sorting.  Required.
#' @param r Named numeric vector of reliabilites with length m. Required.
#' @param H Named m*m variance-covariance matrix of estimated breeding to internally derive the residual variance-covariance matrix.
#' @param i Selection intensity
#' @param h2 named numeric vector of length n containing heritabilities for the traits
#' @param d_real named numeric vector of length n containing the observed composition of the genetic gain scaled in genetic standard deviations. If sum(d_real) != 1, it will be rescaled.
#' @param verbose Shall information be printed?
#'
#' @details The framework allows to have less traits in the selection index than in the breeding goal (m < n). Calculation of realized economic weights, however, requires m == n.
#'
#' @return A list of class SelInd
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
#' i = 0.02,
#' h2 = c(A=0.2,B=0.2,C=0.2,D=0.3)
#' )
#'
SelInd <- function(
    w,
    G,
    r,
    H = NULL,
    i = NULL,
    h2 = NULL,
    d_real = NULL,
    verbose = TRUE
){
  # initialize central object --------------------------------------------------
  out <- list(
    w = NULL,
    G = NULL,
    E = NULL,
    r = r,
    b = NULL,
    i = i,
    d = NULL,
    dG = NULL,
    h2 = h2,
    d_real = d_real
  )
  class(out) <- "SelInd"

  # check input ----------------------------------------------------------------
  ## w
  if(!exists("w", inherits = FALSE)){
    stop("No economic weights (w) supplied")
  }else if(is.null(names(w))){
    stop("w is not named")
  }else{
    if(sum(abs(w)) != 1){
      if(verbose) message("- sum(abs(w)) != 1\n  --> rescaling w")
    }
    out$w <- w/sum(abs(w))
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
  }else if(dim(G)[1] != dim(G)[2] || any(G[upper.tri(G)] != t(G)[upper.tri(G)])){
    stop("G is not squared")
  }else if(any(colnames(G) != rownames(G))){
    stop("row- and colnames of G do not match")
  }else if(dim(G)[1] < length(w)){
    stop("more traits in w than in G")
  }else if(any(!names(w) %in% colnames(G))){
    stop("Some trait names of w are not in G")
  }else{
    if(any(!colnames(G) %in% names(w))){
      if(verbose) message("- missing traits in w\n  --> adding them with zero weight")
      temp <- rep(0,sum(!colnames(G) %in% names(w)))
      names(temp) <- colnames(G)[!colnames(G) %in% names(w)]
      w <- c(w,temp)
      out$w <- w
    }
    G <- G[names(w),names(w)]
    out$G <- G
  }

  ## E and r
  if(is.null(out$r)){
    stop("No reliabilities supplied")
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
    if(is.null(H)){
      g <- diag(out$D %*% out$G %*% t(out$D))
      out$E <- diag((1-r)/r*g)
      dimnames(out$E) <- list(names(r),names(r))
      if(verbose) message("- only reliabilities given\n  --> setting up E based on r and G\n  --> residual errors are assumed to be uncorrelated!")
    }else{
      # include check of H
      out$H <- H[names(out$r),names(out$r)]
      out$E <- solve(sqrt(R)) %*% H %*% solve(sqrt(R))
      out$E <- out$E - out$D %*% out$G %*% t(out$D)
    }
  }


  # check h2
  if(!is.null(out$h2)){
    if(length(out$h2) != length(out$w)){
      stop("length of h2 does not equal length of w")
    }else if(any(!names(out$h2) %in% names(out$w))){
      stop("h2 containes traits not in w")
    }else{
      out$h2 <- out$h2[names(out$w)]
    }
  }

  # check d_real
  if(!is.null(out$d_real)){
    if(length(out$d_real) != length(out$r)){
      stop("length of d_real does not equal length of r")
    }else if(any(!names(out$d_real) %in% names(out$r))){
      stop("d_real containes traits not in r")
    }else{
      out$d_real <- out$d_real[names(out$r)]
    }
    if(sum(out$d_real) != 1){
      if(verbose) message("- sum(d_real) != 1 -- rescaling d_real")
      out$d_real <- out$d_real/sum(out$d_real)
    }
  }

  # calc index weights ---------------------------------------------------------
  out$b <- solve(R %*% (out$D %*% out$G %*% t(out$D)) %*% R) %*% R %*% out$D %*% out$G %*% out$w
  out$b_scaled <- sum(abs(out$b))

  # calc variance of index -----------------------------------------------------
  out$var_I <- t(out$b) %*% R %*% (out$D %*% out$G %*% t(out$D) + out$E) %*% R %*% out$b

  # calc expected composition of genetic and phenotypic trend ---------------------------------
  if(!is.null(out$i)){
    out$d <- (out$i / sqrt(out$var_I)[1,1] ) * (out$G %*% t(out$D) %*% R %*% out$b)
    out$dG <- t(out$d) %*% out$w
    if(!is.null(out$h2)){
      out$d_P <- out$d * sqrt(out$h2) / sqrt(g)
    }else{
      if(verbose) message("- no heritabilities provided\n  --> cannot compute the expected phenotypic trend")
    }
  }else{
    if(verbose) message("- no selection intensity provided\n  --> can only compute the relative genetic and phenotypic trend")
  }
  # relative trend
  out$d_rel <- (out$G %*% t(out$D) %*% R %*% out$b)
  out$d_rel <- out$d_rel/sum(abs(out$d_rel))
  if(!is.null(out$h2)){
    out$d_P <- out$d * sqrt(out$h2) / sqrt(g)
  }else{
    if(verbose) message("- no heritabilities provided\n  --> cannot compute the expected relative phenotypic trend")
  }

  # calc analytical measures ---------------------------------------------------
  out$r_IP <- (t(out$b) %*% (R %*% out$D %*% out$G %*% t(out$D))) / (sqrt(out$var_I[1,1] * diag(R %*% out$D %*% out$G %*% t(out$D))))
  out$r_IH <- 1 - sqrt(1 - t(out$b^2) /
                         (t(out$b) %*% R %*% ( out$D %*% out$G %*% t(out$D) + out$E) %*% R %*% out$b %*% diag(solve(R %*% ( out$D %*% out$G %*% t(out$D) + out$E) %*% R))) )

  # first derivative -----------------------------------------------------------
  if(!is.null(out$i)){
    out$del_d <- (out$i / sqrt(out$var_I))[1,1] * out$G %*% t(out$D) %*% solve(out$D %*% out$G %*% t(out$D) + out$E) %*% out$D %*% out$G
    #out$del_d <- (out$i / sqrt(out$var_I))[1,1] * out$G %*% t(out$D) %*% solve(out$D %*% out$G %*% t(out$D) + out$E) %*% out$D %*% out$G %*% w
  }else{
    if(verbose) message("- first derivative of d by w can only be calculated if selescion intensity (i) is supplied\n  --> calculating only scaled version")
  }
  out$del_d_scaled <- out$G %*% t(out$D) %*% solve(out$D %*% out$G %*% t(out$D) + out$E) %*% out$D %*% out$G
  for(i in 1:nrow(out$del_d_scaled)){
    srow <- sum(abs(out$del_d_scaled[i,]))
    out$del_d_scaled[i,] <- out$del_d_scaled[i,] / srow
  }

  # realized weights -----------------------------------------------------

  if(!is.null(out$d_real)){
    if(any(dim(out$G) != dim(out$E))){
      if(verbose) message("- m != n\n  --> subsetting w  and G to length m for calculation of realized weights")
      tmp <- colnames(R)
      out$b_real <-  solve(out$G[tmp,tmp] %*% t(out$D[,tmp]) %*% R) %*% out$d_real
      out$w_real <- solve(out$D[,tmp] %*% out$G[tmp,tmp]) %*% (out$D[,tmp] %*% out$G[tmp,tmp] %*% t(out$D[,tmp]) + out$E) %*% solve(out$G[tmp,tmp] %*% t(out$D[,tmp])) %*% out$d_real
    }else{
      out$b_real <-  solve(out$G %*% t(out$D) %*% R) %*% out$d_real
      out$w_real <- solve(out$D %*% out$G) %*% (out$D %*% out$G %*% t(out$D) + out$E) %*% solve(out$G %*% t(out$D)) %*% out$d_real
    }
    out$b_real <- out$b_real/sum(abs(out$b_real))
    out$w_real <- out$w_real/sum(abs(out$w_real))

  }else{
    if(verbose) message("- No observed proportion of genetic progress given\n  --> cannot calculate realized weights")
  }

  # return output --------------------------------------------------------------
  out$b <- out$b[,1]
  out$var_I <- out$var_I[1,1]
  if(!is.null(out$d)) out$d <- out$d[,1]
  if(!is.null(out$d_rel)) out$d_rel <- out$d_rel[,1]
  if(!is.null(out$dG)) out$dG <- out$dG[1,1]
  #if(!is.null(out$del_d)) out$del_d <- out$del_d[,1]
  if(!is.null(out$b_real)) out$b_real <- out$b_real[,1]
  if(!is.null(out$w_real)) out$w_real <- out$w_real[,1]
  return(out)
}


