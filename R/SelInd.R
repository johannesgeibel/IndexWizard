#' Function to calculate selection index
#'
#' @param w Numeric vector of n economic weights. Traits present in G, but not part of the index need to be coded as 0. If traits of G are missing, they will be added automatically with zero weight. Required.
#' @param G Named n*n genetic variance- covariance matrix. Dimnames of G need to match w to ensure correct sorting.  Required.
#' @param r2 Named numeric vector of reliabilities with length m. Required.
#' @param H Named m*m variance-covariance matrix of estimated breeding to internally derive the residual variance-covariance matrix. If H contains more traits than r2, it will be subsetted.
#' @param i Selection intensity
#' @param h2 named numeric vector of length n containing heritabilities for the traits
#' @param d_G_obs named numeric vector of length n containing the observed composition of the genetic gain scaled in genetic standard deviations. If sum(d_G_obs) != 1, it will be rescaled.
#' @param verbose Shall information be printed?
#'
#' @details The framework allows to have less traits in the selection index than in the breeding goal (m < n). Calculation of realized economic weights, however, requires m == n.
#'
#' @return A list of class SelInd
#' @export
#'
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
#' SelInd(
#'   w = w,
#'   G = G,
#'   r2 = r2
#' )
#'
SelInd <- function(
    w,
    G,
    r2,
    H = NULL,
    i = NULL,
    h2 = NULL,
    d_G_obs = NULL,
    verbose = TRUE
){
  # initialize central object --------------------------------------------------
  out <- list(
    w = NULL,
    G = NULL,
    E = NULL,
    H = H,
    r2 = r2,
    h2 = h2,
    b = NULL,
    b_scaled = NULL,
    w_real = NULL,
    b_real = NULL,
    var_I = NULL,
    i = i,
    d_G_obs = d_G_obs,
    d_G_exp = NULL,
    d_P_exp = NULL,
    d_G_exp_scaled = NULL,
    d_P_exp_scaled = NULL,
    dG = NULL,
    r_IP = NULL,
    r_IH = NULL,
    del_d = NULL,
    del_d_scaled = NULL,
    D = NULL # not printed
  )
  class(out) <- "SelInd"

  # check input ----------------------------------------------------------------
  ## w -------------------------------------------------------------------------
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

  ## G -------------------------------------------------------------------------
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

  ## r2 and H -------------------------------------------------------------------
  if(is.null(out$r2)){
    stop("No reliabilities supplied")
  }else{
    if(any(!names(out$r2) %in% colnames(out$G))){
      stop("r2 containes traits not in G")
    }
    # set up D
    out$D <- matrix(0,length(out$r2),ncol(out$G),dimnames = list(names(out$r2),colnames(out$G)))
    for(i in 1:length(r2)){
      out$D[names(out$r2)[i],names(out$r2)[i]] <- 1
    }
    # set up R
    R <- diag(out$r2)
    dimnames(R) <- list(names(out$r2), names(out$r2))
    #derive E from r2 and G
    if(is.null(H)){
      g <- diag(out$D %*% out$G %*% t(out$D))
      out$E <- diag((1-r2)/r2*g)
      dimnames(out$E) <- list(names(r2),names(r2))
      if(verbose) message("- only reliabilities given\n  --> setting up E based on r2 and G\n  --> residual errors are assumed to be uncorrelated!")
    }else{
      # include check of H
      if(dim(H)[1] != dim(H)[2] || any(H[upper.tri(H)] != t(H)[upper.tri(H)])){
        stop("H is not squared")
      }else if(any(colnames(H) != rownames(H))){
        stop("row- and colnames of H do not match")
      }else if(dim(H)[1] < length(r2)){
        stop("more traits in r2 than in H")
      }else if(any(!names(r2) %in% colnames(H))){
        stop("Some trait names of r2 are not in H")
      }
      if(verbose) message("- H is given\n  --> setting up E based on r2, G and H\n  --> residual errors are assumed to be correlated!")
      out$H <- H[names(out$r2),names(out$r2)]
      out$E <- solve(sqrt(R)) %*% H %*% solve(sqrt(R))
      out$E <- out$E - out$D %*% out$G %*% t(out$D)
    }
  }


  ## check h2 ------------------------------------------------------------------
  if(!is.null(out$h2)){
    if(length(out$h2) != length(out$w)){
      stop("length of h2 does not equal length of w")
    }else if(any(!names(out$h2) %in% names(out$w))){
      stop("h2 containes traits not in w")
    }else{
      out$h2 <- out$h2[names(out$w)]
    }
  }

  ## check d_G_obs --------------------------------------------------------------
  if(!is.null(out$d_G_obs)){
    if(length(out$d_G_obs) != length(out$r2)){
      stop("length of d_G_obs does not equal length of r2")
    }else if(any(!names(out$d_G_obs) %in% names(out$r2))){
      stop("d_G_obs containes traits not in r2")
    }else{
      out$d_G_obs <- out$d_G_obs[names(out$r2)]
    }
    if(sum(out$d_G_obs) != 1){
      if(verbose) message("- sum(d_G_obs) != 1 -- rescaling d_G_obs")
      out$d_G_obs <- out$d_G_obs/sum(out$d_G_obs)
    }
  }

  # calc index weights ---------------------------------------------------------
  out$b <- solve(R %*% (out$D %*% out$G %*% t(out$D) + out$E) %*% R) %*% R %*% out$D %*% out$G %*% out$w
  out$b_scaled <- out$b / sum(abs(out$b))

  # calc variance of index -----------------------------------------------------
  out$var_I <- t(out$b) %*% R %*% (out$D %*% out$G %*% t(out$D) + out$E) %*% R %*% out$b

  # calc expected composition of genetic and phenotypic trend ---------------------------------
  if(!is.null(out$i)){
    out$d_G_exp <- (out$i / sqrt(out$var_I)[1,1] ) * (out$G %*% t(out$D) %*% R %*% out$b)
    out$dG <- t(out$d_G_exp) %*% out$w
    if(!is.null(out$h2)){
      out$d_P_exp <- out$d_G_exp * sqrt(out$h2) / sqrt(diag(G))
    }else{
      if(verbose) message("- no heritabilities provided\n  --> cannot compute the expected phenotypic trend")
    }
  }else{
    if(verbose) message("- no selection intensity provided\n  --> can only compute the relative genetic and phenotypic trend")
  }
  # relative trend -------------------------------------------------------------
  out$d_G_exp_scaled <- (out$G %*% t(out$D) %*% R %*% out$b)
  out$d_G_exp_scaled <- out$d_G_exp_scaled/sum(abs(out$d_G_exp_scaled))
  if(!is.null(out$h2)){
    # if(!is.null(out$d_G_exp)){
    #   out$d_P_exp <- out$d_G_exp * sqrt(out$h2) / sqrt(diag(G))
    # }
    out$d_P_exp_scaled <- out$d_G_exp_scaled * sqrt(out$h2) / sqrt(diag(G))
    out$d_P_exp_scaled <- out$d_P_exp_scaled/sum(abs(out$d_P_exp_scaled))
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
    if(verbose) message("- first derivative of d_G_exp by w can only be calculated if selescion intensity (i) is supplied\n  --> calculating only scaled version")
  }
  # old del_d_scaled
  out$del_d_scaled <- out$G %*% t(out$D) %*% solve(out$D %*% out$G %*% t(out$D) + out$E) %*% out$D %*% out$G
  for(i in 1:nrow(out$del_d_scaled)){
    srow <- sum(abs(out$del_d_scaled[i,]))
    out$del_d_scaled[i,] <- out$del_d_scaled[i,] / srow
  }
  ##############################################################################
  # new scaled
  delta <- 0.001
  out$del_d_scaled_new <- matrix(0,length(out$w),length(out$w),dimnames = list(names(out$w),names(out$w)))
  out$del_d_scaled_new_diff <- out$del_d_scaled_new_rel <- out$del_d_scaled_new1 <- out$del_d_scaled_new2 <- out$del_d_scaled_new
  d0 <- t(out$G %*% t(out$D) %*% R %*% out$b) / (sqrt(out$var_I)[1,1])

  for(i in 1:length(out$w)){
    # set up modified w
    wmod <- out$w*(1-delta/(1-out$w[i]))
    wmod[i]<-out$w[i]+delta
    
    # get modified b
    bmod <- solve(R %*% (out$D %*% out$G %*% t(out$D) + out$E) %*% R) %*% R %*% out$D %*% out$G %*% wmod
    var_mod <- t(bmod) %*% R %*% (out$D %*% out$G %*% t(out$D) + out$E) %*% R %*% bmod
    #(out$i / sqrt(out$var_I)[1,1] ) * (out$G %*% t(out$D) %*% R %*% out$b)
    
    out$del_d_scaled_new[i,] <- (out$G %*% t(out$D) %*% R %*% bmod) / sqrt(var_mod)[1,1] # no scaling by i/sd 
    out$del_d_scaled_new[i,] <- out$del_d_scaled_new[i,] #/sum(abs(out$del_d_scaled_new[i,]))
    out$del_d_scaled_new_diff[i,] <- out$del_d_scaled_new[i,] - d0 # would be gain different to previous
    out$del_d_scaled_new_rel[i,] <- out$del_d_scaled_new[i,] / d0 - 1 # would be gain relative to previous
    
    out$del_d_scaled_new1[i,] <- out$del_d_scaled_new_diff[i,]/sum(abs(out$del_d_scaled_new_diff[i,])) # would be scaled difference
    out$del_d_scaled_new2[i,] <- out$del_d_scaled_new_rel[i,]/sum(abs(out$del_d_scaled_new_rel[i,])) # would be scaled relative change 
  }
  
  ##############################################################################
  ##############################################################################
  # realized weights -----------------------------------------------------

  if(!is.null(out$d_G_obs)){
    if(any(dim(out$G) != dim(out$E))){
      if(verbose) message("- m != n\n  --> subsetting w  and G to length m for calculation of realized weights\n  !!! this discards a part of the variance-covariance structure !!! \n")
      tmp <- colnames(R)
      out$b_real <-  solve(out$G[tmp,tmp] %*% t(out$D[,tmp]) %*% R) %*% out$d_G_obs
      out$w_real <- solve(out$D[,tmp] %*% out$G[tmp,tmp]) %*% (out$D[,tmp] %*% out$G[tmp,tmp] %*% t(out$D[,tmp]) + out$E) %*% solve(out$G[tmp,tmp] %*% t(out$D[,tmp])) %*% out$d_G_obs
    }else{
      out$b_real <-  solve(out$G %*% t(out$D) %*% R) %*% out$d_G_obs
      out$w_real <- solve(out$D %*% out$G) %*% (out$D %*% out$G %*% t(out$D) + out$E) %*% solve(out$G %*% t(out$D)) %*% out$d_G_obs
    }
    out$b_real <- out$b_real/sum(abs(out$b_real))
    out$w_real <- out$w_real/sum(abs(out$w_real))

  }else{
    if(verbose) message("- No observed composition of genetic progress given\n  --> cannot calculate realized weights")
  }

  # return output --------------------------------------------------------------
  out$b <- out$b[,1]
  out$var_I <- out$var_I[1,1]
  if(!is.null(out$b_scaled)) out$b_scaled <- out$b_scaled[,1]
  if(!is.null(out$d_G_exp)) out$d_G_exp <- out$d_G_exp[,1]
  if(!is.null(out$d_P_exp)) out$d_P_exp <- out$d_P_exp[,1]
  if(!is.null(out$d_G_exp_scaled)) out$d_G_exp_scaled <- out$d_G_exp_scaled[,1]
  if(!is.null(out$d_P_exp_scaled)) out$d_P_exp_scaled <- out$d_P_exp_scaled[,1]
  if(!is.null(out$dG)) out$dG <- out$dG[1,1]
  #if(!is.null(out$del_d)) out$del_d <- out$del_d[,1]
  if(!is.null(out$b_real)) out$b_real <- out$b_real[,1]
  if(!is.null(out$w_real)) out$w_real <- out$w_real[,1]
  if(!is.null(out$r_IP)) out$r_IP <- out$r_IP[1,]
  if(!is.null(out$r_IH)) out$r_IH <- out$r_IH[1,]

  return(out)
}


