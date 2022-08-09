
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IndexWizard

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6977265.svg)](https://doi.org/10.5281/zenodo.6977265)

<!-- badges: end -->

The goal of IndexWizard is to provide a framework for the exploration of
effects of complex selection indices. It allows to construct selection
indices based on estimated breeding values in animal and plant breeding
and to calculate several analytical measures around. The methodology
thereby allows to analyze genetic gain of traits in the breeding goal
which are not part of the actual index and automatically computes
several analytical measures. It further allows to retrospecively derive
realized economic weights from observed genetic trends. See Simianer *et
al.* (in preparation) for a detailed description of the methodology.

## Installation

You can install the development version of `IndexWizard` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("johannesgeibel/IndexWizard")
```

Note that `devtools` requires
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) to be installed
on Windows systems.

It is planned to submit the package to cran soon.

## Example

All calculations are solely performed in the function `SelInd()`. Its
minimal input requirements are a named genetic covariance matrix (`G`)
for all n breeding goal traits, a named vector of economic weights (`w`)
for the n breeding goal traits and a named vector of reliabilities
(`r2`) of the estimated breeding values for the
![m \le n](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;m%20%5Cle%20n "m \le n")
index traits.

``` r
library(IndexWizard)
tn <- c("RZM", "RZN", "RZEo") # trait names of breeding goal traits
G <- matrix(
    c(1.0,0.13,0.13,
    0.13,1.0,0.23,
    0.13,0.23,1.0),
    3, 3, dimnames = list(tn,tn)
    ) * 144 # genetic covariance matrix
w <- c(0.7, 0.3, 0) # economic weights, giving weight only to the first two traits
names(w) <- tn
r2 <- c(0.743, 0.673) # reliabilities for the Index traits
names(r2) <- tn[1:2]
SelInd(
  w = w,
  G = G,
  r2 = r2
)
```

Read the vignette `CaseStudy` for a detailed introduction, covering
different use cases of the framework.

``` r
vignette("CaseStudy", package = "IndexWizard")
```

Further find the analysis
[script](https://github.com/johannesgeibel/IndexWizard/blob/main/scripts/makePlots.r)
for Simianer *et al.* (to be submitted) in this repository.
