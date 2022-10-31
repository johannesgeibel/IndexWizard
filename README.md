
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IndexWizard

<!-- badges: start -->

[![](https://img.shields.io/badge/doi-https://doi.org/10.5281/zenodo.6977265-blue.svg)](https://doi.org/https://doi.org/10.5281/zenodo.6977265)
[![](https://img.shields.io/badge/devel%20version-0.1.3.2-blue.svg)](https://github.com/johannesgeibel/IndexWizard)
[![](https://www.r-pkg.org/badges/version/IndexWizard?color=orange)](https://cran.r-project.org/package=IndexWizard)

[![CRAN
checks](https://cranchecks.info/badges/summary/IndexWizard)](https://cran.r-project.org/web/checks/check_results_IndexWizard.html)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)

<!-- badges: end -->

The goal of ‘IndexWizard’ is to provide a framework for the exploration
of effects of complex selection indices. It allows to construct
selection indices based on estimated breeding values in animal and plant
breeding and to calculate several analytic measures around. The
methodology thereby allows to analyze genetic gain of traits in the
breeding goal which are not part of the actual index and automatically
computes several analytic measures. It further allows to retrospectively
derive realized economic weights from observed genetic trends. See
Simianer *et al.* (submitted to GSE) for a detailed description of the
methodology.

## Installation

You can install the development version of `IndexWizard` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("johannesgeibel/IndexWizard", build_vignettes = TRUE)
```

Note that `devtools` requires
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) to be installed
on Windows systems.

It is planned to submit the package to CRAN soon.

## Example

All calculations are solely performed in the function `SelInd()`. Its
minimal input requirements are a named genetic covariance matrix (`G`)
for all n breeding goal traits, a named vector of economic weights (`w`)
for the n breeding goal traits and a named vector of reliabilities
(`r2`) of the estimated breeding values for the $m \le n$ index traits.

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
for Simianer *et al.* (submitted to GSE) in this repository.
