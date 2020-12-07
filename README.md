
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

![R-CMD-check](https://github.com/audrey-b/BUGSnet/workflows/R-CMD-check/badge.svg)

![test-coverage](https://github.com/Harmohit-Singh/BUGSnet/workflows/test-coverage/badge.svg)

<!-- badges: end -->

# BUGSnet

BUGSnet (Bayesian inference Using Gibbs Sampling to conduct NETwork
meta-analysis) is a feature-rich R package to conduct Bayesian network
meta-analyses in compliance with best practice and reporting guidelines.
Bayesian analyses are conducted with JAGS, and BUGS code is
automatically generated by the package based on the user’s inputs.
Outputs are highly customizable and include network plots, tables of
network characteristics, league tables and league heat plots, SUCRA
plots, rankograms, forest plots, leverage plots, traceplots, posterior
mean deviance comparison plots.

## Installation instructions

### 1\. Install JAGS

Go to the [Sourceforge](https://sourceforge.net/projects/mcmc-jags/)
page.

Click on Files \> JAGS \> 4.x

Select your operating system and download JAGS-4.3.0, then install.

### 2\. Install R

Download and install the most recent version of base R from
[CRAN](https://cran.r-project.org/).

### 3\. Install Rstudio

Download and install the most recent version of RStudio Desktop (free
version) from
[RStudio](https://www.rstudio.com/products/rstudio/download).

### 4\. Install BUGSnet

In the RStudio console, type

``` r
install.packages("remotes")
remotes::install_github("audrey-b/BUGSnet")
```

## Start using BUGSnet

To start using BUGSnet, follow the vignettes available
[here](https://bugsnetsoftware.github.io/).

## License

BUGSnet is available under the Creative Commons
Attribution-NonCommercial-ShareAlike 4.0 International License (CC
BY-NC-SA 4.0). Kindly review the LICENSE file. You are responsible for
conforming to the terms of this license. BUGSnet is provided as-is and
comes with absolutely no warranties. Commercial use is prohibited;
please contact the authors for more information.

## How to cite BUGSnet?

Béliveau A., Boyne D., Slater J., Brenner D. and Arora P. (2019).
BUGSnet: an R package to facilitate the conduct and reporting of
Bayesian network Meta-analyses. *BMC Medical Research Methodology*,
19(196).

## Contribution

Please report any [issues](https://github.com/audrey-b/BUGSnet/issues).
