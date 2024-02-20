# mlrmini

**mlrmini**
A mini-package for Machine Learning in R.

Package website: [mlrmini](https://advaprogr-2324.github.io/mlrmini/)


<!-- badges: start -->
[![R-CMD-check](https://github.com/AdvaProgR-2324/mlrmini/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AdvaProgR-2324/mlrmini/actions/workflows/R-CMD-check.yaml) [![test-coverage](https://github.com/AdvaProgR-2324/mlrmini/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/AdvaProgR-2324/mlrmini/actions/workflows/test-coverage.yaml) [![lint](https://github.com/AdvaProgR-2324/mlrmini/actions/workflows/lint.yaml/badge.svg)](https://github.com/AdvaProgR-2324/mlrmini/actions/workflows/lint.yaml)
<!-- badges: end -->

## Short description

mlrmini is a package developed to provide a simpler and consistent way for using machine learning algorithms in R.
It allows therefore simplified evaluation and optimization of algorithms and is oriented on the mlr package. Currently it supports learners like xgboost, linear models and rpart


## Installation

Install the last release from CRAN:

``` r
install.packages("mlrmini")
```

Install the development version from GitHub:

``` r
remotes::install_github("AdvaProgR-2324/mlrmini")
```

## What ChatGPT says this package is made for

In the realm of data, where patterns hide,  
A tool emerges, mlrmini, with knowledge as its guide.

With features rich and functions grand,  
A powerhouse it is, across the data land.

Ensemble models, they dance in sync,  
A symphony of predictions, no room for a jinx.

Preprocessing magic, transforming the raw,  
Into insights profound, without a flaw.

Cross-validation, a trustworthy guide,  
Ensuring robustness, standing by your side.

In the heart of machine learning's domain,  
mlrmini reigns, its excellence to sustain.

And behold, mlrmini, a companion so true,  
Mini but mighty, enhancing what's due.

So into the world of data, let's venture and spin,  
With mlrmini, let the learning begin!

## Example

### defnining an Inducer
``` r
library(mlrmini)
inducerxgb <- InducerXGBoost(nrounds = 4)

```

### fitting a model
``` r
cars.data <- Dataset(data = cars, target = "dist")
fitxgb <- fit(inducerxgb, .data = cars.data)
```
### reviewing the results

``` r
modelObject(fitxgb)
fitxgb$modelInfo
```






