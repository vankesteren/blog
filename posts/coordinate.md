# Multiple regression via coordinate descent
Erik-Jan van Kesteren  



<style type="text/css">
  .table {
  
      margin-left: auto;
      margin-right: auto;
      display: block;
  
  }
  blockquote {
  
      font-size: 1em;
    
  }
</style>

###[Back to index](../index.html)



## Introduction
This blog is about multiple regression, where we seek to estimate the relation between variables in a data matrix $\boldsymbol{X}$ and an outcome vector $\boldsymbol{y}$:

$$\boldsymbol{y} = \boldsymbol{X}\boldsymbol{\beta}+\boldsymbol{\epsilon}$$


Intuitively, the goal is to estimate $\boldsymbol{\beta}$ such that the sum of squared error terms $\boldsymbol{\epsilon}'\boldsymbol{\epsilon}$ is minimal[^ls]. In multiple regression, the regression parameters can be very quickly estimated from data using the ordinary least squares (OLS) estimator:

[^ls]: This is the _least squares_ interpretation, which for this model coincides with likelihood-based methods.

$$\hat{\boldsymbol{\beta}} = (\boldsymbol{X}'\boldsymbol{X})^{-1}\boldsymbol{X}'\boldsymbol{y}$$

I have always found the simplicity of the formula almost magical. In `R`, the fastest implementation of the above formula I've found is the following:


```r
betahat <- solve(crossprod(X), crossprod(X, y))
```


In this blog post, I will outline and show an even more magical method to obtain the same parameter estimates: coordinate descent. The main advantage is that it does not require any matrix inversion meaning that it works even when there are more variables than cases (the $\boldsymbol{X}$ matrix has more columns than rows).

## The dataset
You can skip this section and go straight to [the next section](#coordinate-descent) if you are not interested in simulation. 

Let's simulate an example dataset with 100 observations, 11 $X$ variables with covariance matrix $S$, and a little error.

```r
library(MASS)
set.seed(142857)

n <- 100  # number of observations
p <- 11  # number of covariates

# generate covariance matrix
# (https://stats.stackexchange.com/a/215647/116878)
P <- qr.Q(qr(matrix(rnorm(p^2), p)))
S <- cov2cor(crossprod(P, P * (p:1)))

# generate X
X <- mvrnorm(n = n, mu = rep(0, p), Sigma = S)

# define a beta vector
beta <- seq(-1, 1, len = p)

# generate residuals such that R^2 = 2/3
e <- rnorm(n = n, sd = 0.5 * sqrt(beta %*% S %*% beta))

# generate y
y <- X %*% beta + e
```

In this generated dataset, the true $\boldsymbol{\beta}$ vector looks like this: 


```r
beta
```

```
##  [1] -1.0 -0.8 -0.6 -0.4 -0.2  0.0  0.2  0.4  0.6  0.8  1.0
```


## Coordinate descent

This algorithm splits the multivariate problem of estimating the vector $\boldsymbol{\beta}$ into a series of univariate problems of estimating each $\beta_j$ separately for $j \in 1 \dots p$. It does this by performing univariate regression not on the original variable but on the residuals of $\boldsymbol{y}$ with respect to the remaining variables. So one step of the algorithm looks like this:

For $j \in 1 \dots p$:

  1. Calculate the residuals $$\boldsymbol{y}_{res_j} = \boldsymbol{y} - \boldsymbol{X}_{-j}\hat{\boldsymbol{\beta}}_{-j}$$ where $\boldsymbol{X}_{-j}$ is the data matrix excluding column $j$ and $\hat{\boldsymbol{\beta}}_{-j}$ is the vector of current parameter estimates excluding $\hat{\beta}_{j}$.
  
  2. Calculate $\hat{\beta}_{j}$ using simple linear regression on these residuals: $$\hat{\beta}_{j} = (\boldsymbol{X}'_j\boldsymbol{X}_j)^{-1}\boldsymbol{X}_j'\boldsymbol{y}_{res_j} = \frac{\langle\boldsymbol{x}_j\,,\boldsymbol{y}_{res_j}\rangle}{\langle\boldsymbol{x}_j\,,\boldsymbol{x}_j\rangle}$$

Note that the matrix inversion is replaced by a scalar inversion using inner products.

In `R`, we can achieve this through a for loop:

```r
# initialise bhat as vector of 0s
bhat <- numeric(ncol(X))

for (j in 1:length(bhat)) {
  # calculate residuals w.r.t. remaining variables
  yres <- y - X[,-j] %*% bhat[-j]
  # calculate current bhat using univariate regression of y_res on x_j
  bhat[j] <- crossprod(X[,j], yres) / crossprod(X[,j])
}
```

With one step of this algorithm, we are not there yet. If we would perform another step with the first predictor, we would get a different value for $\hat{\beta}_{1}$ because now the residual will be different: $\hat{\boldsymbol{\beta}}_{-j}$ is not a vector of 0s as it was in the first step. This is why we cycle through the variables continuously until we arrive to a stable point, essentially wrapping the step in the `R` code above in another loop that checks for convergence:


```r
# init ----
# initialise bhat as vector of 0s
bhat <- numeric(ncol(X))
# control variables
convergence <- FALSE 
tol <- 1e-15 # tolerance or precision
max.iter <- 1000
i <- 0

# Coordinate descent ----
while (!convergence && i < max.iter) {
  i <- i + 1
  bprev <- bhat # remember previous beta estimate
  
  for (j in 1:length(bhat)) {
    # calculate residuals w.r.t. remaining variables
    yres <- y - X[,-j] %*% bhat[-j]
    # calculate current bhat using univariate regression of y_res on x_j
    bhat[j] <- crossprod(X[,j], yres) / crossprod(X[,j])
  }
  
  # calculate squared difference to previous beta to determine convergence
  dif <- (bprev - bhat) %*% (bprev - bhat)
  if (dif < tol) convergence <- TRUE
}
```


So let's compare the results with the OLS estimator


```r
olsbhat <- solve(crossprod(X), crossprod(X, y))
olsbhat == bhat
```

```
##        [,1]
##  [1,] FALSE
##  [2,] FALSE
##  [3,] FALSE
##  [4,] FALSE
##  [5,] FALSE
##  [6,] FALSE
##  [7,] FALSE
##  [8,] FALSE
##  [9,] FALSE
## [10,] FALSE
## [11,] FALSE
```

## Multiple regression with coordinate descent
Let's wrap it all in a function. Feel free to use it / adapt it in your work.

```r
mrc <- function(X, y, max.iter = 1000, tol = .Machine$double.eps) {
  # Multiple regression using coordinate descent
  # Input: X matrix, y vector
  # Output: estimated beta vector
  
  # init ----
  bhat <- numeric(ncol(X))
  convergence <- FALSE
  i <- 0L
  
  # Coordinate descent ----
  # calculate the sum of squares of each x
  xss <- apply(X, 2, crossprod)
  
  while (!convergence && i < max.iter) {
    i <- i + 1
    bprev <- bhat # remember previous beta estimate
    
    for (j in 1:length(bhat)) {
      # calculate residuals w.r.t. remaining variables
      yres <- y - X[,-j] %*% bhat[-j]
      # calculate current bhat using univariate regression of y_res on x_j
      bhat[j] <- t(X[,j]) %*% yres / xss[j]
    }
    
    # calculate squared difference to determine convergence
    dif <- (bprev - bhat) %*% (bprev - bhat)
    if (dif < tol) convergence <- TRUE
  }
  
  if (!convergence) warning("Algorithm did not converge!")
  
  bhat
}
```

###[Back to index](../index.html)
