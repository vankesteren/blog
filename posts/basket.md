# Basket
Erik-Jan van Kesteren  



###[Back to index](../index.html)



## Introduction
transmat etc, basketball player.


```r
(b <- c("miss","miss","miss","hit","miss","hit","miss","miss","hit","hit","miss","miss","miss","miss","miss","miss","miss","hit","hit","hit","miss","hit"))
```

```
##  [1] "miss" "miss" "miss" "hit"  "miss" "hit"  "miss" "miss" "hit"  "hit" 
## [11] "miss" "miss" "miss" "miss" "miss" "miss" "miss" "hit"  "hit"  "hit" 
## [21] "miss" "hit"
```
so that's the chain.


```r
trans.matrix <- function(x, prob = T) {
    X <- t(as.matrix(x))
    tt <- table(c(X[, -ncol(X)]), c(X[, -1]))
    if (prob) 
        tt <- tt/rowSums(tt)
    tt
}

(m <- trans.matrix(b))
```

```
##       
##              hit      miss
##   hit  0.4285714 0.5714286
##   miss 0.3571429 0.6428571
```

And we can nicely visualise it using this markov chain generator post by setosa. A equals a hit, B equals a miss. (Go ahead, play around, it's completely interactive!)

<iframe src="http://setosa.io/markov/index.html#%7B%22tm%22%3A%5B%5B0.6428571%2C0.3571429%5D%2C%5B0.5714286%2C0.4285714%5D%5D%7D" width="100%" height="350px" style="border:none"></iframe>


###[Back to index](../index.html)
