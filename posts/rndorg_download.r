# random.org package tryout
setwd("~/3-Personal/blog/posts")
rm(list=ls())
library(random)
library(ggplot2)
?random

load("rndorg.Rdata")

# pull down the whole quotum
while (quotaCheck() == TRUE){
  rndorg <- rbind(rndorg,randomNumbers(n=10000, min = 0, max = 9))
}

save(rndorg, file = "rndorg.Rdata")

rndorgvec <- as.vector(rndorg)
rndorgraw <- as.raw(rndorg)
rndorggzip <- memCompress(rndorgraw, type = "gzip")
rndorgbzip <- memCompress(rndorgraw, type = "bzip2")
rndorgxz <- memCompress(rndorgraw, type = "xz")

runifvec <- ceiling(runif(160000, min = -1, max = 9))
runifraw <- as.raw(runifvec)
runifgzip <- memCompress(runifraw, type = "gzip")
runifbzip <- memCompress(runifraw, type = "bzip2")
runifxz <- memCompress(runifraw, type = "xz")


df <- data.frame(name=rep(c("Random.org", "stats::runif"), each=3),
                 value=c(length(rndorggzip),
                         length(rndorgbzip),
                         length(rndorgxz),
                         length(runifgzip),
                         length(runifbzip),
                         length(runifxz)),
                 type=rep(c("gzip","bzip","xz"),2))

p <- ggplot(data=df, mapping=aes(x=type, y=value, fill=name)) + geom_bar(stat = "identity", position = "dodge")
