# cluster schools based on modes of transport
library(dplyr)
library(ggplot2)

dat <- read.csv("Data/2014_Absolute_Cleaned.csv")
names(dat) <- sub("...", "_", names(dat), fixed = T)

# contemporary
dat <- dat %>% filter(Year == 2014)

# kmeans cannot handle NA's
lose <- apply(dat, 1, function(x) any(is.na(x)))
dat <- dat[-which(lose==T),]

# need to mean standardise and scale
datVals <- dat[,5:12] 
datValsScaled <- scale(datVals)
datScaled <- cbind(dat[,1:4], datValsScaled)

# logLik for kmeans
logLik.kmeans <- function(object) structure(
  object$tot.withinss,
  df = nrow(object$centers)*ncol(object$centers),
  nobs = length(object$cluster)
)

# perform clustering
kMax <- 10
bic <- rep(NA, kMax)
for(i in 1:kMax){
  clust <- kmeans(datValsScaled, i, iter.max = 1e4, nstart = 20)
  bic[i] <- BIC(clust)
  aic[i] <- AIC(clust)
}
plot(bic)
