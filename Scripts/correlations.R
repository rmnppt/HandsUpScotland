### Explore
library(dplyr)
library(ggplot2)
library(grid)
library(GGally)

dat <- read.csv("Data/2014_Absolute_Cleaned.csv")
names(dat) <- sub("...", "_", names(dat), fixed = T)

dat <- dat %>% filter(Year == 2014)

# need to mean standardise and scale
datVals <- dat[,5:12] 
datValsScaled <- scale(datVals)
datScaled <- cbind(dat[,1:4], datValsScaled)

# correlations
p <- ggpairs(datScaled, 5:10) +
  theme(panel.grid = element_blank())

# bit hacky here
addAlpha <- function(x){
  if(length(grep("points", x)) > 0){
    x <- sub("))", "), alpha = 0.1)", x)
  }else{
    x <- x
  }
}
p$plots <- lapply(p$plots, addAlpha)

# produce
jpeg("Plots/Figure3.jpeg", 800, 500, quality = 100)
print(p, bottom = 0.3, left = 0.3, spacing = 0.2) 
dev.off()

