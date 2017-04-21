library(raster)
library(dplyr)

xy <- read.csv("liana_xy.csv")

test <- getData('worldclim', var='bio', res=0.5, lon=xy[1,3], lat=xy[1,2])
test2 <- getData('worldclim', var='bio', res=0.5, lon=xy[2,3], lat=xy[2,2])


dat <- extract(test, xy[,c(3,2)])
dat2 <- extract(test2, xy[,c(3,2)])

colnames(dat) = c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")
colnames(dat2) = c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")

dat <- as.data.frame(dat)
dat2 <- as.data.frame(dat2)

dat <- cbind(xy, dat)
dat2 <- cbind(xy, dat2)

liana_bioclim <- left_join(dat, dat2, by = c("population", "latitude", "longitude", "spp"))

for(i in 5:23){
  liana_bioclim[,i] <- ifelse(is.na(liana_bioclim[,i]), liana_bioclim[,(i+19)], liana_bioclim[,i])
}

liana_bioclim <- liana_bioclim %>% 
  select(-(24:42))

colnames(liana_bioclim) <- c("population", "latitude", "longitude", "spp", "bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")

write.csv(liana_bioclim, "liana_bioclim.csv")

plot(bio11 ~ latitude, data = liana_bioclim)
pairs(liana_bioclim[,c(2,5:23)])
