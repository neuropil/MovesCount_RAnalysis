dat = data.frame(x = runif(1000), y = runif(1000), cat = rep(c("A","B"), each = 500))
ggplot(aes(x = x, y = y, color = cat), data = dat) + geom_point(alpha = 0.3)


library(automap)
library(ggplot2)
library(plyr)
library(sp)
loadMeuse()
theme_set(theme_bw())

meuse = as.data.frame(meuse)
chull_per_soil = ddply(meuse, .(soil), 
                       function(sub) sub[chull(sub$x, sub$y),c("x","y")])

ggplot(aes(x = x, y = y), data = meuse) +
  geom_point(aes(size = log(zinc), color = ffreq)) +
  geom_polygon(aes(color = soil), data = chull_per_soil, fill = NA) +
  coord_equal()