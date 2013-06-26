ggplot(data=elev) + 
  
  
  geom_histogram(aes(x=elev_carta), fill = "red", alpha = 0.2) +
  
  
  geom_histogram(aes(x=elev_aster), fill = "blue", alpha = 0.2) +
  
  
  geom_histogram(aes(x=elev_srtm), fill = "green", alpha = 0.2)


#######

d <- data.frame(kind=rep(c("a", "b", "c"), each=100), x=c(rnorm(100, 1), rnorm(100, 2), rnorm(100, 3)))
library("ggplot2")
# base histogram, the bars are stacked on top of each other
ggplot() + geom_histogram(aes(x=x, fill=kind), data=d, binwidth=0.2)
# shift the bars relative to each other (=dodging) i.e. what other software (Excel, ...) usually does
ggplot() + geom_histogram(aes(x=x, fill=kind), data=d, binwidth=0.2, position="dodge")
# superpose the bars
ggplot() + geom_histogram(aes(x=x, fill=kind), data=d, binwidth=0.2, position="identity")
# except that, if you want to see something, you have to use trasnparency
ggplot() + geom_histogram(aes(x=x, fill=kind), data=d, binwidth=0.2, position="identity", alpha=0.5, colour=scales::alpha("black", 0.2))

# but I'd say this is messy and difficult to read, in that configuration, using a density estimate provides a better, more readable graph I think
ggplot() + geom_density(aes(x=x, fill=kind), data=d, binwidth=0.2, position="identity", alpha=0.5, colour=scales::alpha("black", 0.2))