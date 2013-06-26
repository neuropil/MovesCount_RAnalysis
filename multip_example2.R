library(plyr) 
library(ggplot2)


preemlong = read.csv('preemlong.csv')

ggplot(preemlong, aes(event, SCORE)) + geom_line(aes(group = id)) + geom_smooth(aes(group = hlv, colour = hlv), se = F, size = 2)



avg.w <- read.csv("avg.country.csv") 


avg.c <- ddply(avg.w, "country", function(DF) { # for each country 
  # get the start index 
  start <- c(1, which(diff(as.numeric(DF$democracy)) !=0)) 
  # get the end index 
  l <- rle(as.character(DF$democracy)) 
  end <- cumsum(l$lengths) 
  # return a data.frame of: 
  data.frame(democracy = l$values, #democracy 
             start=DF[start, "birth_year"], #start year 
             end=DF[end, "birth_year"] #end year 
  ) 
}) 



## plot 
p1 <- ggplot(data=avg.w)+ 
  geom_rect(aes(xmin=start, 
                xmax=end, 
                ymin=-Inf, 
                ymax=Inf, 
                fill=democracy, alpha=.4), 
            data=avg.c) + 
  geom_point(aes(x=birth_year, 
                 y=mean, 
                 group=wealth2, 
                 color=wealth2)) + 
  geom_smooth(aes(x=birth_year, 
                  y=mean, 
                  group=wealth2, 
                  color=wealth2)) + 
  facet_wrap(~country) 

p1