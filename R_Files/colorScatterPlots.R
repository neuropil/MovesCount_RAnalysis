#Load ggplot default data: Diamonds
library(ggplot2)
library(gridExtra)
data(diamonds)
head(diamonds)
diamonds <-     diamonds[diamonds$color < "J",]
#http://127.0.0.1:25615/library/ggplot2/html/diamonds.html

#Gradient Colors - Good For Continuous Scales

G5 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=price)
) + facet_wrap(~color)   + labs(title = "Base Plot \n" )

G6 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=price)
) + facet_wrap(~color) + scale_colour_gradientn(colours=rainbow(2))   + labs(title = " + scale_colour_gradientn(colours=rainbow(2)) \n" )

G7 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=price)
) + facet_wrap(~color) + scale_colour_gradientn(colours=c("red", "blue"))   + labs(title = " + scale_colour_gradientn(colours=c(''red'', ''blue'')) \n" )

G8 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=price)
) + facet_wrap(~color) + scale_colour_gradientn(colours=c("white", "dodgerblue")) + labs(title = " + scale_colour_gradientn(colours=c(''white'', ''dodgerblue'')) \n" )

Gradient2 <- grid.arrange(G5, G6, G7, G8, ncol=2)


#Load ggplot default data: Diamonds
library(ggplot2)
library(gridExtra)
data(diamonds)
head(diamonds)
diamonds <-     diamonds[diamonds$color < "J",]
#http://127.0.0.1:25615/library/ggplot2/html/diamonds.html


B1 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="Set1")   + labs(title = "Palette=''Set1''\n" )


B2 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="Set2") + labs(title = "Palette=''Set2''\n" )


B3 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="Paired")   + labs(title = "Palette=''Paired''\n" )


B4 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="Pastel2") + labs(title = "Palette=''Pastel2''\n" )


Brewers <- grid.arrange(B1, B2, B3, B4, ncol=2)


B5 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="Blues")   + labs(title = "Palette=''Blues''\n" )


B6 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="Oranges") + labs(title = "Palette=''Oranges''\n" )


B7 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="Reds")   + labs(title = "Palette=''Reds''\n" )


B8 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="Purples") + labs(title = "Palette=''Purples''\n" )



Brewers.2 <- grid.arrange(B5, B6, B7, B8, ncol=2)



B9 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="RdYlGn")   + labs(title = "Palette=''RdYlGn''\n" )


B10 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="RdBu") + labs(title = "Palette=''RdBu''\n" )


B11 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="PRGn")   + labs(title = "Palette=''PRGn''\n" )


B12 <- ggplot(data = diamonds ) + geom_point(aes( x = carat, y = price, color=color)
) + facet_wrap(~color) + scale_colour_brewer(palette="BrBg") + labs(title = "Palette=''BrBg''\n" )



Brewers.3 <- grid.arrange(B9, B10, B11, B12, ncol=2)