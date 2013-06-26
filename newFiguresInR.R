library(MASS)
library(lattice)
library(RColorBrewer)

barchart(Claims/Holders ~ Age | Group, groups = District, 
         data = Insurance, origin = 0, auto.key = TRUE)

barchart(Claims/Holders ~ Age | Group, groups = District,
         data = Insurance, main = "Claims frequency", 
         auto.key = list(space = "top", columns = 4, 
                         title = "District", cex.title = 1))

xyplot(Claims/Holders ~ Age | Group, groups = District,
       data = Insurance, t = "l", main = "Claims frequency",
       auto.key = list(space = "top", columns = 4, 
                       title = "District", cex.title = 1,
                       lines = TRUE, points = FALSE))

myColours <- brewer.pal(6, "Blues")

my.settings <- list(superpose.polygon = list(col = myColours[2:5], border = "transparent"),
                    strip.background = list(col = myColours[6]),
                    strip.border = list(col = "black"))


barchart(Claims/Holders*100 ~ Age | Group, groups = District,
         data = Insurance, origin = 0, main = "Motor Insurance claims frequency",
         xlab = "Age", ylab = "Claims frequency %",
         scales = list(alternating = 1),
         auto.key = list(space = "top", columns = 4,
                         points = FALSE, rectangles = TRUE, title = "District", cex.title = 1),
         par.settings = my.settings, par.strip.text = list(col = "white", font = 2),
         panel = function(x,y,...){
           panel.grid(h = -1, v = 0);
           panel.barchart(x,y,...)
         })


######

data <- read.delim("states-data.txt")

data$total.unified <- data$Unified.D + data$Unified.R

plot(data$Year,data$total.unified,type = 'l',ylim = c(0,50))

plot(data$Year,data$total.unified,type='l',ylim=c(0,50),xlab="Year",
     ylab="States",
     main="States with unified control of state government since 1938",
     col="red",lwd=3)

abline(h=c(0,10,20,30,40,50),col='lightgrey')

abline(v=c(1940,1960,1980,2000),col='lightgrey')

plot(data$Year,data$Divided,type='l',ylim=c(0,30))

lines(data$Year,data$Unified.R,col="red")

lines(data$Year,data$Unified.D,col="blue")


#just the numbers we want to plot
data.we.need<-data[,c("Unified.D","Divided","Unified.R")] 
#a simple reshaping, transposing our data
transposed<-t(data.we.need) 

barplot(transposed,ylim=c(0,50),col=c('blue','grey','red'),border=F)

abline(h=c(1:50),col='white')


pdf(file="stacked-bars.pdf",width=8,height=5)

barplot(transposed,ylim=c(0,50),col=c('blue','grey','red'))

abline(h=c(10,20,30,40),col='white')

dev.off()
