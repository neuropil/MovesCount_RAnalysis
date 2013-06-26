getwd()
setwd("G:/R Workspaces")

library(arules)
data("AdultUCI")
dframe = AdultUCI[, c("education", "hours-per-week")]
colnames(dframe) = c("education", "hours_per_week")
         # get rid of the annoying minus signs in the column names
         # We want to compare the distribution of work-week length to education, using a box-and-whisker plot that is overlaid on a jittered scatterplot of the data.


library(ggplot2)
ggplot(dframe, aes(x=education, y=hours_per_week)) +
          geom_point(colour="lightblue", alpha=0.1, position="jitter") +
          geom_boxplot(outlier.size=0, alpha=0.2) + coord_flip()


#######2nd graph
path = "http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data"
saheart = read.table(path, sep=",",head=T,row.names=1)
fmla = "chd ~ sbp + tobacco + ldl + adiposity + famhist + typea + obesity"
model = glm(fmla, data=saheart, family=binomial(link="logit"),
             na.action=na.exclude)


dframe = data.frame(chd=as.factor(saheart$chd),
                    prediction=predict(model, type="response"))
ggplot(dframe, aes(x=prediction, colour=chd)) + geom_density()

ggplot(dframe, aes(x=prediction, fill=chd)) +
               geom_histogram(position="identity", binwidth=0.05, alpha=0.5)

ggplot(dframe, aes(x=chd, y=prediction)) +
               geom_point(position="jitter", alpha=0.2) +
               geom_boxplot(outlier.size=0, alpha=0.5)


#####3rd Graph
load 
fmla = "lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45"
model = lm(fmla, data=prostate.data)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model)

dframe = data.frame(lpsa=prostate.data$lpsa, prediction=predict(model))

title = sprintf("Prostate Cancer model\n R-squared = %1.3f",
                summary(model)$r.squared)
ggplot(dframe, aes(x=lpsa, y=prediction)) +
               geom_point(alpha=0.2) +
               geom_line(aes(y=lpsa), colour="blue") +
               opts(title=title)