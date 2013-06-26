

library(gcookbook)
library(ggplot2)

cabbage_exp

ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(position = "dodge")

ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(position = "dodge", colour = "black") +
  scale_fill_brewer(palette = "Pastel1")


# rank function generates indicies in ascending order
# get 10 fast-growing: look for rank values higher than 40
upc <- subset(uspopchange, rank(Change)>40)

ggplot(upc, aes(x = Abb, y = Change, fill = Region)) +
  geom_bar(stat = "identity")

# reorder function (x, y)  reorders Change in ascending order and
# obtains indices of Abb 
ggplot(upc, aes(x = reorder(Abb, Change), y = Change, fill = Region)) +
  geom_bar(stat = "identity", colour = "black") + 
  scale_fill_manual(values = c("#669933", "#FFCC66")) +
  xlab("State")


csub <- subset(climate, Source == "Berkeley" & Year >= 1900)
csub$pos <- csub$Anomaly10y >= 0

ggplot(csub, aes(x = Year, y = Anomaly10y, fill = pos)) +
  geom_bar(stat = "identity", position = "identity")