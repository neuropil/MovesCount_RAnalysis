hsb2.small <- read.csv("http://www.ats.ucla.edu/stat/data/hsb2_small.csv")

names(hsb2.small)

hsb3 <- hsb2.small[, c(1, 7, 8)]

hsb5 <- hsb2.small[1:10, ]

hsb6 <- hsb2.small[hsb2.small$ses == 1, ]

hsb7 <- hsb2.small[hsb2.small$id %in% c(12, 48, 86, 11, 20, 195),]

hsb8 <- hsb2.small[hsb2.small$read %in% c(44,34,57),]

hsb9 <- hsb2.small[with(hsb2.small, ses == 3 & female == 0), ]

write.1 <- subset(hsb2.small, write > 50 & read > 60)

write.2 <- subset(hsb2.small, write > 50 & read > 60, select = c(write, read))

write.3 <- subset(hsb2.small, science < 55, select = read:science)

hsb10 <- hsb2.small[hsb2.small$ses == 3, c(1:4, 7)]