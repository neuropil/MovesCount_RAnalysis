library(RCurl)
library(ggplot2)
library(scales)
library(reshape2)
library(plyr)
library(ggthemes)

data_url <- "https://raw.github.com/ropensci/sandbox/master/inst/demo/githubcommitsdata.csv"
outdf <- read.csv(text=getURL(data_url),sep=",")
outdf$date <- as.Date(as.character(outdf$date))
outdf_subset <- outdf[!outdf$.id %in% c("citeulike", "challenge", "docs", "ropensci-book",
                                        "usecases", "textmine", "usgs", "ropenscitoolkit", "neotoma", "rEWDB", "rgauges",
                                        "rodash", "ropensci.github.com", "ROAuth"), ]
outdf_subset$.id <- tolower(outdf_subset$.id)
outdf_subset <- ddply(outdf_subset, .(.id, date), summarise, value = sum(value))

mindates <- llply(unique(outdf_subset$.id), function(x) {
  min(outdf_subset[outdf_subset$.id == x, "date"])
})
names(mindates) <- unique(outdf_subset$.id)
mindates <- sort(do.call(c, mindates))
outdf_subset$.id <- factor(outdf_subset$.id, levels = names(mindates))

print( ggplot(outdf_subset, aes(date, value, fill = .id)) +
         geom_bar(stat = "identity", width = 0.5) +
         geom_rangeframe(sides = "b", colour = "grey") +
         theme_bw(base_size = 9) +
         scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) +
         scale_y_log10() +
         facet_grid(.id ~ .) +
         labs(x = "", y = "") +
         theme(axis.text.y = element_blank(),
               axis.text.x = element_text(colour = "black"),
               axis.ticks.y = element_blank(),
               strip.text.y = element_text(angle = 0, size = 8, ),
               strip.background = element_rect(size = 0),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               legend.text = element_text(size = 8),
               legend.position = "none",
               panel.border = element_blank())
)