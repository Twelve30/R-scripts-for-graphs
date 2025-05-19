library(ggplot2)
library(ggpmisc)
library(dplyr)
library(matrixStats)
library(ggthemes)
datad = read.csv("WST.csv")
newdata2 = datad %>% 
  select(Sample, Well1, Well2, Well3, order) %>%
  mutate(Absorb = rowMeans(datad[c("Well1", "Well2", "Well3")])) %>%
  mutate(deviation = rowSds(as.matrix(datad[c("Well1", "Well2", "Well3")]))) %>%
  mutate(error = deviation/sqrt(3)) %>%
  mutate(tAbsorb = round(Absorb, digits = 1))

p = ggplot(newdata2, aes(x = reorder(Sample, order))) + geom_errorbar(aes(y = Absorb, ymin = Absorb - error, ymax = Absorb + error), width = 0.15) + geom_col(aes(y = Absorb, color=Sample), width = 0.5, fill = NA)
p = p + geom_point(aes(y = Well1), position = position_nudge(x = 0.1), shape = 17, size = 0.5) +
  geom_point(aes(y = Well2), position = position_nudge(x = -0.1), shape = 17, size = 0.5) +
  geom_point(aes(y = Well3), position = position_nudge(x = -0.1), shape = 17, size = 0.5)

p = p + xlab("Sample") + ylab("Percent viability") + ggtitle("Cytotoxicity Assay")
p = p + geom_text(aes(y = Absorb + error, label = tAbsorb), size = 2.5, vjust = -0.75)
p = p + theme_few() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1.1, hjust=1))

print(p)
ggsave("WST.svg", width = 18, height = 15, units = "cm")
