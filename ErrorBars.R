library(ggplot2)
library(ggpmisc)
library(dplyr)
library(matrixStats)

datad = read.csv("BSA.csv")

newdata2 = datad %>%
  select(Sample, Well1, Well2, Well3) %>%
  mutate(Absorb = rowMeans(datad[c("Well1", "Well2", "Well3")])) %>%
  mutate(deviation = rowSds(as.matrix(datad[c("Well1", "Well2", "Well3")])))
  %>% mutate(error = deviation/sqrt(3))

p = ggplot(newdata2, aes(x = Sample, y = Absorb)) +
  geom_errorbar(aes(ymin = Absorb - error, ymax = Absorb + error), width = 0.005) +
  geom_point() + geom_smooth(method = "lm", se=FALSE, color="black") +
  stat_poly_eq(use_label(c("eq", "R2")))

p = p + xlab("Concentration of protein (mg/ml)") +
  ylab("Absorbance") +
  ggtitle("Bradford Assay") + theme_bw()

print(p)
ggsave("Bradford.pdf", width = 3840, height = 2160, units = "px")
