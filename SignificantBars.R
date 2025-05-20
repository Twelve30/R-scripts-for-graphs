library(ggplot2)
library(dplyr)
library(matrixStats)
library(ggthemes)
library(ggsignif)

datad = read.csv("PLA2.csv")

newdata2 = datad %>%
  select(Sample, Well1, Well2, Well3) %>%
  mutate(Absorb = rowMeans(datad[c("Well1", "Well2", "Well3")])) %>%
  mutate(deviation = rowSds(as.matrix(datad[c("Well1", "Well2", "Well3")]))) %>%
  mutate(error = deviation/sqrt(3)) %>%
  mutate(tAbsorb = round(Absorb, digits = 1))

cvals = newdata2 %>%
  filter(Sample == "EA") %>%
  select(Well1, Well2, Well3) %>%
  unlist()

pvals = newdata2 %>%
  filter(Sample != "EA") %>%
  rowwise() %>%
  mutate(pval = t.test(c_across(Well1:Well3), cvals)$p.value) %>%
  ungroup()
sig = pvals %>% filter(pval < 0.05)
max_y = max(newdata2$Absorb + newdata2$error)

sig = sig %>%
  mutate(
    y.position = max_y + 6*row_number(),
    group1 = Sample,
    group2 = "EA",
    signif_label = case_when(
      pval < 0.001 ~ "***",
      pval < 0.01 ~ "**",
      pval < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  filter(signif_label != "ns")

p = ggplot(newdata2, aes(x = Sample)) + 
  geom_errorbar(aes(y = Absorb, ymin = Absorb - error, ymax = Absorb + error), width = 0.15) + 
  geom_col(aes(y = Absorb, color=Sample), width = 0.5, fill = NA)

p = p + geom_point(aes(y = Well1), position = position_nudge(x = 0.1), shape = 17, size = 0.5) +
  geom_point(aes(y = Well2), position = position_nudge(x = -0.1), shape = 17, size = 0.5) +
  geom_point(aes(y = Well3), position = position_nudge(x = -0.1), shape = 17, size = 0.5)

p = p + geom_text(aes(y = Absorb + error, label = tAbsorb), size = 2.5, vjust = -0.75)

p = p + geom_signif(
  data = sig,
  aes(xmin = group1, xmax = group2, annotations = signif_label, y_position = y.position, colour = Sample),
  manual = TRUE,
  tip_length = 0,
  textsize = 3
)

p = p + xlab("Venom:Plant Extract 1:100") + ylab("Percent Inhibition") + ggtitle("PLA2 Assay")
p = p + theme_few() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1.1, hjust=1))

print(p)
ggsave("Significant.svg", width = 16.31, height = 15.27, units = "cm")
