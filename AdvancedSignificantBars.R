library(ggplot2)
library(ggpmisc)
library(dplyr)
library(matrixStats)
library(ggthemes)
library(ggsignif)

datad = read.csv("WST.csv")

newdata2 = datad%>%
  mutate(Absorb = rowMeans(datad[c("Well1", "Well2", "Well3")])) %>%
  mutate(deviation = rowSds(as.matrix(datad[c("Well1", "Well2", "Well3")])))
newdata2 = newdata2 %>%
  mutate(error = deviation/sqrt(3)) %>%
  mutate(tAbsorb = round(Absorb, digits = 1))

pairs = newdata2 %>%
  filter(!grepl("Control|No venom|Venom only", Sample)) %>%
  mutate(Control = paste0(Sample, " Control"))

tres = pairs %>%
  rowwise() %>%
  mutate(
    sample_name = Sample,
    control_name = Control,
    
    tvals = list(
      newdata2 %>%
        filter(Sample == sample_name) %>%
        select(Well1, Well2, Well3) %>%
        unlist(use.names = FALSE)
    ),
    
    cvals = list(
      newdata2 %>%
        filter(Sample == control_name) %>%
        select(Well1, Well2, Well3) %>%
        unlist(use.names = FALSE)
    )
  ) %>%
  ungroup()

tres = tres %>%
  mutate(pval = t.test(tvals[[1]], cvals[[1]])$p.value)


max_y = max(newdata2$Absorb + newdata2$error)
tres = tres %>%
  mutate(
    y.position = max_y + row_number(),
    group1 = Sample,
    group2 = Control,
    signif_label = case_when(
      pval < 0.001 ~ "***",
      pval < 0.01 ~ "**",
      pval < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  filter(signif_label != "ns")

p = ggplot(newdata2, aes(x = reorder(Sample, order))) + geom_errorbar(aes(y = Absorb, ymin = Absorb - error, ymax = Absorb + error), width = 0.15) + geom_col(aes(y = Absorb, color=Sample), width = 0.5, fill = NA)
p = p + geom_point(aes(y = Well1), position = position_nudge(x = 0.1), shape = 17, size = 0.5) +
  geom_point(aes(y = Well2), position = position_nudge(x = -0.1), shape = 17, size = 0.5) +
  geom_point(aes(y = Well3), position = position_nudge(x = -0.1), shape = 17, size = 0.5)
p = p + geom_text(aes(y = Absorb + error, label = tAbsorb), size = 2.5, vjust = -0.75)

p = p + geom_signif(
  data = tres,
  aes(xmin = group1, xmax = group2, annotations = signif_label, y_position = y.position, colour = Sample),
  manual = TRUE,
  tip_length = 0,
  textsize = 3
)

p = p + xlab("Venom:Plant Extract 1:100") + ylab("Percent Inhibition") + ggtitle("PLA2 Assay")
p = p + theme_few() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1.1, hjust=1))
print(p)
ggsave("Protease.svg", width = 16.31, height = 15.27, units = "cm")