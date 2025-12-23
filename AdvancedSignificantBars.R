library(ggplot2)
library(ggpmisc)
library(dplyr)
library(matrixStats)
library(ggthemes)
library(ggsignif)
library(extrafont)
library(ggtext)

loadfonts(device = "win")

datad = read.csv("Luciferase 191225.csv")

newdata = datad # %>%
# mutate(nor1 = (F1/R1)) %>% mutate(nor2 = (F2/R2)) %>% mutate(nor3 = (F3/R3))
newdata2 = newdata %>%
  mutate(avgread = rowMeans(newdata[c("F1", "F2", "F3")])) %>%
  mutate(deviation = rowSds(as.matrix(newdata[c("F1", "F2", "F3")]))) %>%
 filter(!grepl("UT", Sample))

pairs = newdata2 %>%
  filter(!grepl("UT|mut", Sample)) %>%
  mutate(moredna = paste0(Sample, " mut"))


tres = pairs %>%
  rowwise() %>%
  mutate(
    sample_name = Sample,
    control_name = moredna,

    tvals = list(
      newdata2 %>%
        filter(Sample == sample_name) %>%
        select(F1, F2, F3) %>%
        unlist(use.names = FALSE)
    ),

    cvals = list(
      newdata2 %>%
        filter(Sample == control_name) %>%
        select(F1, F2, F3) %>%
        unlist(use.names = FALSE)
    )
  ) %>%
  ungroup()

val = 1
pval = numeric()
 
ymax = max(newdata2$avgread + newdata2$deviation, na.rm = TRUE)
 
pval <- numeric(length(tres$tvals))

for (i in seq_along(tres$tvals)) {
  pval[i] <- t.test(
    tres$tvals[[i]],
    tres$cvals[[i]],
    var.equal = TRUE
  )$p.value
}
 
 
#max_y = max(newdata2$avgread + newdata2$deviation + 3000)
tres = tres %>%
  mutate(
    group1 = Sample,
    group2 = moredna,
    signif_label = case_when(
      pval < 0.05 ~ paste0("p = ", formatC(pval, format = "f", digits = 4)),
      TRUE ~ "ns"
    )
  ) %>%
filter(signif_label != "ns")

p = ggplot(newdata2, aes(x = reorder(Sample, order))) +
  geom_errorbar(aes(y = avgread, ymin = avgread - deviation, ymax = avgread + deviation), width = 0.15) +
  geom_col(aes(y = avgread, color=Sample), width = 0.5, fill = NA) +
  scale_color_manual(values = c("rrl" = "black","rrl mut" = "#ff0066","wge" = "#108081", "wge mut" = "#400080"))
p = p + geom_point(aes(y = F1), position = position_nudge(x = 0.1), shape = 17, size = 1.5) +
  geom_point(aes(y = F2), position = position_nudge(x = -0.1), shape = 17, size = 1.5) +
  geom_point(aes(y = F3), position = position_nudge(x = -0.15), shape = 17, size = 1.5)

p = p + geom_signif(
  data = tres,
  aes(xmin = group1, xmax = group2, annotations = signif_label, y_position = (avgread + deviation + 5000)),
  manual = TRUE,
  color="black",
  tip_length = 0,
  textsize = 3
)

p = p + xlab("Construct") + ylab("Luciferase activity") + ggtitle("*In vitro* translation") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.25)), labels = scales::label_number(big.mark = ""))
p = p + theme_few() + theme(legend.position = "none") + 
  scale_x_discrete(limits = c("rrl", "rrl mut", "wge", "wge mut"),
                   labels = c(paste("*HBB*-*FLuc* <br> Rabbit <br> reticulocyte <br> lysate"), paste("*HBB*-E6V-*FLuc* <br> Rabbit <br> reticulocyte <br> lysate"), paste("*HBB*-*FLuc* <br> Wheat <br> germ <br> extract"), paste("*HBB*-E6V-*FLuc* <br> Wheat <br> germ <br> extract")))
p = p + theme(text=element_text(family="Times New Roman"), axis.text.x = ggtext::element_markdown(), plot.title = ggtext::element_markdown())
print(p)
ggsave("Luciferase 191225.svg", width = 12.31, height = 15.27, units = "cm")
#ggsave("Dual Luciferase 131025.pdf", width = 15.31, height = 15.27, units = "cm")
