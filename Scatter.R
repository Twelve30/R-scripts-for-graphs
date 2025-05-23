library(ggplot2)
library(ggpmisc)
library(ggpp)

pdata = read.csv("Data.csv")

p = ggplot(pdata, aes(x=Concentration, y=Absorbance)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE) +
  stat_poly_eq(use_label(c("eq", "R2")))

p = p + ggtitle("Standard Curve of Paracetamol") + xlab("Concentration (mg/ml)") + ylab("Absorbance")


print(p)
ggsave("PCM.pdf", width = 3840, height = 2160, units = "px")
