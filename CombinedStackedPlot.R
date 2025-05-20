library(ggplot2)
library(nls2)
library(ggpmisc)

datad = read.csv("Bradford.csv")

p = ggplot(datad, aes(x = Sample, y = A)) + geom_point(aes(color=Type)) + geom_bar(aes(color=Type), fill = NA, stat = "identity", width = 0.02)

p = p +
  xlab("Concentration of Protein (mg/ml)") +
  ylab("Absorbance (595 nm)") +
  ggtitle("Bradford Assay") 

p = p +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  facet_grid(Type ~ ., scales = "free_y") 

p = p +
  theme_bw() + 
  theme(legend.position = "none") +
  theme(strip.background = element_blank(), strip.placement = "outside", panel.spacing = unit(0, "lines"), panel.border = element_rect(color = "black", fill = NA))

p = p + geom_smooth(data = subset(datad, Type == "loess"), aes(x = Sample, y = A), method = "loess", se = FALSE, color="green") +
  geom_smooth(data = subset(datad, Type == "lm"), aes(x = Sample, y = A), method = "lm", se = FALSE, color = "red") +
  stat_poly_eq(data = subset(datad, Type == "lm"), use_label(c("eq", "R2"))) +
  geom_smooth(data = subset(datad, Type == "nls"), aes(x = Sample, y = A), method = "nls", formula = y ~ SSlogis(x, Aysm, xmid, scal), se = FALSE, color="blue")

print(p)
ggsave("Bradford3.pdf", width = 3840, height = 2160, units = "px")
