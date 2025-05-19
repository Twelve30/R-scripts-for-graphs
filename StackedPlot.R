library(ggplot2)

datad = read.csv("Past.csv")

p = ggplot(datad, aes(x = Year, y = Value)) + geom_point(aes(color=Type)) + 
  geom_line(aes(color=Type)) + xlab("Year") + labs(title = "Changes in Global Carbon dioxide Concentration and Temperature in past 65  years ") +
  labs(y = "") +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  facet_grid(Type ~ ., scales = "free_y") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.placement  = "outsidedata",
        panel.spacing    = unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA))

print(p)
ggsave("plot.png", width = 3840, height = 2160, units = "px")
