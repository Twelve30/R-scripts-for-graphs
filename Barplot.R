library(ggplot2)

p = ggplot(maindata, aes(x=Primary.function, y=number.of.organisms, fill=Primary.function)) + geom_bar(stat = "identity")
p = p + theme_minimal() + geom_text(aes(label=number.of.organisms), hjust=-0.3, size=3.5) + theme(legend.position = "none") + coord_flip() 
print(p)

ggsave("barplot.pdf", width = 3840, height = 2160, units = "px")
