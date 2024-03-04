p = ggplot(datad, aes(fill=Type, y=Index, x=Time)) + geom_bar(position='dodge', stat='identity')
p = p + ggtitle("Plot of Analgesic Index vs Time") + xlab("Time (in minutes)") + ylab("Analgesic Index")
p = p + labs(fill = "Drug used")
p = p + geom_text(aes(label=Index), vjust=-0.3, size=3.5, position = position_dodge(8))
p
ggsave("Plot.pdf", width = 72.5, height = 33.5, units = "cm" )