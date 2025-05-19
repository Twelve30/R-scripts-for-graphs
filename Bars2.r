library(ggplot2)

p = ggplot(datad, aes(x=Protein, y=number, fill="blue")) + geom_bar(stat="identity")
p = p + theme_bw() + geom_text(aes(label=number), vjust=-0.3, size=2.5, hjust=0.5) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p = p + xlab("Proteins") + ylab("Number of Organisms") + ggtitle("Distribution of Moonlight Proteins in Bacteria") + theme(axis.line = element_line(colour = "black"))

print(p)
ggsave("Project graph.pdf", width=4096, height = 2160, units = "px")
