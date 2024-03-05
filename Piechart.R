library(ggplot2)
library(ggpubr)
datad = read.csv("Book1.csv")
pie = ggpie(datad, "Number", fill = "Species", title = "Frequency of Moonlighting proteins") + theme(legend.position = "right")
pie