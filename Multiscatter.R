library(ggplot2)
library(nls2)
datad = read.csv("Data.csv")
p = ggplot(datad, aes(x=dose)) + geom_point(aes(y=Ach), color="red") + geom_smooth(aes(y=Ach), method="nls", formula = y ~ SSlogis(x, Aysm, xmid, scal), color="red", se=FALSE)
p = p + geom_point(aes(y=AchR), color="blue") + stat_smooth(aes(y=AchR), method="nls", formula = y ~ SSlogis(x, Aysm, xmid, scal), se=FALSE, color="blue")
p = p + xlab("log(Dose)") + ylab("% Response") + theme_bw()
p