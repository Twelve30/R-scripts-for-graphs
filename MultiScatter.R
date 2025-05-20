library(ggplot2)
library(nls2)
library(ggthemes)

datad = read.csv("Data.csv")

fit_AchR = nls(AchR~SSlogis(dose, Asym, xmid, scal), data = Data)
coeffs_AchR = coef(fit_AchR)
achreq = paste0("y == frac(", round(coeffs_AchR["Asym"], 2),
                ", 1 + e^{(", round(coeffs_AchR["xmid"], 2),
                "- x) / ", round(coeffs_AchR["scal"], 2), "})"
                )
achrec = round(coeffs_AchR["xmid"], 2)
achrectext = paste0("EC50 ==", achrec)

fit_Ach = nls(Ach~SSlogis(dose, Asym, xmid, scal), data = Data)
coeffs_Ach = coef(fit_Ach)
acheq = paste0("y == frac(", round(coeffs_Ach["Asym"], 2),
                ", 1 + e^{(", round(coeffs_Ach["xmid"], 2),
                "- x) / ", round(coeffs_Ach["scal"], 2), "})"
)
achec = round(coeffs_Ach["xmid"], 2)
achectext = paste0("EC50 ==", achec)

p = ggplot(datad, aes(x=dose)) +
  geom_point(aes(y=Ach), color="red") + geom_smooth(aes(y=Ach), method="nls", formula = y ~ SSlogis(x, Aysm, xmid, scal), color="red", se=FALSE) +
  geom_point(aes(y=AchR), color="blue") + stat_smooth(aes(y=AchR), method="nls", formula = y ~ SSlogis(x, Aysm, xmid, scal), se=FALSE, color="blue")
p = p + xlab("log(Dose)") + ylab("% Response") + theme_few()
p = p + 
  annotate("text", x = -Inf, y = Inf, label = achectext, parse = TRUE, color = "red", size = 3.5, vjust = 2.8, hjust = -1.8) +
  annotate("text", x = -Inf, y = Inf, label = achrectext, parse = TRUE, color = "blue", size = 3.5, vjust = 7.2, hjust = -1.8) +
  annotate("text", x = -Inf, y = Inf, label = acheq, parse = TRUE, color = "red", size = 3.5, vjust = 1.2, hjust = -0.1) +
  annotate("text", x = -Inf, y = Inf, label = achreq, parse = TRUE, color = "blue", size = 3.5, vjust = 2.5, hjust = -0.1)

print(p)
ggsave("PCM.svg", width = 27.31, height = 15.27, units = "cm")
