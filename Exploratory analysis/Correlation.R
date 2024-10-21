library(readxl)
library(writexl)
library(ggplot2)

d_combined <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_combined.xlsx")


#Correlation
#Calculating the correlation matrix
correlation <- cor(d_combined[c("Fetalweight", "Placentalweight", "ratio")])

#Viewing the correlation matrix
print(correlation)

#Fetalgender
d_combined$Fetalgender <-  as.factor(d_combined$Fetalgender)
boxplot(d_combined$ratio ~ d_combined$Fetalgender)
#Chi-Square Test
chisq.test(table(d_combined$outcome ,d_combined$Fetalgender))


#MaternalAge
#Kruskal-Wallis Test
shapiro.test(d_combined$MaternalAge)
kruskal.test(MaternalAge ~ outcome, data = d_combined)


#GA
#Kruskal-Wallis Test
shapiro.test(d_combined$GA)
kruskal.test(GA ~ outcome, data = d_combined)

#Fetalweight
#Kruskal-Wallis Test
shapiro.test(d_combined$Fetalweight)
kruskal.test(Fetalweight ~ outcome, data = d_combined)

#Placentalweight
#Kruskal-Wallis Test
shapiro.test(d_combined$Placentalweight)
kruskal.test(Placentalweight ~ outcome, data = d_combined)

#Diameter1
#Kruskal-Wallis Test
shapiro.test(d_combined$Diameter1)
kruskal.test(Diameter1 ~ outcome, data = d_combined)

#Diameter2
#Kruskal-Wallis Test
shapiro.test(d_combined$Diameter2)
kruskal.test(Diameter2 ~ outcome, data = d_combined)

#Placentalthickness
#Kruskal-Wallis Test
shapiro.test(d_combined$Placentalthickness)
kruskal.test(Placentalthickness ~ outcome, data = d_combined)

#ratio
#Kruskal-Wallis Test
shapiro.test(d_combined$ratio)
kruskal.test(ratio ~ outcome, data = d_combined)


ggplot(d_combined, aes(x = factor(outcome), y = ratio, fill = factor(outcome))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "#7B68EE", "1" = "#FFD700", "2" = "#CCFF99"), 
                    labels = c("Newborn", "Intrauterine fetal death", "Neonatal death"),
                    name = "") +
  scale_x_discrete(labels = NULL) +
  labs(
    x = "Obstetric outcome",
    y = "Fetal/placental weight ratio"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "grey"),
    legend.position = "top"
  )

