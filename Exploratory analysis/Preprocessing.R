library(readxl)
library(writexl)
library(dplyr)
library(Amelia)
library(ggplot2)
library(moments)
library(dplyr)
library(tidyr)

dataset_placenta <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/dataset_placenta.xlsx")

#Preliminary information from the dataset
head(dataset_placenta)
tail(dataset_placenta)

dim(dataset_placenta)
features <- colnames(dataset_placenta)
features

#Encoding a new variable
dataset_placenta <- dataset_placenta %>%
  mutate(outcome = case_when(
    Pregnancyoutcome == "0" ~ "1",
    Pregnancyoutcome == "1" ~ "2",
    Pregnancyoutcome == "2" ~ "0"
  ))

#Remove the columns with the "process number" and "pregnancyoutcome"
dataset_placenta <- dataset_placenta[, -c(2, 7)]


write_xlsx(dataset_placenta, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/dataset_placenta_final.xlsx")


#Dataset final

dataset_placenta_final <- read_excel("/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/dataset_placenta_final.xlsx")

dim(dataset_placenta_final)
str(dataset_placenta_final)
summary(dataset_placenta_final)

#Converting Columns to Numeric Type
names(dataset_placenta_final[, sapply(dataset_placenta_final, class) == 'character'])

dataset_placenta_final[, c("MaternalAge", "Fetalweight", "Placentalweight", "Diameter1", "Diameter2", "Placentalthickness", "ratio")] <- lapply(dataset_placenta_final[, c("MaternalAge", "Fetalweight", "Placentalweight", "Diameter1", "Diameter2", "Placentalthickness", "ratio")], as.numeric)

#Check if exists NA values
missing_values <- sum(is.na(dataset_placenta_final))
missing_values        

missmap(dataset_placenta_final, main = "Missingness map",x.cex = 0.65)

#Remove NA values
d_placenta_without_na <- na.omit(dataset_placenta_final)
print(d_placenta_without_na)


#Check if exists duplicates
duplicates <- sum(duplicated(d_placenta_without_na))
duplicates

#Duplicate lines
print(d_placenta_without_na[duplicated(d_placenta_without_na), ])

d_without_duplicates <- unique(d_placenta_without_na)
d_without_duplicates


#Boxplots - see outliers
#Fetalweight
boxplot(d_without_duplicates$Fetalweight, 
        ylab = "Fetal weight (g)", 
        col = "lightblue", 
        border = "black",
        frame.plot = FALSE)

axis(side = 2, at = pretty(d_without_duplicates$Fetalweight),   
     labels = pretty(d_without_duplicates$Fetalweight))


#Placentalweight
boxplot(d_without_duplicates$Placentalweight,
        ylab = "Placental weight (g)", 
        col = "#B7E1CD", 
        border = "black",
        frame.plot = FALSE)

axis(side = 2, at = pretty(d_without_duplicates$Fetalweight),   
     labels = pretty(d_without_duplicates$Fetalweight))


#Ratio FPR
boxplot(d_without_duplicates$ratio, 
        ylab = "Fetal/Placental weight ratio", 
        col = "pink", 
        border = "black",
        frame.plot = FALSE)

axis(side = 2, at = pretty(d_without_duplicates$Fetalweight),   
     labels = pretty(d_without_duplicates$Fetalweight))


#Exploratory data analysis

#Sample Collection Locations
count_local <- table(d_without_duplicates$local)
count_local
min_y <- 0  
max_y <- 500

par(mgp = c(5, 1, 0))
barplot(count_local,
        xlab = "Local",
        ylab = "Count",
        col = "#66c2a5",  
        border = "black",
        ylim = c(min_y, max_y),
        las = 2)


#Higher count
count_tamega <- count_local["Tâmega"]
print(count_tamega)

#Lower count
min_count <- min(count_local)
print(min_count)

positions_min_count <- which(count_local == min(count_local))
print(positions_min_count)

locals_min_count <- names(count_local)[positions_min_count]
print(locals_min_count)

#Fetal gender
count_gender <- table(d_without_duplicates$Fetalgender)
count_gender
barplot(count_gender, 
        main = "Fetal gender",
        xlab = "Fetal gender",
        ylab = "Count",
        col = "#FFFF99",  
        border = "black")

#Outcome
count_outcome <- table(d_without_duplicates$outcome)
count_outcome
barplot(count_outcome, 
        main = "Pregnancy outcome",
        xlab = "Pregnancy outcome",
        ylab = "Count",
        col = "#98FB98",  
        border = "black")

#Histogram
#Maternal Age
hist(d_without_duplicates$MaternalAge,
     xlab = "Maternal age (years)",
     ylab = "Frequency",
     col = "lightblue",  
     border = "black",   
     breaks = 10)

#Gestational Age
hist(d_without_duplicates$GA, 
     main = "Histogram of distribution of gestational age",
     xlab = "Gestational age (weeks)",
     ylab = "Frequency",
     col = "brown",  
     border = "black",   
     
     breaks = 5)

#Placentalthickness
hist(d_without_duplicates$Placentalthickness, 
     main = "Histogram of distribution of placental thickness",
     xlab = "Placental thickness (cm)",
     ylab = "Frequency",
     col = "#FF9999",  
     border = "black",   
     breaks = 5)

#Diameter1
hist(d_without_duplicates$Diameter1, 
     main = "Histogram of distribution of Diameter 1",
     xlab = "Diameter 1 (cm)",
     ylab = "Frequency",
     col = "#B7E1CD",  
     border = "black",   
     breaks = 5)

#Diameter2
hist(d_without_duplicates$Diameter2, 
     main = "Histogram of distribution of Diameter 2",
     xlab = "Diameter 2 (cm)",
     ylab = "Frequency",
     col = "#D3AEB0",  
     border = "black",   
     breaks = 5)

#ratio
hist(d_without_duplicates$ratio, 
     main = "Histogram of distribution of ratio",
     xlab = "Ratio",
     ylab = "Frequency",
     col = "pink",  
     border = "black",   
     breaks = 5)


#Scatter plots

#GA / Fetal weight
ggplot(d_without_duplicates, aes(x = GA, y = Fetalweight)) +
  geom_point(color = "red") +  
  labs(title = "Relationship between fetal weight and GA",
       x = "Gestational age (weeks)",
       y = "Fetal weight (g)") +
  theme_bw() +
  theme(panel.grid = element_blank())


#GA / Placental weight
ggplot(d_without_duplicates, aes(x = GA, y = Placentalweight)) +
  geom_point(color = "lightblue") +  
  labs(title = "Relationship between placental weight and GA",
       x = "Gestational age (weeks)",
       y = "Placental weight (g)") +
  theme_bw() +
  theme(panel.grid = element_blank())

#GA / ratio FPR
ggplot(d_without_duplicates, aes(x = GA, y = ratio)) +
  geom_point(color = "#D3AEB0") +  
  labs(title = "Relationship between ratio FPR and GA",
       x = "Gestational age (weeks)",
       y = "Fetal/Placental weight ratio ") +
  theme_bw() +
  theme(panel.grid = element_blank())


#Tables

#MaternalAge

ma_table <- d_without_duplicates %>%
  group_by(outcome) %>%
  summarise(
    Mean = mean(MaternalAge),
    SD = sd(MaternalAge),
    Min = min(MaternalAge),
    Max = max(MaternalAge),
    Median = median(MaternalAge),
    Skewness = skewness(MaternalAge),
    Kurtosis = kurtosis(MaternalAge)
  )%>%
  
  pivot_longer(cols = -outcome, names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = outcome, values_from = Value)

ma_table

#GA

ga_table <- d_without_duplicates %>%
  group_by(outcome) %>%
  summarise(
    Mean = mean(GA),
    SD = sd(GA),
    Min = min(GA),
    Max = max(GA),
    Median = median(GA),
    Skewness = skewness(GA),
    Kurtosis = kurtosis(GA)
  )%>%
  
pivot_longer(cols = -outcome, names_to = "Statistic", values_to = "Value") %>%
pivot_wider(names_from = outcome, values_from = Value)

ga_table

#Placental thickness

pt_table <- d_without_duplicates %>%
  group_by(outcome) %>%
  summarise(
    Mean = mean(Placentalthickness),
    SD = sd(Placentalthickness),
    Min = min(Placentalthickness),
    Max = max(Placentalthickness),
    Median = median(Placentalthickness),
    Skewness = skewness(Placentalthickness),
    Kurtosis = kurtosis(Placentalthickness)
  )%>%
  
  pivot_longer(cols = -outcome, names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = outcome, values_from = Value)

pt_table

#Diameter1

d1_table <- d_without_duplicates %>%
  group_by(outcome) %>%
  summarise(
    Mean = mean(Diameter1),
    SD = sd(Diameter1),
    Min = min(Diameter1),
    Max = max(Diameter1),
    Median = median(Diameter1),
    Skewness = skewness(Diameter1),
    Kurtosis = kurtosis(Diameter1)
  )%>%
  
  pivot_longer(cols = -outcome, names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = outcome, values_from = Value)

d1_table


#Diameter2

d2_table <- d_without_duplicates %>%
  group_by(outcome) %>%
  summarise(
    Mean = mean(Diameter2),
    SD = sd(Diameter2),
    Min = min(Diameter2),
    Max = max(Diameter2),
    Median = median(Diameter2),
    Skewness = skewness(Diameter2),
    Kurtosis = kurtosis(Diameter2)
  )%>%
  
  pivot_longer(cols = -outcome, names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = outcome, values_from = Value)

d2_table

#ratio FPR

ratio_table <- d_without_duplicates %>%
  group_by(outcome) %>%
  summarise(
    Mean = mean(ratio),
    SD = sd(ratio),
    Min = min(ratio),
    Max = max(ratio),
    Median = median(ratio),
    Skewness = skewness(ratio),
    Kurtosis = kurtosis(ratio)
  )%>%
  
  pivot_longer(cols = -outcome, names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = outcome, values_from = Value)

ratio_table


#Fetalgender 2+9
d_without_duplicates <- mutate(d_without_duplicates, Fetalgender = recode(Fetalgender, `2` = 2, `9` = 2))

count_2 <- table(d_without_duplicates$Fetalgender)
print(count_2)

barplot(count_2, 
        main = "Fetal gender",
        xlab = "Fetal gender",
        ylab = "Count",
        col = "#FFFF99",  
        border = "black")


write_xlsx(d_without_duplicates, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_combined.xlsx")

#Remove the columns "local", "Fetalweight", "Placentalweight"
d_finished <- d_without_duplicates[, -c(1, 4, 6)]

write_xlsx(d_finished, "/Users/marciaoliveira/Desktop/MSc Bioinformatics/2º ANO/DISSERTAÇÃO/Dados/d_finished.xlsx")

