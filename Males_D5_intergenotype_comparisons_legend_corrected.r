#Comparisons between genotypes of D5 males

# Install necessary packages one by one
install.packages("readxl")
install.packages("openxlsx")
install.packages ("carData")
install.packages("dunn.test")
install.packages("PMCMRplus")
install.packages("ggsignif")
install.packages("stringr")

# Load necessary packages
library(readxl)
library(car)
library(openxlsx)
library(dunn.test)
library(PMCMRplus)
library(ggsignif)
library(ggplot2)
library(tidyr)
library(stringr)

# Set working directory and import data from Excel file
MachosD5 <- read_excel("MachosD5.xlsx")     


# Generate vectors with data                                                                         
w1118 <- MachosD5$w1118
prtpΔ1 <- MachosD5$prtpΔ1
parkina <- MachosD5$parkina
prtpΔ1parkina <- MachosD5$prtpΔ1parkina
#Ploteo del histograma para cada genotipo
hist(w1118)
hist(prtpΔ1)
hist(parkina)
hist(prtpΔ1parkina)
#Análisis de normalidad Shapiro-Wilk test
shapiro.test(w1118)
shapiro.test(prtpΔ1)
shapiro.test(parkina)
shapiro.test(prtpΔ1parkina)
# Create a data frame with data
df <- data.frame(
  group = c(rep("w1118", length(w1118)),
            rep("prtpΔ1", length(prtpΔ1)),
            rep("parkina", length(parkina)),
            rep("prtpΔ1parkina", length(prtpΔ1parkina))),
  value = c(w1118, prtpΔ1, parkina, prtpΔ1parkina)
)
# Perform the Levene test
leveneTest(value ~ group, data = df)
# Run Dunn's test (Kruskal-Wallis included) over w1118, parkina
dunn_test <- dunn.test(df$value, df$group, method = "bonferroni")
# Print results
print(dunn_test)
# Combine 4 vectors into single data frame
data <- data.frame(w1118, prtpΔ1, parkina, prtpΔ1parkina)

# Convert data from wide to long format
data_long <- gather(data, key = "group", value = "value")

# Define new order of groups
new_order <- c("w1118", "prtpΔ1", "parkina", "prtpΔ1parkina")

# Change order of levels of "group" factor
data_long$group <- factor(data_long$group, levels = new_order)

# Violin plot with statistical layer
ggplot(data_long, aes(x = group, y = value, fill = group)) + 
  geom_violin(draw_quantiles = FALSE) + 
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5) + 
  labs(x = "", y = "Climbing (n° de moscas que cruzan la línea a 8 cm)") +  
  scale_fill_manual(values = c("#B6B3B2", "#D3AE5F", "#D34848", "#8BC250")) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(face = "bold.italic", size = 14), 
        legend.position = "none", 
        axis.text.y = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(vjust = 4, size = 14, face = "bold"), 
        axis.line = element_line (colour = "black"), 
        plot.margin = unit(c(1, 5, 1, 1), "lines"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(limits = c(0, 16), breaks = seq(0, 12, by = 2), expand = c(0,0)) +
  scale_x_discrete(
    labels = c(
      "w1118" = "w",
      "prtpΔ1" = "prtp",
      "parkina" = "park",
      "prtpΔ1parkina" = "prtp-park"
    )
  )+
  geom_signif(comparisons = list(c("prtpΔ1parkina", "parkina")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              color = "black",
              size = 1,
              textsize = 5,
              y_position = c(9, 9.5)) + 
  geom_signif(comparisons = list(c("prtpΔ1parkina", "prtpΔ1")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              color = "black",
              size = 0.5,
              textsize = 5,
              y_position = c(10.5, 11)) +
  geom_signif(comparisons = list(c("prtpΔ1parkina", "w1118")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              color = "black",
              size = 0.5,
              textsize = 5,
              y_position = c(14.5, 15)) +
  geom_signif(comparisons = list(c("parkina", "prtpΔ1")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              color = "black",
              size = 0.5,
              textsize =5 ,
              y_position = c(9, 9.5)) +
  geom_signif(comparisons = list(c("parkina", "w1118")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              color = "black",
              size = 0.5,
              textsize = 5,
              y_position = c(13.5, 14)) +
  geom_signif(comparisons = list(c("prtpΔ1", "w1118")),
              test = "wilcox.test",
              map_signif_level = TRUE,
              color = "black",
              size = 0.5,
              textsize = 5,
              y_position = c(12, 12.5))

# Assuming data contains multiple repeated measures for each genotype
# Combine 4 vectors into single data frame
data_friedman <- data.frame(w1118, prtpΔ1, parkina, prtpΔ1parkina)

# Convert data from wide to long format
data_friedman_long <- gather(data_friedman, key = "group", value = "value")

# Generate a subject identifier assuming rows correspond to subjects (replicates)
data_friedman_long$subject <- rep(1:nrow(data_friedman), times = ncol(data_friedman))

# Run the Friedman test
friedman_result <- friedman.test(value ~ group | subject, data = data_friedman_long)

# Print Friedman test results
print(friedman_result)


# Post-hoc pairwise Wilcoxon tests with Bonferroni correction
pairwise.wilcox.test(data_friedman_long$value, data_friedman_long$group, 
                     p.adjust.method = "bonferroni", paired = TRUE)
