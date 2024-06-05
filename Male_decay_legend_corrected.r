#Comparisons between ages
# Install necessary packages one by one
install.packages("readxl")
install.packages("openxlsx")
install.packages("reshape2")

# Load necessary packages
library(readxl)
library(openxlsx)
library(ggplot2)
library(reshape2)
library(dplyr)

# Set working directory (manual) and import data from Excel file
Decaimiento <- read_excel("Decaimiento.xlsx")  

# Generate vectors with data  
Edad <- Decaimiento$Edad_en_días
w1118 <- Decaimiento$Machos_w1118
prtpΔ1<- Decaimiento$Machos_prtpΔ1
parkina<- Decaimiento$Machos_parkina
prtpΔ1parkina<- Decaimiento$Machos_prtpΔ1parkina

# Create a dataframe with data and age for females
df1 <- data.frame(Edad, w1118, prtpΔ1, parkina, prtpΔ1parkina)

# Convert female’s data from wide to long format 
df_long1 <- reshape2::melt(df1, id.vars = "Edad", variable.name = "Genotipo", value.name = "Valor")

# Graph for females data frame
ggplot(df_long1, aes(x = Edad, y = Valor, color = Genotipo, shape = Genotipo)) + 
  geom_line() + geom_point(size = 2) +
  xlab("Edad en días") + ylab("Nº de moscas que cruzan la línea a 8 cm") + 
  scale_x_continuous(breaks = c(5, 25)) + 
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, 1)) + 
  theme(axis.text.x = element_text(size = 10),  
        axis.title.x = element_text(size = 10, face = "bold" ), 
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(vjust = 4, size = 9, face = "bold"), 
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(1, 5, 1, 1), "lines"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), 
        legend.text = element_text(face = "italic")) + 
  scale_color_manual(values = c("black", "orange", "red", "darkgreen"), 
                     name = "",
                     labels = c("w", "prtp", "park", "prtp-park")) + 
  labs(color = "", shape = "") +
  scale_shape_manual(values = c(15, 17, 18, 19), 
                     name = "",
                     labels = c("w", "prtp", "park", "prtp-park"))

