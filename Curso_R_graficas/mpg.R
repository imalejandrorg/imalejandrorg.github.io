library(tidyverse)
library(cowplot)
library(ggthemes)
data("mpg")

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 3)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = drv, y = cyl))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = cut, y = depth))

data("PlantGrowth")
ggplot(PlantGrowth) +
  geom_point(aes(x = group, y = weight)) +
  labs(x = "", y = "Peso seco (g)") +
  scale_x_discrete(labels = c("Control", "Tratamiento 1", "Tratamiento 2")) +
  theme_classic()

set.seed(123)
x <- round(rnorm(100, 5, 2))

ggplot(data.frame(x), aes(x)) + 
  geom_histogram(binwidth = 1, color = "gray20", fill = "lightblue") +
  geom_vline(xintercept = mean(x), linetype = "dashed", size = 1, color = "red") +
  geom_vline(xintercept = median(x), size = 1, color = "blue") +
  geom_text(aes(x = mean(x)+1, y = 20, label = "Media"), color = "red", size = 5) +
  geom_text(aes(x = median(x)-1, y = 20, label = "Mediana"), color = "blue", size = 5) +
  labs(x = "", y = "Frecuencia") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()

range(x)
max(x) - min(x)
sd(x)

ggplot(data.frame(x), aes(x)) + 
  geom_histogram(binwidth = 1, color = "gray20", fill = "lightblue") +
  geom_segment(aes(x = mean(x), y = 5, xend = mean(x) + sd(x), yend = 5), color = "gray20", linetype = "dashed", size = 1) +
  geom_segment(aes(x = mean(x) + sd(x), y = 0, xend = mean(x) + sd(x), yend = 5), color = "gray20", linetype = "dashed", size = 1) +
  geom_segment(aes(x = mean(x), y = 10, xend = mean(x) + 2*sd(x), yend = 10), color = "gray20", linetype = "dashed", size = 1) +
  geom_segment(aes(x = mean(x) + 2*sd(x), y = 0, xend = mean(x) + 2*sd(x), yend = 10), color = "gray20", linetype = "dashed", size = 1) +
  geom_segment(aes(x = mean(x), y = 15, xend = mean(x) + 3*sd(x), yend = 15), color = "gray20", linetype = "dashed", size = 1) +
  geom_segment(aes(x = mean(x) + 3*sd(x), y = 0, xend = mean(x) + 3*sd(x), yend = 15), color = "gray20", linetype = "dashed", size = 1) +
  geom_text(aes(x = mean(x) + sd(x), y = 6, label = "1 SD"), color = "gray20", size = 5) +
  geom_text(aes(x = mean(x) + 2*sd(x), y = 11, label = "2 SD"), color = "gray20", size = 5) +
  geom_text(aes(x = mean(x) + 3*sd(x), y = 16, label = "3 SD"), color = "gray20", size = 5) +
  geom_vline(xintercept = mean(x), size = 1, color = "gray20") +
  labs(x = "", y = "Frecuencia") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()

ggplot(data.frame(x), aes(x)) +
  geom_vline(xintercept = mean(x), size = 0.5, color = "gray20", linetype = "dashed") +
  geom_text(aes(x = mean(x) + 0.3, y = 0.5, label = "bar(x)"), parse = TRUE, size = 5) +
  geom_segment(aes(x = 4, y = 0.36, xend = mean(x), yend = 0.36), linetype = "dashed") +
  geom_text(aes(x = 4.6, y = 0.38, label = "x[i] - bar(x)"), parse = TRUE) +
  geom_segment(aes(x = 6, y = 0.28, xend = mean(x), yend = 0.28), linetype = "dashed") +
  geom_text(aes(x = 5.6, y = 0.3, label = "x[i] - bar(x)"), parse = TRUE) +
  geom_dotplot(aes(x), dotsize = 0.5, fill = "gray10", color = "white") +
  labs(x = "", y = "Frecuencia") +
  scale_x_continuous(breaks = c(0:9)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()


Z <- (x - mean(x))/sd(x)
head(Z)

Z2 <- scale(x)
head(Z2)

table(x)
head(prop.table(x))
summary(x)

max(x) - min(x)

data(PlantGrowth)
ggplot(PlantGrowth) + geom_boxplot(aes(group, weight, fill = group)) + 
  xlab("") + #Nombre del eje x
  ylab("Peso") + #Nombre del eje y
  scale_x_discrete(labels = c("Control", "Tratamiento 1", "Tratamiento 2")) + #Nombre de los valores en el eje x
  scale_fill_discrete(name = "Tratamientos", labels = c("Control", "Tratamiento 1", "Tratamiento 2")) + #Nombre de los grupos en la leyenda
  theme_classic() #Tema del gráfico


ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dnorm, size = 0.6) +
  scale_y_continuous(breaks = NULL) +
  geom_segment(aes(x = 1, y = 0, xend = 1, yend = 0.242), linetype = "dashed") +
  geom_segment(aes(x = -1, y = 0, xend = -1, yend = 0.242), linetype = "dashed") +
  geom_segment(aes(x = 2, y = 0, xend = 2, yend = 0.05), linetype = "dashed") +
  geom_segment(aes(x = -2, y = 0, xend = -2, yend = 0.05), linetype = "dashed") +
  geom_segment(aes(x = 3, y = 0, xend = 3, yend = 0.0044), linetype = "dashed") +
  geom_segment(aes(x = -3, y = 0, xend = -3, yend = 0.0044), linetype = "dashed") +
  ylab("") +
  xlab("") +
  scale_y_continuous(expand = c(0,0), breaks = NULL) +
  scale_x_discrete(limit = c(-4:4)) +
  theme_minimal_hgrid(color = "black")

set.seed(123)
y <- rnorm(25, 63, 12.2)
library(rstatix)
shapiro_test(y)
shapiro.test(y)
