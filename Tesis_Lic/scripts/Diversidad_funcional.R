####-----Librerías-----####
library(FD)
library(gawdis)
library(pairwiseAdonis)
library(tidyverse)
library(readxl)
library(fossil)
library(patchwork)
library(traitdata)
library(ggsignif)
library(ggpubr)
library(rstatix)
library(ggprism)
library(plyr)
library(stats)
library(CCA)
library(FactoMineR)
library(factoextra)
library(glue)


####-----Diversidad funcional-----####
# Setear la semilla.
set.seed(2022)

####-----Índices multidimensionales-----####
rasgos <- as.data.frame(read_excel("Aves_final.xlsx", sheet = "Rasgos"))
Datos <- read_excel("Aves_final.xlsx", sheet = "Muestreos") %>% as.data.frame()
Datos$Hábitat <- factor(Datos$Hábitat, levels = c("HA", "VS", "VP"))
Datos.abun <- Datos %>% dplyr::select(-c("Sitio", "Hábitat", "Muestreo")) %>% dplyr::select(-c("Empidonax occidentalis/difficilis", "Trochilidae sp.", "Myiarchus sp.", "Patagioenas sp."))
Habitat <- paste(Datos$Sitio, Datos$Muestreo)
rownames(Datos.abun) <- paste(Habitat)
Datos.abun <- Datos.abun[rowSums(Datos.abun[])>0,]


# Transformamos en factor los rasgos que están como caracteres.
rasgos$Dieta <- as.factor(rasgos$Dieta)
rasgos$EstratoForrajeo <- as.factor(rasgos$EstratoForrajeo)
rasgos$EstrategiaForrajeo <- as.factor(rasgos$EstrategiaForrajeo)
rasgos$EstatusMigratorio <- as.factor(rasgos$EstatusMigratorio)

# Creamos un vector con el nombre de nuestras especies.
sp <- rasgos$Especie

# Pegamos los nombres de nuestras especies.
rownames(rasgos) <- paste(sp)

# Filtramos nuestra base de datos para quitar la columna de especie.
rasgos.no.sp <- rasgos %>% dplyr::select(-Especie)
rasgos.cor <- rasgos.no.sp %>% dplyr::select(-c("EstrategiaForrajeo", "EstratoForrajeo", "Dieta", "EstatusMigratorio")) %>% 
  cor(method = "pearson", use = "complete.obs") %>% as.data.frame()

rasgos.no.sp <- rasgos.no.sp %>% dplyr::select(-c("AltoPico", "CuerdaAla", "LongitudTarso", "EstrategiaForrajeo", "EstratoForrajeo"))

# Calculo de la distancia de Gower utilizando la función 'gawdis' para encontrar los pesos para cada rasgo.
gower.dist <- gawdis(rasgos.no.sp, w.type = "optimized", opti.maxiter = 9999)

 # Podemos ver la contribución de cada rasgo y la correlación que tiene.
attr(gower.dist,"correls")
attr(gower.dist,"weights")


# Calculo de la diversidad funcional para cada uno de los sitios.
FD <- dbFD(gower.dist, Datos.abun, corr = "lingoes", stand.FRic = TRUE, CWM.type = "all")
FD.indices <- data.frame(FRic = FD$FRic, FEve = FD$FEve, FDis = FD$FDis, FDiv = FD$FDiv) %>% na.omit() %>% rownames_to_column(var = "Habitat") %>% 
  mutate(Habitat = str_replace_all(string = Habitat, pattern = "1|2|3|4|5|6|7|8|9| ", replacement = ""))

FD.indices$Habitat <- factor(FD.indices$Habitat, levels = c("HA", "VS", "VP"))
  
# Resumen de los índices de diversidad funcional.
FD.resumen <- FD.indices %>% dplyr::group_by(Habitat) %>% dplyr::summarise(FRic.media = mean(FRic, na.rm = T),
                                                                    FEve.media = mean(FEve), FDis.media = mean(FDis),
                                                                    FDiv.media = mean(FDiv), FRic.sd = sd(FRic),
                                                                    FEve.sd = sd(FEve), FDis.sd = sd(FDis), FDiv.sd = sd(FDiv))

# Exportamos archivo con los valores de la diversidad funcional.
write.csv(FD.resumen, "FD.csv")

# Kruskal-Wallis de FRic
Krusk.FRic <- kruskal_test(FRic ~ Habitat, data = FD.indices)
Krusk.FRic

Dunn.FRic <- FD.indices %>% dunn_test(FRic ~ Habitat, p.adjust.method = "bonferroni") %>% add_xy_position(x = "Habitat")
Dunn.FRic

# Boxplot de FRic.
FRic <- ggplot(FD.indices, aes(x = Habitat, y = FRic, fill = Habitat)) + 
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test") +
  stat_compare_means(label = "p.signif", ref.group = "VS", label.y = c(0.215, 0, 0.18)) +
  labs(x = "", y = "Functional richness (FRic)") +
  scale_y_continuous(guide = "prism_offset_minor", minor_breaks = seq(0, 0.4, 0.02), limits = c(0, 0.4)) +
  scale_x_discrete(guide = "prism_offset", labels = c("AO", "SF", "PF")) +
  scale_fill_brewer(palette = "Dark2", labels = c("AO", "SF", "PF")) +
  theme_prism(base_fontface = "plain") + 
  guides(fill = guide_legend(title = "Habitat")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14))

FRic

# Kruskal-Wallis de FEve
Krusk.FEve <- kruskal_test(FEve ~ Habitat, data = FD.indices)
Krusk.FEve

Dunn.FEve <- FD.indices %>% dunn_test(FEve ~ Habitat, p.adjust.method = "bonferroni") %>% add_xy_position(x = "Habitat")
Dunn.FEve

# Boxplot de FEve.
FEve <- ggplot(FD.indices, aes(x = Habitat, y = FEve, fill = Habitat)) + 
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test", label.y = 1) +
  labs(x = "", y = "Functional evenness (FEve)") +
  scale_x_discrete(guide = "prism_offset", labels = c("AO", "SF", "PF")) +
  scale_y_continuous(guide = "prism_offset_minor", minor_breaks = seq(0, 1, 0.05), limits = c(0, 1)) +
  scale_fill_brewer(palette = "Dark2", labels = c("AO", "SF", "PF")) +
  theme_prism(base_fontface = "plain") + 
  guides(fill = guide_legend(title = "Habitat")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14))

FEve


# Kruskal-Wallis de FDis.
Krusk.FDis <- kruskal_test(FDis ~ Habitat, data = FD.indices)
Krusk.FDis

Dunn.FDis <- FD.indices %>% dunn_test(FDis ~ Habitat, p.adjust.method = "bonferroni") %>% add_xy_position(x = "Habitat")
Dunn.FDis

# Boxplot de FDis
FDis <- ggplot(FD.indices, aes(x = Habitat, y = FDis, fill = Habitat)) + 
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test", label.y = 0.3) +
  stat_compare_means(label = "p.signif", ref.group = "VS", label.y = c(0.225, 0, 0.185)) +
  labs(x = "", y = "Functional dispersion (FDis)") +
  scale_x_discrete(guide = "prism_offset", labels = c("AO", "SF", "PF")) +
  scale_y_continuous(guide = "prism_offset_minor", minor_breaks = seq(0, 0.3, 0.01), limits = c(0, 0.3)) +
  scale_fill_brewer(palette = "Dark2", labels = c("AO", "SF", "PF")) +
  theme_prism(base_fontface = "plain") + 
  guides(fill = guide_legend(title = "Habitat")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14))

FDis

# Kruskal-Wallis de FDiv.
Krusk.FDiv <- kruskal_test(FDiv ~ Habitat, data = FD.indices)
Krusk.FDiv

Dunn.FDiv <- FD.indices %>% dunn_test(FDiv ~ Habitat, p.adjust.method = "bonferroni") %>% add_xy_position(x = "Habitat")
Dunn.FDiv

# Boxplot de FDiv
FDiv <- ggplot(FD.indices, aes(x = Habitat, y = FDiv, fill = Habitat)) + 
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test", label.y = 1) +
  labs(x = "", y = "Functional divergence (FDiv)") +
  scale_x_discrete(guide = "prism_offset", labels = c("AO", "SF", "PF")) +
  scale_y_continuous(guide = "prism_offset_minor", minor_breaks = seq(0, 1, 0.05), limits = c(0, 1)) +
  scale_fill_brewer(palette = "Dark2", labels = c("AO", "SF", "PF")) +
  theme_prism(base_fontface = "plain") + 
  guides(fill = guide_legend(title = "Habitat")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14))

FDiv

# Gráfico de valores de diversidad funcional
fun.graph <- FRic + FEve + FDis + FDiv +
  plot_layout(nrow = 2, guides = "collect") +
  plot_annotation(tag_levels = "A", tag_suffix = ")") +
  labs(guide = "Habitat") &
  theme(legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 14))

fun.graph
ggsave("O:/Documentos/Universidad/Tesis/Analisis_R/Graficos/div_funcional_eng.png", fun.graph, width = 10, height = 10, dpi = 600)

####-----Índices unidimensionales-----####

# Para calcular el CWM.
rasgos.no.sp2 <- rasgos.no.sp
rasgos.no.sp2$Masa <- log(rasgos.no.sp2$Masa)

FD.CWM <- dbFD(rasgos.no.sp2, Datos.abun, corr = "lingoes", CWM.type = "all")

FD.CWM.resumen <- FD.CWM$CWM %>% rownames_to_column(var = "Habitat") %>% 
  mutate(Habitat = str_replace_all(string = Habitat, pattern = "1|2|3|4|5|6|7|8|9| ", replacement = ""))

FD.CWM.resumen$Habitat <- as.factor(FD.CWM.resumen$Habitat)

# PERMANOVA de los resultados del CWM.
PERMANOVA.CWM <- pairwise.adonis2(FD.CWM.resumen[,2:15] ~ Habitat, FD.CWM.resumen, perm = 9999)
PERMANOVA.CWM

# CWM con dom
FD.CWM.dom <- dbFD(rasgos.no.sp2, Datos.abun, corr = "lingoes", CWM.type = "dom") %>% .$CWM %>% rownames_to_column(var = "Habitat") %>% 
  mutate(Habitat = str_replace_all(string = Habitat, pattern = "1|2|3|4|5|6|7|8|9| ", replacement = ""))

FD.CWM.dom$Masa <- exp(FD.CWM.dom$Masa)

write.csv(FD.CWM.dom, "CWM.csv")

