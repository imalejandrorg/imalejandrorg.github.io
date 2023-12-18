####-----Librerías-----####
library(vegan)
library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(DescTools)
library(patchwork)
library(iNEXT)
library(readxl)
library(grid)
library(gridExtra)
library(BiodiversityR)
library(betapart)
library(pairwiseAdonis)
library(ggfortify)
library(ggrepel)
library(ggvegan)
library(ggpubr)
library(ggsignif)
library(rstatix)
library(PMCMR)
library(broom)
library(ggprism)
library(reshape2)


####-----Datos-----####
set.seed(2022)
# Importamos los datos y los convertimos en un data frame (la función 'read_excel()' genera un archivo tipo tibble).
Datos <- read_excel("Aves_final.xlsx", sheet = "Muestreos") %>% as.data.frame()
Datos$Hábitat <- factor(Datos$Hábitat, levels = c("HA", "VS", "VP"))

# Corroboramos la estructura de nuestros datos.
str(Datos)

# Separamos los datos por tipo de hábitat:
HA <- Datos %>% dplyr::filter(Hábitat == "HA") %>% dplyr::select(-c("Sitio", "Hábitat", "Muestreo"))
VS <- Datos %>% dplyr::filter(Hábitat == "VS") %>% dplyr::select(-c("Sitio", "Hábitat", "Muestreo"))
VP <- Datos %>% dplyr::filter(Hábitat == "VP") %>% dplyr::select(-c("Sitio", "Hábitat", "Muestreo"))

# Obtenemos las abundancias totales de las especies en cada tipo de vegetación.
HA.abun <- HA %>% colSums() %>% StripAttr("names") %>% .[!. %in% 0] %>% sort(decreasing = TRUE)
VS.abun <- VS %>% colSums() %>% StripAttr("names") %>% .[!. %in% 0] %>% sort(decreasing = TRUE)
VP.abun <- VP %>% colSums() %>% StripAttr("names") %>% .[!. %in% 0] %>% sort(decreasing = TRUE)

####-----Números de Hill-----####
# Juntamos todas las abundancias en una sola lista.
Abun <- list(HA = HA.abun, VS = VS.abun, VP = VP.abun)

# Cálculo de número de Hill con q = 0.
Hill <- iNEXT(Abun, q = c(0, 1, 2), datatype = "abundance", nboot = 999, conf = 0.95)

# Tres tipos de gráfica.
# Type=1: Estimados de diversidad como función del tamaño de muestreo.
RE <- ggiNEXT(Hill, type = 1, facet.var = "Assemblage") + 
  theme_bw(base_size = 10) + 
  labs(y = "Species diversity (± CI 95%)", x = "Number of individuals") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_linetype_discrete(labels = c("Interpolation", "Extrapolation"))

RE
ggsave("O:/Documentos/Universidad/Tesis/Analisis_R/Graficos/hill_graph1.png", RE, width = 8, height = 6, dpi = 300)

# Type=2: Cobertura de muestreo con respecto del tamaño de muestreo.1
SC <- ggiNEXT(Hill, type = 2) + 
  theme_bw(base_size = 10) + 
  labs(y = "Completitud de muestreo (± IC 95%)", x = "Número de individuos") +
  scale_color_brewer(palette = "Dark2", limits = c("HA", "VS", "VP")) +
  scale_fill_brewer(palette = "Dark2", limits = c("HA", "VS", "VP")) +
  scale_shape_discrete(limits = c("HA", "VS", "VP")) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_linetype_discrete(labels = c("Interpolación", "Extrapolación"))

SC
ggsave("O:/Documentos/Universidad/Tesis/Analisis_R/Graficos/hill_graph2.png", SC, width = 8, height = 6, dpi = 300)

# Type=3: Estimados de diversidad como función de la cobertura de muestreo.
CRE <- ggiNEXT(Hill, type = 3, facet = "Assemblage") + 
  theme_bw(base_size = 10) + 
  labs(y = "Diversidad de especies (± IC 95%)", x = "Completitud de muestreo") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_linetype_discrete(labels = c("Interpolación", "Extrapolación"))

CRE
ggsave("O:/Documentos/Universidad/Tesis/Analisis_R/Graficos/hill_graph3.png", CRE, width = 8, height = 6, dpi = 300)

# Cálculo de los números de Hill basado en la cobertura de muestreo.
Hill.est <- estimateD(Abun, datatype = "abundance", base = "coverage", nboot = 9999, conf = 0.95, level = 0.946)
Hill.est2 <- estimateD(Abun, datatype = "abundance", base = "size", nboot = 9999, conf = 0.95, level = 314)

Hill.est.q0 <- Hill.est[-c(2, 3, 5, 6, 8, 9), ]
Hill.est.q0$Assemblage <- factor(Hill.est.q0$Assemblage, levels = c("HA", "VS", "VP"))
Hill.est.q0$group <- c("A", "B", "AB")
Hill.est.q1 <- Hill.est[-c(1, 3, 4, 6, 7, 9), ]
Hill.est.q1$Assemblage <- factor(Hill.est.q0$Assemblage, levels = c("HA", "VS", "VP"))
Hill.est.q1$group <- c("A", "B", "AB")
Hill.est.q2 <- Hill.est[-c(1, 2, 4, 5, 7, 8), ]
Hill.est.q2$Assemblage <- factor(Hill.est.q0$Assemblage, levels = c("HA", "VS", "VP"))
Hill.est.q2$group <- c("A", "B", "AB")

# Boxplot de las interpolaciones.
# q = 0
q0.box <- ggplot(Hill.est.q0, aes(x = Assemblage, fill = Assemblage)) +
  geom_errorbar(aes(ymin = qD.LCL, ymax = qD.UCL, color = Assemblage), size = 1, width = 0.2, stat = "identity", show.legend = F) +
  geom_point(data = Hill.est.q0, mapping = aes(x = Assemblage, y = qD, color = Assemblage), size = 3, show.legend = F) +
  geom_text(aes(label = group, y = qD.UCL + 1.5)) +
  theme_prism(base_fontface = "plain") +
  scale_x_discrete(guide = "prism_offset") +
  scale_y_continuous(guide = "prism_offset") +
  guides(color = guide_legend(title = "Hábitat")) +
  labs(x = "", y = "Riqueza específica (S)") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_classic() +
  theme(axis.text = element_text(size = 12))


# q = 1
q1.box <- ggplot(Hill.est.q1, aes(x = Assemblage, fill = Assemblage)) +
  geom_errorbar(aes(ymin = qD.LCL, ymax = qD.UCL, color = Assemblage), size = 1, width = 0.2, stat = "identity", show.legend = F) +
  geom_point(data = Hill.est.q1, mapping = aes(x = Assemblage, y = qD, color = Assemblage), size = 3, show.legend = F) +
  geom_text(aes(label = group, y = qD.UCL + 0.5)) +
  theme_prism(base_fontface = "plain") +
  scale_x_discrete(guide = "prism_offset") +
  scale_y_continuous(guide = "prism_offset") +
  guides(color = guide_legend(title = "Hábitat")) +
  labs(x = "", y = "Shannon-Hill (exp(H'))") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_classic() +
  theme(axis.text = element_text(size = 12))

# q = 2
q2.box <- ggplot(Hill.est.q2, aes(x = Assemblage, fill = Assemblage)) +
  geom_errorbar(aes(ymin = qD.LCL, ymax = qD.UCL, color = Assemblage), size = 1, width = 0.2, stat = "identity", show.legend = F) +
  geom_point(data = Hill.est.q2, mapping = aes(x = Assemblage, y = qD, color = Assemblage), size = 3, show.legend = F) +
  geom_text(aes(label = group, y = qD.UCL + 0.25)) +
  theme_prism(base_fontface = "plain") +
  scale_x_discrete(guide = "prism_offset") +
  scale_y_continuous(guide = "prism_offset") +
  guides(color = guide_legend(title = "Hábitat")) +
  labs(x = "", y = "Simpson-Hill (1/D)") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_classic() +
  theme(axis.text = element_text(size = 12))

hill.graph <- q0.box + q1.box + q2.box +
  plot_layout(nrow = 1, guides = "collect") +
  plot_annotation(tag_levels = "a", tag_suffix = ")") +
  labs(guide = "Tipo de hábitat") &
  theme(legend.position = "none")

hill.graph

ggsave("O:/Documentos/Universidad/Tesis/Analisis_R/Graficos/div_alfa_hill.png", hill.graph, width = 8, height = 6, dpi = 300)

#####-----Gráficas de rangos de abundancias-----#####
# Utilizamos la función rankabundance().
HA.rank <- HA %>% .[,colSums(.[])>0] %>% rankabundance() %>% as.data.frame()
VS.rank <- VS %>% .[,colSums(.[])>0] %>% rankabundance() %>% as.data.frame()
VP.rank <- VP %>% .[,colSums(.[])>0] %>% rankabundance() %>% as.data.frame()

Datos.sp <- Datos %>% dplyr::select(-c("Sitio", "Muestreo", "Hábitat"))
Datos.env <- Datos %>% dplyr::select(c("Sitio", "Muestreo", "Hábitat"))
Datos.env$Hábitat <- as.factor(Datos.env$Hábitat)

Datos.rank.abun <- rankabuncomp(Datos.sp, y = Datos.env, factor = "Hábitat", return.data = TRUE, specnames = (1:3), legend = FALSE)
Datos.rank.abun$Grouping <- factor(Datos.rank.abun$Grouping, levels = c("HA", "VS", "VP"))

rank.graph <- ggplot(Datos.rank.abun, aes(x = rank, y = abundance)) +
  scale_x_continuous(expand = c(0,1), sec.axis = dup_axis(labels = NULL, name = NULL)) +
  scale_y_continuous(expand = c(0,1), sec.axis = dup_axis(labels = NULL, name = NULL)) +
  geom_line(aes(color = Grouping), size = 1, show.legend = FALSE) +
  geom_point(aes(color = Grouping), size = 1.7, alpha = 0.8, show.legend = FALSE) +
  geom_text_repel(data = subset(Datos.rank.abun, labelit == TRUE), aes(label = species), size = 3.5, angle = 55, nudge_x = 27, nudge_y = -10, show.legend = FALSE) +
  facet_wrap(~ Grouping) +
  labs(x = "Rango", y = "Abundancia", color = "Hábitat") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()

rank.graph
ggsave("O:/Documentos/Universidad/Tesis/Analisis_R/Graficos/rank_graph.png", rank.graph, width = 9, height = 6, dpi = 300)

####-----Diversidad beta-----####
# Primero sacamos las abundancias totales por cada uno de los puntos de muestreo.
Datos.total <- Datos %>% dplyr::select(-c("Hábitat", "Muestreo")) %>% dplyr::group_by(Sitio) %>% summarise_each(funs(sum))
Datos.names <- Datos.total$Sitio
Datos.total <- Datos.total %>% dplyr::select(-c("Sitio"))
rownames(Datos.total) <- paste(Datos.names)

# Análisis basado en datos de presencia-ausencia.
beta.total.pa <- Datos.total

# Transformamos a datos de presencia-ausencia.
beta.total.pa[beta.total.pa > 0] <- 1
beta.total.SOR <- beta.total.pa %>% beta.multi()

# Análisis basado en datos de abundancia.
beta.total.BC <- Datos.total %>% beta.multi.abund()
beta.total.BC

# Remuestreo de los sitios basado en presencia-ausencia. 9999 muestras de 10 sitios.
beta.SOR.sample <- beta.sample(beta.total.pa, index.family = "sorensen", sites = 10, samples = 9999)
dist.beta.SOR <- beta.SOR.sample$sampled.values
beta.SOR.sample

SOR.density <- ggplot(dist.beta.SOR) +
  geom_density(aes(x = beta.SIM), color = "blue", size = 0.8, linetype = "dashed") +
  geom_density(aes(x = beta.SNE), color = "red", size = 0.8, linetype = "dotted") +
  geom_density(aes(x = beta.SOR), color = "grey20", size = 0.8) +
  labs(x = "Disimilitud basada en presencia-ausencia", y = "Densidad") +
  scale_x_continuous(expand = c(0.025,0.025)) +
  theme_bw()

SOR.density
ggsave("O:/Documentos/Universidad/Tesis/Analisis_R/Graficos/SOR_density.png", SOR.density, width = 5, height = 3, dpi = 300)

# Remuestreo de los sitios basado en abundancias. 9999 muestras de 10 sitios.
beta.BC.sample <- beta.sample.abund(Datos.total, index.family = "bray", sites = 10, samples = 9999)
dist.beta.BC <- beta.BC.sample$sampled.values
beta.BC.sample

BC.density <- ggplot(dist.beta.BC) +
  geom_density(aes(x = beta.BRAY.BAL), color = "blue", size = 0.8, linetype = "dashed") +
  geom_density(aes(x = beta.BRAY.GRA), color = "red", size = 0.8, linetype = "dotted") +
  geom_density(aes(x = beta.BRAY), color = "grey20", size = 0.8) +
  labs(x = "Disimilitud basada en abundancias", y = "") +
  scale_x_continuous(expand = c(0.025,0.025)) +
  theme_bw()

BC.density
ggsave("O:/Documentos/Universidad/Tesis/Analisis_R/Graficos/BC_density.png", BC.density, width = 5, height = 3, dpi = 300)


####-----PERMANOVA-----####
# Prueba de PERMANOVA.
Datos.env <- data.frame(Habitat = c(rep("HA", 8), rep("VP", 7), rep("VS", 8)))
Datos.env$Habitat <- factor(Datos.env$Habitat, levels = c("HA", "VP", "VS"))
Datos.dist <- vegdist(log(Datos.total + 1), method = "bray")

# Prueba de homogeneidad de varianzas.
PERMDISP <- betadisper(Datos.dist, Datos.env$Habitat, type = "centroid")
anova(PERMDISP)
permutest(PERMDISP, permutations = 9999)

Datos.PERMANOVA <- adonis2(Datos.total ~ Datos.env$Habitat, data = Datos.total, permutations = 9999, method = "bray")
Datos.PERMANOVA

# Hacemos una prueba de pares.
Datos.PERMANOVA.resultados <- pairwise.adonis(Datos.total, Datos.env$Habitat, sim.function = "vegdist", sim.method = "bray", p.adjust.m = "bonferroni")
Datos.PERMANOVA.resultados



####-----NMDS-----####
set.seed(2022)
# Utilizamos la función metaMDS().
NMDS <- metaMDS(log(Datos.total+1), distance = "bray", autotransform = F, k = 2)

# Medimos el estrés.
NMDS$stress
stress.plot <- stressplot(NMDS, pch = 19, p.col = "gray60", l.col = "black", lwd = 1)
stress.plot

# Observación con ggplot2.
NMDS_scores <- data.frame(scores(NMDS, display = "sites"))
write.csv(NMDS_scores, file = "scores.csv")
scores <- cbind(as.data.frame(NMDS_scores), Habitat = Datos.env$Habitat) #Extrae los scores del NMDS y agrega la variable de ambiente.
centroides <- aggregate(cbind(NMDS1, NMDS2) ~ Habitat, data = scores, FUN = mean) #Extrae los centroides.
seg <- merge(scores, setNames(centroides, c("Habitat", "oNMDS1", "oNMDS2")), by = "Habitat", sort = F) #Combina el df de los scores y centroides.
fort <- fortify(NMDS)

# Gráfico de arañas.
spider.plot <- ggplot(scores, aes(x = NMDS1, y = NMDS2, colour = Habitat)) +
  geom_segment(data = seg, aes(xend = oNMDS1, yend = oNMDS2), show.legend = F) + # add spiders
  geom_point(data = centroides, size = 4, shape = 21, color = "black", aes(fill = Habitat)) + # add centroids
  geom_point(size = 1, shape = 21, color = "black", aes(fill = Habitat), show.legend = F) +                                              
  coord_fixed() +                                              
  scale_color_brewer(palette = "Dark2", limits = c("HA", "VS", "VP")) +
  scale_fill_brewer(palette = "Dark2", limits = c("HA", "VS", "VP")) +
  theme_bw() + 
  stat_ellipse(show.legend = F) +
  guides(color = guide_legend(title = "Hábitat")) +
  geom_text_repel(data = subset(fort, Score == "sites"),
                  mapping = aes(label = Label, x = NMDS1 * 1, y = NMDS2 * 1.05),
                  colour = "gray20",
                  size = 3) +
  theme(legend.position = "right",legend.text = element_text(size = 7),legend.direction = 'vertical')

spider.plot
ggsave("O:/Documentos/Universidad/Tesis/Analisis_R/Graficos/NMDS.png", spider.plot, width = 7, height = 5, dpi = 300)

# Gráfico de polígonos mínimos convexos.
fort <- fortify(NMDS)
NMDS_scores <- data.frame(scores(NMDS, display = "sites"))
data.scores <- cbind(as.data.frame(NMDS_scores), Habitat = Datos.env$Habitat)
grp.a <- data.scores[data.scores$Habitat == "HA", ][chull(data.scores[data.scores$Habitat == "HA", c("NMDS1", "NMDS2")]), ]
grp.b <- data.scores[data.scores$Habitat == "VS", ][chull(data.scores[data.scores$Habitat == "VS", c("NMDS1", "NMDS2")]), ]
grp.c <- data.scores[data.scores$Habitat == "VP", ][chull(data.scores[data.scores$Habitat == "VP", c("NMDS1", "NMDS2")]), ]
hull.data <- rbind(grp.a, grp.b, grp.c)

# Gráfico con el primero NMDS basado en polígonos mínimos convexos.
convex.graph <- ggplot() +
  geom_point(data = subset(fort, Score == "species"),
             mapping = aes(x = NMDS1, y = NMDS2),
             colour = "gray50",
             alpha = 0.8,
             size = 1.2) +
  geom_polygon(data = hull.data, aes(x = NMDS1, y = NMDS2, fill = Habitat, group = Habitat), alpha = 0.30) +
  geom_point(data = subset(fort, Score == "sites"),
             mapping = aes(x = NMDS1, y = NMDS2, colour = Datos.env$Habitat),
             alpha = 1,
             size = 3,
             shape = 18, show.legend = F) +
  geom_text_repel(data = subset(fort, Score == "sites"),
                  mapping = aes(label = Label, x = NMDS1 * 1.1, y = NMDS2 * 1.1),
                  colour = "gray20",
                  size = 3) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed", size = 0.6, colour = "gray20") +
  geom_vline(aes(xintercept = 0), linetype = "dashed", size = 0.6, colour = "gray20") +
  guides(fill = guide_legend(title = "Hábitat")) +
  scale_color_brewer(palette = "Dark2", limits = c("HA", "VS", "VP")) +
  scale_fill_brewer(palette = "Dark2", limits = c("HA", "VS", "VP")) +
  theme_bw()

convex.graph
ggsave("O:/Documentos/Universidad/Tesis/Analisis_R/Graficos/NMDS2.png", convex.graph, width = 8, height = 5, dpi = 300)

NMDS_df <- NMDS$points %>% as.data.frame()
write.csv(NMDS_df, "NMDS_df.csv")

####-----Diversidad gama-----####
# Datos de familias y órdenes.
Datos.gama <- read_excel("Aves_final.xlsx", sheet = "Listado_especies")

# Gráfica de órdenes.
ggplot(Datos.gama) +
  geom_bar(aes(y = Orden)) +
  labs(x = "Número de especies", y = "Órdenes") +
  scale_x_continuous(expand = c(0,0.8)) +
  theme_bw()

# Gráfica de órdenes.
ordenes <- ggplot(Datos.gama) +
  geom_bar(aes(y = Familia, fill = Orden)) +
  labs(x = "Número de especies", y = "Familias") +
  scale_x_continuous(expand = c(0,0.15)) +
  theme_bw()
  
ggsave("O:/Documentos/Universidad/Tesis/Analisis_R/Graficos/ordenes.png", ordenes, width = 8, height = 5, dpi = 300)

