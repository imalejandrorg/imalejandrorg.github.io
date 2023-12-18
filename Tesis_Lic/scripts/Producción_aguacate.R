####-----Librerías-----####
library(tidyverse)
library(readxl)
library(lubridate)
library(ggrepel)
library(ggsave)
library(RColorBrewer)
library(ggprism)



####-----Datos-----####
# Para todo Jalisco.
Aguacates <- read_excel("Aguacates_estados.xlsx", sheet = "Aguacates") %>% 
  dplyr::filter(Entidad == "Jalisco") %>% 
  dplyr::select(-c("Rendimiento", "Valor de la producción", "Siniestrada", "Cosechada", "Municipio", "Cultivo", "Unidades", "Entidad", "Precio")) %>% 
  dplyr::group_by(Año) %>% 
  summarise_each(funs(sum)) %>% 
  dplyr::ungroup()

Aguacates$Año <- make_date(year = Aguacates$Año)

# Gráfica por hectáreas.
graph1 <- ggplot(Aguacates, aes(x = Año, y = Sembrada)) +
  geom_point(color = "grey40", size = 1.8) +
  geom_line(color = "grey40", size = 1) +
  labs(x = "Año", y = "Área cultivada (ha)") +
  theme_prism(base_fontface = "plain") +
  scale_x_date(guide = "prism_offset", date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(guide = "prism_offset_minor", minor_breaks = seq(0, 30000, 1000), limits = c(0,30000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11),
        axis.line = element_line(size = 0.8),
        axis.ticks = element_line(size = 0.8))

ggsave("O:/Documentos/Universidad/Tesis/Analisis_R/Graficos/Produccion_aguacate.png", graph1, width = 9, dpi = 300)

# Gráfica por toneladas.
ggplot(Aguacates, aes(x = Año, y = `Volumen de producción`)) +
  geom_point(color = "grey40") +
  geom_line(color = "grey40", size = 1) +
  labs(x = "", y = "Producción (toneladas)") +
  theme_bw()

# Para los principales municipios productores.
Municipios <- c("Zapotlán el Grande", "San Gabriel", "Tuxpan", "Tonila", "Zapotitlán de Vadillo")

Aguacates.munc <- read_excel("Aguacates_estados.xlsx", sheet = "Aguacates") %>%
  dplyr::filter(Entidad == "Jalisco") %>% 
  dplyr::filter(Municipio %in% Municipios) %>% 
  dplyr::select(-c("Rendimiento", "Valor de la producción", "Siniestrada", "Cosechada", "Cultivo", "Unidades", "Entidad", "Precio"))

Aguacates.munc$Año <- make_date(year = Aguacates.munc$Año)

graph2 <- ggplot(Aguacates.munc, aes(x = Año, y = Sembrada)) +
  geom_point(aes(color = Municipio, shape = Municipio), size = 2) +
  geom_line(aes(color = Municipio, linetype = Municipio), size = 1) +
  labs(x = "", y = "Área cultivada (ha)") +
  scale_x_date(guide = "prism_offset", date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(guide = "prism_offset_minor", minor_breaks = seq(0, 6000, 200), limits = c(0,6000)) +
  theme_prism(base_fontface = "plain") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 11))

ggsave("O:/Documentos/Universidad/Tesis/Analisis_R/Graficos/Produccion_aguacate_municipios.png", graph2, width = 10, dpi = 300)
