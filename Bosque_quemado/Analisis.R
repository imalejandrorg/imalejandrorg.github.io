# Importe de datos
library(tidyverse)
library(readxl)
Datos <- read_excel("Muestreo_incendio.xlsx", sheet = "2_Calc_plasticidad")

# Cambio de nombre de columnas
Datos <- Datos %>% rename(plast_sust = `plasticidad sustrato`) %>% 
  rename(plast_diet = `plasticidad dieta`) %>% 
  rename(masa = `masa corporal`)

# Calculo de la abundancia relativa
Datos <- Datos %>% group_by(Sitio) %>% mutate(Abun_relativa = Abundancia / sum(Abundancia))

# Generación de los valores ponderados
Datos <- Datos %>% mutate(CWM_habitat = Abun_relativa * plast_habitat) %>%
  mutate(CWM_dieta = Abun_relativa * plast_diet) %>% 
  mutate(CWM_sustrato = Abun_relativa * plast_sust) %>% 
  mutate(CWM_masa = Abun_relativa * masa)

# Escritura de la tabla con los indices calculados
write.csv(Datos, "Muestreo_incendio.csv")
