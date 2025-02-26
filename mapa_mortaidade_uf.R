# Instale o pacote ggspatial se ainda não o tiver instalado
# install.packages("ggspatial")

library(sf)
library(geobr)
library(stringi)
library(ggplot2)
library(ggspatial)
library(dplyr)

options(scipen = 999)

# Ler todos os municípios do país em um determinado ano
estados_brasil <- read_state(code_state = "all", year = 2020)

#projetando
estados_brasil <- st_transform(estados_brasil, 5880)

# Renomear coluna
colnames(estados_brasil)[3] <- "REG_ESTAB"

# Normalizar nomes
estados_brasil <- estados_brasil %>%
  mutate(nome_normalizado = stri_trans_general(tolower(REG_ESTAB), "Latin-ASCII"))


mortalidade_ufs <- dados_sih_neo %>%
  group_by(REG_ESTAB) %>%
  summarise(n_int = n()) %>%
  left_join(
    dados_sih_neo %>%
      group_by(REG_ESTAB, MORTE) %>%
      summarise(n_obitos = n()), 
    by = "REG_ESTAB"
  ) %>%
  mutate(n_mil = round((n_obitos / n_int) * 1000, 1))

mortalidade_ufs<- mortalidade_ufs %>%
  filter(MORTE == "Óbito")

# Normalizar nomes
mortalidade_ufs <- mortalidade_ufs %>%
  mutate(nome_normalizado = stri_trans_general(tolower(REG_ESTAB), "Latin-ASCII"))



# Fazer o left_join com a coluna normalizada
mapa_dados <- mortalidade_ufs %>%
  left_join(estados_brasil, by = "nome_normalizado") %>%
  st_as_sf() %>%
  st_set_crs(5880)  # Mantém o CRS após o join

# Criar coluna com coordenadas dos centróides dos estados
mapa_dados <- mapa_dados %>%
  mutate(
    centroide = st_centroid(geom),  # Calcula o centroide
    x = st_coordinates(centroide)[,1],  # Extrai coordenada X
    y = st_coordinates(centroide)[,2]   # Extrai coordenada Y
  )

mapa_mort <- ggplot(data = mapa_dados) +
  geom_sf(color = "grey50", size = 0.1, aes(fill = n_mil, geometry = geom)) +
  scale_fill_gradient(
    low = "lavenderblush",
    high = "palevioletred3",
    na.value = "white"
  ) +
  geom_text(aes(x = x, y = y, label = format(n_cem_mil, decimal.mark = ",", nsmall = 1)), 
            size = 3, color = "black") +  # Adiciona os números no centro dos estados
  labs(
    title = "Mortalidade Neonatal",
    fill = " "
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  annotation_scale(location = "bl", width_hint = 0.6) +
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.15, "in"), pad_y = unit(0.15, "in")) +
  coord_sf()

ggsave("mapa_mort_neo.png", plot = mapa_mort, width = 10, height = 6, dpi = 800)

