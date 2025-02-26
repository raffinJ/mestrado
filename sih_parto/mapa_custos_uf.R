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

# Plot do mapa dos estados
plot(st_geometry(estados_brasil))
estados_brasil <- st_transform(estados_brasil, 5880)


# Renomear coluna
colnames(estados_brasil)[3] <- "REG_ESTAB"

# Normalizar nomes
estados_brasil <- estados_brasil %>%
  mutate(nome_normalizado = stri_trans_general(tolower(REG_ESTAB), "Latin-ASCII"))


# Normalizar nomes
custo_ufs <- custo_ufs %>%
  mutate(nome_normalizado = stri_trans_general(tolower(REG_ESTAB), "Latin-ASCII"))



# Fazer o left_join com a coluna normalizada
mapa_dados <- custo_ufs %>%
  left_join(estados_brasil, by = "nome_normalizado") %>%
  st_as_sf() %>%
  st_set_crs(5880)  # Mantém o CRS após o join


# Gráfico do mapa com rosa dos ventos e escala
mapa_neo_tot <- ggplot(data = mapa_dados) +
  geom_sf(color = "grey50", size = 0.1, aes(fill = tot_mil, geometry = geom)) +
  scale_fill_gradient(
    low = "#deebf7",   # Azul claro
    high = "#08306b",  # Azul escuro
    na.value = "white" # Cor para valores ausentes
  ) +
  labs(
    title = "Custo para parto - Total",
    fill = " "
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  annotation_scale(location = "bl", width_hint = 0.6) +  # Adiciona escala
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.15, "in"), pad_y = unit(0.15, "in")) +  # Adiciona rosa dos ventos
  coord_sf()

ggsave("mapa_parto_tot.png", plot = mapa_neo_tot, width = 10, height = 6, dpi = 800)




# Repita para o segundo gráfico (uti_mil)
mapa_neo_uti <- ggplot(data = mapa_dados) +
  geom_sf(color = "grey50", size = 0.1, aes(fill = uti_mil, geometry = geom)) +
  scale_fill_gradient(
    low = "#deebf7",   # Azul claro
    high = "#08306b",  # Azul escuro
    na.value = "white" # Cor para valores ausentes
  ) +
  labs(
    title = "Custo para parto - UTI",
    fill = " "
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  annotation_scale(location = "bl", width_hint = 0.6) +  # Adiciona escala
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.15, "in"), pad_y = unit(0.15, "in")) +  # Adiciona rosa dos ventos
  coord_sf()

ggsave("mapa_parto_uti.png", plot = mapa_neo_uti, width = 10, height = 6, dpi = 800)

