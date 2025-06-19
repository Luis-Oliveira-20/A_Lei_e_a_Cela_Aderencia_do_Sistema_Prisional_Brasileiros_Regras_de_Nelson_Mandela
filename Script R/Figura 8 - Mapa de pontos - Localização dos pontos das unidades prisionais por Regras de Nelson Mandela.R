
library(dplyr)
library(ggplot2)
library(geobr)
library(sf)
library(stringr)

# 1. Filtrar dados de 2024
df_icpr_2024 <- df_com_ICPR_NM_2016_a_2024 %>%
  filter(Ano == 2024) %>%
  mutate(cod_ibge = as.character(`Código IBGE`))

# 2. Juntar com coordenadas
df_icpr_2024_geo <- df_icpr_2024 %>%
  left_join(municipios, by = "cod_ibge") %>%
  filter(!is.na(lat), !is.na(lon), !is.na(ICPR_NM))

# 3. Carregar mapa dos estados
estados <- read_state(year = 2020, showProgress = FALSE)

# 4. Criar o mapa
mapa_unidades <- ggplot() +
  geom_sf(data = estados, fill = "#f0f0f0", color = "#222222", size = 0.5) +  # fundo claro, traço escuro
  geom_point(
    data = df_icpr_2024_geo,
    aes(x = lon, y = lat, size = ICPR_NM, color = ICPR_NM),
    alpha = 0.8
  ) +
  scale_size_continuous(
    range = c(1.5, 6),
    limits = c(0, 1),
    name = "ICPR-NM"
  ) +
  scale_color_gradientn(
    colors = c("#0d0887", "#6a00a8", "#b12a90", "#f0a202", "#f06b00"),
    values = scales::rescale(c(0, 0.25, 0.5, 0.75, 1)),
    limits = c(0, 1),
    name = "ICPR-NM"
  ) +
  labs(
    title = "Unidades Prisionais por Unidade Federativa em 2024",
    subtitle = "",
    caption = "Fonte: SISDEPEN"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = "#333333"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#555555"),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

# 5. Exibir
print(mapa_unidades)
