
# Carregar pacotes
library(dplyr)
library(ggplot2)
library(geobr)
library(cowplot)
library(stringr)

# 1. Preparar dados
df_icpr <- df_com_ICPR_NM_2016_a_2024 %>%
  filter(Ano %in% 2016:2024) %>%
  mutate(UF = str_trim(toupper(UF)))

# 2. Calcular média do índice por UF e ano
icpr_por_uf_ano <- df_icpr %>%
  group_by(UF, Ano) %>%
  summarise(indice_icpr = mean(ICPR_NM, na.rm = TRUE), .groups = "drop")

# 3. Carregar shapefile dos estados
estados <- read_state(year = 2020, showProgress = FALSE) %>%
  mutate(abbrev_state = str_trim(toupper(abbrev_state)))

# 4. Gerar mapas por ano com nova paleta e subtítulo limpo
mapas_icpr <- list()

for (ano_i in 2016:2024) {
  dados_ano <- icpr_por_uf_ano %>%
    filter(Ano == ano_i)
  
  dados_mapa <- estados %>%
    left_join(dados_ano, by = c("abbrev_state" = "UF"))
  
  cat("Ano:", ano_i, "- Estados com dado:", sum(!is.na(dados_mapa$indice_icpr)), "\n")
  
  mapa <- ggplot(dados_mapa) +
    geom_sf(aes(fill = indice_icpr), color = "white") +
    scale_fill_gradient(
      low = "white", high = "#d95f02",  # nova paleta
      name = "ICPR-NM", limits = c(0, 1),
      na.value = "grey90"
    ) +
    labs(subtitle = paste(ano_i)) +  # apenas o ano
    theme_void() +
    theme(
      plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
  
  mapas_icpr[[as.character(ano_i)]] <- mapa
}

# 5. Título e rodapé
titulo_icpr <- ggdraw() +
  draw_label(
    "Evolução do Indicador de Conformidade Prisional às Regras de Nelson Mandela",
    fontface = "bold", size = 16, x = 0.5, hjust = 0.5
  )

rodape_icpr <- ggdraw() +
  draw_label(
    "Fonte: SISDEPEN",
    size = 10, x = 1, hjust = 1, fontface = "italic"
  )

# 6. Montar e exibir painel
painel_icpr <- plot_grid(
  titulo_icpr,
  plot_grid(plotlist = mapas_icpr, ncol = 3),
  rodape_icpr,
  ncol = 1,
  rel_heights = c(0.08, 1, 0.07)
)

print(painel_icpr)



