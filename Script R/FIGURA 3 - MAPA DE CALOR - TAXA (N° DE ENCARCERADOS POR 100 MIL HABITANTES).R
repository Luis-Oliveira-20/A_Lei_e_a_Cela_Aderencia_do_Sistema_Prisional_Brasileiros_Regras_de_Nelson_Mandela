
############################################################################################
######## FIGURA 3 - MAPA DE CALOR - TAXA (N° DE ENCARCERADOS POR 100 MIL HABITANTES) ####  
############################################################################################

# Pacotes
library(tidyverse)
library(geobr)
library(sf)
library(cowplot)

# Dados em formato longo
habitantes_long <- HABITANTES_POR_UF_E_ANO %>%
  pivot_longer(-UF, names_to = "ano", values_to = "populacao") %>%
  mutate(ano = as.integer(ano))

prisao_long <- tabela_populacao_prisional_por_ano %>%
  pivot_longer(-UF, names_to = "ano", values_to = "encarcerados") %>%
  mutate(ano = as.integer(ano))

# Calcular taxa por 100 mil habitantes
base <- left_join(prisao_long, habitantes_long, by = c("UF", "ano")) %>%
  mutate(taxa = round((encarcerados / populacao) * 100000, 1))

# Shapefile
estados <- read_state(year = 2020, showProgress = FALSE)

# Juntar shapefile com os dados
base_geo <- estados %>%
  left_join(base, by = c("abbrev_state" = "UF"))

# Criar lista de mapas por ano
mapas <- list()

for (ano_i in 2016:2024) {
  dados_mapa <- base_geo %>% filter(ano == ano_i)
  
  mapa <- ggplot(dados_mapa) +
    geom_sf(aes(fill = taxa), color = "white") +
    scale_fill_gradient(
      low = "#fddbc7",   # laranja claro
      high = "#d95f02",  # laranja escuro
      name = "Taxa por\n100 mil",
      na.value = "grey90"
    ) +
    labs(subtitle = as.character(ano_i)) +
    theme_void() +
    theme(
      plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
  
  mapas[[as.character(ano_i)]] <- mapa
}

# Criar título geral
titulo_geral <- ggdraw() +
  draw_label(
    "Taxa de Encarceramento por 100 mil Habitantes",
    fontface = "bold",
    size = 18,
    x = 0.5,
    hjust = 0.5
  )

# Criar rodapé com a fonte
rodape_fonte <- ggdraw() +
  draw_label(
    "Fonte: Sisdepen e IBGE",
    size = 10,
    x = 1,
    hjust = 1,
    fontface = "italic"
  )

# Montar o layout final
plot_final <- plot_grid(
  titulo_geral,
  plot_grid(plotlist = mapas, ncol = 3),
  rodape_fonte,
  ncol = 1,
  rel_heights = c(0.08, 1, 0.07)
)

# Exibir
print(plot_final)


























