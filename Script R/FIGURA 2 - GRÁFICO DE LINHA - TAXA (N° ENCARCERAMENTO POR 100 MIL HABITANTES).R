
###################################################################################
## FIGURA 2 - GRÁFICO DE LINHA - TAXA (N° ENCARCERAMENTO POR 100 MIL HABITANTES) ##
###################################################################################

# Pacotes
library(tidyverse)
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

# Criar lista de gráficos por UF
graficos_ufs <- base %>%
  filter(ano >= 2016, ano <= 2024) %>%
  group_split(UF) %>%
  map(~{
    dados_uf <- .
    uf_nome <- unique(dados_uf$UF)
    
    ggplot(dados_uf, aes(x = ano, y = taxa)) +
      geom_line(color = "#1b9e77", size = 1.2) +
      geom_point(color = "#d95f02", size = 2) +
      labs(title = uf_nome, x = "Ano", y = "Taxa por 100 mil") +
      scale_x_continuous(breaks = 2016:2024) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)
      )
  })

# Título geral
titulo_geral <- ggdraw() +
  draw_label(
    "Evolução da Taxa de Encarceramento por 100 mil habitantes no Brasil",
    fontface = "bold",
    size = 18,
    x = 0.5,
    hjust = 0.5
  )

# Rodapé com a fonte
rodape_fonte <- ggdraw() +
  draw_label(
    "Fonte: SISDEPEN e IBGE",
    size = 10,
    x = 1,
    hjust = 1,
    fontface = "italic"
  )

# Montar o layout com todos os gráficos (4 colunas)
grid_graficos <- plot_grid(plotlist = graficos_ufs, ncol = 3)

# Layout final com título e rodapé
plot_final <- plot_grid(
  titulo_geral,
  grid_graficos,
  rodape_fonte,
  ncol = 1,
  rel_heights = c(0.08, 1, 0.05)
)

# Exibir
print(plot_final)


























