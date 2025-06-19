
library(dplyr)
library(stringr)

SISDEPEN_FILTRADO <- SISDEPEN %>%
  filter(str_detect(`Tipo do Estabelecimento`, regex("cela física", ignore_case = TRUE)))

library(tidyverse)

library(scales)  # para label_comma()

grafico_evolucao_por_uf <- dados_uf_ano %>%
  ggplot(aes(x = Ano, y = encarcerados, group = UF)) +
  geom_line(color = "#1b9e77", size = 1) +
  geom_point(color = "#d95f02", size = 2) +
  facet_wrap(~ UF, scales = "free_y", ncol = 3) +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_x_continuous(breaks = 2016:2024) +
  labs(
    title = "Evolução do Número de Encarcerados por Unidade Federativa",
    subtitle = "",
    x = "Ano",
    y = "Número de Encarcerados",
    caption = "Fonte: SISDEPEN"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1, face = "italic")
  )

print(grafico_evolucao_por_uf)




library(tidyverse)
library(scales)

# Inserir linha com valor zero no ano anterior (2015) para cada UF
ufs_unicas <- unique(dados_uf_ano$UF)

dados_base_zero <- tibble(
  UF = rep(ufs_unicas, each = 1),
  Ano = 2015,
  encarcerados = 0
)

# Combinar com os dados originais
dados_uf_ano_zero <- bind_rows(dados_base_zero, dados_uf_ano)

# Gráfico com facet por UF, incluindo 2015 como ponto inicial com 0
grafico_evolucao_por_uf <- dados_uf_ano_zero %>%
  ggplot(aes(x = Ano, y = encarcerados, group = UF)) +
  geom_line(color = "#1b9e77", size = 1) +
  geom_point(color = "#d95f02", size = 2) +
  facet_wrap(~ UF, scales = "free_y", ncol = 3) +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_x_continuous(breaks = 2016:2024) +
  labs(
    title = "Evolução do Número de Encarcerados por Unidade Federativa",
    subtitle = "",
    x = "Ano",
    y = "Número de Encarcerados",
    caption = "Fonte: SISDEPEN"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1, face = "italic")
  )

print(grafico_evolucao_por_uf)
