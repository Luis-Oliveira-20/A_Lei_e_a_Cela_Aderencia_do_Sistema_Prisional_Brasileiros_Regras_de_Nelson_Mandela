#####################################################################################
############## Figura 4 - Gráfico de Linha - Quantidade de vagas ####################
##########    disponíveis  e quantidade de encarcerados no Brasil ###################
#####################################################################################

library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

# 1. Preparar os dados
dados_linha_por_ano <-  %>%
  rename(
    capacidade_masc = `1.3 Capacidade do estabelecimento | Masculino | Total`,
    capacidade_fem  = `1.3 Capacidade do estabelecimento | Feminino | Total`,
    populacao_total = `4.1 População prisional | Total`
  ) %>%
  mutate(
    capacidade_masc = as.numeric(gsub(",", "", capacidade_masc)),
    capacidade_fem  = as.numeric(gsub(",", "", capacidade_fem)),
    populacao_total = as.numeric(gsub(",", "", populacao_total)),
    capacidade_total = capacidade_masc + capacidade_fem
  ) %>%
  filter(
    Ano %in% 2016:2024,
    grepl("/2$", Referência)  # só segundo semestre
  ) %>%
  group_by(UF, Ano) %>%
  summarise(
    vagas_disponiveis = sum(capacidade_total, na.rm = TRUE),
    encarcerados = sum(populacao_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(vagas_disponiveis, encarcerados),
    names_to = "tipo",
    values_to = "quantidade"
  ) %>%
  mutate(
    tipo = recode(tipo,
                  "vagas_disponiveis" = "Capacidade Prisional",
                  "encarcerados" = "Encarcerados"),
    UF = fct_reorder(UF, quantidade, .fun = sum, .desc = FALSE)
  )

# 2. Gráfico com 1 painel por ano (facets)
ggplot(dados_linha_por_ano, aes(x = UF, y = quantidade, group = tipo, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_color_manual(
    values = c("Capacidade Prisional" = "#1b9e77", "Encarcerados" = "#d95f02"),
    name = NULL
  ) +
  facet_wrap(~ Ano, ncol = 3) +
  labs(
    title = "Capacidade Prisional e Encarcerados por Unidade Federativa",
    subtitle = "",
    x = "",
    y = "Quantidade de pessoas",
    caption = "Fonte: SISDEPEN (2016–2024)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12)
  )





library(dplyr)

SISDEPEN <- SISDEPEN %>%
  filter(`Tipo do Estabelecimento` == "Cela física")


library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

# 1. Filtrar apenas "Cela física"
SISDEPEN_CELA_FISICA <- SISDEPEN %>%
  filter(`Tipo do Estabelecimento` == "Cela física")

# 2. Preparar os dados
dados_linha_por_ano <- SISDEPEN_CELA_FISICA %>%
  rename(
    capacidade_masc = `1.3 Capacidade do estabelecimento | Masculino | Total`,
    capacidade_fem  = `1.3 Capacidade do estabelecimento | Feminino | Total`,
    populacao_total = `4.1 População prisional | Total`
  ) %>%
  mutate(
    capacidade_masc = as.numeric(gsub(",", "", capacidade_masc)),
    capacidade_fem  = as.numeric(gsub(",", "", capacidade_fem)),
    populacao_total = as.numeric(gsub(",", "", populacao_total)),
    capacidade_total = capacidade_masc + capacidade_fem
  ) %>%
  filter(
    Ano %in% 2016:2024,
    grepl("/2$", Referência)  # apenas segundo semestre
  ) %>%
  group_by(UF, Ano) %>%
  summarise(
    vagas_disponiveis = sum(capacidade_total, na.rm = TRUE),
    encarcerados = sum(populacao_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(vagas_disponiveis, encarcerados),
    names_to = "tipo",
    values_to = "quantidade"
  ) %>%
  mutate(
    tipo = recode(tipo,
                  "vagas_disponiveis" = "Capacidade Prisional",
                  "encarcerados" = "Encarcerados"),
    UF = fct_reorder(UF, quantidade, .fun = sum, .desc = FALSE)
  )

# 3. Gráfico com painéis por ano (facets)
ggplot(dados_linha_por_ano, aes(x = UF, y = quantidade, group = tipo, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_color_manual(
    values = c("Capacidade Prisional" = "#1b9e77", "Encarcerados" = "#d95f02"),
    name = NULL
  ) +
  facet_wrap(~ Ano, ncol = 3) +
  labs(
    title = "Capacidade Prisional e Encarcerados por Unidade Federativa",
    subtitle = "",
    x = "",
    y = "Quantidade de pessoas",
    caption = "Fonte: SISDEPEN"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12)
  )




