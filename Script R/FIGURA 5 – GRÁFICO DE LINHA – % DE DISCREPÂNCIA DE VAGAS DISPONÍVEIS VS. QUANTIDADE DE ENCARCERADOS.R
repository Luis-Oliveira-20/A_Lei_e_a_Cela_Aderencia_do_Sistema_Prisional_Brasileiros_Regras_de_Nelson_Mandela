#################################################################################
########## FIGURA 5 – GRÁFICO DE LINHA – % DE DISCREPÂNCIA DE ################### 
##########  VAGAS DISPONÍVEIS VS. QUANTIDADE DE ENCARCERADOS  ###################
#################################################################################

library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(scales)

# Preparar os dados
dados_percentuais <- SISDEPEN %>%
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
    grepl("/2$", Referência)
  ) %>%
  group_by(UF, Ano) %>%
  summarise(
    vagas_disponiveis = sum(capacidade_total, na.rm = TRUE),
    encarcerados = sum(populacao_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percentual_ocupacao = round((encarcerados / vagas_disponiveis) * 100, 1),
    status = ifelse(percentual_ocupacao > 100, "Acima de 100%", "Até 100%"),
    UF = fct_reorder(UF, percentual_ocupacao, .fun = mean)
  )

# Gráfico por UF: Facetado com linha

ggplot(dados_percentuais, aes(x = Ano, y = percentual_ocupacao, group = UF)) +
  geom_line(color = "gray40", linewidth = 1) +
  geom_point(aes(color = status), size = 2) +
  facet_wrap(~ UF, scales = "free_y", ncol = 4) +  # ← Aqui está o ajuste
  scale_color_manual(
    values = c("Acima de 100%" = "#d95f02", "Até 100%" = "#1b9e77"),
    name = "Lotação"
  ) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray40") +
  scale_x_continuous(breaks = 2016:2024) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, NA)) +
  labs(
    title = "Percentual de Ocupação do Sistema Prisional",
    subtitle = "",
    x = "Ano",
    y = "Percentual de Ocupação (%)",
    caption = "Fonte: SISDEPEN"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

