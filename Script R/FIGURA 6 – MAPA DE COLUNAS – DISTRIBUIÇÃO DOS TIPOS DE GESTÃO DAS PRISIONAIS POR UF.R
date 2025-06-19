
library(dplyr)
library(ggplot2)

# Renomear variável para facilitar (opcional)
SISDEPEN <- SISDEPEN %>%
  rename(gestao = `1.4 Gestão do estabelecimento`)

# Agrupar e contar por UF e tipo de gestão
tabela_gestao <- SISDEPEN %>%
  group_by(UF, gestao) %>%
  summarise(total = n(), .groups = "drop")

# Criar o gráfico de barras (colunas)
ggplot(tabela_gestao, aes(x = UF, y = total, fill = gestao)) +
  geom_col(position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Set2") +  # ou Set3, Paired etc.
  labs(title = "Tipos de Gestão por UF",
       x = "Unidade Federativa (UF)",
       y = "Número de Estabelecimentos",
       fill = "Gestão") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



library(dplyr)
library(ggplot2)

# Renomear variável para facilitar (caso ainda não tenha feito)
SISDEPEN <- SISDEPEN %>%
  rename(
    gestao = `1.4 Gestão do estabelecimento`
  )

# Filtrar os anos desejados e agrupar
tabela_gestao_anos <- SISDEPEN %>%
  filter(Ano %in% 2016:2024) %>%
  group_by(UF, gestao, Ano) %>%
  summarise(total = n(), .groups = "drop")

# Criar gráfico com um painel por ano
ggplot(tabela_gestao_anos, aes(x = UF, y = total, fill = gestao)) +
  geom_col(position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Tipos de Gestão por Unidade Federativa",
       x = "",
       y = "",
       fill = "Gestão") +
  theme_minimal(base_size = 13, base_family = "Arial") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Arial"),
    axis.title = element_text(family = "Arial", face = "bold"),
    plot.title = element_text(family = "Arial", face = "bold", size = 16, hjust = 0.5),
    legend.title = element_text(family = "Arial", face = "bold")
  ) +
  facet_wrap(~ Ano, ncol = 2)
