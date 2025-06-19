
library(dplyr)
library(writexl)

# Criar tabela com estatísticas descritivas por Ano
tabela_estatisticas_icpr <- df_com_ICPR_NM_2016_a_2024 %>%
  group_by(Ano) %>%
  summarise(
    `Número de Presídios` = n(),
    `Nota Mínima` = min(ICPR_NM, na.rm = TRUE),
    `Nota Máxima` = max(ICPR_NM, na.rm = TRUE),
    `Média` = mean(ICPR_NM, na.rm = TRUE),
    `Desvio Padrão` = sd(ICPR_NM, na.rm = TRUE)
  ) %>%
  arrange(Ano)

# Visualizar a tabela
print(tabela_estatisticas_icpr)


# Salvar a tabela como arquivo Excel
write_xlsx(tabela_estatisticas_icpr, "estatisticas_icpr_nm_2016_a_2024.xlsx")
