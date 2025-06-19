#######################################################################################
######## TABELA 1- ESTATÍSTICA DESCRITIVA DE ENCARCERADOS NO BRASIL (2016-2024) #######
#######################################################################################

library(dplyr)
library(writexl)

# Verifique se o nome da variável está corretamente escrito:
# Use crase (`) se o nome tem espaços e caracteres especiais
estatisticas_ano <- SISDEPEN %>%
  filter(Ano %in% 2016:2024) %>%
  group_by(Ano) %>%
  summarise(
    N           = sum(!is.na(`4.1 População prisional | Total`)),
    Minimo      = min(`4.1 População prisional | Total`, na.rm = TRUE),
    Maximo      = max(`4.1 População prisional | Total`, na.rm = TRUE),
    Media       = mean(`4.1 População prisional | Total`, na.rm = TRUE),
    DesvioPadrao = sd(`4.1 População prisional | Total`, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  arrange(desc(Ano))  # Ordena do mais recente para o mais antigo

# Visualizar
View(estatisticas_ano)

# Exportar para Excel
write_xlsx(estatisticas_ano, "estatisticas_descritivas_populacao_prisional.xlsx")
