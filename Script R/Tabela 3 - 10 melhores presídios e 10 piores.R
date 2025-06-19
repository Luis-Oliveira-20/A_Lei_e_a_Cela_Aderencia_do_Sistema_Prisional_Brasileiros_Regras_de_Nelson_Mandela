
library(dplyr)
library(writexl)

# Selecionar colunas e calcular os extremos
tabela_icpr_extremos <- df_com_ICPR_NM_2016_a_2024 %>%
  select(UF, `Nome do Estabelecimento`, ICPR_NM) %>%
  distinct() %>%
  filter(!is.na(ICPR_NM)) 

# 10 melhores
melhores_presidios <- tabela_icpr_extremos %>%
  arrange(desc(ICPR_NM)) %>%
  slice_head(n = 10) %>%
  mutate(Classificação = "Melhores")

# 10 piores
piores_presidios <- tabela_icpr_extremos %>%
  arrange(ICPR_NM) %>%
  slice_head(n = 10) %>%
  mutate(Classificação = "Piores")

# Juntar
presidios_extremos <- bind_rows(melhores_presidios, piores_presidios)

# Salvar como Excel
write_xlsx(presidios_extremos, "presidios_extremos_icpr_nm.xlsx")
