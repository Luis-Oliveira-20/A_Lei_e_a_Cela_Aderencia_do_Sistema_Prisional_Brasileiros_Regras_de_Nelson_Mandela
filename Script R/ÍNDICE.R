# Padronizar variáveis (0 a 1, onde 1 é condição ideal)
normalize <- function(x) {
  if (is.numeric(x)) {
    return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  } else if (is.factor(x) || is.character(x)) {
    return(as.numeric(as.factor(x)) - 1)
  } else {
    return(x)
  }
}

# Aplicar ao dataframe
df_normalizado <- as.data.frame(lapply(SISDEPEN_LIMPO_COM_AS_VARIAVEIS, normalize))


library(dplyr)

# Função robusta para recodificar todas as colunas como 0 e 1
recodificar_dummy <- function(x) {
  x <- as.character(x)  # força a tratar como texto
  return(ifelse(x == "Sim", 1, 0))  # Sim = 1, todo o resto = 0 (inclusive NA e "Não")
}

# Aplicar recodificação ao dataframe inteiro
df_normalizado <- as.data.frame(lapply(SISDEPEN_LIMPO_COM_AS_VARIAVEIS, recodificar_dummy))

# Vetor com colunas que não entram no índice
colunas_excluir <- c(
  "ciclo", "Ano", "Referência", "Tipo do Estabelecimento", "Situação de Preenchimento",
  "ID", "Nome do Estabelecimento", "UF", "Situação do Estabelecimento",
  "Outras Denominações", "Endereço", "Bairro", "CEP", "Âmbito",
  "Município", "Código IBGE", "Telefone Principal",
  "1.1 Estabelecimento originalmente destinado a pessoa privadas de liberdade do sexo"
)

# Selecionar somente as variáveis que entram no índice
variaveis_icpr <- setdiff(names(df_normalizado), colunas_excluir)

# Calcular o índice como média simples das variáveis dummy
df_normalizado <- df_normalizado %>%
  mutate(ICPR_NM = rowMeans(across(all_of(variaveis_icpr)), na.rm = TRUE))

# Ver os primeiros resultados
head(df_normalizado)

# Calcular média geral do índice
media_geral_icpr <- mean(df_normalizado$ICPR_NM, na.rm = TRUE)
print(media_geral_icpr)


library(writexl)

# Salvar em formato Excel (.xlsx)
write_xlsx(df_normalizado, "df_normalizado_com_ICPR_NM.xlsx")

library(readr)

write_csv(df_normalizado, "df_normalizado_com_ICPR_NM.csv")








library(dplyr)
library(readr)
library(writexl)

# 1. Filtrar apenas 'Cela física'
df_filtrado <- SISDEPEN_LIMPO_COM_AS_VARIAVEIS %>%
  filter(`Tipo do Estabelecimento` == "Cela física") %>%
  filter(Ano %in% 2016:2024)  # Garante apenas os anos desejados

# 2. Identificar colunas que devem ser excluídas do índice
colunas_excluir <- c(
  "ciclo", "Ano", "Referência", "Tipo do Estabelecimento", "Situação de Preenchimento",
  "ID", "Nome do Estabelecimento", "UF", "Situação do Estabelecimento",
  "Outras Denominações", "Endereço", "Bairro", "CEP", "Âmbito",
  "Município", "Código IBGE", "Telefone Principal",
  "1.1 Estabelecimento originalmente destinado a pessoa privadas de liberdade do sexo"
)

# 3. Separar as colunas que entram no índice
variaveis_icpr <- setdiff(names(df_filtrado), colunas_excluir)

# 4. Recodificar variáveis: Sim = 1, outro = 0
df_dummy <- df_filtrado %>%
  mutate(across(all_of(variaveis_icpr), ~ ifelse(as.character(.) == "Sim", 1, 0)))

# 5. Calcular o índice como média simples das variáveis dummy
df_com_icpr <- df_dummy %>%
  mutate(ICPR_NM = rowMeans(across(all_of(variaveis_icpr)), na.rm = TRUE))

# 6. Verificar distribuição de anos incluídos
print(table(df_com_icpr$Ano))

# 7. Salvar resultados
write_xlsx(df_com_icpr, "df_com_ICPR_NM_2016_a_2024.xlsx")
write_csv(df_com_icpr, "df_com_ICPR_NM_2016_a_2024.csv")

# 8. Exibir média geral do índice
media_geral_icpr <- mean(df_com_icpr$ICPR_NM, na.rm = TRUE)
print(media_geral_icpr)





