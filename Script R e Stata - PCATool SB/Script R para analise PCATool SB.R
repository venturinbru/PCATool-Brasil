# ============================================================
# PCATool - Saúde Bucal para Profissionais Dentistas
# QUESTIONÁRIO COM TODOS OS CAMPOS OBRIGATÓRIOS
# Conversão do script Stata para R
# ============================================================


# ============================================================
# 0. PACOTES
# ============================================================

# Instale os pacotes uma única vez, se necessário:
# install.packages(c("haven", "dplyr", "tidyr", "ggplot2", "readr"))

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)


# ============================================================
# 1. IMPORTAR O BANCO DE DADOS
# ============================================================

# ------------------------------------------------------------
# OPÇÃO 1: Banco Stata (.dta)
# ------------------------------------------------------------

dados <- read_dta("caminho/do/banco.dta")


# ------------------------------------------------------------
# OPÇÃO 2: Banco Excel (.xlsx)
# Caso seu banco seja Excel, use:
#
# install.packages("readxl")
# library(readxl)
# dados <- read_excel("caminho/do/banco.xlsx")
# ------------------------------------------------------------


# Visualizar as primeiras linhas
head(dados)


# ============================================================
# 2. EXCLUIR VARIÁVEIS
# ============================================================

variaveis_excluir <- c(
  "excluir",
  "excluir2",
  "excluir3",
  "excluir4",
  "excluir5",
  "excluir6",
  "excluir7",
  "excluir8",
  "excluir9"
)

# Excluir somente as variáveis que realmente existem no banco
dados <- dados %>%
  select(-any_of(variaveis_excluir))


# ============================================================
# 3. DESCREVER O BANCO
# ============================================================

# Estrutura do banco
str(dados)

# Dimensões do banco
dim(dados)

# Nomes das variáveis
names(dados)

# Resumo das variáveis
summary(dados)


# ============================================================
# 4. CODEBOOK
# ============================================================

# Informações gerais das variáveis
glimpse(dados)

# Para visualizar classes e quantidade de NA
codebook <- data.frame(
  variavel = names(dados),
  classe = sapply(dados, class),
  n_missing = sapply(dados, function(x) sum(is.na(x))),
  n = sapply(dados, length)
)

print(codebook)


# ============================================================
# 5. CONVERSÃO DAS VARIÁVEIS DOS ITENS PARA NUMÉRICO
# ============================================================

# Caso as variáveis tenham sido importadas como labelled pelo Stata,
# esta etapa garante que os itens sejam numéricos.

itens <- c(
  paste0("a", 1:7),
  paste0("b", 1:13),
  paste0("c", 1:5),
  paste0("d", 1:3),
  paste0("e", 1:23),
  paste0("f", 1:7),
  paste0("g", 1:4),
  paste0("h", 1:13),
  paste0("i", 1:6)
)

itens_existentes <- intersect(itens, names(dados))

dados[itens_existentes] <- lapply(
  dados[itens_existentes],
  function(x) as.numeric(x)
)


# ============================================================
# 6. 1º PASSO - INVERSÃO DO ITEM A7
# ============================================================

# Escala original:
# 4 = 1
# 3 = 2
# 2 = 3
# 1 = 4
#
# Equação:
# novo A7 = 5 - A7

dados <- dados %>%
  mutate(
    a7 = ifelse(
      a7 %in% c(1, 2, 3, 4),
      5 - a7,
      a7
    )
  )


# Conferir A7
table(dados$a7, useNA = "ifany")


# ============================================================
# 7. FUNÇÃO PARA CALCULAR AS ESCALAS
# ============================================================

# Esta função reproduz a lógica utilizada no Stata:
#
# 1. Conta NA e valores 9 como ausentes.
# 2. Se o número de ausentes for menor que 50%:
#       - transforma 9 em 2;
#       - calcula a média dos itens.
# 3. Se o número de ausentes for >= 50%:
#       - escore = NA.
#
# Importante:
# Os pontos de corte seguem exatamente o seu Stata:
#
# A: < 3.5
# B: < 6.5
# C: < 2.5
# D: < 1.5
# E: < 11.5
# F: < 3.5
# G: < 2
# H: < 6.5
# I: < 3


calcular_escala <- function(dados, itens, nome_escala, ponto_corte) {
  
  # Verificar se todas as variáveis existem
  itens_faltantes <- setdiff(itens, names(dados))
  
  if (length(itens_faltantes) > 0) {
    stop(
      paste(
        "As seguintes variáveis não foram encontradas:",
        paste(itens_faltantes, collapse = ", ")
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Contagem de valores ausentes
  # NA + valor 9
  # ----------------------------------------------------------
  
  dados[[paste0("nmiss_", nome_escala)]] <- apply(
    dados[itens],
    1,
    function(x) {
      sum(is.na(x) | x == 9)
    }
  )
  
  
  # ----------------------------------------------------------
  # Transformar 9 em 2 quando < 50% ausentes
  # ----------------------------------------------------------
  
  permitido <- dados[[paste0("nmiss_", nome_escala)]] < ponto_corte
  
  for (item in itens) {
    dados[[item]][
      dados[[item]] == 9 &
        permitido
    ] <- 2
  }
  
  
  # ----------------------------------------------------------
  # Calcular escore
  # ----------------------------------------------------------
  
  dados[[paste0("escore_", nome_escala)]] <- NA_real_
  
  dados[[paste0("escore_", nome_escala)]][permitido] <-
    rowMeans(
      dados[permitido, itens, drop = FALSE],
      na.rm = FALSE
    )
  
  
  return(dados)
}


# ============================================================
# 8. ESCALA A
# Acesso de Primeiro Contato - Acessibilidade
# 7 itens: A1-A7
# ============================================================

dados <- calcular_escala(
  dados = dados,
  itens = paste0("a", 1:7),
  nome_escala = "A",
  ponto_corte = 3.5
)

table(dados$nmiss_A, useNA = "ifany")

summary(dados$escore_A)


# Histograma
ggplot(dados, aes(x = escore_A)) +
  geom_histogram(
    binwidth = 0.25,
    color = "black",
    fill = "steelblue",
    na.rm = TRUE
  ) +
  labs(
    title = "Histograma - Escore A",
    x = "Escore A",
    y = "Frequência"
  ) +
  theme_minimal()


# ============================================================
# 9. ESCALA B
# Longitudinalidade
# 13 itens: B1-B13
# ============================================================

dados <- calcular_escala(
  dados = dados,
  itens = paste0("b", 1:13),
  nome_escala = "B",
  ponto_corte = 6.5
)

table(dados$nmiss_B, useNA = "ifany")

summary(dados$escore_B)


ggplot(dados, aes(x = escore_B)) +
  geom_histogram(
    binwidth = 0.25,
    color = "black",
    fill = "steelblue",
    na.rm = TRUE
  ) +
  labs(
    title = "Histograma - Escore B",
    x = "Escore B",
    y = "Frequência"
  ) +
  theme_minimal()


# ============================================================
# 10. ESCALA C
# Coordenação - Integração de Cuidados
# 5 itens: C1-C5
# ============================================================

dados <- calcular_escala(
  dados = dados,
  itens = paste0("c", 1:5),
  nome_escala = "C",
  ponto_corte = 2.5
)

table(dados$nmiss_C, useNA = "ifany")

summary(dados$escore_C)


ggplot(dados, aes(x = escore_C)) +
  geom_histogram(
    binwidth = 0.25,
    color = "black",
    fill = "steelblue",
    na.rm = TRUE
  ) +
  labs(
    title = "Histograma - Escore C",
    x = "Escore C",
    y = "Frequência"
  ) +
  theme_minimal()


# ============================================================
# 11. ESCALA D
# Coordenação - Sistema de Informações
# 3 itens: D1-D3
# ============================================================

dados <- calcular_escala(
  dados = dados,
  itens = paste0("d", 1:3),
  nome_escala = "D",
  ponto_corte = 1.5
)

table(dados$nmiss_D, useNA = "ifany")

summary(dados$escore_D)


ggplot(dados, aes(x = escore_D)) +
  geom_histogram(
    binwidth = 0.25,
    color = "black",
    fill = "steelblue",
    na.rm = TRUE
  ) +
  labs(
    title = "Histograma - Escore D",
    x = "Escore D",
    y = "Frequência"
  ) +
  theme_minimal()


# ============================================================
# 12. ESCALA E
# Integralidade - Serviços Disponíveis
# 23 itens: E1-E23
# ============================================================

dados <- calcular_escala(
  dados = dados,
  itens = paste0("e", 1:23),
  nome_escala = "E",
  ponto_corte = 11.5
)

table(dados$nmiss_E, useNA = "ifany")

summary(dados$escore_E)


ggplot(dados, aes(x = escore_E)) +
  geom_histogram(
    binwidth = 0.25,
    color = "black",
    fill = "steelblue",
    na.rm = TRUE
  ) +
  labs(
    title = "Histograma - Escore E",
    x = "Escore E",
    y = "Frequência"
  ) +
  theme_minimal()


# ============================================================
# 13. ESCALA F
# Integralidade - Serviços Prestados
# 7 itens: F1-F7
# ============================================================

dados <- calcular_escala(
  dados = dados,
  itens = paste0("f", 1:7),
  nome_escala = "F",
  ponto_corte = 3.5
)

table(dados$nmiss_F, useNA = "ifany")

summary(dados$escore_F)


ggplot(dados, aes(x = escore_F)) +
  geom_histogram(
    binwidth = 0.25,
    color = "black",
    fill = "steelblue",
    na.rm = TRUE
  ) +
  labs(
    title = "Histograma - Escore F",
    x = "Escore F",
    y = "Frequência"
  ) +
  theme_minimal()


# ============================================================
# 14. ESCALA G
# Orientação Familiar
# 4 itens: G1-G4
# ============================================================

dados <- calcular_escala(
  dados = dados,
  itens = paste0("g", 1:4),
  nome_escala = "G",
  ponto_corte = 2
)

table(dados$nmiss_G, useNA = "ifany")

summary(dados$escore_G)


ggplot(dados, aes(x = escore_G)) +
  geom_histogram(
    binwidth = 0.25,
    color = "black",
    fill = "steelblue",
    na.rm = TRUE
  ) +
  labs(
    title = "Histograma - Escore G",
    x = "Escore G",
    y = "Frequência"
  ) +
  theme_minimal()


# ============================================================
# 15. ESCALA H
# Orientação Comunitária
# 13 itens: H1-H13
# ============================================================

dados <- calcular_escala(
  dados = dados,
  itens = paste0("h", 1:13),
  nome_escala = "H",
  ponto_corte = 6.5
)

table(dados$nmiss_H, useNA = "ifany")

summary(dados$escore_H)


ggplot(dados, aes(x = escore_H)) +
  geom_histogram(
    binwidth = 0.25,
    color = "black",
    fill = "steelblue",
    na.rm = TRUE
  ) +
  labs(
    title = "Histograma - Escore H",
    x = "Escore H",
    y = "Frequência"
  ) +
  theme_minimal()


# ============================================================
# 16. ESCALA I
# Competência Cultural
# 6 itens: I1-I6
# ============================================================

dados <- calcular_escala(
  dados = dados,
  itens = paste0("i", 1:6),
  nome_escala = "I",
  ponto_corte = 3
)

table(dados$nmiss_I, useNA = "ifany")

summary(dados$escore_I)


ggplot(dados, aes(x = escore_I)) +
  geom_histogram(
    binwidth = 0.25,
    color = "black",
    fill = "steelblue",
    na.rm = TRUE
  ) +
  labs(
    title = "Histograma - Escore I",
    x = "Escore I",
    y = "Frequência"
  ) +
  theme_minimal()


# ============================================================
# 17. VISUALIZAR OS ESCORES
# ============================================================

escores_componentes <- c(
  "escore_A",
  "escore_B",
  "escore_C",
  "escore_D",
  "escore_E",
  "escore_F",
  "escore_G",
  "escore_H",
  "escore_I"
)

summary(dados[escores_componentes])


# Estatísticas descritivas mais completas
estatisticas_componentes <- data.frame(
  variavel = escores_componentes,
  n = sapply(dados[escores_componentes], function(x) sum(!is.na(x))),
  missing = sapply(dados[escores_componentes], function(x) sum(is.na(x))),
  media = sapply(dados[escores_componentes], mean, na.rm = TRUE),
  dp = sapply(dados[escores_componentes], sd, na.rm = TRUE),
  mediana = sapply(dados[escores_componentes], median, na.rm = TRUE),
  minimo = sapply(dados[escores_componentes], min, na.rm = TRUE),
  maximo = sapply(dados[escores_componentes], max, na.rm = TRUE)
)

print(estatisticas_componentes)


# ============================================================
# 18. ESCORE ESSENCIAL DA APS EM SAÚDE BUCAL
#
# Componentes:
# A, B, C, D, E, F
# ============================================================

componentes_essenciais <- c(
  "escore_A",
  "escore_B",
  "escore_C",
  "escore_D",
  "escore_E",
  "escore_F"
)


# Número de componentes ausentes
dados$essencial_nmiss <- rowSums(
  is.na(dados[componentes_essenciais])
)


# Soma dos componentes
dados$essencial_soma <- rowSums(
  dados[componentes_essenciais],
  na.rm = TRUE
)


# Escore essencial
#
# Regra:
# máximo de 2 componentes ausentes

dados$escore_essencial <- ifelse(
  dados$essencial_nmiss <= 2,
  dados$essencial_soma /
    (6 - dados$essencial_nmiss),
  NA_real_
)


# Conferência
table(dados$essencial_nmiss, useNA = "ifany")

summary(dados$essencial_soma)

summary(dados$escore_essencial)


# ============================================================
# 19. ESCORE GERAL DA APS EM SAÚDE BUCAL
#
# Componentes:
# A, B, C, D, E, F, G, H, I
# ============================================================

componentes_geral <- c(
  "escore_A",
  "escore_B",
  "escore_C",
  "escore_D",
  "escore_E",
  "escore_F",
  "escore_G",
  "escore_H",
  "escore_I"
)


# Número de componentes ausentes
dados$geral_nmiss <- rowSums(
  is.na(dados[componentes_geral])
)


# Soma dos componentes
dados$geral_soma <- rowSums(
  dados[componentes_geral],
  na.rm = TRUE
)


# Escore geral
#
# Regra:
# máximo de 3 componentes ausentes

dados$escore_geral <- ifelse(
  dados$geral_nmiss <= 3,
  dados$geral_soma /
    (9 - dados$geral_nmiss),
  NA_real_
)


# Conferência
table(dados$geral_nmiss, useNA = "ifany")

summary(dados$geral_soma)

summary(dados$escore_geral)


# ============================================================
# 20. TRANSFORMAÇÃO DOS COMPONENTES PARA ESCALA 0 A 10
# ============================================================

# Fórmula:
#
# ((escore - 1) / 3) * 10
#
# Escala original: 1 a 4
# Nova escala:     0 a 10


for (x in LETTERS[1:9]) {
  
  dados[[paste0("escore_", x, "_0_10")]] <-
    ((dados[[paste0("escore_", x)]] - 1) / 3) * 10
}


# ============================================================
# 21. TRANSFORMAÇÃO DO ESCORE ESSENCIAL PARA 0 A 10
# ============================================================

dados$escore_essencial_0_10 <-
  ((dados$escore_essencial - 1) / 3) * 10


table(
  cut(
    dados$escore_essencial_0_10,
    breaks = seq(0, 10, by = 1),
    include.lowest = TRUE
  ),
  useNA = "ifany"
)

summary(dados$escore_essencial_0_10)


# ============================================================
# 22. TRANSFORMAÇÃO DO ESCORE GERAL PARA 0 A 10
# ============================================================

dados$escore_geral_0_10 <-
  ((dados$escore_geral - 1) / 3) * 10


table(
  cut(
    dados$escore_geral_0_10,
    breaks = seq(0, 10, by = 1),
    include.lowest = TRUE
  ),
  useNA = "ifany"
)

summary(dados$escore_geral_0_10)


# ============================================================
# 23. VERIFICAÇÃO DOS ESCORES DOS COMPONENTES
# ============================================================

escores_originais <- c(
  "escore_A",
  "escore_B",
  "escore_C",
  "escore_D",
  "escore_E",
  "escore_F",
  "escore_G",
  "escore_H",
  "escore_I",
  "escore_essencial",
  "escore_geral"
)

summary(dados[escores_originais])


# ============================================================
# 24. VERIFICAÇÃO DOS ESCORES 0 A 10
# ============================================================

escores_0_10 <- c(
  "escore_A_0_10",
  "escore_B_0_10",
  "escore_C_0_10",
  "escore_D_0_10",
  "escore_E_0_10",
  "escore_F_0_10",
  "escore_G_0_10",
  "escore_H_0_10",
  "escore_I_0_10",
  "escore_essencial_0_10",
  "escore_geral_0_10"
)

summary(dados[escores_0_10])


# ============================================================
# 25. TABELA FINAL DE ESTATÍSTICAS DESCRITIVAS
# ============================================================

estatisticas_finais <- data.frame(
  variavel = escores_0_10,
  n = sapply(
    dados[escores_0_10],
    function(x) sum(!is.na(x))
  ),
  missing = sapply(
    dados[escores_0_10],
    function(x) sum(is.na(x))
  ),
  media = sapply(
    dados[escores_0_10],
    mean,
    na.rm = TRUE
  ),
  dp = sapply(
    dados[escores_0_10],
    sd,
    na.rm = TRUE
  ),
  mediana = sapply(
    dados[escores_0_10],
    median,
    na.rm = TRUE
  ),
  minimo = sapply(
    dados[escores_0_10],
    min,
    na.rm = TRUE
  ),
  maximo = sapply(
    dados[escores_0_10],
    max,
    na.rm = TRUE
  )
)

print(estatisticas_finais)


# ============================================================
# 26. VISUALIZAÇÃO DOS ESCORES 0 A 10
# ============================================================

dados_longos <- dados %>%
  select(all_of(escores_0_10)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "escala",
    values_to = "escore"
  )


ggplot(
  dados_longos,
  aes(x = escore)
) +
  geom_histogram(
    bins = 20,
    color = "black",
    fill = "steelblue",
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ escala,
    scales = "free"
  ) +
  labs(
    title = "Distribuição dos Escores - Escala 0 a 10",
    x = "Escore",
    y = "Frequência"
  ) +
  theme_minimal()


# ============================================================
# 27. EXPORTAR BANCO FINAL
# ============================================================

# ------------------------------------------------------------
# Salvar como CSV
# ------------------------------------------------------------

write_csv(
  dados,
  "PCATool_Saude_Bucal_resultados.csv"
)


# ------------------------------------------------------------
# Salvar como Excel
# ------------------------------------------------------------

# Caso queira salvar em Excel:
#
# install.packages("writexl")
# library(writexl)
#
# write_xlsx(
#   dados,
#   "PCATool_Saude_Bucal_resultados.xlsx"
# )


# ------------------------------------------------------------
# Salvar como arquivo RDS
# ------------------------------------------------------------

saveRDS(
  dados,
  "PCATool_Saude_Bucal_resultados.rds"
)


# ============================================================
# FIM DO SCRIPT
# ============================================================
