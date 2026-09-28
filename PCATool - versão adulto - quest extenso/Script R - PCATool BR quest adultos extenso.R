# ================================================================
# PCATOOL-BRASIL 2020 - ADULTOS - versão extensa
# Conversão do código Stata para R
#
# Componentes:
# A = Afiliação
# B = Acesso de Primeiro Contato - Utilização
# C = Acesso de Primeiro Contato - Acessibilidade
# D = Longitudinalidade
# E = Coordenação - Integração de Cuidados
# F = Coordenação - Sistema de Informações
# G = Integralidade - Serviços Disponíveis
# H = Integralidade - Serviços Prestados
# I = Orientação Familiar
# J = Orientação Comunitária
#
# Regras:
# 1. Inversão C9-C12 e D14
# 2. Valor 9 = "Não sei/Não lembro"
# 3. Missing + 9 para regra de perda >= 50%
# 4. Se <50% ausentes: 9 -> 2
# 5. Se >=50% ausentes: escore do componente = NA
# 6. H depende do sexo
# 7. Escores Essencial e Geral com regras de perda
# 8. Transformação final para escala 0-10
# ================================================================


# ================================================================
# PACOTES
# ================================================================

# Instale os pacotes uma única vez, se necessário:
# install.packages(c("haven", "dplyr", "tidyr"))

library(haven)
library(dplyr)
library(tidyr)


# ================================================================
# IMPORTAR BANCO DE DADOS
# ================================================================

dados <- read_dta(
  "/Users/brunaventurin/Downloads/pcatooladultoextenso.dta"
)


# ================================================================
# LISTA DE ITENS
# ================================================================

itens <- c(
  "a1", "a2", "a3",
  
  "b1", "b2", "b3",
  
  paste0("c", 1:12),
  
  paste0("d", 1:14),
  
  paste0("e", 1:9),
  
  paste0("f", 1:3),
  
  paste0("g", 1:22),
  
  paste0("h", 1:13),
  
  paste0("i", 1:3),
  
  paste0("j", 1:6)
)


# ================================================================
# VERIFICAÇÃO DAS VARIÁVEIS
# ================================================================

variaveis_faltantes <- setdiff(itens, names(dados))

if (length(variaveis_faltantes) > 0) {
  
  warning(
    paste(
      "ERRO: as seguintes variáveis não foram encontradas:",
      paste(variaveis_faltantes, collapse = ", ")
    )
  )
  
} else {
  
  message("Todas as variáveis necessárias foram encontradas.")
  
}


# ================================================================
# CÓPIA DAS VARIÁVEIS ORIGINAIS
#
# Não altera as respostas originais.
# Cria calc_* para o cálculo.
# ================================================================

dados <- dados %>%
  mutate(
    across(
      all_of(intersect(itens, names(dados))),
      ~ as.numeric(.x),
      .names = "calc_{.col}"
    )
  )


# ================================================================
# INVERSÃO DOS ITENS
#
# C9, C10, C11, C12 e D14
#
# 4 -> 1
# 3 -> 2
# 2 -> 3
# 1 -> 4
#
# O valor 9 permanece 9.
# ================================================================

itens_invertidos <- c(
  "c9", "c10", "c11", "c12", "d14"
)

for (var in itens_invertidos) {
  
  calc_var <- paste0("calc_", var)
  
  dados[[calc_var]] <- ifelse(
    !is.na(dados[[calc_var]]) &
      dados[[calc_var]] >= 1 &
      dados[[calc_var]] <= 4,
    5 - dados[[calc_var]],
    dados[[calc_var]]
  )
}


# ================================================================
# FUNÇÃO AUXILIAR PARA CÁLCULO DOS COMPONENTES
#
# Regra:
#
# ausentes = NA + 9
#
# Se ausentes >= 50%:
#     escore = NA
#
# Se ausentes < 50%:
#     9 -> 2
#     média dos itens válidos
# ================================================================

calcular_componente <- function(data, variaveis, limite) {
  
  # Número de itens ausentes:
  # NA + valor 9
  
  nmiss <- rowSums(
    sapply(
      variaveis,
      function(v) {
        is.na(data[[v]]) | data[[v]] == 9
      }
    ),
    na.rm = TRUE
  )
  
  
  # Cópia dos valores para imputação
  valores <- data[, variaveis, drop = FALSE]
  
  
  # Quando há menos de 50% de ausentes,
  # substituir 9 por 2
  
  for (v in variaveis) {
    
    valores[[v]][
      valores[[v]] == 9 & nmiss < limite
    ] <- 2
  }
  
  
  # Média dos itens
  escore <- rowMeans(
    valores,
    na.rm = TRUE
  )
  
  
  # Se >= 50% ausentes, escore = NA
  escore[nmiss >= limite] <- NA_real_
  
  
  list(
    nmiss = nmiss,
    escore = escore
  )
}


# ================================================================
# COMPONENTE B
#
# B = (B1 + B2 + B3) / 3
#
# 3 itens
# 50% = 1,5
#
# 0 ou 1 ausente -> calcula
# 2 ou 3 ausentes -> NA
# ================================================================

resultado_b <- calcular_componente(
  dados,
  paste0("calc_b", 1:3),
  limite = 1.5
)

dados$nmiss_b <- resultado_b$nmiss
dados$esc_b <- resultado_b$escore


# ================================================================
# 8. COMPONENTE C
#
# C = média dos 12 itens
#
# 12 itens
# 50% = 6
# ================================================================

resultado_c <- calcular_componente(
  dados,
  paste0("calc_c", 1:12),
  limite = 6
)

dados$nmiss_c <- resultado_c$nmiss
dados$esc_c <- resultado_c$escore


# ================================================================
# COMPONENTE D
#
# D = média dos 14 itens
#
# 14 itens
# 50% = 7
# ================================================================

resultado_d <- calcular_componente(
  dados,
  paste0("calc_d", 1:14),
  limite = 7
)

dados$nmiss_d <- resultado_d$nmiss
dados$esc_d <- resultado_d$escore


# ================================================================
# COMPONENTE E
#
# E1 NÃO entra no cálculo.
# São utilizados E2-E9.
#
# 8 itens
# 50% = 4
# ================================================================

resultado_e <- calcular_componente(
  dados,
  paste0("calc_e", 2:9),
  limite = 4
)

dados$nmiss_e <- resultado_e$nmiss
dados$esc_e <- resultado_e$escore


# ================================================================
# COMPONENTE F
#
# F = média de F1-F3
#
# 3 itens
# 50% = 1,5
# ================================================================

resultado_f <- calcular_componente(
  dados,
  paste0("calc_f", 1:3),
  limite = 1.5
)

dados$nmiss_f <- resultado_f$nmiss
dados$esc_f <- resultado_f$escore


# ================================================================
# COMPONENTE G
#
# G = média de G1-G22
#
# 22 itens
# 50% = 11
# ================================================================

resultado_g <- calcular_componente(
  dados,
  paste0("calc_g", 1:22),
  limite = 11
)

dados$nmiss_g <- resultado_g$nmiss
dados$esc_g <- resultado_g$escore


# ================================================================
# COMPONENTE H — SERVIÇOS PRESTADOS
#
# Mulheres:
# H1-H13
#
# Homens:
# H1-H11
#
# sexo:
# 1 = feminino
# 2 = masculino
# ================================================================

if (!"sexo" %in% names(dados)) {
  
  warning(
    "A variável 'sexo' não foi encontrada. ",
    "Verifique o nome/codificação da variável no banco."
  )
  
  dados$esc_h <- NA_real_
  
} else {
  
  # --------------------------------------------------------------
  # H - FEMININO
  # --------------------------------------------------------------
  
  resultado_hf <- calcular_componente(
    dados,
    paste0("calc_h", 1:13),
    limite = 6.5
  )
  
  dados$nmiss_hf <- resultado_hf$nmiss
  dados$esc_hf <- resultado_hf$escore
  
  
  # --------------------------------------------------------------
  # H - MASCULINO
  # --------------------------------------------------------------
  
  resultado_hm <- calcular_componente(
    dados,
    paste0("calc_h", 1:11),
    limite = 5.5
  )
  
  dados$nmiss_hm <- resultado_hm$nmiss
  dados$esc_hm <- resultado_hm$escore
  
  
  # --------------------------------------------------------------
  # ESCOLHA SEGUNDO SEXO
  # --------------------------------------------------------------
  
  dados$esc_h <- NA_real_
  
  dados$esc_h[dados$sexo == 1] <-
    dados$esc_hf[dados$sexo == 1]
  
  dados$esc_h[dados$sexo == 2] <-
    dados$esc_hm[dados$sexo == 2]
}


# ================================================================
# COMPONENTE I
#
# I = média de I1-I3
#
# 3 itens
# 50% = 1,5
# ================================================================

resultado_i <- calcular_componente(
  dados,
  paste0("calc_i", 1:3),
  limite = 1.5
)

dados$nmiss_i <- resultado_i$nmiss
dados$esc_i <- resultado_i$escore


# ================================================================
# COMPONENTE J
#
# J = média de J1-J6
#
# 6 itens
# 50% = 3
# ================================================================

resultado_j <- calcular_componente(
  dados,
  paste0("calc_j", 1:6),
  limite = 3
)

dados$nmiss_j <- resultado_j$nmiss
dados$esc_j <- resultado_j$escore


# ================================================================
#  COMPONENTE A — AFILIAÇÃO
# ================================================================

dados$esc_a <- NA_real_


# ---------------------------------------------------------------
# CASO 1:
# A1 = NÃO, A2 = NÃO, A3 = NÃO
# ---------------------------------------------------------------

dados$esc_a[
  dados$a1 == 0 &
    dados$a2 == 0 &
    dados$a3 == 0
] <- 1


# ---------------------------------------------------------------
# UMA resposta SIM
# grau de afiliação = 2
# ---------------------------------------------------------------

dados$esc_a[
  is.na(dados$esc_a) &
    (
      (
        dados$a1 == 1 &
          dados$a2 == 0 &
          dados$a3 == 0
      ) |
        (
          dados$a1 == 0 &
            dados$a2 > 0 &
            dados$a3 == 0
        ) |
        (
          dados$a1 == 0 &
            dados$a2 == 0 &
            dados$a3 > 0
        )
    )
] <- 2


# ---------------------------------------------------------------
# A1 = SIM
#
# A2 = 1
# A3 = 1
# -> os três são o mesmo serviço
# -> grau 4
# ---------------------------------------------------------------

dados$esc_a[
  dados$a1 == 1 &
    dados$a2 == 1 &
    dados$a3 == 1
] <- 4


# ---------------------------------------------------------------
# Grau 3
# ---------------------------------------------------------------

dados$esc_a[
  dados$a1 == 1 &
    dados$a2 == 2 &
    dados$a3 == 1
] <- 3

dados$esc_a[
  dados$a1 == 1 &
    dados$a2 == 2 &
    dados$a3 == 2
] <- 3

dados$esc_a[
  dados$a1 == 1 &
    dados$a2 == 1 &
    dados$a3 == 2
] <- 3


# ---------------------------------------------------------------
# A1 = SIM
# A2 = serviço diferente
# A3 = diferente de A1 e A2
# -> grau 2
# ---------------------------------------------------------------

dados$esc_a[
  dados$a1 == 1 &
    dados$a2 == 2 &
    dados$a3 == 4
] <- 2


# ---------------------------------------------------------------
# A1 = NÃO
# A2 = SIM, serviço diferente
# A3 = mesmo serviço de A2
# -> grau 3
# ---------------------------------------------------------------

dados$esc_a[
  dados$a1 == 0 &
    dados$a2 == 2 &
    dados$a3 == 3
] <- 3


# ---------------------------------------------------------------
# A1 = NÃO
# A2 = SIM, serviço diferente
# A3 = diferente
# -> grau 2
# ---------------------------------------------------------------

dados$esc_a[
  dados$a1 == 0 &
    dados$a2 == 2 &
    dados$a3 == 4
] <- 2


# ---------------------------------------------------------------
# A1 = SIM
# A2 = NÃO
# A3 = somente o mesmo serviço de A1
# -> grau 3
# ---------------------------------------------------------------

dados$esc_a[
  dados$a1 == 1 &
    dados$a2 == 0 &
    dados$a3 == 2
] <- 3


# ---------------------------------------------------------------
# A1 = SIM
# A2 = NÃO
# A3 = diferente
# -> dois serviços diferentes
# -> grau 2
# ---------------------------------------------------------------

dados$esc_a[
  dados$a1 == 1 &
    dados$a2 == 0 &
    dados$a3 == 4
] <- 2


# ---------------------------------------------------------------
# A1 = NÃO
# A2 = SIM
# A3 = diferente
# -> dois serviços diferentes
# -> grau 2
# ---------------------------------------------------------------

dados$esc_a[
  dados$a1 == 0 &
    dados$a2 > 0 &
    dados$a3 == 4
] <- 2


# ---------------------------------------------------------------
# A1 = NÃO
# A2 = NÃO
# A3 = SIM
# -> somente uma resposta SIM
# -> grau 2
# ---------------------------------------------------------------

dados$esc_a[
  dados$a1 == 0 &
    dados$a2 == 0 &
    dados$a3 > 0
] <- 2


# ================================================================
# ESCORE ESSENCIAL DA APS
#
# A + B + C + D + E + F + G + H
#
# >= 4 componentes missing -> NA
#
# <= 3 missing -> média dos componentes disponíveis
# ================================================================

componentes_essenciais <- c(
  "esc_a",
  "esc_b",
  "esc_c",
  "esc_d",
  "esc_e",
  "esc_f",
  "esc_g",
  "esc_h"
)


dados$nmiss_essencial <- rowSums(
  is.na(dados[, componentes_essenciais])
)


dados$soma_essencial <- rowSums(
  dados[, componentes_essenciais],
  na.rm = TRUE
)


dados$esc_essencial <- NA_real_


dados$esc_essencial[
  dados$nmiss_essencial <= 3
] <- dados$soma_essencial[
  dados$nmiss_essencial <= 3
] /
  (
    8 -
      dados$nmiss_essencial[
        dados$nmiss_essencial <= 3
      ]
  )


# ================================================================
# ESCORE GERAL DA APS
#
# A + B + C + D + E + F + G + H + I + J
#
# >= 5 componentes missing -> NA
#
# <= 4 missing -> média dos componentes disponíveis
# ================================================================

componentes_gerais <- c(
  "esc_a",
  "esc_b",
  "esc_c",
  "esc_d",
  "esc_e",
  "esc_f",
  "esc_g",
  "esc_h",
  "esc_i",
  "esc_j"
)


dados$nmiss_geral <- rowSums(
  is.na(dados[, componentes_gerais])
)


dados$soma_geral <- rowSums(
  dados[, componentes_gerais],
  na.rm = TRUE
)


dados$esc_geral <- NA_real_


dados$esc_geral[
  dados$nmiss_geral <= 4
] <- dados$soma_geral[
  dados$nmiss_geral <= 4
] /
  (
    10 -
      dados$nmiss_geral[
        dados$nmiss_geral <= 4
      ]
  )


# ================================================================
# TRANSFORMAÇÃO PARA ESCALA 0-10
#
# Fórmula:
#
# (escore - 1) / 3 * 10
# ================================================================

escores <- c(
  "esc_a",
  "esc_b",
  "esc_c",
  "esc_d",
  "esc_e",
  "esc_f",
  "esc_g",
  "esc_h",
  "esc_i",
  "esc_j",
  "esc_essencial",
  "esc_geral"
)


for (var in escores) {
  
  nova_var <- paste0(var, "_10")
  
  dados[[nova_var]] <- ifelse(
    !is.na(dados[[var]]),
    ((dados[[var]] - 1) / 3) * 10,
    NA_real_
  )
}


# ================================================================
# RÓTULOS / LABELS
#
# O R não possui equivalente direto aos variable labels do Stata
# para data.frame base. O pacote haven permite trabalhar com labels.
# ================================================================

labels_escores <- c(
  esc_a_10 = "Afiliação - escala 0 a 10",
  esc_b_10 = "Acesso Utilização - escala 0 a 10",
  esc_c_10 = "Acesso Acessibilidade - escala 0 a 10",
  esc_d_10 = "Longitudinalidade - escala 0 a 10",
  esc_e_10 = "Coordenação Integração - escala 0 a 10",
  esc_f_10 = "Coordenação Informação - escala 0 a 10",
  esc_g_10 = "Integralidade Serviços Disponíveis - escala 0 a 10",
  esc_h_10 = "Integralidade Serviços Prestados - escala 0 a 10",
  esc_i_10 = "Orientação Familiar - escala 0 a 10",
  esc_j_10 = "Orientação Comunitária - escala 0 a 10",
  esc_essencial_10 = "Escore Essencial APS - escala 0 a 10",
  esc_geral_10 = "Escore Geral APS - escala 0 a 10"
)


for (var in names(labels_escores)) {
  
  attr(
    dados[[var]],
    "label"
  ) <- labels_escores[[var]]
}


# ================================================================
# LABELS DOS COMPONENTES
# ================================================================

labels_componentes <- c(
  esc_a = "Afiliação",
  esc_b = "Acesso de Primeiro Contato - Utilização (1-4)",
  esc_c = "Acesso de Primeiro Contato - Acessibilidade (1-4)",
  esc_d = "Longitudinalidade (1-4)",
  esc_e = "Coordenação - Integração de Cuidados (1-4)",
  esc_f = "Coordenação - Sistema de Informações (1-4)",
  esc_g = "Integralidade - Serviços Disponíveis (1-4)",
  esc_h = "Integralidade - Serviços Prestados (1-4)",
  esc_hf = "H - Serviços Prestados - Feminino",
  esc_hm = "H - Serviços Prestados - Masculino",
  esc_i = "Orientação Familiar (1-4)",
  esc_j = "Orientação Comunitária (1-4)",
  esc_essencial = "Escore Essencial da APS - escala 1 a 4",
  esc_geral = "Escore Geral da APS - escala 1 a 4"
)


for (var in names(labels_componentes)) {
  
  if (var %in% names(dados)) {
    
    attr(
      dados[[var]],
      "label"
    ) <- labels_componentes[[var]]
    
  }
}


# ================================================================
# LABELS DOS DIAGNÓSTICOS
# ================================================================

labels_nmiss <- c(
  nmiss_b = "N ausentes - B",
  nmiss_c = "N ausentes - C",
  nmiss_d = "N ausentes - D",
  nmiss_e = "N ausentes - E",
  nmiss_f = "N ausentes - F",
  nmiss_g = "N ausentes - G",
  nmiss_hf = "N ausentes - H feminino",
  nmiss_hm = "N ausentes - H masculino",
  nmiss_i = "N ausentes - I",
  nmiss_j = "N ausentes - J",
  nmiss_essencial = "N componentes essenciais missing",
  nmiss_geral = "N componentes gerais missing"
)


for (var in names(labels_nmiss)) {
  
  if (var %in% names(dados)) {
    
    attr(
      dados[[var]],
      "label"
    ) <- labels_nmiss[[var]]
    
  }
}


# ================================================================
# CHECAGEM DOS ESCORES
# ================================================================

cat("\n")
cat("==========================================================\n")
cat("CHECAGEM DOS ESCORES\n")
cat("==========================================================\n")


componentes <- c(
  "esc_a",
  "esc_b",
  "esc_c",
  "esc_d",
  "esc_e",
  "esc_f",
  "esc_g",
  "esc_h",
  "esc_i",
  "esc_j",
  "esc_essencial",
  "esc_geral"
)


print(
  summary(
    dados[, componentes]
  )
)


# ================================================================
# ESCORES 0-10
# ================================================================

cat("\n")
cat("----------------------------------------------------------\n")
cat("ESCORE 0-10\n")
cat("----------------------------------------------------------\n")


componentes_10 <- c(
  "esc_a_10",
  "esc_b_10",
  "esc_c_10",
  "esc_d_10",
  "esc_e_10",
  "esc_f_10",
  "esc_g_10",
  "esc_h_10",
  "esc_i_10",
  "esc_j_10",
  "esc_essencial_10",
  "esc_geral_10"
)


print(
  summary(
    dados[, componentes_10]
  )
)


# ================================================================
# CHECAR VALORES FORA DA ESCALA 1-4
# ================================================================

for (var in componentes) {
  
  n_menor_1 <- sum(
    dados[[var]] < 1,
    na.rm = TRUE
  )
  
  n_maior_4 <- sum(
    dados[[var]] > 4,
    na.rm = TRUE
  )
  
  
  if (n_menor_1 > 0) {
    
    warning(
      paste0(
        var,
        ": existem ",
        n_menor_1,
        " valores menores que 1."
      )
    )
    
  }
  
  
  if (n_maior_4 > 0) {
    
    warning(
      paste0(
        var,
        ": existem ",
        n_maior_4,
        " valores maiores que 4."
      )
    )
    
  }
}


# ================================================================
# DISTRIBUIÇÃO DA AFILIAÇÃO
# ================================================================

cat("\n")
cat("==========================================================\n")
cat("DISTRIBUIÇÃO DA AFILIAÇÃO\n")
cat("==========================================================\n")


print(
  table(
    dados$esc_a,
    useNA = "ifany"
  )
)


# ================================================================
# 27. QUANTIDADE DE COMPONENTES MISSING
# ================================================================

cat("\n")
cat("==========================================================\n")
cat("COMPONENTES MISSING\n")
cat("==========================================================\n")


print(
  table(
    dados$nmiss_essencial,
    useNA = "ifany"
  )
)


print(
  table(
    dados$nmiss_geral,
    useNA = "ifany"
  )
)


# ================================================================
# TABELA RESUMIDA DOS ESCORES
#
# N
# Média
# Desvio-padrão
# Mínimo
# Mediana
# Máximo
# ================================================================

estatisticas <- data.frame(
  
  n = sapply(
    dados[, componentes_10],
    function(x) sum(!is.na(x))
  ),
  
  mean = sapply(
    dados[, componentes_10],
    function(x) mean(x, na.rm = TRUE)
  ),
  
  sd = sapply(
    dados[, componentes_10],
    function(x) sd(x, na.rm = TRUE)
  ),
  
  min = sapply(
    dados[, componentes_10],
    function(x) min(x, na.rm = TRUE)
  ),
  
  p50 = sapply(
    dados[, componentes_10],
    function(x) median(x, na.rm = TRUE)
  ),
  
  max = sapply(
    dados[, componentes_10],
    function(x) max(x, na.rm = TRUE)
  )
)


print(
  round(
    estatisticas,
    3
  )
)


# ================================================================
# SALVAR BANCO FINAL
# ================================================================

# Para salvar como RDS:
#
# saveRDS(
#   dados,
#   "pcatool_adulto_resultados.rds"
# )


# Para salvar como CSV:
#
# write.csv(
#   dados,
#   "pcatool_adulto_resultados.csv",
#   row.names = FALSE,
#   na = ""
# )


# Para salvar novamente como Stata:
#
# write_dta(
#   dados,
#   "pcatool_adulto_resultados.dta"
# )


# ================================================================
# FIM
# ================================================================