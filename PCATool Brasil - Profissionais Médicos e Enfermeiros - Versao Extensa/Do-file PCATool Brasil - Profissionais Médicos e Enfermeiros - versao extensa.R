# ================================================================
# PCATool - CÁLCULO DOS ESCORES
# Questionário Profissional: Médico e Enfermeiro
# Versão Extensa em R
# ================================================================


# ================================================================
# PACOTES
# ================================================================

library(dplyr)


# ================================================================
# LISTA DE TODOS OS ITENS DO INSTRUMENTO
# ================================================================

itens <- c(
  paste0("a", 1:9),
  paste0("b", 1:13),
  paste0("c", 1:6),
  paste0("d", 1:8),
  paste0("e", 1:22),
  paste0("f", 1:18),
  paste0("g", 1:14),
  paste0("h", 1:21)
)


# ================================================================
# CRIAR CÓPIAS DOS ITENS PARA ANÁLISE
#
# As variáveis originais não serão modificadas.
# As cópias terão o prefixo p_
# ================================================================

for (v in itens) {
  
  banco[[paste0("p_", v)]] <- banco[[v]]
  
}


# ================================================================
# TRANSFORMAR 9 = NÃO SEI/NÃO LEMBRO EM NA
#
# O código 9 é considerado ausente.
# ================================================================

for (v in itens) {
  
  banco[[paste0("p_", v)]][
    banco[[paste0("p_", v)]] == 9
  ] <- NA
  
}


# ================================================================
# INVERSÃO DO ITEM A9
#
# 4 -> 1
# 3 -> 2
# 2 -> 3
# 1 -> 4
#
# Fórmula:
# novo valor = 5 - valor original
# ================================================================

banco$p_a9 <- ifelse(
  banco$p_a9 %in% 1:4,
  5 - banco$p_a9,
  banco$p_a9
)


# ================================================================
# FUNÇÃO PARA CALCULAR OS COMPONENTES
#
# Regra:
#
# - conta os missing;
# - se missing < 50% dos itens:
#       substitui missing por 2;
#       calcula a média;
#
# - se missing >= 50%:
#       escore = NA.
#
# IMPORTANTE:
# A função recebe uma condição de aplicabilidade.
# ================================================================

pcat_score <- function(data, vars, condicao = rep(TRUE, nrow(data))) {
  
  n <- length(vars)
  
  # Número de missing antes da imputação
  nmiss <- rowSums(
    is.na(data[, vars, drop = FALSE])
  )
  
  # Cria cópia dos itens
  dados_itens <- data[, vars, drop = FALSE]
  
  # Critério: menos de 50% missing
  pode_calcular <- condicao & (nmiss < (n / 2))
  
  # Imputação de missing para 2
  for (v in vars) {
    
    dados_itens[[v]][
      pode_calcular & is.na(dados_itens[[v]])
    ] <- 2
    
  }
  
  # Média
  escore <- rowMeans(
    dados_itens,
    na.rm = TRUE
  )
  
  # Quando não é aplicável, retorna NA
  escore[!condicao] <- NA
  
  # Quando >= 50% missing, retorna NA
  escore[nmiss >= (n / 2)] <- NA
  
  return(
    list(
      escore = escore,
      nmiss = nmiss
    )
  )
}


# ================================================================
# COMPONENTE A
#
# A1-A9 = 9 itens
# ================================================================

resultado_A <- pcat_score(
  banco,
  paste0("p_a", 1:9)
)

banco$escore_A <- resultado_A$escore
banco$nmiss_A <- resultado_A$nmiss


# ================================================================
# COMPONENTE B
#
# B1-B13 = 13 itens
# ================================================================

resultado_B <- pcat_score(
  banco,
  paste0("p_b", 1:13)
)

banco$escore_B <- resultado_B$escore
banco$nmiss_B <- resultado_B$nmiss


# ================================================================
# COMPONENTE C
#
# C1-C6 = 6 itens
# ================================================================

resultado_C <- pcat_score(
  banco,
  paste0("p_c", 1:6)
)

banco$escore_C <- resultado_C$escore
banco$nmiss_C <- resultado_C$nmiss


# ================================================================
# COMPONENTE D
#
# D1-D8 = 8 itens
# ================================================================

resultado_D <- pcat_score(
  banco,
  paste0("p_d", 1:8)
)

banco$escore_D <- resultado_D$escore
banco$nmiss_D <- resultado_D$nmiss


# ================================================================
# COMPONENTE E
#
# E1-E22 = 22 itens
# ================================================================

resultado_E <- pcat_score(
  banco,
  paste0("p_e", 1:22)
)

banco$escore_E <- resultado_E$escore
banco$nmiss_E <- resultado_E$nmiss


# ================================================================
# BLOCO F
#
# REGRAS CORRETAS DO QUESTIONÁRIO
#
# F1-F3:
#   todas as idades
#   apenas adultos
#   apenas crianças
#
# F4-F13:
#   todas as idades
#   apenas adultos
#
# F14:
#   somente todas as idades
#
# F15-F18:
#   todas as idades
#   apenas crianças
#
#
# IMPORTANTE:
#
# É permitido:
#
# todas as idades = NÃO
# apenas adultos = SIM
# apenas crianças = SIM
#
# Isso NÃO é inconsistência.
#
# Nesse caso:
# F1-F3  -> aplicável
# F4-F13 -> aplicável
# F14    -> NÃO aplicável
# F15-F18 -> aplicável
# ================================================================


# ================================================================
# CRIAR VARIÁVEIS DE FILTRO
# ================================================================

banco$atende_todas <- banco$atendepcttodasasidades == 1

banco$atende_adultos <- banco$atendeapenasadulto == 1

banco$atende_criancas <- banco$atendeapenascriancas == 1


# ================================================================
# CONFERÊNCIA DOS FILTROS
# ================================================================

cat("\n============================================================\n")
cat("CONFERÊNCIA DOS FILTROS DO BLOCO F\n")
cat("============================================================\n\n")

print(table(
  banco$atendepcttodasasidades,
  useNA = "ifany"
))

print(table(
  banco$atendeapenasadulto,
  useNA = "ifany"
))

print(table(
  banco$atendeapenascriancas,
  useNA = "ifany"
))


# ================================================================
# F1-F3
#
# Aplicável para:
#
# - todas as idades
# - apenas adultos
# - apenas crianças
#
# Portanto:
#
# atende adultos OU atende crianças
#
# ================================================================

cond_F1_F3 <-
  banco$atende_adultos |
  banco$atende_criancas


resultado_F1_F3 <- pcat_score(
  banco,
  paste0("p_f", 1:3),
  condicao = cond_F1_F3
)

banco$escore_F1_F3 <- resultado_F1_F3$escore
banco$nmiss_F1_F3 <- resultado_F1_F3$nmiss


# ================================================================
# F4-F13
#
# Aplicável para:
#
# - todas as idades
# - apenas adultos
#
# Portanto:
#
# atende adultos
#
# ================================================================

cond_F4_F13 <-
  banco$atende_adultos


resultado_F4_F13 <- pcat_score(
  banco,
  paste0("p_f", 4:13),
  condicao = cond_F4_F13
)

banco$escore_F4_F13 <- resultado_F4_F13$escore
banco$nmiss_F4_F13 <- resultado_F4_F13$nmiss


# ================================================================
# F14
#
# Aplicável SOMENTE quando:
#
# todas as idades = SIM
# ================================================================

cond_F14 <-
  banco$atende_todas


resultado_F14 <- pcat_score(
  banco,
  "p_f14",
  condicao = cond_F14
)

banco$escore_F14 <- resultado_F14$escore
banco$nmiss_F14 <- resultado_F14$nmiss


# ================================================================
# F15-F18
#
# Aplicável para:
#
# - todas as idades
# - apenas crianças
#
# Portanto:
#
# atende crianças
#
# ================================================================

cond_F15_F18 <-
  banco$atende_criancas


resultado_F15_F18 <- pcat_score(
  banco,
  paste0("p_f", 15:18),
  condicao = cond_F15_F18
)

banco$escore_F15_F18 <- resultado_F15_F18$escore
banco$nmiss_F15_F18 <- resultado_F15_F18$nmiss


# ================================================================
# ESCORE F FINAL
#
# Como os conjuntos de perguntas dependem do filtro,
# o cálculo do F será feito diretamente com todos os itens
# aplicáveis a cada profissional.
#
# A regra de missing é:
#
# - considera somente os itens que são aplicáveis;
# - se >= 50% dos itens aplicáveis estiverem missing,
#   o escore F fica NA;
# - se < 50%, os missing são imputados para 2.
# ================================================================


# ---------------------------------------------------------------
# Criar matriz dos itens F
# ---------------------------------------------------------------

F <- banco[, paste0("p_f", 1:18)]


# ---------------------------------------------------------------
# Definir quais itens são aplicáveis para cada profissional
# ---------------------------------------------------------------

aplicavel_F <- matrix(
  FALSE,
  nrow = nrow(banco),
  ncol = 18
)

colnames(aplicavel_F) <- paste0("p_f", 1:18)


# ---------------------------------------------------------------
# F1-F3
# ---------------------------------------------------------------

aplicavel_F[, 1:3] <-
  banco$atende_adultos |
  banco$atende_criancas


# ---------------------------------------------------------------
# F4-F13
# ---------------------------------------------------------------

aplicavel_F[, 4:13] <-
  banco$atende_adultos


# ---------------------------------------------------------------
# F14
# ---------------------------------------------------------------

aplicavel_F[, 14] <-
  banco$atende_todas


# ---------------------------------------------------------------
# F15-F18
# ---------------------------------------------------------------

aplicavel_F[, 15:18] <-
  banco$atende_criancas


# ================================================================
# NÚMERO DE ITENS APLICÁVEIS
# ================================================================

n_aplicaveis_F <-
  rowSums(aplicavel_F)


# ================================================================
# NÚMERO DE MISSING ENTRE OS ITENS APLICÁVEIS
# ================================================================

nmiss_F <- numeric(nrow(banco))

for (i in seq_len(nrow(banco))) {
  
  itens_aplicaveis <- aplicavel_F[i, ]
  
  if (sum(itens_aplicaveis) > 0) {
    
    nmiss_F[i] <-
      sum(is.na(F[i, itens_aplicaveis]))
    
  } else {
    
    nmiss_F[i] <- NA
    
  }
  
}


# ================================================================
# IMPUTAÇÃO DOS MISSING DO BLOCO F
#
# Se missing < 50% dos itens aplicáveis:
#
# missing -> 2
# ================================================================

F_calculo <- F


for (i in seq_len(nrow(banco))) {
  
  itens_aplicaveis <- aplicavel_F[i, ]
  
  n_itens <- sum(itens_aplicaveis)
  
  if (n_itens > 0) {
    
    if (nmiss_F[i] < (n_itens / 2)) {
      
      for (j in which(itens_aplicaveis)) {
        
        if (is.na(F_calculo[i, j])) {
          
          F_calculo[i, j] <- 2
          
        }
        
      }
      
    }
    
  }
  
}


# ================================================================
# CALCULAR ESCORE F
# ================================================================

escore_F <- rep(NA_real_, nrow(banco))


for (i in seq_len(nrow(banco))) {
  
  itens_aplicaveis <- aplicavel_F[i, ]
  
  n_itens <- sum(itens_aplicaveis)
  
  if (n_itens > 0) {
    
    # Menos de 50% de missing
    if (nmiss_F[i] < (n_itens / 2)) {
      
      escore_F[i] <-
        mean(
          F_calculo[i, itens_aplicaveis],
          na.rm = TRUE
        )
      
    }
    
  }
  
}


banco$escore_F <- escore_F
banco$nmiss_F <- nmiss_F


# ================================================================
# CRIAR TIPO DE ATENDIMENTO
#
# Esta variável é apenas descritiva.
#
# 1 = Todas as idades
# 2 = Somente adultos
# 3 = Somente crianças
# 4 = Adultos + crianças, sem marcar todas as idades
# ================================================================

banco$tipo_F <- NA_integer_


banco$tipo_F[
  banco$atende_todas
] <- 1


banco$tipo_F[
  !banco$atende_todas &
    banco$atende_adultos &
    !banco$atende_criancas
] <- 2


banco$tipo_F[
  !banco$atende_todas &
    !banco$atende_adultos &
    banco$atende_criancas
] <- 3


banco$tipo_F[
  !banco$atende_todas &
    banco$atende_adultos &
    banco$atende_criancas
] <- 4


banco$tipo_F_label <- factor(
  banco$tipo_F,
  levels = 1:4,
  labels = c(
    "Todas as idades",
    "Somente adultos",
    "Somente crianças",
    "Adultos + crianças"
  )
)


# ================================================================
# COMPONENTE G
#
# G1-G14
# ================================================================

resultado_G <- pcat_score(
  banco,
  paste0("p_g", 1:14)
)

banco$escore_G <- resultado_G$escore
banco$nmiss_G <- resultado_G$nmiss


# ================================================================
# COMPONENTE H
#
# H1-H21
# ================================================================

resultado_H <- pcat_score(
  banco,
  paste0("p_h", 1:21)
)

banco$escore_H <- resultado_H$escore
banco$nmiss_H <- resultado_H$nmiss


# ================================================================
# ESCORES DOS COMPONENTES
#
# A-H = escala 1 a 4
# ================================================================

cat("\n============================================================\n")
cat("ESCORES DOS COMPONENTES - ESCALA 1 A 4\n")
cat("============================================================\n\n")

print(
  summary(
    banco[, c(
      "escore_A",
      "escore_B",
      "escore_C",
      "escore_D",
      "escore_E",
      "escore_F",
      "escore_G",
      "escore_H"
    )]
  )
)


# ================================================================
# COMPONENTES ESSENCIAIS
#
# A
# B
# C
# D
# E
# F
#
# Se 3 ou mais forem missing:
#   escore essencial = NA
#
# Caso contrário:
#   média dos componentes disponíveis
# ================================================================

componentes_essenciais <- c(
  "escore_A",
  "escore_B",
  "escore_C",
  "escore_D",
  "escore_E",
  "escore_F"
)


banco$ncomp_missing_essencial <-
  rowSums(
    is.na(
      banco[, componentes_essenciais, drop = FALSE]
    )
  )


banco$escore_essencial <-
  rowMeans(
    banco[, componentes_essenciais, drop = FALSE],
    na.rm = TRUE
  )


banco$escore_essencial[
  banco$ncomp_missing_essencial >= 3
] <- NA


# ================================================================
# COMPONENTES DERIVADOS
#
# G
# H
# ================================================================

componentes_derivados <- c(
  "escore_G",
  "escore_H"
)


banco$ncomp_missing_derivado <-
  rowSums(
    is.na(
      banco[, componentes_derivados, drop = FALSE]
    )
  )


banco$escore_derivado <-
  rowMeans(
    banco[, componentes_derivados, drop = FALSE],
    na.rm = TRUE
  )


banco$escore_derivado[
  banco$ncomp_missing_derivado >= 2
] <- NA


# ================================================================
# ESCORE GERAL DA APS
#
# A-H
#
# Se 4 ou mais componentes forem missing:
#   escore geral = NA
#
# Caso contrário:
#   média dos componentes disponíveis
# ================================================================

componentes_geral <- c(
  "escore_A",
  "escore_B",
  "escore_C",
  "escore_D",
  "escore_E",
  "escore_F",
  "escore_G",
  "escore_H"
)


banco$ncomp_missing_geral <-
  rowSums(
    is.na(
      banco[, componentes_geral, drop = FALSE]
    )
  )


banco$escore_geral <-
  rowMeans(
    banco[, componentes_geral, drop = FALSE],
    na.rm = TRUE
  )


banco$escore_geral[
  banco$ncomp_missing_geral >= 4
] <- NA


# ================================================================
# TRANSFORMAÇÃO DOS COMPONENTES PARA ESCALA 0-10
#
# Fórmula:
#
# (escore - 1) / 3 * 10
# ================================================================

componentes <- c(
  "A", "B", "C", "D",
  "E", "F", "G", "H"
)


for (x in componentes) {
  
  banco[[paste0("escore_", x, "_10")]] <-
    (banco[[paste0("escore_", x)]] - 1) /
    3 * 10
  
}


# ================================================================
# TRANSFORMAÇÃO DO ESCORE ESSENCIAL
# ================================================================

banco$escore_essencial_10 <-
  (banco$escore_essencial - 1) /
  3 * 10


# ================================================================
# TRANSFORMAÇÃO DO ESCORE DERIVADO
# ================================================================

banco$escore_derivado_10 <-
  (banco$escore_derivado - 1) /
  3 * 10


# ================================================================
# TRANSFORMAÇÃO DO ESCORE GERAL
# ================================================================

banco$escore_geral_10 <-
  (banco$escore_geral - 1) /
  3 * 10


# ================================================================
# TRANSFORMAÇÃO DOS ITENS PARA ESCALA 0-10
# ================================================================

for (v in itens) {
  
  banco[[paste0("p10_", v)]] <-
    (banco[[paste0("p_", v)]] - 1) /
    3 * 10
  
}


# ================================================================
# CONFERÊNCIA DO BLOCO F
# ================================================================

cat("\n============================================================\n")
cat("CONFERÊNCIA DO BLOCO F\n")
cat("============================================================\n\n")


cat("\nTipo de atendimento:\n")

print(
  table(
    banco$tipo_F_label,
    useNA = "ifany"
  )
)


cat("\nEscore F:\n")

print(
  summary(
    banco$escore_F
  )
)


cat("\nNúmero de missing no F:\n")

print(
  summary(
    banco$nmiss_F
  )
)


# ================================================================
# CONFERÊNCIA DA APLICABILIDADE DAS QUESTÕES F
# ================================================================

cat("\n============================================================\n")
cat("APLICABILIDADE DAS QUESTÕES DO BLOCO F\n")
cat("============================================================\n\n")


cat("F1-F3: adultos OU crianças\n")
cat("F4-F13: adultos\n")
cat("F14: todas as idades\n")
cat("F15-F18: crianças\n\n")


cat("Quantidade de profissionais para cada conjunto:\n\n")


cat(
  "F1-F3 aplicável: ",
  sum(cond_F1_F3, na.rm = TRUE),
  "\n"
)


cat(
  "F4-F13 aplicável: ",
  sum(cond_F4_F13, na.rm = TRUE),
  "\n"
)


cat(
  "F14 aplicável: ",
  sum(cond_F14, na.rm = TRUE),
  "\n"
)


cat(
  "F15-F18 aplicável: ",
  sum(cond_F15_F18, na.rm = TRUE),
  "\n"
)


# ================================================================
# CONFERÊNCIA DOS ESCORES PRINCIPAIS
# ================================================================

cat("\n============================================================\n")
cat("ESCORES PRINCIPAIS\n")
cat("============================================================\n\n")


print(
  summary(
    banco[, c(
      "escore_essencial",
      "escore_geral",
      "escore_essencial_10",
      "escore_geral_10"
    )]
  )
)


# ================================================================
# MISSING DOS COMPONENTES
# ================================================================

cat("\n============================================================\n")
cat("MISSING POR COMPONENTE\n")
cat("============================================================\n\n")


print(
  summary(
    banco[, c(
      "nmiss_A",
      "nmiss_B",
      "nmiss_C",
      "nmiss_D",
      "nmiss_E",
      "nmiss_F",
      "nmiss_G",
      "nmiss_H"
    )]
  )
)


# ================================================================
# QUANTOS COMPONENTES ESSENCIAIS ESTÃO AUSENTES
# ================================================================

cat("\n============================================================\n")
cat("COMPONENTES ESSENCIAIS AUSENTES\n")
cat("============================================================\n\n")


print(
  table(
    banco$ncomp_missing_essencial,
    useNA = "ifany"
  )
)


# ================================================================
# QUANTOS COMPONENTES ESTÃO AUSENTES NO ESCORE GERAL
# ================================================================

cat("\n============================================================\n")
cat("COMPONENTES AUSENTES NO ESCORE GERAL\n")
cat("============================================================\n\n")


print(
  table(
    banco$ncomp_missing_geral,
    useNA = "ifany"
  )
)


# ================================================================
# COMPARAÇÃO DO ESCORE F POR TIPO DE ATENDIMENTO
# ================================================================

cat("\n============================================================\n")
cat("ESCORE F POR TIPO DE ATENDIMENTO\n")
cat("============================================================\n\n")


resultado_F_por_tipo <-
  banco %>%
  group_by(tipo_F_label) %>%
  summarise(
    n = n(),
    n_escore = sum(!is.na(escore_F)),
    media = mean(escore_F, na.rm = TRUE),
    desvio_padrao = sd(escore_F, na.rm = TRUE),
    mediana = median(escore_F, na.rm = TRUE),
    minimo = min(escore_F, na.rm = TRUE),
    maximo = max(escore_F, na.rm = TRUE)
  )


print(resultado_F_por_tipo)


# ================================================================
# 43. CONFERÊNCIA FINAL
# ================================================================

cat("\n============================================================\n")
cat("CONFERÊNCIA FINAL DOS ESCORES\n")
cat("============================================================\n\n")


print(
  banco %>%
    summarise(
      n = n(),
      
      escore_A_n = sum(!is.na(escore_A)),
      escore_B_n = sum(!is.na(escore_B)),
      escore_C_n = sum(!is.na(escore_C)),
      escore_D_n = sum(!is.na(escore_D)),
      escore_E_n = sum(!is.na(escore_E)),
      escore_F_n = sum(!is.na(escore_F)),
      escore_G_n = sum(!is.na(escore_G)),
      escore_H_n = sum(!is.na(escore_H)),
      
      essencial_n =
        sum(!is.na(escore_essencial)),
      
      geral_n =
        sum(!is.na(escore_geral))
    )
)


# ================================================================
# FIM DO SCRIPT
# ================================================================
