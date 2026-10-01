###############################################################################
###############################################################################
#
#     PCATOOL-BRASIL - SAÚDE BUCAL - PACIENTE ADULTO
#     VERSÃO EXTENSA
#
#     SCRIPT COMPLETO PARA ANÁLISE EM R
#
###############################################################################
#
#     Instrumento:
#     PCATool-Brasil - Saúde Bucal para paciente adulto, segundo Manual MS
#
#     Banco REDCap:
#     form_pcatoolbrasilsaudebucaladulto
#
###############################################################################

# =============================================================================
# PACOTES
# =============================================================================

# Instalar se necessário:
# install.packages(c("tidyverse", "haven", "labelled"))

library(tidyverse)
library(haven)
library(labelled)

options(dplyr.summarise.inform = FALSE)


# =============================================================================
# IMPORTAÇÃO DO BANCO
# =============================================================================

# ---------- Opção A: banco .dta ----------
#
# dados <- read_dta(
#   "C:/CAMINHO/pcatool_bucal_adulto.dta"
# )

# ---------- Opção B: banco .csv ----------
#
# dados <- read_csv(
#   "C:/CAMINHO/pcatool_bucal_adulto.csv",
#   show_col_types = FALSE
# )

# -----------------------------------------------------------------------------
# IMPORTANTE:
# Descomente UMA das opções acima.
# -----------------------------------------------------------------------------

# Exemplo:
# dados <- read_dta("pcatool_bucal_adulto.dta")


# =============================================================================
# IDENTIFICAÇÃO DA BASE
# =============================================================================

cat("\n")
cat("==============================================================\n")
cat(" PCATOOL-BRASIL - SAÚDE BUCAL - ADULTO\n")
cat(" Início da análise\n")
cat("==============================================================\n\n")


# =============================================================================
# CONFERÊNCIA DAS VARIÁVEIS PRINCIPAIS
# =============================================================================

vars_identificacao <- c(
  "record_id",
  "a1", "a2", "a3"
)

print(dados %>% select(all_of(vars_identificacao)) %>% glimpse())


# =============================================================================
# LISTA DE ITENS
# =============================================================================

B_items <- c(
  "b1", "b2", "b3"
)

C_items <- c(
  "c1", "c2", "c3", "c4", "c5", "c6",
  "c7", "c8", "c9", "c10", "c11", "c12"
)

D_items <- c(
  "d1", "d2", "d3", "d4", "d5",
  "d6", "d7", "d8", "d9", "d10",
  "d11", "d12", "d13", "d14", "d15"
)

E_items <- c(
  "e1", "e3", "e4", "e5", "e6",
  "e7", "e8", "e9", "e10"
)

F_items <- c(
  "f1", "f2", "f3"
)

G_items <- c(
  "g1", "g2", "g3", "g4", "g5",
  "g6", "g7", "g8", "g9", "g10",
  "g11", "g12", "g13", "g14", "g15",
  "g16", "g17", "g18", "g19", "g20",
  "g21", "g22", "g23"
)

H_items <- c(
  "h1", "h2", "h3", "h4", "h5",
  "h6", "h7", "h8", "h9"
)

I_items <- c(
  "i1", "i2", "i3"
)

J_items <- c(
  "j1", "j2", "j3", "j4", "j5", "j6"
)


# Todos os itens Likert
likert_items <- c(
  B_items,
  C_items,
  D_items,
  E_items,
  F_items,
  G_items,
  H_items,
  I_items,
  J_items
)


# =============================================================================
# VERIFICAÇÃO DA EXISTÊNCIA DAS VARIÁVEIS
# =============================================================================

vars_faltantes <- setdiff(
  c("record_id", "a1", "a2", "a3", likert_items),
  names(dados)
)

if (length(vars_faltantes) > 0) {
  stop(
    paste(
      "As seguintes variáveis não foram encontradas no banco:",
      paste(vars_faltantes, collapse = ", ")
    )
  )
}


# =============================================================================
# VERIFICAÇÃO DOS CÓDIGOS
# =============================================================================

cat("\n")
cat("==============================================================\n")
cat(" VERIFICAÇÃO DOS ITENS\n")
cat("==============================================================\n")


for (v in likert_items) {
  
  cat("\n---", v, "---\n")
  
  print(
    table(
      dados[[v]],
      useNA = "ifany"
    )
  )
}


# =============================================================================
# VERIFICAÇÃO DE VALORES INVÁLIDOS
# =============================================================================

cat("\n")
cat("==============================================================\n")
cat(" VALORES INVÁLIDOS NOS ITENS LIKERT\n")
cat("==============================================================\n")


for (v in likert_items) {
  
  valores_invalidos <- dados %>%
    filter(
      !is.na(.data[[v]]) &
        !(.data[[v]] %in% c(1, 2, 3, 4, 9))
    )
  
  if (nrow(valores_invalidos) > 0) {
    
    cat(
      "\nERRO:", v,
      "possui", nrow(valores_invalidos),
      "valor(es) inválido(s).\n"
    )
    
    print(
      valores_invalidos %>%
        select(record_id, all_of(v))
    )
  }
}


# =============================================================================
# AFILIAÇÃO
# =============================================================================

cat("\n")
cat("==============================================================\n")
cat(" AFILIAÇÃO\n")
cat("==============================================================\n")


# -----------------------------------------------------------------------------
# Inicialização
# -----------------------------------------------------------------------------

dados <- dados %>%
  mutate(
    esc_afiliacao = NA_real_,
    afiliacao_nsim = NA_real_,
    afiliacao_ndist = NA_real_,
    afiliacao_inconsistente = 0
  )


# -----------------------------------------------------------------------------
# Verificar códigos inválidos de A1
# -----------------------------------------------------------------------------

invalid_a1 <- !is.na(dados$a1) &
  !(dados$a1 %in% c(0, 1))

if (any(invalid_a1)) {
  
  cat("\nAtenção: A1 possui códigos inválidos.\n")
  
  print(
    dados %>%
      filter(invalid_a1) %>%
      select(record_id, a1)
  )
  
  dados$afiliacao_inconsistente[invalid_a1] <- 1
}


# -----------------------------------------------------------------------------
# Verificar códigos inválidos de A2
# -----------------------------------------------------------------------------

invalid_a2 <- !is.na(dados$a2) &
  !(dados$a2 %in% c(0, 1, 2))

if (any(invalid_a2)) {
  
  cat("\nAtenção: A2 possui códigos inválidos.\n")
  
  print(
    dados %>%
      filter(invalid_a2) %>%
      select(record_id, a2)
  )
  
  dados$afiliacao_inconsistente[invalid_a2] <- 1
}


# -----------------------------------------------------------------------------
# Verificar códigos inválidos de A3
# -----------------------------------------------------------------------------

invalid_a3 <- !is.na(dados$a3) &
  !(dados$a3 %in% c(0, 1, 2, 3, 4))

if (any(invalid_a3)) {
  
  cat("\nAtenção: A3 possui códigos inválidos.\n")
  
  print(
    dados %>%
      filter(invalid_a3) %>%
      select(record_id, a3)
  )
  
  dados$afiliacao_inconsistente[invalid_a3] <- 1
}


# -----------------------------------------------------------------------------
# Número de respostas SIM
# -----------------------------------------------------------------------------

dados <- dados %>%
  mutate(
    afiliacao_nsim = if_else(
      !is.na(a1) & !is.na(a2) & !is.na(a3),
      
      (a1 == 1) +
        (a2 %in% c(1, 2)) +
        (a3 %in% c(1, 2, 3, 4)),
      
      NA_real_
    )
  )


# =============================================================================
# 9. IDENTIFICAÇÃO DOS SERVIÇOS
# =============================================================================

dados <- dados %>%
  mutate(
    afil_s1 = NA_real_,
    afil_s2 = NA_real_,
    afil_s3 = NA_real_
  )


# -----------------------------------------------------------------------------
# Serviço indicado em A1
# -----------------------------------------------------------------------------

dados <- dados %>%
  mutate(
    afil_s1 = if_else(
      a1 == 1,
      1,
      NA_real_
    )
  )


# -----------------------------------------------------------------------------
# Serviço indicado em A2
#
# A2 = 1 -> mesmo serviço de A1
# A2 = 2 -> serviço diferente
# -----------------------------------------------------------------------------

dados <- dados %>%
  mutate(
    afil_s2 = case_when(
      
      a2 == 1 & a1 == 1 ~ 1,
      
      a2 == 2 ~ 2,
      
      TRUE ~ NA_real_
    )
  )


# -----------------------------------------------------------------------------
# Serviço indicado em A3
#
# A3 = 1 -> mesmo serviço de A1/A2
# A3 = 2 -> mesmo serviço de A1
# A3 = 3 -> mesmo serviço de A2
# A3 = 4 -> novo serviço
# -----------------------------------------------------------------------------

dados <- dados %>%
  mutate(
    afil_s3 = case_when(
      
      a3 == 1 & a1 == 1 & a2 == 1 ~ 1,
      
      a3 == 2 & a1 == 1 ~ 1,
      
      a3 == 3 & a2 == 1 ~ 1,
      
      a3 == 3 & a2 == 2 ~ 2,
      
      a3 == 4 ~ 3,
      
      TRUE ~ NA_real_
    )
  )


# =============================================================================
# CASOS EM QUE A1 NÃO POSSUI SERVIÇO
# =============================================================================

# A1=0 e A2=1 é inconsistente
dados <- dados %>%
  mutate(
    afiliacao_inconsistente = if_else(
      a1 == 0 & a2 == 1,
      1,
      afiliacao_inconsistente
    )
  )


# A1=0 e A3=1/2 são inconsistentes
dados <- dados %>%
  mutate(
    afiliacao_inconsistente = if_else(
      a1 == 0 & a3 %in% c(1, 2),
      1,
      afiliacao_inconsistente
    )
  )


# A2=0 e A3=1/3 são inconsistentes
dados <- dados %>%
  mutate(
    afiliacao_inconsistente = if_else(
      a2 == 0 & a3 %in% c(1, 3),
      1,
      afiliacao_inconsistente
    )
  )


# =============================================================================
# CORRIGIR REPRESENTAÇÃO DOS SERVIÇOS
# =============================================================================

dados <- dados %>%
  mutate(
    
    # A1=0 e A2=2
    afil_s2 = if_else(
      a1 == 0 & a2 == 2,
      1,
      afil_s2
    ),
    
    # A1=0, A2=0 e A3=4
    afil_s3 = if_else(
      a1 == 0 & a2 == 0 & a3 == 4,
      1,
      afil_s3
    ),
    
    # A1=0, A2=2 e A3=3
    afil_s3 = if_else(
      a1 == 0 & a2 == 2 & a3 == 3,
      1,
      afil_s3
    ),
    
    # A1=0, A2=2 e A3=4
    afil_s3 = if_else(
      a1 == 0 & a2 == 2 & a3 == 4,
      2,
      afil_s3
    ),
    
    # A1=1, A2=0 e A3=2
    afil_s3 = if_else(
      a1 == 1 & a2 == 0 & a3 == 2,
      1,
      afil_s3
    ),
    
    # A1=1, A2=0 e A3=4
    afil_s3 = if_else(
      a1 == 1 & a2 == 0 & a3 == 4,
      2,
      afil_s3
    )
  )


# =============================================================================
# NÚMERO DE SERVIÇOS DISTINTOS
# =============================================================================

dados <- dados %>%
  mutate(
    
    # Nenhum serviço
    afiliacao_ndist = case_when(
      
      afiliacao_nsim == 0 ~ 0,
      
      # Apenas um SIM
      afiliacao_nsim == 1 ~ 1,
      
      # Dois SIM - mesmo serviço
      afiliacao_nsim == 2 &
        afil_s1 == afil_s2 ~ 1,
      
      # Dois SIM - serviços diferentes
      afiliacao_nsim == 2 &
        afil_s1 != afil_s2 ~ 2,
      
      # Três SIM - todos iguais
      afiliacao_nsim == 3 &
        afil_s1 == afil_s2 &
        afil_s1 == afil_s3 ~ 1,
      
      # Três SIM - exatamente dois iguais
      afiliacao_nsim == 3 &
        (
          (afil_s1 == afil_s2 & afil_s1 != afil_s3) |
            (afil_s1 == afil_s3 & afil_s1 != afil_s2) |
            (afil_s2 == afil_s3 & afil_s1 != afil_s2)
        ) ~ 2,
      
      # Três SIM - todos diferentes
      afiliacao_nsim == 3 &
        afil_s1 != afil_s2 &
        afil_s1 != afil_s3 &
        afil_s2 != afil_s3 ~ 3,
      
      TRUE ~ NA_real_
    )
  )


cat("\nDistribuição do número de serviços distintos:\n")

print(
  table(
    dados$afiliacao_ndist,
    useNA = "ifany"
  )
)

# =============================================================================
# NÚMERO DE SERVIÇOS DISTINTOS
# =============================================================================

dados <- dados %>%
  mutate(
    
    afiliacao_ndist = case_when(
      
      # ---------------------------------------------------------------
      # Nenhum SIM
      # ---------------------------------------------------------------
      afiliacao_nsim == 0 ~ 0,
      
      
      # ---------------------------------------------------------------
      # Apenas um SIM
      # ---------------------------------------------------------------
      afiliacao_nsim == 1 ~ 1,
      
      
      # ---------------------------------------------------------------
      # Dois SIM
      #
      # Mesmo serviço -> 1
      # Serviços diferentes -> 2
      # ---------------------------------------------------------------
      
      afiliacao_nsim == 2 &
        afil_s1 == afil_s2 ~ 1,
      
      afiliacao_nsim == 2 &
        afil_s1 != afil_s2 ~ 2,
      
      
      # ---------------------------------------------------------------
      # Três SIM
      #
      # Todos iguais -> 1 serviço
      # Exatamente dois iguais -> 2 serviços
      # Todos diferentes -> 3 serviços
      # ---------------------------------------------------------------
      
      afiliacao_nsim == 3 &
        afil_s1 == afil_s2 &
        afil_s1 == afil_s3 ~ 1,
      
      afiliacao_nsim == 3 &
        (
          (afil_s1 == afil_s2 & afil_s1 != afil_s3) |
            (afil_s1 == afil_s3 & afil_s1 != afil_s2) |
            (afil_s2 == afil_s3 & afil_s1 != afil_s2)
        ) ~ 2,
      
      afiliacao_nsim == 3 &
        afil_s1 != afil_s2 &
        afil_s1 != afil_s3 &
        afil_s2 != afil_s3 ~ 3,
      
      
      # ---------------------------------------------------------------
      # Demais situações
      # ---------------------------------------------------------------
      
      TRUE ~ NA_real_
    )
  )


# =============================================================================
# CONFERÊNCIA
# =============================================================================

cat("\n")
cat("Distribuição do número de serviços distintos:\n")

print(
  table(
    dados$afiliacao_ndist,
    useNA = "ifany"
  )
)
# =============================================================================
# ESCORE DE AFILIAÇÃO
# =============================================================================

dados <- dados %>%
  mutate(
    
    # Todas NÃO
    esc_afiliacao = case_when(
      
      a1 == 0 &
        a2 == 0 &
        a3 == 0 ~ 1,
      
      # Todos SIM e mesmo serviço
      a1 == 1 &
        a2 == 1 &
        a3 == 1 ~ 4,
      
      # Dois ou mais SIM e pelo menos dois pertencem ao mesmo serviço
      afiliacao_nsim >= 2 &
        afiliacao_ndist < afiliacao_nsim ~ 3,
      
      # SIM relacionados a serviços diferentes
      afiliacao_nsim >= 1 &
        afiliacao_ndist == afiliacao_nsim ~ 2,
      
      TRUE ~ NA_real_
    )
  )


# -----------------------------------------------------------------------------
# Afiliação missing se qualquer A1/A2/A3 estiver missing
# -----------------------------------------------------------------------------

dados <- dados %>%
  mutate(
    esc_afiliacao = if_else(
      is.na(a1) | is.na(a2) | is.na(a3),
      NA_real_,
      esc_afiliacao
    )
  )


# -----------------------------------------------------------------------------
# Conferência
# -----------------------------------------------------------------------------

cat("\nDistribuição do escore de Afiliação:\n")
print(table(dados$esc_afiliacao, useNA = "ifany"))

cat("\nNúmero de respostas SIM:\n")
print(table(dados$afiliacao_nsim, useNA = "ifany"))

cat("\nNúmero de serviços distintos:\n")
print(table(dados$afiliacao_ndist, useNA = "ifany"))

cat("\nCasos potencialmente inconsistentes:\n")

print(
  dados %>%
    filter(afiliacao_inconsistente == 1) %>%
    select(
      record_id,
      a1,
      a2,
      a3,
      afiliacao_nsim,
      afiliacao_ndist,
      esc_afiliacao
    )
)


# =============================================================================
# FUNÇÃO PARA PROCESSAR COMPONENTES
# =============================================================================

pcat_component <- function(
    data,
    prefix,
    items,
    invert = character()
) {
  
  # ---------------------------------------------------------------------------
  # Criar cópias dos itens
  # ---------------------------------------------------------------------------
  
  for (v in items) {
    
    nome_sc <- paste0(prefix, "_", v, "_sc")
    
    data[[nome_sc]] <- data[[v]]
    
    # Inversão: 1 <-> 4 e 2 <-> 3
    if (v %in% invert) {
      
      idx <- !is.na(data[[v]]) &
        data[[v]] %in% c(1, 2, 3, 4)
      
      data[[nome_sc]][idx] <-
        5 - data[[v]][idx]
      
      # 9 permanece 9
      data[[nome_sc]][
        !is.na(data[[v]]) & data[[v]] == 9
      ] <- 9
    }
  }
  
  
  # ---------------------------------------------------------------------------
  # Contar 9 + missing
  # ---------------------------------------------------------------------------
  
  nmiss_nome <- paste0("nmiss_", prefix)
  
  data[[nmiss_nome]] <- 0
  
  for (v in items) {
    
    data[[nmiss_nome]] <-
      data[[nmiss_nome]] +
      ifelse(
        is.na(data[[v]]) | data[[v]] == 9,
        1,
        0
      )
  }
  
  
  # ---------------------------------------------------------------------------
  # Regra de 50%
  #
  # Se ausentes >= 50%:
  #   escore = missing
  #
  # Se ausentes < 50%:
  #   9 -> 2
  # ---------------------------------------------------------------------------
  
  limite <- length(items) / 2
  
  for (v in items) {
    
    nome_sc <- paste0(prefix, "_", v, "_sc")
    
    idx_9 <- !is.na(data[[v]]) &
      data[[v]] == 9 &
      data[[nmiss_nome]] < limite
    
    data[[nome_sc]][idx_9] <- 2
  }
  
  
  # ---------------------------------------------------------------------------
  # Média dos itens
  # ---------------------------------------------------------------------------
  
  sclist <- paste0(
    prefix,
    "_",
    items,
    "_sc"
  )
  
  data[[paste0("esc_", prefix)]] <-
    rowMeans(
      data[, sclist, drop = FALSE],
      na.rm = TRUE
    )
  
  
  # ---------------------------------------------------------------------------
  # Se >=50% ausentes, escore = NA
  # ---------------------------------------------------------------------------
  
  idx_missing <-
    data[[nmiss_nome]] >= limite
  
  data[[paste0("esc_", prefix)]][idx_missing] <-
    NA_real_
  
  
  # ---------------------------------------------------------------------------
  # Caso extremo: todos os itens missing
  # rowMeans(..., na.rm=TRUE) produz NaN
  # ---------------------------------------------------------------------------
  
  data[[paste0("esc_", prefix)]][
    is.nan(data[[paste0("esc_", prefix)]])
  ] <- NA_real_
  
  
  return(data)
}


# =============================================================================
# COMPONENTE B
# =============================================================================

dados <- pcat_component(
  data = dados,
  prefix = "b",
  items = B_items
)


# =============================================================================
# COMPONENTE C
# =============================================================================

dados <- pcat_component(
  data = dados,
  prefix = "c",
  items = C_items,
  invert = c("c9", "c10", "c11", "c12")
)


# =============================================================================
# COMPONENTE D
# =============================================================================

dados <- pcat_component(
  data = dados,
  prefix = "d",
  items = D_items,
  invert = c("d14", "d15")
)


# =============================================================================
# 18. COMPONENTE E
# =============================================================================

dados <- pcat_component(
  data = dados,
  prefix = "e",
  items = E_items
)


# =============================================================================
# COMPONENTE F
# =============================================================================

dados <- pcat_component(
  data = dados,
  prefix = "f",
  items = F_items
)


# =============================================================================
# COMPONENTE G
# =============================================================================

dados <- pcat_component(
  data = dados,
  prefix = "g",
  items = G_items
)


# =============================================================================
# COMPONENTE H
# =============================================================================

dados <- pcat_component(
  data = dados,
  prefix = "h",
  items = H_items
)


# =============================================================================
# COMPONENTE I
# =============================================================================

dados <- pcat_component(
  data = dados,
  prefix = "i",
  items = I_items
)


# =============================================================================
# COMPONENTE J
# =============================================================================

dados <- pcat_component(
  data = dados,
  prefix = "j",
  items = J_items
)


# =============================================================================
# ESCORES DOS COMPONENTES
# =============================================================================

componentes <- c(
  "esc_afiliacao",
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

cat("\n")
cat("==============================================================\n")
cat(" ESCORES DOS COMPONENTES - ESCALA ORIGINAL 1 A 4\n")
cat("==============================================================\n\n")

print(
  summary(
    dados[, componentes]
  )
)


# =============================================================================
# TRANSFORMAÇÃO 1-4 PARA 0-10
# =============================================================================

for (v in componentes) {
  
  novo_nome <- paste0(v, "_10")
  
  dados[[novo_nome]] <-
    ((dados[[v]] - 1) / 3) * 10
}


# =============================================================================
# CLASSIFICAÇÃO DOS COMPONENTES
#
# 0 = Baixo
# 1 = Alto
# =============================================================================

for (v in componentes) {
  
  nome_10 <- paste0(v, "_10")
  nome_class <- sub("^esc_", "class_", v)
  
  dados[[nome_class]] <- case_when(
    
    is.na(dados[[nome_10]]) ~ NA_real_,
    
    dados[[nome_10]] < 6.6 ~ 0,
    
    dados[[nome_10]] >= 6.6 ~ 1
  )
}


# =============================================================================
# ESCORE ESSENCIAL
# =============================================================================

componentes_essenciais <- c(
  "esc_afiliacao",
  "esc_b",
  "esc_c",
  "esc_d",
  "esc_e",
  "esc_f",
  "esc_g",
  "esc_h"
)


dados$nmiss_essencial <-
  rowSums(
    is.na(
      dados[, componentes_essenciais]
    )
  )


dados$esc_essencial <-
  rowMeans(
    dados[, componentes_essenciais],
    na.rm = TRUE
  )


# Se 4 ou mais componentes forem missing
dados$esc_essencial[
  dados$nmiss_essencial >= 4
] <- NA_real_


# Corrigir NaN
dados$esc_essencial[
  is.nan(dados$esc_essencial)
] <- NA_real_


# =============================================================================
# ESCORE ESSENCIAL 0-10
# =============================================================================

dados$esc_essencial_10 <-
  ((dados$esc_essencial - 1) / 3) * 10


# =============================================================================
# CLASSIFICAÇÃO DO ESCORE ESSENCIAL
# =============================================================================

dados$class_essencial <- case_when(
  
  is.na(dados$esc_essencial_10) ~ NA_real_,
  
  dados$esc_essencial_10 < 6.6 ~ 0,
  
  dados$esc_essencial_10 >= 6.6 ~ 1
)


# =============================================================================
# ESCORE GERAL
# =============================================================================

componentes_gerais <- c(
  "esc_afiliacao",
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


dados$nmiss_geral <-
  rowSums(
    is.na(
      dados[, componentes_gerais]
    )
  )


dados$esc_geral <-
  rowMeans(
    dados[, componentes_gerais],
    na.rm = TRUE
  )


# Se 5 ou mais componentes forem missing
dados$esc_geral[
  dados$nmiss_geral >= 5
] <- NA_real_


# Corrigir NaN
dados$esc_geral[
  is.nan(dados$esc_geral)
] <- NA_real_


# =============================================================================
# ESCORE GERAL 0-10
# =============================================================================

dados$esc_geral_10 <-
  ((dados$esc_geral - 1) / 3) * 10


# =============================================================================
# CLASSIFICAÇÃO DO ESCORE GERAL
# =============================================================================

dados$class_geral <- case_when(
  
  is.na(dados$esc_geral_10) ~ NA_real_,
  
  dados$esc_geral_10 < 6.6 ~ 0,
  
  dados$esc_geral_10 >= 6.6 ~ 1
)


# =============================================================================
# LABELS
# =============================================================================

var_label(dados$esc_afiliacao) <-
  "Afiliação - escore 1 a 4"

var_label(dados$afiliacao_nsim) <-
  "Afiliação - número de respostas SIM"

var_label(dados$afiliacao_ndist) <-
  "Afiliação - número de serviços distintos"

var_label(dados$afiliacao_inconsistente) <-
  "Afiliação - combinação potencialmente inconsistente"


var_label(dados$esc_b) <-
  "Acesso primeiro contato - utilização (1-4)"

var_label(dados$esc_c) <-
  "Acesso primeiro contato - acessibilidade (1-4)"

var_label(dados$esc_d) <-
  "Longitudinalidade (1-4)"

var_label(dados$esc_e) <-
  "Coordenação - integração de cuidados (1-4)"

var_label(dados$esc_f) <-
  "Coordenação - sistemas de informações (1-4)"

var_label(dados$esc_g) <-
  "Integralidade - serviços disponíveis (1-4)"

var_label(dados$esc_h) <-
  "Integralidade - serviços prestados (1-4)"

var_label(dados$esc_i) <-
  "Orientação familiar (1-4)"

var_label(dados$esc_j) <-
  "Orientação comunitária (1-4)"


var_label(dados$esc_b_10) <-
  "Acesso utilização (0-10)"

var_label(dados$esc_c_10) <-
  "Acesso acessibilidade (0-10)"

var_label(dados$esc_d_10) <-
  "Longitudinalidade (0-10)"

var_label(dados$esc_e_10) <-
  "Coordenação integração (0-10)"

var_label(dados$esc_f_10) <-
  "Coordenação informação (0-10)"

var_label(dados$esc_g_10) <-
  "Integralidade serviços disponíveis (0-10)"

var_label(dados$esc_h_10) <-
  "Integralidade serviços prestados (0-10)"

var_label(dados$esc_i_10) <-
  "Orientação familiar (0-10)"

var_label(dados$esc_j_10) <-
  "Orientação comunitária (0-10)"


var_label(dados$esc_essencial) <-
  "Escore Essencial APS Saúde Bucal (1-4)"

var_label(dados$esc_essencial_10) <-
  "Escore Essencial APS Saúde Bucal (0-10)"

var_label(dados$esc_geral) <-
  "Escore Geral APS Saúde Bucal (1-4)"

var_label(dados$esc_geral_10) <-
  "Escore Geral APS Saúde Bucal (0-10)"

var_label(dados$nmiss_essencial) <-
  "Número de componentes essenciais missing"

var_label(dados$nmiss_geral) <-
  "Número de componentes gerais missing"


# =============================================================================
# TABELA DESCRITIVA - ESCALA 1-4
# =============================================================================

cat("\n")
cat("==============================================================\n")
cat(" ESTATÍSTICAS DESCRITIVAS - ESCALA 1 A 4\n")
cat("==============================================================\n")


estatisticas_1_4 <- dados %>%
  select(
    all_of(componentes),
    esc_essencial,
    esc_geral
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  group_by(variavel) %>%
  summarise(
    n = sum(!is.na(valor)),
    mean = mean(valor, na.rm = TRUE),
    sd = sd(valor, na.rm = TRUE),
    p50 = median(valor, na.rm = TRUE),
    min = min(valor, na.rm = TRUE),
    max = max(valor, na.rm = TRUE)
  )

print(estatisticas_1_4)


# =============================================================================
# 35. TABELA DESCRITIVA - ESCALA 0-10
# =============================================================================

componentes_10 <- c(
  "esc_afiliacao_10",
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


cat("\n")
cat("==============================================================\n")
cat(" ESTATÍSTICAS DESCRITIVAS - ESCALA 0 A 10\n")
cat("==============================================================\n")


estatisticas_0_10 <- dados %>%
  select(all_of(componentes_10)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  group_by(variavel) %>%
  summarise(
    n = sum(!is.na(valor)),
    mean = mean(valor, na.rm = TRUE),
    sd = sd(valor, na.rm = TRUE),
    p50 = median(valor, na.rm = TRUE),
    min = min(valor, na.rm = TRUE),
    max = max(valor, na.rm = TRUE)
  )

print(estatisticas_0_10)


# =============================================================================
# DISTRIBUIÇÃO DOS COMPONENTES
# =============================================================================

classificacoes <- c(
  "class_afiliacao",
  "class_b",
  "class_c",
  "class_d",
  "class_e",
  "class_f",
  "class_g",
  "class_h",
  "class_i",
  "class_j"
)


cat("\n")
cat("==============================================================\n")
cat(" CLASSIFICAÇÃO DOS COMPONENTES\n")
cat("==============================================================\n")


for (v in classificacoes) {
  
  cat("\n---", v, "---\n")
  
  print(
    table(
      dados[[v]],
      useNA = "ifany"
    )
  )
}


# =============================================================================
#  LABELS DAS CLASSIFICAÇÕES
# =============================================================================

class_labels <- c(
  `0` = "Baixo (<6,6)",
  `1` = "Alto (>=6,6)"
)


# =============================================================================
# CLASSIFICAÇÃO ESCORE ESSENCIAL
# =============================================================================

cat("\n")
cat("==============================================================\n")
cat(" CLASSIFICAÇÃO - ESCORE ESSENCIAL\n")
cat("==============================================================\n")

print(
  table(
    factor(
      dados$class_essencial,
      levels = c(0, 1),
      labels = c(
        "Baixo (<6,6)",
        "Alto (>=6,6)"
      )
    ),
    useNA = "ifany"
  )
)


# =============================================================================
# CLASSIFICAÇÃO ESCORE GERAL
# =============================================================================

cat("\n")
cat("==============================================================\n")
cat(" CLASSIFICAÇÃO - ESCORE GERAL\n")
cat("==============================================================\n")

print(
  table(
    factor(
      dados$class_geral,
      levels = c(0, 1),
      labels = c(
        "Baixo (<6,6)",
        "Alto (>=6,6)"
      )
    ),
    useNA = "ifany"
  )
)


# =============================================================================
# COMPONENTES MISSING
# =============================================================================

cat("\n")
cat("==============================================================\n")
cat(" COMPONENTES MISSING\n")
cat("==============================================================\n")


cat("\nEscore Essencial:\n")
print(
  table(
    dados$nmiss_essencial,
    useNA = "ifany"
  )
)


cat("\nEscore Geral:\n")
print(
  table(
    dados$nmiss_geral,
    useNA = "ifany"
  )
)


# =============================================================================
# PERCENTUAL DE RESPONDENTES COM ESCORE CALCULADO
# =============================================================================

cat("\n")
cat("==============================================================\n")
cat(" COMPLETUDE DOS ESCORES\n")
cat("==============================================================\n")


todos_escores <- c(
  componentes,
  "esc_essencial",
  "esc_geral"
)


for (v in todos_escores) {
  
  n_ok <- sum(!is.na(dados[[v]]))
  
  n_total <- nrow(dados)
  
  perc <- 100 * n_ok / n_total
  
  cat(
    sprintf(
      "%-20s : %8d de %8d (%6.2f%%)\n",
      v,
      n_ok,
      n_total,
      perc
    )
  )
}


# =============================================================================
# VERIFICAÇÃO DA FAIXA DOS ESCORES 1-4
# =============================================================================

cat("\n")
cat("==============================================================\n")
cat(" VERIFICAÇÃO DA FAIXA DOS ESCORES\n")
cat("==============================================================\n")


for (v in todos_escores) {
  
  invalidos <- which(
    !is.na(dados[[v]]) &
      (
        dados[[v]] < 1 |
          dados[[v]] > 4
      )
  )
  
  if (length(invalidos) > 0) {
    
    cat(
      "\nERRO:", v,
      "possui valores fora de 1-4.\n"
    )
    
    print(
      dados[
        invalidos,
        c("record_id", v),
        drop = FALSE
      ]
    )
  }
}


# =============================================================================
# VERIFICAÇÃO DA FAIXA 0-10
# =============================================================================

cat("\n")
cat("==============================================================\n")
cat(" VERIFICAÇÃO DA FAIXA 0-10\n")
cat("==============================================================\n")


for (v in componentes_10) {
  
  invalidos <- which(
    !is.na(dados[[v]]) &
      (
        dados[[v]] < 0 |
          dados[[v]] > 10
      )
  )
  
  if (length(invalidos) > 0) {
    
    cat(
      "\nERRO:", v,
      "possui valores fora de 0-10.\n"
    )
  }
}


# =============================================================================
#  TABELA FINAL DOS ESCORES
# =============================================================================

cat("\n")
cat("==============================================================\n")
cat(" TABELA FINAL - PCATOOL\n")
cat("==============================================================\n")


tabela_final <- dados %>%
  select(all_of(componentes_10)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  group_by(variavel) %>%
  summarise(
    n = sum(!is.na(valor)),
    mean = mean(valor, na.rm = TRUE),
    sd = sd(valor, na.rm = TRUE),
    min = min(valor, na.rm = TRUE),
    max = max(valor, na.rm = TRUE)
  )


print(tabela_final)


# =============================================================================
#  EXPORTAÇÃO PARA STATA
# =============================================================================

write_dta(
  dados,
  "pcatool_bucal_adulto_com_escores.dta"
)