# ======================================================================
# PCATOOL-BRASIL - CRIANÇAS - VERSÃO EXTENSA
# ANÁLISE NO R
#
#
# Instrumento:
# PCATool Brasil - Para Pacientes Crianças - Versão Extensa
#
# COMPONENTES:
#
# A = Afiliação
# B = Acesso de Primeiro Contato - Utilização
# C = Acesso de Primeiro Contato - Acessibilidade
# D = Longitudinalidade
# E = Coordenação - Integração de Cuidados
# F = Coordenação - Sistemas de Informações
# G = Integralidade - Serviços Disponíveis
# H = Integralidade - Serviços Prestados
# I = Orientação Familiar
# J = Orientação Comunitária
#
# ======================================================================


# ======================================================================
# INSTALAÇÃO E CARREGAMENTO DOS PACOTES
# ======================================================================

pacotes <- c(
  "haven",
  "dplyr",
  "tidyr",
  "ggplot2",
  "openxlsx"
)

pacotes_faltantes <- pacotes[
  !(pacotes %in% installed.packages()[, "Package"])
]

if (length(pacotes_faltantes) > 0) {
  
  install.packages(
    pacotes_faltantes
  )
  
}

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)


# ======================================================================
# CARREGANDO O BANCO DE DADOS
# ======================================================================

# ALTERE O CAMINHO ABAIXO PARA O SEU ARQUIVO.
#dados <- read_dta(
#  "C:/PCATool/dados_pcatool_criancas.dta"
#)


# Se o banco já estiver carregado no R como "dados",
# não execute a linha acima.


# ======================================================================
# VERIFICANDO O BANCO
# ======================================================================

cat("\n")
cat("==============================================================\n")
cat("BANCO DE DADOS\n")
cat("==============================================================\n")

cat(
  "Número de observações:",
  nrow(dados),
  "\n"
)

cat(
  "Número de variáveis:",
  ncol(dados),
  "\n"
)


# ======================================================================
# DEFININDO OS ITENS
# ======================================================================

itens_afiliacao <- c(
  "a1",
  "a2",
  "a3"
)

itens_b <- c(
  "b1",
  "b2",
  "b3"
)

itens_c <- c(
  "c1",
  "c2",
  "c3",
  "c4",
  "c5",
  "c6"
)

itens_d <- paste0(
  "d",
  1:14
)

itens_e <- c(
  "e2",
  "e3",
  "e4",
  "e5",
  "e6"
)

itens_f <- c(
  "f1",
  "f2",
  "f3"
)

itens_g <- paste0(
  "g",
  1:9
)

itens_h <- paste0(
  "h",
  1:5
)

itens_i <- c(
  "i1",
  "i2",
  "i3"
)

itens_j <- c(
  "j1",
  "j2",
  "j3",
  "j4"
)


todos_itens <- c(
  itens_afiliacao,
  itens_b,
  itens_c,
  itens_d,
  "e1",
  itens_e,
  itens_f,
  itens_g,
  itens_h,
  itens_i,
  itens_j
)


# ======================================================================
# VERIFICANDO SE AS VARIÁVEIS EXISTEM
# ======================================================================

variaveis_faltantes <- setdiff(
  todos_itens,
  names(dados)
)

if (length(variaveis_faltantes) > 0) {
  
  stop(
    paste(
      "As seguintes variáveis não foram encontradas no banco:",
      paste(
        variaveis_faltantes,
        collapse = ", "
      )
    )
  )
  
} else {
  
  cat("\nTodas as variáveis esperadas foram encontradas.\n")
  
}


# ======================================================================
# CONFERÊNCIA DOS CÓDIGOS DOS ITENS
# ======================================================================

cat("\n")
cat("==============================================================\n")
cat("CONFERÊNCIA DOS CÓDIGOS DOS ITENS\n")
cat("==============================================================\n")


for (var in todos_itens) {
  
  cat(
    "\n----------------------------------------------------------\n"
  )
  
  cat(
    var,
    "\n"
  )
  
  print(
    table(
      dados[[var]],
      useNA = "ifany"
    )
  )
  
}


# ======================================================================
# AFILIAÇÃO
# ======================================================================
#
# A1:
#   0 = Não
#   1 = Sim
#
# A2:
#   0 = Não
#   1 = Sim, mesmo serviço/profissional de A1
#   2 = Sim, serviço/profissional diferente de A1
#
# A3:
#   0 = Não
#   1 = Sim, mesmo serviço/profissional referido em A1 e A2
#   2 = Sim, mesmo serviço/profissional referido em A1
#   3 = Sim, mesmo serviço/profissional referido em A2
#   4 = Sim, diferente dos referidos em A1 e A2
#
# Escore:
#
#   1 = nenhuma afiliação
#   2 = serviços/profissionais diferentes
#   3 = dois itens identificam o mesmo serviço/profissional
#   4 = os três itens identificam o mesmo serviço/profissional
# ======================================================================


dados$afiliacao <- NA_real_


# Caso 1
dados$afiliacao[
  dados$a1 == 0 &
    dados$a2 == 0 &
    dados$a3 == 0
] <- 1


# Caso 2
dados$afiliacao[
  dados$a1 == 1 &
    dados$a2 == 1 &
    dados$a3 == 1
] <- 4


# Caso 3
dados$afiliacao[
  dados$a1 == 1 &
    dados$a2 == 0 &
    dados$a3 == 2
] <- 3


# Caso 4
dados$afiliacao[
  dados$a1 == 1 &
    dados$a2 == 2 &
    dados$a3 == 2
] <- 3


# Caso 5
dados$afiliacao[
  dados$a1 == 1 &
    dados$a2 == 2 &
    dados$a3 == 3
] <- 3


# Caso 6
dados$afiliacao[
  dados$a1 == 0 &
    dados$a2 == 2 &
    dados$a3 == 3
] <- 3


# Caso 7
dados$afiliacao[
  dados$a1 == 1 &
    dados$a2 == 2 &
    dados$a3 == 4
] <- 2


# Caso 8
dados$afiliacao[
  dados$a1 == 0 &
    dados$a2 == 2 &
    dados$a3 == 4
] <- 2


# Caso 9
dados$afiliacao[
  dados$a1 == 1 &
    dados$a2 == 0 &
    dados$a3 == 4
] <- 2


# ======================================================================
# RÓTULO DA AFILIAÇÃO
# ======================================================================

dados$afiliacao_label <- factor(
  dados$afiliacao,
  levels = c(
    1,
    2,
    3,
    4
  ),
  labels = c(
    "Grau 1 - sem afiliacao",
    "Grau 2 - servicos diferentes",
    "Grau 3 - dois itens mesmo servico",
    "Grau 4 - tres itens mesmo servico"
  )
)


# ======================================================================
# CONFERIR AFILIAÇÃO
# ======================================================================

cat("\n")
cat("==============================================================\n")
cat("DISTRIBUIÇÃO DA AFILIAÇÃO\n")
cat("==============================================================\n")

print(
  table(
    dados$afiliacao,
    useNA = "ifany"
  )
)


cat("\nA1 x A2:\n")

print(
  table(
    dados$a1,
    dados$a2,
    useNA = "ifany"
  )
)


cat("\nA1 x A3:\n")

print(
  table(
    dados$a1,
    dados$a3,
    useNA = "ifany"
  )
)


cat("\nA2 x A3:\n")

print(
  table(
    dados$a2,
    dados$a3,
    useNA = "ifany"
  )
)


cat("\n")
cat("==============================================================\n")
cat("CASOS DE AFILIAÇÃO NÃO CLASSIFICADOS\n")
cat("==============================================================\n")


if ("record_id" %in% names(dados)) {
  
  print(
    dados %>%
      filter(
        is.na(afiliacao)
      ) %>%
      select(
        record_id,
        a1,
        a2,
        a3,
        afiliacao
      )
  )
  
} else {
  
  print(
    dados %>%
      filter(
        is.na(afiliacao)
      ) %>%
      select(
        a1,
        a2,
        a3,
        afiliacao
      )
  )
  
}


# ======================================================================
# CRIANDO CÓPIAS DOS ITENS
# ======================================================================

itens_transformados <- c(
  itens_b,
  itens_c,
  itens_d,
  itens_e,
  itens_f,
  itens_g,
  itens_h,
  itens_i,
  itens_j
)


for (var in itens_transformados) {
  
  dados[[paste0(var, "_t")]] <-
    as.numeric(dados[[var]])
  
}


# ======================================================================
# INVERSÃO DOS ITENS
#
# C2
# C4
# C5
# D10
#
# 4 -> 1
# 3 -> 2
# 2 -> 3
# 1 -> 4
#
# O valor 9 permanece 9.
# ======================================================================

itens_invertidos <- c(
  "c2",
  "c4",
  "c5",
  "d10"
)


for (var in itens_invertidos) {
  
  var_t <- paste0(
    var,
    "_t"
  )
  
  dados[[var_t]] <- ifelse(
    !is.na(dados[[var]]) &
      dados[[var]] >= 1 &
      dados[[var]] <= 4,
    
    5 - dados[[var]],
    
    dados[[var]]
  )
  
}


# ======================================================================
# CONFERIR INVERSÃO
# ======================================================================

cat("\n")
cat("==============================================================\n")
cat("CONFERÊNCIA DA INVERSÃO\n")
cat("==============================================================\n")


for (var in itens_invertidos) {
  
  var_t <- paste0(
    var,
    "_t"
  )
  
  cat(
    "\n",
    var,
    " x ",
    var_t,
    "\n",
    sep = ""
  )
  
  print(
    table(
      dados[[var]],
      dados[[var_t]],
      useNA = "ifany"
    )
  )
  
}


# ======================================================================
# FUNÇÃO CORRIGIDA PARA CALCULAR OS COMPONENTES
# ======================================================================
#
# Esta função reproduz a lógica utilizada no Stata:
#
# egen x_nmiss = rowmiss(...)
#
# Depois, o código 9 é acrescentado à contagem de missing.
#
# Se a quantidade de missing for inferior a 50%:
#
#     9 -> 2
#
# Depois:
#
#     se todos os itens forem válidos -> calcula média
#
# Caso contrário:
#
#     escore = missing
#
# ======================================================================


calcular_componente <- function(
    data,
    itens,
    nome_escore,
    limite_missing
) {
  
  # ---------------------------------------------------------------
  # Nomes das variáveis transformadas
  # ---------------------------------------------------------------
  
  vars_t <- paste0(
    itens,
    "_t"
  )
  
  
  # ---------------------------------------------------------------
  # Verificar se todas as variáveis existem
  # ---------------------------------------------------------------
  
  variaveis_faltantes <- setdiff(
    vars_t,
    names(data)
  )
  
  
  if (length(variaveis_faltantes) > 0) {
    
    stop(
      paste(
        "As seguintes variáveis não existem no banco:",
        paste(
          variaveis_faltantes,
          collapse = ", "
        )
      )
    )
    
  }
  
  
  # ---------------------------------------------------------------
  # Criar matriz dos itens
  # ---------------------------------------------------------------
  
  matriz_itens <- as.matrix(
    data[, vars_t, drop = FALSE]
  )
  
  
  # ---------------------------------------------------------------
  # Garantir que a matriz seja numérica
  # ---------------------------------------------------------------
  
  matriz_itens <- apply(
    matriz_itens,
    2,
    function(x) {
      as.numeric(x)
    }
  )
  
  
  # Garantir matriz mesmo quando houver apenas uma coluna
  if (is.null(dim(matriz_itens))) {
    
    matriz_itens <- matrix(
      matriz_itens,
      ncol = length(vars_t)
    )
    
  }
  
  
  # ---------------------------------------------------------------
  # Número de NA verdadeiros
  # ---------------------------------------------------------------
  
  n_na <- rowSums(
    is.na(matriz_itens)
  )
  
  
  # ---------------------------------------------------------------
  # Número de códigos 9
  # ---------------------------------------------------------------
  
  n_9 <- rowSums(
    matriz_itens == 9,
    na.rm = TRUE
  )
  
  
  # ---------------------------------------------------------------
  # Total de missing segundo a lógica do Stata
  # ---------------------------------------------------------------
  
  n_miss_total <-
    n_na +
    n_9
  
  
  # ---------------------------------------------------------------
  # Substituir código 9 por 2
  # quando o número de missing for menor que o limite
  # ---------------------------------------------------------------
  
  for (j in seq_along(vars_t)) {
    
    idx <- (
      !is.na(matriz_itens[, j]) &
        matriz_itens[, j] == 9 &
        n_miss_total < limite_missing
    )
    
    matriz_itens[idx, j] <- 2
    
  }
  
  
  # ---------------------------------------------------------------
  # Número de valores válidos depois da substituição
  # ---------------------------------------------------------------
  
  n_valid <- rowSums(
    !is.na(matriz_itens)
  )
  
  
  # ---------------------------------------------------------------
  # Criar vetor do escore
  # ---------------------------------------------------------------
  
  escore <- rep(
    NA_real_,
    nrow(data)
  )
  
  
  # ---------------------------------------------------------------
  # Condição para calcular o escore
  # ---------------------------------------------------------------
  
  idx_calcular <- (
    n_miss_total < limite_missing &
      n_valid == length(vars_t)
  )
  
  
  # ---------------------------------------------------------------
  # Calcular média
  # ---------------------------------------------------------------
  
  if (any(idx_calcular)) {
    
    escore[idx_calcular] <- rowMeans(
      matriz_itens[
        idx_calcular,
        ,
        drop = FALSE
      ],
      na.rm = FALSE
    )
    
  }
  
  
  # ---------------------------------------------------------------
  # Adicionar escore ao banco
  # ---------------------------------------------------------------
  
  data[[nome_escore]] <-
    escore
  
  
  # ---------------------------------------------------------------
  # Criar variáveis auxiliares
  # ---------------------------------------------------------------
  
  prefixo <- sub(
    "_.*$",
    "",
    nome_escore
  )
  
  
  data[[paste0(
    prefixo,
    "_nmiss"
  )]] <-
    n_miss_total
  
  
  data[[paste0(
    prefixo,
    "_nvalid"
  )]] <-
    n_valid
  
  
  # ---------------------------------------------------------------
  # Retornar banco
  # ---------------------------------------------------------------
  
  return(data)
  
}


# ======================================================================
# COMPONENTE B
# ======================================================================

dados <- calcular_componente(
  data = dados,
  itens = itens_b,
  nome_escore = "escore_b",
  limite_missing = 1.5
)


# ======================================================================
# COMPONENTE C
# ======================================================================

dados <- calcular_componente(
  data = dados,
  itens = itens_c,
  nome_escore = "escore_c",
  limite_missing = 3
)


# ======================================================================
# COMPONENTE D
# ======================================================================

dados <- calcular_componente(
  data = dados,
  itens = itens_d,
  nome_escore = "escore_d",
  limite_missing = 7
)


# ======================================================================
# COMPONENTE E
# ======================================================================

dados <- calcular_componente(
  data = dados,
  itens = itens_e,
  nome_escore = "escore_e",
  limite_missing = 2.5
)


# ======================================================================
# COMPONENTE F
# ======================================================================

dados <- calcular_componente(
  data = dados,
  itens = itens_f,
  nome_escore = "escore_f",
  limite_missing = 1.5
)


# ======================================================================
# COMPONENTE G
# ======================================================================

dados <- calcular_componente(
  data = dados,
  itens = itens_g,
  nome_escore = "escore_g",
  limite_missing = 4.5
)


# ======================================================================
# COMPONENTE H
# ======================================================================

dados <- calcular_componente(
  data = dados,
  itens = itens_h,
  nome_escore = "escore_h",
  limite_missing = 2.5
)


# ======================================================================
# COMPONENTE I
# ======================================================================

dados <- calcular_componente(
  data = dados,
  itens = itens_i,
  nome_escore = "escore_i",
  limite_missing = 1.5
)


# ======================================================================
# COMPONENTE J
# ======================================================================

dados <- calcular_componente(
  data = dados,
  itens = itens_j,
  nome_escore = "escore_j",
  limite_missing = 2
)


# ======================================================================
# VERIFICANDO OS COMPONENTES
# ======================================================================

componentes <- c(
  "afiliacao",
  "escore_b",
  "escore_c",
  "escore_d",
  "escore_e",
  "escore_f",
  "escore_g",
  "escore_h",
  "escore_i",
  "escore_j"
)


cat("\n")
cat("==============================================================\n")
cat("ESCORE DOS COMPONENTES - ESCALA ORIGINAL 1 A 4\n")
cat("==============================================================\n")


print(
  summary(
    dados[, componentes]
  )
)


# ======================================================================
# ESCORE ESSENCIAL DA APS
#
# Afiliação
# B
# C
# D
# E
# F
# G
# H
#
# Total = 8
#
# 4 ou mais missing -> não calcula
# 0 a 3 missing -> calcula
# ======================================================================


componentes_essenciais <- c(
  "afiliacao",
  "escore_b",
  "escore_c",
  "escore_d",
  "escore_e",
  "escore_f",
  "escore_g",
  "escore_h"
)


dados$essencial_nmiss <- rowSums(
  is.na(
    dados[, componentes_essenciais, drop = FALSE]
  )
)


dados$essencial_nvalid <- rowSums(
  !is.na(
    dados[, componentes_essenciais, drop = FALSE]
  )
)


dados$escore_essencial <- NA_real_


idx_essencial <- (
  dados$essencial_nmiss <= 3
)


dados$escore_essencial[
  idx_essencial
] <- rowMeans(
  dados[
    idx_essencial,
    componentes_essenciais,
    drop = FALSE
  ],
  na.rm = TRUE
)


# ======================================================================
# ESCORE GERAL DA APS
#
# Afiliação
# B
# C
# D
# E
# F
# G
# H
# I
# J
#
# Total = 10
#
# 5 ou mais missing -> não calcula
# 0 a 4 missing -> calcula
# ======================================================================


componentes_gerais <- c(
  "afiliacao",
  "escore_b",
  "escore_c",
  "escore_d",
  "escore_e",
  "escore_f",
  "escore_g",
  "escore_h",
  "escore_i",
  "escore_j"
)


dados$geral_nmiss <- rowSums(
  is.na(
    dados[, componentes_gerais, drop = FALSE]
  )
)


dados$geral_nvalid <- rowSums(
  !is.na(
    dados[, componentes_gerais, drop = FALSE]
  )
)


dados$escore_geral <- NA_real_


idx_geral <- (
  dados$geral_nmiss <= 4
)


dados$escore_geral[
  idx_geral
] <- rowMeans(
  dados[
    idx_geral,
    componentes_gerais,
    drop = FALSE
  ],
  na.rm = TRUE
)


# ======================================================================
# TRANSFORMAÇÃO DOS ESCORES PARA 0-10
#
# Fórmula:
#
# ((escore - 1) / 3) * 10
#
# 1 = 0
# 2 = 3,33
# 3 = 6,67
# 4 = 10
# ======================================================================


transformar_0_10 <- function(x) {
  
  resultado <- rep(
    NA_real_,
    length(x)
  )
  
  idx <- !is.na(x)
  
  resultado[idx] <-
    ((x[idx] - 1) / 3) * 10
  
  resultado
  
}


# Afiliação

dados$afiliacao_0_10 <-
  transformar_0_10(
    dados$afiliacao
  )


# Componentes B-J

escores_componentes <- paste0(
  "escore_",
  letters[2:10]
)


for (var in escores_componentes) {
  
  dados[[paste0(
    var,
    "_0_10"
  )]] <-
    transformar_0_10(
      dados[[var]]
    )
  
}


# Escore essencial

dados$escore_essencial_0_10 <-
  transformar_0_10(
    dados$escore_essencial
  )


# Escore geral

dados$escore_geral_0_10 <-
  transformar_0_10(
    dados$escore_geral
  )


# ======================================================================
# TRANSFORMAR ITENS INDIVIDUAIS PARA 0-10
# ======================================================================


for (var in paste0(
  itens_transformados,
  "_t"
)) {
  
  nome_0_10 <- paste0(
    var,
    "_0_10"
  )
  
  
  x <- dados[[var]]
  
  
  dados[[nome_0_10]] <- ifelse(
    !is.na(x) &
      x >= 1 &
      x <= 4,
    
    ((x - 1) / 3) * 10,
    
    NA_real_
  )
  
}


# ======================================================================
# RESUMO DOS ESCORES ORIGINAIS
# ======================================================================


escores_originais <- c(
  "afiliacao",
  "escore_b",
  "escore_c",
  "escore_d",
  "escore_e",
  "escore_f",
  "escore_g",
  "escore_h",
  "escore_i",
  "escore_j",
  "escore_essencial",
  "escore_geral"
)


cat("\n")
cat("==============================================================\n")
cat("ESCORE ORIGINAL - 1 A 4\n")
cat("==============================================================\n")


print(
  summary(
    dados[, escores_originais]
  )
)


# ======================================================================
# RESUMO DOS ESCORES 0-10
# ======================================================================


escores_0_10 <- c(
  "afiliacao_0_10",
  "escore_b_0_10",
  "escore_c_0_10",
  "escore_d_0_10",
  "escore_e_0_10",
  "escore_f_0_10",
  "escore_g_0_10",
  "escore_h_0_10",
  "escore_i_0_10",
  "escore_j_0_10",
  "escore_essencial_0_10",
  "escore_geral_0_10"
)


cat("\n")
cat("==============================================================\n")
cat("ESCORE TRANSFORMADO - 0 A 10\n")
cat("==============================================================\n")


print(
  summary(
    dados[, escores_0_10]
  )
)


# ======================================================================
# NÚMERO DE COMPONENTES DISPONÍVEIS
# ======================================================================


cat("\n")
cat("==============================================================\n")
cat("NÚMERO DE COMPONENTES VÁLIDOS\n")
cat("==============================================================\n")


cat("\nMissing no escore essencial:\n")

print(
  table(
    dados$essencial_nmiss,
    useNA = "ifany"
  )
)


cat("\nMissing no escore geral:\n")

print(
  table(
    dados$geral_nmiss,
    useNA = "ifany"
  )
)


# ======================================================================
# PERCENTUAL DE MISSING POR COMPONENTE
# ======================================================================


cat("\n")
cat("==============================================================\n")
cat("PERCENTUAL DE MISSING POR COMPONENTE\n")
cat("==============================================================\n")


for (var in escores_originais) {
  
  n_total <- nrow(dados)
  
  n_missing <- sum(
    is.na(
      dados[[var]]
    )
  )
  
  percentual <- (
    100 *
      n_missing /
      n_total
  )
  
  
  cat(
    sprintf(
      "%-25s : %6.2f%%\n",
      var,
      percentual
    )
  )
  
}


# ======================================================================
# VERIFICANDO LIMITES DOS ESCORES 1-4
# ======================================================================


cat("\n")
cat("==============================================================\n")
cat("VERIFICAÇÃO DOS ESCORES 1-4\n")
cat("==============================================================\n")


for (var in escores_originais) {
  
  fora_limite <- (
    !is.na(dados[[var]]) &
      (
        dados[[var]] < 1 |
          dados[[var]] > 4
      )
  )
  
  
  n_fora <- sum(
    fora_limite
  )
  
  
  if (n_fora > 0) {
    
    cat(
      "\n",
      var,
      ": ATENÇÃO - ",
      n_fora,
      " valores fora de 1 a 4\n",
      sep = ""
    )
    
    
    if ("record_id" %in% names(dados)) {
      
      print(
        dados[
          fora_limite,
          c(
            "record_id",
            var
          )
        ]
      )
      
    } else {
      
      print(
        dados[
          fora_limite,
          var,
          drop = FALSE
        ]
      )
      
    }
    
  } else {
    
    cat(
      var,
      ": OK\n"
    )
    
  }
  
}


# ======================================================================
# VERIFICANDO LIMITES DOS ESCORES 0-10
# ======================================================================


cat("\n")
cat("==============================================================\n")
cat("VERIFICAÇÃO DOS ESCORES 0-10\n")
cat("==============================================================\n")


for (var in escores_0_10) {
  
  fora_limite <- (
    !is.na(dados[[var]]) &
      (
        dados[[var]] < 0 |
          dados[[var]] > 10
      )
  )
  
  
  n_fora <- sum(
    fora_limite
  )
  
  
  if (n_fora > 0) {
    
    cat(
      "\n",
      var,
      ": ATENÇÃO - ",
      n_fora,
      " valores fora de 0 a 10\n",
      sep = ""
    )
    
  } else {
    
    cat(
      var,
      ": OK\n"
    )
    
  }
  
}


# ======================================================================
# ESTATÍSTICAS DESCRITIVAS
#
# N
# Média
# Desvio-padrão
# P25
# Mediana
# P75
# Mínimo
# Máximo
# ======================================================================


estatisticas <- data.frame(
  
  variavel = escores_0_10,
  
  N = sapply(
    dados[escores_0_10],
    function(x) {
      sum(!is.na(x))
    }
  ),
  
  media = sapply(
    dados[escores_0_10],
    function(x) {
      mean(
        x,
        na.rm = TRUE
      )
    }
  ),
  
  dp = sapply(
    dados[escores_0_10],
    function(x) {
      sd(
        x,
        na.rm = TRUE
      )
    }
  ),
  
  p25 = sapply(
    dados[escores_0_10],
    function(x) {
      quantile(
        x,
        0.25,
        na.rm = TRUE,
        names = FALSE
      )
    }
  ),
  
  mediana = sapply(
    dados[escores_0_10],
    function(x) {
      median(
        x,
        na.rm = TRUE
      )
    }
  ),
  
  p75 = sapply(
    dados[escores_0_10],
    function(x) {
      quantile(
        x,
        0.75,
        na.rm = TRUE,
        names = FALSE
      )
    }
  ),
  
  minimo = sapply(
    dados[escores_0_10],
    function(x) {
      min(
        x,
        na.rm = TRUE
      )
    }
  ),
  
  maximo = sapply(
    dados[escores_0_10],
    function(x) {
      max(
        x,
        na.rm = TRUE
      )
    }
  ),
  
  row.names = NULL
  
)


cat("\n")
cat("==============================================================\n")
cat("ESTATÍSTICAS DESCRITIVAS - ESCALA 0 A 10\n")
cat("==============================================================\n")


print(
  round(
    estatisticas[, -1],
    2
  )
)


# ======================================================================
# MEDIANA E INTERVALO INTERQUARTIL
# ======================================================================


cat("\n")
cat("==============================================================\n")
cat("MEDIANA E INTERVALO INTERQUARTIL\n")
cat("==============================================================\n")


for (var in escores_0_10) {
  
  x <- dados[[var]]
  
  
  if (all(is.na(x))) {
    
    cat(
      "\n",
      var,
      ": todos os valores são missing\n",
      sep = ""
    )
    
    next
    
  }
  
  
  p25 <- quantile(
    x,
    0.25,
    na.rm = TRUE,
    names = FALSE
  )
  
  
  p50 <- median(
    x,
    na.rm = TRUE
  )
  
  
  p75 <- quantile(
    x,
    0.75,
    na.rm = TRUE,
    names = FALSE
  )
  
  
  iqr <- p75 - p25
  
  
  cat(
    "\n-----------------------------------------------\n"
  )
  
  cat(
    var,
    "\n"
  )
  
  cat(
    "N       = ",
    sum(!is.na(x)),
    "\n",
    sep = ""
  )
  
  cat(
    "Mediana = ",
    sprintf(
      "%.2f",
      p50
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "P25     = ",
    sprintf(
      "%.2f",
      p25
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "P75     = ",
    sprintf(
      "%.2f",
      p75
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "IQR     = ",
    sprintf(
      "%.2f",
      iqr
    ),
    "\n",
    sep = ""
  )
  
}


# ======================================================================
# HISTOGRAMA - ESCORE ESSENCIAL
# ======================================================================


grafico_essencial <- ggplot(
  dados,
  aes(
    x = escore_essencial_0_10
  )
) +
  
  geom_histogram(
    bins = 10,
    color = "black",
    fill = "steelblue",
    na.rm = TRUE
  ) +
  
  labs(
    title = "Escore Essencial da APS",
    x = "Escore 0-10",
    y = "Frequência"
  ) +
  
  theme_minimal()


print(
  grafico_essencial
)


# ======================================================================
# HISTOGRAMA - ESCORE GERAL
# ======================================================================


grafico_geral <- ggplot(
  dados,
  aes(
    x = escore_geral_0_10
  )
) +
  
  geom_histogram(
    bins = 10,
    color = "black",
    fill = "steelblue",
    na.rm = TRUE
  ) +
  
  labs(
    title = "Escore Geral da APS",
    x = "Escore 0-10",
    y = "Frequência"
  ) +
  
  theme_minimal()


print(
  grafico_geral
)


# ======================================================================
# BOX PLOT DOS COMPONENTES
# ======================================================================


dados_boxplot <- dados %>%
  
  select(
    escore_b_0_10,
    escore_c_0_10,
    escore_d_0_10,
    escore_e_0_10,
    escore_f_0_10,
    escore_g_0_10,
    escore_h_0_10,
    escore_i_0_10,
    escore_j_0_10
  ) %>%
  
  pivot_longer(
    cols = everything(),
    names_to = "componente",
    values_to = "escore"
  )


grafico_boxplot <- ggplot(
  dados_boxplot,
  aes(
    x = componente,
    y = escore
  )
) +
  
  geom_boxplot(
    fill = "steelblue",
    color = "black",
    na.rm = TRUE
  ) +
  
  labs(
    title = "Escores dos componentes da APS",
    x = "Componente",
    y = "Escore 0-10"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


print(
  grafico_boxplot
)


# ======================================================================
# SALVAR GRÁFICOS
# ======================================================================


ggsave(
  filename = "histograma_escore_essencial.png",
  plot = grafico_essencial,
  width = 8,
  height = 6,
  dpi = 300
)


ggsave(
  filename = "histograma_escore_geral.png",
  plot = grafico_geral,
  width = 8,
  height = 6,
  dpi = 300
)


ggsave(
  filename = "boxplot_componentes.png",
  plot = grafico_boxplot,
  width = 10,
  height = 6,
  dpi = 300
)


# ======================================================================
# SALVAR BANCO FINAL EM RDS
# ======================================================================
saveRDS(
  dados,
  file = "pcatool_criancas_escores_final.rds"
)


# ======================================================================
# SALVAR BANCO FINAL EM DTA- STATA
# ======================================================================

write_dta(
  dados,
  path = "pcatool_criancas_escores_final.dta"
)

# ======================================================================
# EXPORTAR RESUMO DOS ESCORES PARA EXCEL
# ======================================================================


variaveis_exportacao <- c(
  "record_id",
  
  "afiliacao",
  
  "escore_b",
  "escore_c",
  "escore_d",
  "escore_e",
  "escore_f",
  "escore_g",
  "escore_h",
  "escore_i",
  "escore_j",
  
  "escore_essencial",
  "escore_geral",
  
  "afiliacao_0_10",
  
  "escore_b_0_10",
  "escore_c_0_10",
  "escore_d_0_10",
  "escore_e_0_10",
  "escore_f_0_10",
  "escore_g_0_10",
  "escore_h_0_10",
  "escore_i_0_10",
  "escore_j_0_10",
  
  "escore_essencial_0_10",
  "escore_geral_0_10"
)


# Manter somente variáveis que existem

variaveis_exportacao <-
  variaveis_exportacao[
    variaveis_exportacao %in% names(dados)
  ]


dados_exportacao <- dados %>%
  select(
    all_of(
      variaveis_exportacao
    )
  )


write.xlsx(
  dados_exportacao,
  file = "pcatool_criancas_escores.xlsx",
  overwrite = TRUE
)


# ======================================================================
# EXPORTAR ESTATÍSTICAS DESCRITIVAS
# ======================================================================


write.xlsx(
  estatisticas,
  file = "pcatool_criancas_estatisticas.xlsx",
  overwrite = TRUE
)


# ======================================================================
# FINAL
# ======================================================================


cat("\n")
cat("==============================================================\n")
cat("ANÁLISE PCATOOL CONCLUÍDA\n")
cat("==============================================================\n")

cat("\nBanco final RDS:\n")
cat(
  "pcatool_criancas_escores_final.rds\n"
)

cat("\nBanco final Stata:\n")
cat(
  "pcatool_criancas_escores_final.dta\n"
)

cat("\nArquivo Excel:\n")
cat(
  "pcatool_criancas_escores.xlsx\n"
)

cat("\nEstatísticas:\n")
cat(
  "pcatool_criancas_estatisticas.xlsx\n"
)

cat("\nEscores principais:\n")

cat("  - afiliacao\n")
cat("  - escore_b\n")
cat("  - escore_c\n")
cat("  - escore_d\n")
cat("  - escore_e\n")
cat("  - escore_f\n")
cat("  - escore_g\n")
cat("  - escore_h\n")
cat("  - escore_i\n")
cat("  - escore_j\n")
cat("  - escore_essencial\n")
cat("  - escore_geral\n")

cat("\nVersões 0-10:\n")
cat("  - *_0_10\n")

cat("\n==============================================================\n")
cat("FIM\n")
cat("==============================================================\n")
