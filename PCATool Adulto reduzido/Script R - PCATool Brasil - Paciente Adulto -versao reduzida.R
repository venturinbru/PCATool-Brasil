# ==============================================================================
# PCATOOL-BRASIL – ADULTOS – VERSÃO REDUZIDA
# ==============================================================================

# ==============================================================================
# PACOTES
# ==============================================================================

# Instale uma vez, se necessário:
#
# install.packages(c("dplyr", "haven"))

library(dplyr)
library(haven)


# ==============================================================================
# CARREGAR O BANCO
# ==============================================================================

# Se o banco for .dta:
#
# dados <- read_dta("C:/caminho/seu_banco.dta")

# Exemplo:
# dados <- read_dta("C:/Users/SeuNome/Desktop/banco.dta")

# Se o banco já estiver carregado no R como "dados", não precisa fazer nada.


# ==============================================================================
# CONFERIR SE O BANCO EXISTE
# ==============================================================================

if (!exists("dados")) {
  stop(
    "O objeto 'dados' não existe no R. ",
    "Carregue seu banco primeiro usando, por exemplo:\n\n",
    "dados <- read_dta('C:/caminho/seu_banco.dta')"
  )
}


# ==============================================================================
# FUNÇÃO PARA PADRONIZAR VARIÁVEIS NUMÉRICAS
#
# Esta função ajuda quando o banco vem do REDCap, Stata ou SPSS e as
# variáveis podem estar como:
#   - numeric
#   - haven_labelled
#   - factor
#   - character contendo números
# ==============================================================================

converter_numerico <- function(x) {
  
  # Se for factor
  if (is.factor(x)) {
    
    x_char <- as.character(x)
    
    resultado <- suppressWarnings(
      as.numeric(x_char)
    )
    
    return(resultado)
  }
  
  # Se for labelled do haven
  if (inherits(x, "haven_labelled")) {
    
    resultado <- suppressWarnings(
      as.numeric(haven::zap_labels(x))
    )
    
    return(resultado)
  }
  
  # Caso normal
  resultado <- suppressWarnings(
    as.numeric(x)
  )
  
  return(resultado)
}


# ==============================================================================
# VARIÁVEIS NECESSÁRIAS
# ==============================================================================

itens <- c(
  "b2",
  "c4",
  "c11",
  "d1",
  "d6",
  "d9",
  "d14",
  "e2",
  "e6",
  "e7",
  "e9",
  "f3",
  "g9",
  "g17",
  "g20",
  "h1",
  "h5",
  "h7",
  "h11",
  "i1",
  "i3",
  "j4"
)


variaveis_necessarias <- c(
  "record_id",
  "a1",
  "a2",
  "a3",
  itens
)


# ==============================================================================
#  VERIFICAR VARIÁVEIS
# ==============================================================================

variaveis_faltantes <- setdiff(
  variaveis_necessarias,
  names(dados)
)

if (length(variaveis_faltantes) > 0) {
  
  stop(
    "As seguintes variáveis não foram encontradas no banco:\n",
    paste(
      variaveis_faltantes,
      collapse = ", "
    )
  )
}


# ==============================================================================
# PADRONIZAR A1, A2, A3 E OS 22 ITENS COMO NUMÉRICOS
# ==============================================================================

variaveis_numericas <- c(
  "a1",
  "a2",
  "a3",
  itens
)

for (var in variaveis_numericas) {
  
  dados[[var]] <- converter_numerico(
    dados[[var]]
  )
}


# ==============================================================================
# CONFERÊNCIA DAS VARIÁVEIS
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("NÚMERO DE PARTICIPANTES\n")
cat("============================================================\n")

cat(
  "N =",
  nrow(dados),
  "\n"
)


cat("\n")
cat("============================================================\n")
cat("NÚMERO DE ITENS\n")
cat("============================================================\n")

cat(
  "Itens do questionário =",
  length(itens),
  "\n"
)

stopifnot(
  length(itens) == 22
)


# ==============================================================================
# CONFERÊNCIA DOS ITENS
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("DISTRIBUIÇÃO DOS 22 ITENS\n")
cat("============================================================\n")

for (var in itens) {
  
  cat("\n--------------------------------------------------\n")
  cat("Variável:", var, "\n")
  cat("--------------------------------------------------\n")
  
  print(
    table(
      dados[[var]],
      useNA = "ifany"
    )
  )
}


# ==============================================================================
# CONFERÊNCIA DA AFILIAÇÃO
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("DISTRIBUIÇÃO DE A1\n")
cat("============================================================\n")

print(
  table(
    dados$a1,
    useNA = "ifany"
  )
)


cat("\n")
cat("============================================================\n")
cat("DISTRIBUIÇÃO DE A2\n")
cat("============================================================\n")

print(
  table(
    dados$a2,
    useNA = "ifany"
  )
)


cat("\n")
cat("============================================================\n")
cat("DISTRIBUIÇÃO DE A3\n")
cat("============================================================\n")

print(
  table(
    dados$a3,
    useNA = "ifany"
  )
)


# ==============================================================================
# VERIFICAR OS VALORES DE A1/A2/A3
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("PRIMEIROS REGISTROS DE A1/A2/A3\n")
cat("============================================================\n")

print(
  dados %>%
    select(
      record_id,
      a1,
      a2,
      a3
    ) %>%
    head(20)
)


# ==============================================================================
# CALCULAR GRAU DE AFILIAÇÃO
#
# Resultado:
#
#   1 = todas as respostas NÃO
#   2 = serviços diferentes / apenas um SIM
#   3 = dois SIM referentes ao mesmo serviço
#   4 = três SIM referentes ao mesmo serviço
# ==============================================================================

dados$afiliacao <- NA_real_


# ------------------------------------------------------------------------------
# GRAU 1
#
# A1 = NÃO
# A2 = NÃO
# A3 = NÃO
# ------------------------------------------------------------------------------

idx <- dados$a1 == 0 &
  dados$a2 == 0 &
  dados$a3 == 0

idx[is.na(idx)] <- FALSE

dados$afiliacao[idx] <- 1


# ------------------------------------------------------------------------------
# GRAU 4
#
# A1 = SIM
# A2 = SIM, mesmo serviço de A1
# A3 = SIM, mesmo serviço de A1 e A2
# ------------------------------------------------------------------------------

idx <- dados$a1 == 1 &
  dados$a2 == 1 &
  dados$a3 == 1

idx[is.na(idx)] <- FALSE

dados$afiliacao[idx] <- 4


# ------------------------------------------------------------------------------
# GRAU 3
#
# A1 e A2 são o mesmo serviço; A3 = NÃO
# ------------------------------------------------------------------------------

idx <- dados$a1 == 1 &
  dados$a2 == 1 &
  dados$a3 == 0

idx[is.na(idx)] <- FALSE

dados$afiliacao[idx] <- 3


# ------------------------------------------------------------------------------
# A1 e A3 são o mesmo serviço; A2 = NÃO
# ------------------------------------------------------------------------------

idx <- dados$a1 == 1 &
  dados$a2 == 0 &
  dados$a3 == 2

idx[is.na(idx)] <- FALSE

dados$afiliacao[idx] <- 3


# ------------------------------------------------------------------------------
# A2 e A3 são o mesmo serviço; A1 = NÃO
# ------------------------------------------------------------------------------

idx <- dados$a1 == 0 &
  dados$a2 == 1 &
  dados$a3 == 3

idx[is.na(idx)] <- FALSE

dados$afiliacao[idx] <- 3


# ------------------------------------------------------------------------------
# GRAU 2
#
# Somente A1 = SIM
# ------------------------------------------------------------------------------

idx <- dados$a1 == 1 &
  dados$a2 == 0 &
  dados$a3 == 0

idx[is.na(idx)] <- FALSE

dados$afiliacao[idx] <- 2


# ------------------------------------------------------------------------------
# Somente A2 = SIM
# ------------------------------------------------------------------------------

idx <- dados$a1 == 0 &
  dados$a2 == 1 &
  dados$a3 == 0

idx[is.na(idx)] <- FALSE

dados$afiliacao[idx] <- 2


# ------------------------------------------------------------------------------
# Somente A3 = SIM
# ------------------------------------------------------------------------------

idx <- dados$a1 == 0 &
  dados$a2 == 0 &
  !is.na(dados$a3) &
  dados$a3 != 0

idx[is.na(idx)] <- FALSE

dados$afiliacao[idx] <- 2


# ------------------------------------------------------------------------------
# Serviços diferentes
# ------------------------------------------------------------------------------

idx <- dados$a1 == 1 &
  dados$a2 == 2 &
  dados$a3 == 4

idx[is.na(idx)] <- FALSE

dados$afiliacao[idx] <- 2


# ==============================================================================
# DISTRIBUIÇÃO FINAL DA AFILIAÇÃO
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("DISTRIBUIÇÃO DO GRAU DE AFILIAÇÃO\n")
cat("============================================================\n")

print(
  table(
    dados$afiliacao,
    useNA = "ifany"
  )
)


# ==============================================================================
# VERIFICAR CASOS DE AFILIAÇÃO NÃO CLASSIFICADOS
# ==============================================================================

casos_afiliacao_na <- dados %>%
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


cat("\n")
cat("============================================================\n")
cat("CASOS DE AFILIAÇÃO NÃO CLASSIFICADOS\n")
cat("============================================================\n")

print(
  casos_afiliacao_na,
  n = Inf
)


# ==============================================================================
# CRIAR CÓPIAS DOS 22 ITENS
# ==============================================================================

for (var in itens) {
  
  nome_novo <- paste0(
    "pc_",
    var
  )
  
  dados[[nome_novo]] <- dados[[var]]
}


pc_itens <- paste0(
  "pc_",
  itens
)


# ==============================================================================
# CONTAGEM DE AUSÊNCIAS
#
# Ausência =
#   - NA
#   - código 9
#
# IMPORTANTE:
# A afiliação NÃO entra nessa contagem, exatamente como no Stata original.
# ==============================================================================

# Criar matriz explicitamente
# Isso corrige o problema quando o banco possui somente 1 participante.

mat_missing <- matrix(
  FALSE,
  nrow = nrow(dados),
  ncol = length(pc_itens)
)

colnames(mat_missing) <- pc_itens


for (i in seq_along(pc_itens)) {
  
  var <- pc_itens[i]
  
  mat_missing[, i] <-
    is.na(dados[[var]]) |
    dados[[var]] == 9
}


dados$n_missing_pcatool <- rowSums(
  mat_missing
)

dados$n_missing_pcatool <- as.integer(
  dados$n_missing_pcatool
)


# ==============================================================================
# PERCENTUAL DE AUSÊNCIA
#
# 22 itens + 1 afiliação = 23 componentes
# ==============================================================================

dados$pct_missing_pcatool <-
  (
    dados$n_missing_pcatool /
      23
  ) * 100


# ==============================================================================
# ELEGIBILIDADE
#
# 50% de 23 = 11,5
#
# Como o número de ausências é inteiro:
#
#   12 ou mais -> não elegível
#   11 ou menos -> elegível
# ==============================================================================

dados$pcatool_elegivel <- NA_integer_


idx <- dados$n_missing_pcatool >= 12

idx[is.na(idx)] <- FALSE

dados$pcatool_elegivel[idx] <- 0


idx <- dados$n_missing_pcatool < 12

idx[is.na(idx)] <- FALSE

dados$pcatool_elegivel[idx] <- 1


# Criar versão com rótulos
dados$pcatool_elegivel_label <- factor(
  dados$pcatool_elegivel,
  levels = c(
    0,
    1
  ),
  labels = c(
    "Não elegível - >=50% ausentes",
    "Elegível - <50% ausentes"
  )
)


# ==============================================================================
# DISTRIBUIÇÃO DA ELEGIBILIDADE
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("ELEGIBILIDADE\n")
cat("============================================================\n")

print(
  table(
    dados$pcatool_elegivel_label,
    useNA = "ifany"
  )
)


# ==============================================================================
# IMPUTAÇÃO DOS CÓDIGOS 9
#
# Apenas participantes elegíveis:
#
#   9 -> 2
#
# NA verdadeiro permanece NA.
# ==============================================================================

for (var in pc_itens) {
  
  idx <-
    dados$pcatool_elegivel == 1 &
    !is.na(dados[[var]]) &
    dados[[var]] == 9
  
  idx[is.na(idx)] <- FALSE
  
  dados[[var]][idx] <- 2
}


# ==============================================================================
# INVERSÃO DE C11
#
# 1 -> 4
# 2 -> 3
# 3 -> 2
# 4 -> 1
# ==============================================================================

dados$pc_c11_inv <- NA_real_


idx <-
  !is.na(dados$pc_c11) &
  dados$pc_c11 >= 1 &
  dados$pc_c11 <= 4

idx[is.na(idx)] <- FALSE


dados$pc_c11_inv[idx] <-
  5 - dados$pc_c11[idx]


# ==============================================================================
# INVERSÃO DE D14
# ==============================================================================

dados$pc_d14_inv <- NA_real_


idx <-
  !is.na(dados$pc_d14) &
  dados$pc_d14 >= 1 &
  dados$pc_d14 <= 4

idx[is.na(idx)] <- FALSE


dados$pc_d14_inv[idx] <-
  5 - dados$pc_d14[idx]


# ==============================================================================
# VERIFICAR INVERSÃO
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("C11 ORIGINAL x C11 INVERTIDO\n")
cat("============================================================\n")

print(
  table(
    dados$pc_c11,
    dados$pc_c11_inv,
    useNA = "ifany"
  )
)


cat("\n")
cat("============================================================\n")
cat("D14 ORIGINAL x D14 INVERTIDO\n")
cat("============================================================\n")

print(
  table(
    dados$pc_d14,
    dados$pc_d14_inv,
    useNA = "ifany"
  )
)


# ==============================================================================
# VERIFICAR VALORES INVÁLIDOS
#
# Valores válidos:
#   1, 2, 3, 4
#
# NA é permitido.
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("VERIFICAÇÃO DE VALORES INVÁLIDOS\n")
cat("============================================================\n")


for (var in pc_itens) {
  
  idx_invalidos <-
    !is.na(dados[[var]]) &
    !(dados[[var]] %in% 1:4)
  
  n_invalidos <-
    sum(idx_invalidos)
  
  if (n_invalidos > 0) {
    
    cat(
      "\nATENÇÃO:",
      var,
      "possui",
      n_invalidos,
      "valor(es) fora da escala 1-4.\n"
    )
    
    print(
      table(
        dados[[var]][idx_invalidos],
        useNA = "ifany"
      )
    )
  }
}


# ==============================================================================
# DEFINIR OS 22 COMPONENTES DO ESCORE
#
# C11 e D14 já aparecem invertidos.
# ==============================================================================

itens_score <- c(
  "pc_b2",
  "pc_c4",
  "pc_c11_inv",
  "pc_d1",
  "pc_d6",
  "pc_d9",
  "pc_d14_inv",
  "pc_e2",
  "pc_e6",
  "pc_e7",
  "pc_e9",
  "pc_f3",
  "pc_g9",
  "pc_g17",
  "pc_g20",
  "pc_h1",
  "pc_h5",
  "pc_h7",
  "pc_h11",
  "pc_i1",
  "pc_i3",
  "pc_j4"
)


# ==============================================================================
# CONTAGEM DE ITENS VÁLIDOS
# ==============================================================================

dados$n_valid_22 <- rowSums(
  !is.na(
    dados[, itens_score, drop = FALSE]
  )
)

dados$n_valid_22 <- as.integer(
  dados$n_valid_22
)


# ==============================================================================
# SOMA DOS 22 ITENS
#
# rowSums(..., na.rm = TRUE) é usado somente para criar a soma.
#
# O cálculo final exige n_valid_22 == 22.
# Portanto, nenhum missing será ignorado indevidamente.
# ==============================================================================

dados$soma_22_itens <- rowSums(
  dados[, itens_score, drop = FALSE],
  na.rm = TRUE
)


# ==============================================================================
# CÁLCULO DO ESCORE GERAL
#
# Escore Geral =
#
#   (Afiliação + 22 itens) / 23
#
# Condições:
#
#   - elegível
#   - afiliação não missing
#   - 22 itens válidos
# ==============================================================================

dados$pcatool_sum <- NA_real_


idx_escore <-
  dados$pcatool_elegivel == 1 &
  !is.na(dados$afiliacao) &
  dados$n_valid_22 == 22

idx_escore[is.na(idx_escore)] <- FALSE


dados$pcatool_sum[idx_escore] <-
  dados$afiliacao[idx_escore] +
  dados$soma_22_itens[idx_escore]


dados$pcatool_geral <- NA_real_


dados$pcatool_geral[idx_escore] <-
  dados$pcatool_sum[idx_escore] /
  23


# ==============================================================================
# TRANSFORMAÇÃO PARA 0–10
#
# Fórmula:
#
#   ((Escore - 1) / 3) * 10
# ==============================================================================

dados$pcatool_0_10 <- NA_real_


idx_0_10 <-
  !is.na(dados$pcatool_geral)

idx_0_10[is.na(idx_0_10)] <- FALSE


dados$pcatool_0_10[idx_0_10] <-
  (
    (
      dados$pcatool_geral[idx_0_10] -
        1
    ) /
      3
  ) * 10


# ==============================================================================
# CLASSIFICAÇÃO
#
# 0 = Baixo
# 1 = Alto
# ==============================================================================

dados$pcatool_class <- NA_integer_


idx <-
  !is.na(dados$pcatool_0_10) &
  dados$pcatool_0_10 < 6.6

idx[is.na(idx)] <- FALSE

dados$pcatool_class[idx] <- 0


idx <-
  !is.na(dados$pcatool_0_10) &
  dados$pcatool_0_10 >= 6.6

idx[is.na(idx)] <- FALSE

dados$pcatool_class[idx] <- 1


dados$pcatool_class_label <- factor(
  dados$pcatool_class,
  levels = c(
    0,
    1
  ),
  labels = c(
    "Baixo (<6,6)",
    "Alto (>=6,6)"
  )
)


# ==============================================================================
# ESCORES INDIVIDUAIS DOS 22 ITENS – ESCALA 0 A 10
# ==============================================================================

nomes_score <- c(
  "score_b2",
  "score_c4",
  "score_c11",
  "score_d1",
  "score_d6",
  "score_d9",
  "score_d14",
  "score_e2",
  "score_e6",
  "score_e7",
  "score_e9",
  "score_f3",
  "score_g9",
  "score_g17",
  "score_g20",
  "score_h1",
  "score_h5",
  "score_h7",
  "score_h11",
  "score_i1",
  "score_i3",
  "score_j4"
)


for (i in seq_along(itens_score)) {
  
  var <- itens_score[i]
  
  novo_nome <- nomes_score[i]
  
  dados[[novo_nome]] <- NA_real_
  
  
  idx <-
    !is.na(dados[[var]]) &
    dados[[var]] >= 1 &
    dados[[var]] <= 4
  
  idx[is.na(idx)] <- FALSE
  
  
  dados[[novo_nome]][idx] <-
    (
      (
        dados[[var]][idx] -
          1
      ) /
        3
    ) * 10
}


# ==============================================================================
# ESCORE DE AFILIAÇÃO – ESCALA 0 A 10
# ==============================================================================

dados$afiliacao_0_10 <- NA_real_


idx <-
  !is.na(dados$afiliacao) &
  dados$afiliacao >= 1 &
  dados$afiliacao <= 4

idx[is.na(idx)] <- FALSE


dados$afiliacao_0_10[idx] <-
  (
    (
      dados$afiliacao[idx] -
        1
    ) /
      3
  ) * 10


# ==============================================================================
# RESUMO DOS RESULTADOS
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("RESUMO – PCATOOL BRASIL ADULTO REDUZIDO\n")
cat("============================================================\n")


cat("\nNúmero de participantes:", nrow(dados), "\n")


cat("\n")
cat("AFILIAÇÃO:\n")

print(
  table(
    dados$afiliacao,
    useNA = "ifany"
  )
)


cat("\n")
cat("ELEGIBILIDADE:\n")

print(
  table(
    dados$pcatool_elegivel_label,
    useNA = "ifany"
  )
)


cat("\n")
cat("CLASSIFICAÇÃO:\n")

print(
  table(
    dados$pcatool_class_label,
    useNA = "ifany"
  )
)


# ==============================================================================
# RESUMO DO ESCORE GERAL – 1 A 4
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("ESCORE GERAL – ESCALA 1 A 4\n")
cat("============================================================\n")

print(
  summary(
    dados$pcatool_geral
  )
)


# ==============================================================================
# RESUMO DO ESCORE GERAL – 0 A 10
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("ESCORE GERAL – ESCALA 0 A 10\n")
cat("============================================================\n")

print(
  summary(
    dados$pcatool_0_10
  )
)


# ==============================================================================
#  MÉDIA E DESVIO-PADRÃO DO ESCORE GERAL
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("MÉDIA E DP – ESCORE GERAL\n")
cat("============================================================\n")


n_geral <- sum(
  !is.na(dados$pcatool_geral)
)


media_geral <- mean(
  dados$pcatool_geral,
  na.rm = TRUE
)


dp_geral <- sd(
  dados$pcatool_geral,
  na.rm = TRUE
)


cat(
  "N =",
  n_geral,
  "\n"
)

cat(
  "Média =",
  round(media_geral, 2),
  "\n"
)

cat(
  "DP =",
  round(dp_geral, 2),
  "\n"
)


# ==============================================================================
# MÉDIA E DP – ESCORE GERAL 0 A 10
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("MÉDIA E DP – ESCORE GERAL 0 A 10\n")
cat("============================================================\n")


n_geral_010 <- sum(
  !is.na(dados$pcatool_0_10)
)


media_geral_010 <- mean(
  dados$pcatool_0_10,
  na.rm = TRUE
)


dp_geral_010 <- sd(
  dados$pcatool_0_10,
  na.rm = TRUE
)


cat(
  "N =",
  n_geral_010,
  "\n"
)

cat(
  "Média =",
  round(media_geral_010, 2),
  "\n"
)

cat(
  "DP =",
  round(dp_geral_010, 2),
  "\n"
)


# ==============================================================================
# MÉDIAS DOS 22 ITENS – ESCALA 0 A 10
# ==============================================================================

variaveis_scores <- c(
  nomes_score,
  "afiliacao_0_10"
)


resumo_itens <- data.frame(
  
  item = variaveis_scores,
  
  N = sapply(
    dados[, variaveis_scores, drop = FALSE],
    function(x) {
      sum(!is.na(x))
    }
  ),
  
  media = sapply(
    dados[, variaveis_scores, drop = FALSE],
    function(x) {
      if (all(is.na(x))) {
        return(NA_real_)
      }
      
      mean(
        x,
        na.rm = TRUE
      )
    }
  ),
  
  dp = sapply(
    dados[, variaveis_scores, drop = FALSE],
    function(x) {
      
      if (sum(!is.na(x)) <= 1) {
        return(NA_real_)
      }
      
      sd(
        x,
        na.rm = TRUE
      )
    }
  )
)


cat("\n")
cat("============================================================\n")
cat("MÉDIAS DOS ITENS – ESCALA 0 A 10\n")
cat("============================================================\n")

print(
  resumo_itens,
  row.names = FALSE
)


# ==============================================================================
# VERIFICAR MISSING APÓS IMPUTAÇÃO
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("MISSING NOS ITENS APÓS IMPUTAÇÃO\n")
cat("============================================================\n")


for (var in pc_itens) {
  
  n_missing <- sum(
    is.na(dados[[var]])
  )
  
  cat(
    var,
    ":",
    n_missing,
    "missing\n"
  )
}


# ==============================================================================
# CASOS SEM ESCORE GERAL
# ==============================================================================

casos_sem_escore <- dados %>%
  filter(
    is.na(pcatool_geral)
  ) %>%
  select(
    record_id,
    a1,
    a2,
    a3,
    afiliacao,
    n_missing_pcatool,
    pct_missing_pcatool,
    pcatool_elegivel,
    pcatool_geral
  )


cat("\n")
cat("============================================================\n")
cat("CASOS SEM ESCORE GERAL\n")
cat("============================================================\n")


cat(
  "Número de casos sem escore =",
  nrow(casos_sem_escore),
  "\n"
)


if (nrow(casos_sem_escore) > 0) {
  
  print(
    casos_sem_escore,
    n = Inf
  )
}


# ==============================================================================
# EXPORTAR BANCO FINAL
# ==============================================================================

# --------------------------------------------------------------------
# Opção 1: salvar como RDS
# --------------------------------------------------------------------
#
# saveRDS(
#   dados,
#   "pcatool_adulto_reduzido_analisado.rds"
# )


# --------------------------------------------------------------------
# Opção 2: salvar como CSV
# --------------------------------------------------------------------
#
# write.csv(
#   dados,
#   "pcatool_adulto_reduzido_analisado.csv",
#   row.names = FALSE,
#   na = ""
# )


# --------------------------------------------------------------------
# Opção 3: salvar novamente como arquivo Stata
# --------------------------------------------------------------------
#
# write_dta(
#   dados,
#   "pcatool_adulto_reduzido_analisado.dta"
# )


# ==============================================================================
# 42. VISUALIZAR AS PRINCIPAIS VARIÁVEIS GERADAS
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("BANCO FINAL – PRINCIPAIS VARIÁVEIS\n")
cat("============================================================\n")


print(
  dados %>%
    select(
      record_id,
      a1,
      a2,
      a3,
      afiliacao,
      n_missing_pcatool,
      pct_missing_pcatool,
      pcatool_elegivel,
      pcatool_geral,
      pcatool_0_10,
      pcatool_class
    ) %>%
    head(20)
)


# ==============================================================================
# FIM
# ==============================================================================

