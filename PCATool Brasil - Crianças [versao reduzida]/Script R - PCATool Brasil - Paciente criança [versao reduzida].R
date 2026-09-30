###############################################################################
###############################################################################
#              PCATOOL-BRASIL – PACIENTES CRIANÇAS
#                    VERSÃO REDUZIDA
#
#                 ANÁLISE E CÁLCULO DOS ESCORES
#
# Script no R
###############################################################################
###############################################################################

# =============================================================================
# PACOTES
# =============================================================================

# Instale uma única vez, se necessário:
#
# install.packages(c(
#   "haven",
#   "dplyr",
#   "readr",
#   "writexl"
# ))

library(haven)
library(dplyr)
library(readr)
library(writexl)


# =============================================================================
# ABRIR O BANCO DE DADOS
# =============================================================================
# >>> ALTERE SOMENTE ESTA LINHA PARA O CAMINHO DO SEU BANCO <<<
#arquivo_entrada <- "seu_banco.dta"
#dados <- read_dta(arquivo_entrada)

# =============================================================================
# LIMPAR VARIÁVEIS DERIVADAS DE EXECUÇÕES ANTERIORES
# =============================================================================

# IMPORTANTE:
#
# Este bloco NÃO exclui nenhuma variável original do questionário.
# Ele exclui somente as variáveis criadas por este script.
#
# Assim, o código pode ser executado novamente no mesmo banco sem
# gerar conflitos de nomes.


# Variáveis utilizadas na afiliação
vars_derivadas_afil <- c(
  "serv1",
  "serv2",
  "serv3",
  "n_sim_afil",
  "n_servicos_afil",
  "afiliacao",
  "afiliacao_0a10"
)


# Variáveis de controle de missing
vars_derivadas_missing <- c(
  "n_missing_pcat",
  "perc_missing_pcat",
  "pcat_calculavel"
)


# Variáveis do escore geral
vars_derivadas_escore <- c(
  "soma_pcat",
  "escore_geral_1a4",
  "escore_geral_0a10"
)


# Variáveis de controle da imputação
vars_derivadas_imputacao <- c(
  "n_nove_orig",
  "houve_imputacao"
)


# Variável de inconsistência
vars_derivadas_consistencia <- c(
  "inconsistencia_afil"
)


# Variáveis derivadas dos 27 itens
vars_itens <- c(
  "b1", "b2",
  "c1", "c3", "c4",
  "d1", "d2", "d6", "d8", "d11",
  "e4", "e5", "e6",
  "f2",
  "g3", "g4", "g5", "g6", "g9",
  "h3", "h4", "h5",
  "i1", "i2", "i3",
  "j2", "j4"
)

vars_derivadas_itens <- c(
  paste0(vars_itens, "_pcat"),
  paste0(vars_itens, "_pcat_0a10")
)


vars_derivadas <- c(
  vars_derivadas_afil,
  vars_derivadas_missing,
  vars_derivadas_escore,
  vars_derivadas_imputacao,
  vars_derivadas_consistencia,
  vars_derivadas_itens
)


# Remove somente as variáveis derivadas, se existirem
dados <- dados %>%
  select(-any_of(vars_derivadas))


# =============================================================================
# CONFERIR AS VARIÁVEIS ORIGINAIS
# =============================================================================

# Variáveis de afiliação
vars_afil <- c("a1", "a2", "a3")


# Conferir A1-A3
faltantes_afil <- setdiff(vars_afil, names(dados))

if (length(faltantes_afil) > 0) {
  stop(
    paste(
      "ERRO: as seguintes variáveis de afiliação não foram encontradas:",
      paste(faltantes_afil, collapse = ", ")
    )
  )
}


# Conferir os 27 itens
faltantes_itens <- setdiff(vars_itens, names(dados))

if (length(faltantes_itens) > 0) {
  stop(
    paste(
      "ERRO: as seguintes variáveis dos itens não foram encontradas:",
      paste(faltantes_itens, collapse = ", ")
    )
  )
}


cat("\nTodas as variáveis necessárias foram encontradas.\n")


# =============================================================================
# CONFERIR NÚMERO DE ITENS
# =============================================================================

n_itens <- length(vars_itens)

cat("\nNúmero de itens B-J =", n_itens, "\n")

if (n_itens != 27) {
  stop("ERRO: o número de itens não é igual a 27.")
}


# =============================================================================
# PARTE I — CÁLCULO DO GRAU DE AFILIAÇÃO
# =============================================================================

# Estrutura:
#
# A1:
#   0 = Não
#   1 = Sim
#
# A2:
#   0 = Não
#   1 = Sim, mesmo serviço referido em A1
#   2 = Sim, serviço diferente de A1
#
# A3:
#   0 = Não
#   1 = Sim, mesmo serviço de A1 e A2
#   2 = Sim, mesmo serviço de A1
#   3 = Sim, mesmo serviço de A2
#   4 = Sim, serviço diferente de A1 e A2


# =============================================================================
# IDENTIFICADOR DO SERVIÇO DE A1
# =============================================================================

dados <- dados %>%
  mutate(
    serv1 = case_when(
      a1 == 0 ~ 0,
      a1 == 1 ~ 1,
      TRUE ~ NA_real_
    )
  )


# =============================================================================
# IDENTIFICADOR DO SERVIÇO DE A2
# =============================================================================

dados <- dados %>%
  mutate(
    serv2 = case_when(
      a2 == 0 ~ 0,
      a2 == 1 ~ 1,
      a2 == 2 ~ 2,
      TRUE ~ NA_real_
    )
  )


# =============================================================================
# IDENTIFICADOR DO SERVIÇO DE A3
# =============================================================================

dados <- dados %>%
  mutate(
    serv3 = case_when(
      
      # A3 = 0: nenhum serviço
      a3 == 0 ~ 0,
      
      # A3 = 1: mesmo serviço de A1 e A2
      a3 == 1 & a1 == 1 & a2 == 1 ~ 1,
      
      # A3 = 2: mesmo serviço de A1
      a3 == 2 & a1 == 1 ~ 1,
      
      # A3 = 3: mesmo serviço de A2
      a3 == 3 & a2 > 0 ~ serv2,
      
      # A3 = 4: novo serviço
      a3 == 4 ~ 3,
      
      TRUE ~ NA_real_
    )
  )


# =============================================================================
# NÚMERO DE RESPOSTAS SIM
# =============================================================================

dados <- dados %>%
  mutate(
    n_sim_afil = case_when(
      !is.na(a1) & !is.na(a2) & !is.na(a3) ~
        (a1 == 1) + (a2 > 0) + (a3 > 0),
      
      TRUE ~ NA_integer_
    )
  )


# =============================================================================
#  NÚMERO DE SERVIÇOS DIFERENTES
# =============================================================================

dados <- dados %>%
  mutate(
    n_servicos_afil = case_when(
      
      # Nenhum serviço
      serv1 == 0 &
        serv2 == 0 &
        serv3 == 0 ~ 0,
      
      # Primeiro serviço
      serv1 > 0 ~ 1,
      
      TRUE ~ NA_real_
    )
  )


# Segundo serviço diferente
dados <- dados %>%
  mutate(
    n_servicos_afil = if_else(
      !is.na(n_servicos_afil) &
        serv2 > 0 &
        serv2 != serv1,
      n_servicos_afil + 1,
      n_servicos_afil
    )
  )


# Terceiro serviço diferente
dados <- dados %>%
  mutate(
    n_servicos_afil = if_else(
      !is.na(n_servicos_afil) &
        serv3 > 0 &
        serv3 != serv1 &
        serv3 != serv2,
      n_servicos_afil + 1,
      n_servicos_afil
    )
  )


# =============================================================================
# CÁLCULO DO GRAU DE AFILIAÇÃO
# =============================================================================

dados <- dados %>%
  mutate(
    afiliacao = case_when(
      
      # Grau 1:
      # Todas as respostas são NÃO
      a1 == 0 &
        a2 == 0 &
        a3 == 0 ~ 1,
      
      # Grau 4:
      # Todas são SIM e todas se referem ao mesmo serviço
      n_sim_afil == 3 &
        n_servicos_afil == 1 ~ 4,
      
      # Grau 3:
      # Duas ou mais respostas SIM e mesmo serviço
      n_sim_afil >= 2 &
        n_servicos_afil == 1 ~ 3,
      
      # Grau 2:
      # Pelo menos uma resposta SIM, sem único serviço comum
      n_sim_afil >= 1 ~ 2,
      
      # Missing em A1, A2 ou A3
      TRUE ~ NA_real_
    )
  )


# =============================================================================
# RÓTULOS DA AFILIAÇÃO
# =============================================================================

dados$afiliacao <- labelled(
  dados$afiliacao,
  labels = c(
    "1 - Todas NÃO" = 1,
    "2 - Serviços diferentes" = 2,
    "3 - Dois ou mais SIM mesmo serviço" = 3,
    "4 - Todos SIM mesmo serviço" = 4
  ),
  label = "Grau de afiliação PCATool"
)


# =============================================================================
# VERIFICAR AFILIAÇÃO
# =============================================================================

cat("\n==============================================================\n")
cat("DISTRIBUIÇÃO DA AFILIAÇÃO\n")
cat("==============================================================\n")

print(table(dados$a1, useNA = "ifany"))
print(table(dados$a2, useNA = "ifany"))
print(table(dados$a3, useNA = "ifany"))

print(table(dados$afiliacao, useNA = "ifany"))

print(table(dados$n_sim_afil, useNA = "ifany"))
print(table(dados$n_servicos_afil, useNA = "ifany"))


# =============================================================================
# PARTE II — CRIAÇÃO DAS CÓPIAS DOS 27 ITENS
# =============================================================================

for (v in vars_itens) {
  
  nome_novo <- paste0(v, "_pcat")
  
  dados[[nome_novo]] <- dados[[v]]
  
  attr(dados[[nome_novo]], "label") <-
    paste0(v, " - variável utilizada no cálculo PCATool")
}


# =============================================================================
# PARTE III — INVERSÃO DO ITEM C4
# =============================================================================

# Original:
#
#   4 -> 1
#   3 -> 2
#   2 -> 3
#   1 -> 4
#   9 -> 9
#   missing -> missing

dados <- dados %>%
  mutate(
    c4_pcat = case_when(
      c4 == 4 ~ 1,
      c4 == 3 ~ 2,
      c4 == 2 ~ 3,
      c4 == 1 ~ 4,
      c4 == 9 ~ 9,
      is.na(c4) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )


attr(dados$c4_pcat, "label") <- "C4 - escala invertida"


# =============================================================================
# CONFERIR INVERSÃO DO C4
# =============================================================================

cat("\n==============================================================\n")
cat("VERIFICAÇÃO DO C4\n")
cat("==============================================================\n")

print(table(dados$c4, dados$c4_pcat, useNA = "ifany"))


# =============================================================================
# PARTE IV — DEFINIÇÃO DOS 28 COMPONENTES
# =============================================================================

componentes <- c(
  "afiliacao",
  "b1_pcat", "b2_pcat",
  "c1_pcat", "c3_pcat", "c4_pcat",
  "d1_pcat", "d2_pcat", "d6_pcat", "d8_pcat", "d11_pcat",
  "e4_pcat", "e5_pcat", "e6_pcat",
  "f2_pcat",
  "g3_pcat", "g4_pcat", "g5_pcat", "g6_pcat", "g9_pcat",
  "h3_pcat", "h4_pcat", "h5_pcat",
  "i1_pcat", "i2_pcat", "i3_pcat",
  "j2_pcat", "j4_pcat"
)


n_componentes <- length(componentes)

cat(
  "\nNúmero de componentes do Escore Geral =",
  n_componentes,
  "\n"
)

if (n_componentes != 28) {
  stop("ERRO: o número de componentes não é igual a 28.")
}


# =============================================================================
# PARTE V — MISSING E VALORES 9
# =============================================================================

# São considerados ausentes:
#
#   - NA
#   - valor 9
#
# São contados os 28 componentes.


dados$n_missing_pcat <- rowSums(
  as.data.frame(
    lapply(
      dados[componentes],
      function(x) is.na(x) | x == 9
    )
  )
)



attr(dados$n_missing_pcat, "label") <-
  "Número de componentes ausentes ou 9"


# =============================================================================
# PERCENTUAL DE AUSENTES
# =============================================================================

dados$perc_missing_pcat <-
  (dados$n_missing_pcat / 28) * 100


attr(dados$perc_missing_pcat, "label") <-
  "Percentual de componentes ausentes"


# =============================================================================
# ESCORE CALCULÁVEL
# =============================================================================

# < 50%:
#   calculável
#
# >= 50%:
#   não calculável
#
# 50% de 28 = 14
#
# 0-13  = calculável
# 14-28 = não calculável

dados$pcat_calculavel <- case_when(
  dados$n_missing_pcat < 14 ~ 1,
  dados$n_missing_pcat >= 14 ~ 0,
  TRUE ~ NA_real_
)


dados$pcat_calculavel <- labelled(
  dados$pcat_calculavel,
  labels = c(
    "Não calculável - >=50% ausentes" = 0,
    "Calculável - <50% ausentes" = 1
  ),
  label = "Escore calculável (<50% de ausentes)"
)


# =============================================================================
# VERIFICAR MISSING
# =============================================================================

cat("\n==============================================================\n")
cat("MISSING DO PCATOOL\n")
cat("==============================================================\n")

print(table(dados$n_missing_pcat, useNA = "ifany"))
print(table(dados$pcat_calculavel, useNA = "ifany"))

cat("\nResumo de componentes ausentes:\n")
print(summary(dados$n_missing_pcat))

cat("\nResumo do percentual de ausentes:\n")
print(summary(dados$perc_missing_pcat))


# =============================================================================
# PARTE VI — IMPUTAÇÃO DE 9 PARA 2
# =============================================================================

# IMPORTANTE:
#
# A imputação ocorre somente quando:
#
#   pcat_calculavel == 1
#
# Regra:
#
#   9 -> 2


for (v in componentes) {
  
  dados[[v]] <- ifelse(
    dados$pcat_calculavel == 1 & dados[[v]] == 9,
    2,
    dados[[v]]
  )
}


# =============================================================================
# PARTE VII — ESCORE GERAL DA APS – 1 A 4
# =============================================================================

# Soma dos 28 componentes.
#
# Como os valores 9 já foram imputados quando calculável,
# podemos calcular a soma diretamente.


dados$soma_pcat <- ifelse(
  dados$pcat_calculavel == 1,
  rowSums(dados[componentes], na.rm = TRUE),
  NA_real_
)


attr(dados$soma_pcat, "label") <-
  "Soma dos 28 componentes do PCATool"


# =============================================================================
# ESCORE GERAL – ESCALA 1 A 4
# =============================================================================

dados$escore_geral_1a4 <- ifelse(
  dados$pcat_calculavel == 1,
  dados$soma_pcat / 28,
  NA_real_
)


attr(dados$escore_geral_1a4, "label") <-
  "Escore Geral da APS - escala 1 a 4"


# =============================================================================
# PARTE VIII — ESCORE GERAL – 0 A 10
# =============================================================================

# Transformação:
#
#   ((Escore - 1) / 3) * 10


dados$escore_geral_0a10 <- ifelse(
  !is.na(dados$escore_geral_1a4),
  ((dados$escore_geral_1a4 - 1) / 3) * 10,
  NA_real_
)


attr(dados$escore_geral_0a10, "label") <-
  "Escore Geral da APS - escala 0 a 10"


# =============================================================================
# PARTE IX — ESCORES INDIVIDUAIS DOS COMPONENTES – 0 A 10
# =============================================================================

for (v in componentes) {
  
  nome_010 <- paste0(v, "_0a10")
  
  dados[[nome_010]] <- ifelse(
    dados$pcat_calculavel == 1,
    ((dados[[v]] - 1) / 3) * 10,
    NA_real_
  )
}


# =============================================================================
# ESCORE DE AFILIAÇÃO 0-10
# =============================================================================

dados$afiliacao_0a10 <- ifelse(
  dados$pcat_calculavel == 1,
  ((dados$afiliacao - 1) / 3) * 10,
  NA_real_
)


attr(dados$afiliacao_0a10, "label") <-
  "Grau de afiliação - escala 0 a 10"


# =============================================================================
# PARTE X — LISTA DOS COMPONENTES EM 0-10
# =============================================================================

componentes_010 <- c(
  "afiliacao_0a10",
  "b1_pcat_0a10", "b2_pcat_0a10",
  "c1_pcat_0a10", "c3_pcat_0a10", "c4_pcat_0a10",
  "d1_pcat_0a10", "d2_pcat_0a10", "d6_pcat_0a10",
  "d8_pcat_0a10", "d11_pcat_0a10",
  "e4_pcat_0a10", "e5_pcat_0a10", "e6_pcat_0a10",
  "f2_pcat_0a10",
  "g3_pcat_0a10", "g4_pcat_0a10", "g5_pcat_0a10",
  "g6_pcat_0a10", "g9_pcat_0a10",
  "h3_pcat_0a10", "h4_pcat_0a10", "h5_pcat_0a10",
  "i1_pcat_0a10", "i2_pcat_0a10", "i3_pcat_0a10",
  "j2_pcat_0a10", "j4_pcat_0a10"
)


# =============================================================================
# PARTE XI — ESTATÍSTICAS DESCRITIVAS
# =============================================================================

cat("\n==============================================================\n")
cat("ESCORE GERAL DA APS – ESCALA 1 A 4\n")
cat("==============================================================\n")

print(summary(dados$escore_geral_1a4))


cat("\n==============================================================\n")
cat("ESCORE GERAL DA APS – ESCALA 0 A 10\n")
cat("==============================================================\n")

print(summary(dados$escore_geral_0a10))


# =============================================================================
# ESTATÍSTICAS PRINCIPAIS
# =============================================================================

estatisticas_gerais <- data.frame(
  variavel = c(
    "escore_geral_1a4",
    "escore_geral_0a10"
  ),
  
  n = c(
    sum(!is.na(dados$escore_geral_1a4)),
    sum(!is.na(dados$escore_geral_0a10))
  ),
  
  media = c(
    mean(dados$escore_geral_1a4, na.rm = TRUE),
    mean(dados$escore_geral_0a10, na.rm = TRUE)
  ),
  
  desvio_padrao = c(
    sd(dados$escore_geral_1a4, na.rm = TRUE),
    sd(dados$escore_geral_0a10, na.rm = TRUE)
  ),
  
  mediana = c(
    median(dados$escore_geral_1a4, na.rm = TRUE),
    median(dados$escore_geral_0a10, na.rm = TRUE)
  ),
  
  p25 = c(
    quantile(
      dados$escore_geral_1a4,
      0.25,
      na.rm = TRUE
    ),
    quantile(
      dados$escore_geral_0a10,
      0.25,
      na.rm = TRUE
    )
  ),
  
  p75 = c(
    quantile(
      dados$escore_geral_1a4,
      0.75,
      na.rm = TRUE
    ),
    quantile(
      dados$escore_geral_0a10,
      0.75,
      na.rm = TRUE
    )
  ),
  
  minimo = c(
    min(dados$escore_geral_1a4, na.rm = TRUE),
    min(dados$escore_geral_0a10, na.rm = TRUE)
  ),
  
  maximo = c(
    max(dados$escore_geral_1a4, na.rm = TRUE),
    max(dados$escore_geral_0a10, na.rm = TRUE)
  )
)

print(estatisticas_gerais)


# =============================================================================
# DISTRIBUIÇÃO DA AFILIAÇÃO
# =============================================================================

cat("\n==============================================================\n")
cat("GRAU DE AFILIAÇÃO\n")
cat("==============================================================\n")

print(table(dados$afiliacao, useNA = "ifany"))


estatisticas_afiliacao <- data.frame(
  variavel = c(
    "afiliacao",
    "afiliacao_0a10"
  ),
  
  n = c(
    sum(!is.na(dados$afiliacao)),
    sum(!is.na(dados$afiliacao_0a10))
  ),
  
  media = c(
    mean(dados$afiliacao, na.rm = TRUE),
    mean(dados$afiliacao_0a10, na.rm = TRUE)
  ),
  
  desvio_padrao = c(
    sd(dados$afiliacao, na.rm = TRUE),
    sd(dados$afiliacao_0a10, na.rm = TRUE)
  ),
  
  mediana = c(
    median(dados$afiliacao, na.rm = TRUE),
    median(dados$afiliacao_0a10, na.rm = TRUE)
  ),
  
  minimo = c(
    min(dados$afiliacao, na.rm = TRUE),
    min(dados$afiliacao_0a10, na.rm = TRUE)
  ),
  
  maximo = c(
    max(dados$afiliacao, na.rm = TRUE),
    max(dados$afiliacao_0a10, na.rm = TRUE)
  )
)

print(estatisticas_afiliacao)


# =============================================================================
# PARTE XII — ESTATÍSTICAS DOS 28 COMPONENTES
# =============================================================================

# Função auxiliar para estatísticas descritivas
calcular_estatisticas <- function(df, vars) {
  
  resultado <- lapply(vars, function(v) {
    
    x <- df[[v]]
    
    data.frame(
      variavel = v,
      n = sum(!is.na(x)),
      media = mean(x, na.rm = TRUE),
      desvio_padrao = sd(x, na.rm = TRUE),
      mediana = median(x, na.rm = TRUE),
      p25 = as.numeric(
        quantile(x, 0.25, na.rm = TRUE)
      ),
      p75 = as.numeric(
        quantile(x, 0.75, na.rm = TRUE)
      ),
      minimo = min(x, na.rm = TRUE),
      maximo = max(x, na.rm = TRUE)
    )
  })
  
  bind_rows(resultado)
}


# =============================================================================
# COMPONENTES – ESCALA ORIGINAL
# =============================================================================

cat("\n==============================================================\n")
cat("ESTATÍSTICAS DOS 28 COMPONENTES – ESCALA 1 A 4\n")
cat("==============================================================\n")

estatisticas_componentes_1a4 <-
  calcular_estatisticas(dados, componentes)

print(estatisticas_componentes_1a4)


# =============================================================================
# COMPONENTES – ESCALA 0 A 10
# =============================================================================

cat("\n==============================================================\n")
cat("ESTATÍSTICAS DOS COMPONENTES – ESCALA 0 A 10\n")
cat("==============================================================\n")

estatisticas_componentes_010 <-
  calcular_estatisticas(dados, componentes_010)

print(estatisticas_componentes_010)


# =============================================================================
# PARTE XIII — CONTROLE DA IMPUTAÇÃO
# =============================================================================

# IMPORTANTE:
#
# A contagem usa as variáveis originais.
#
# Assim, é possível identificar quantas respostas "9"
# cada entrevistado tinha ANTES da imputação.

dados$n_nove_orig <- rowSums(
  as.data.frame(
    lapply(
      dados[vars_itens],
      function(x) is.na(x) | x == 9
    )
  )
)


attr(dados$n_nove_orig, "label") <-
  "Número de respostas 9 originais nos 27 itens"


# =============================================================================
# INDICADOR DE IMPUTAÇÃO
# =============================================================================

dados$houve_imputacao <- ifelse(
  dados$n_nove_orig > 0 &
    dados$pcat_calculavel == 1,
  1,
  0
)


dados$houve_imputacao <- labelled(
  dados$houve_imputacao,
  labels = c(
    "Não" = 0,
    "Sim" = 1
  ),
  label = "Houve imputação de 9 para 2"
)


print(table(dados$houve_imputacao, useNA = "ifany"))


# =============================================================================
# PARTE XIV — VERIFICAÇÃO DE VALORES INVÁLIDOS
# =============================================================================

# Valores esperados nos 27 itens:
#
#   1
#   2
#   3
#   4
#   9
#   missing


cat("\n==============================================================\n")
cat("VERIFICAÇÃO DE VALORES INVÁLIDOS\n")
cat("==============================================================\n")


for (v in vars_itens) {
  
  valores_invalidos <- dados[[v]][
    !is.na(dados[[v]]) &
      !dados[[v]] %in% c(1, 2, 3, 4, 9)
  ]
  
  if (length(valores_invalidos) > 0) {
    
    cat(
      "ATENÇÃO:",
      v,
      "possui",
      length(valores_invalidos),
      "valor(es) inválido(s).\n"
    )
    
    print(unique(valores_invalidos))
  }
}


# =============================================================================
# VALORES VÁLIDOS EM A1
# =============================================================================

invalidos_a1 <- dados$a1[
  !is.na(dados$a1) &
    !dados$a1 %in% c(0, 1)
]

if (length(invalidos_a1) > 0) {
  
  cat("\nATENÇÃO: A1 possui valores inválidos:\n")
  print(unique(invalidos_a1))
}


# =============================================================================
#  VALORES VÁLIDOS EM A2
# =============================================================================

invalidos_a2 <- dados$a2[
  !is.na(dados$a2) &
    !dados$a2 %in% c(0, 1, 2)
]

if (length(invalidos_a2) > 0) {
  
  cat("\nATENÇÃO: A2 possui valores inválidos:\n")
  print(unique(invalidos_a2))
}


# =============================================================================
#  VALORES VÁLIDOS EM A3
# =============================================================================

invalidos_a3 <- dados$a3[
  !is.na(dados$a3) &
    !dados$a3 %in% c(0, 1, 2, 3, 4)
]

if (length(invalidos_a3) > 0) {
  
  cat("\nATENÇÃO: A3 possui valores inválidos:\n")
  print(unique(invalidos_a3))
}


# =============================================================================
#  PARTE XV — CONSISTÊNCIA LÓGICA DA AFILIAÇÃO
# =============================================================================

dados$inconsistencia_afil <- 0


# A2 = 1:
# "Mesmo serviço referido em A1"
# Portanto A1 deveria ser SIM.

dados$inconsistencia_afil[
  !is.na(dados$a2) &
    dados$a2 == 1 &
    dados$a1 != 1
] <- 1


# A3 = 1:
# "Mesmo serviço de A1 e A2"
# Portanto A1 e A2 deveriam ser SIM.

dados$inconsistencia_afil[
  !is.na(dados$a3) &
    dados$a3 == 1 &
    !(dados$a1 == 1 & dados$a2 == 1)
] <- 1


# A3 = 2:
# "Mesmo serviço de A1"
# Portanto A1 deveria ser SIM.

dados$inconsistencia_afil[
  !is.na(dados$a3) &
    dados$a3 == 2 &
    dados$a1 != 1
] <- 1


# A3 = 3:
# "Mesmo serviço de A2"
# Portanto A2 deveria ser SIM.

dados$inconsistencia_afil[
  !is.na(dados$a3) &
    dados$a3 == 3 &
    dados$a2 == 0
] <- 1


dados$inconsistencia_afil <- labelled(
  dados$inconsistencia_afil,
  labels = c(
    "Sem inconsistência identificada" = 0,
    "Possível inconsistência" = 1
  ),
  label = "Possível inconsistência lógica A1-A3"
)


print(
  table(
    dados$inconsistencia_afil,
    useNA = "ifany"
  )
)


# =============================================================================
#  PARTE XVI — RELATÓRIO FINAL
# =============================================================================

N_total <- nrow(dados)

N_calculavel <- sum(
  dados$pcat_calculavel == 1,
  na.rm = TRUE
)

N_nao_calculavel <- sum(
  dados$pcat_calculavel == 0,
  na.rm = TRUE
)


cat("\n\n")
cat("==============================================================\n")
cat("       PCATOOL-BRASIL – CRIANÇAS – VERSÃO REDUZIDA\n")
cat("                    RELATÓRIO FINAL\n")
cat("==============================================================\n\n")

cat(
  "Total de entrevistados:       ",
  N_total,
  "\n"
)

cat(
  "Escore calculável:            ",
  N_calculavel,
  "\n"
)

cat(
  "Escore não calculável:        ",
  N_nao_calculavel,
  "\n"
)


if (N_total > 0) {
  
  cat(
    "Percentual calculável:        ",
    sprintf(
      "%.2f",
      100 * N_calculavel / N_total
    ),
    "%\n"
  )
}


cat("\n--------------------------------------------------------------\n")
cat("Distribuição do número de componentes ausentes\n")
cat("--------------------------------------------------------------\n")

print(
  table(
    dados$n_missing_pcat,
    useNA = "ifany"
  )
)


cat("\n--------------------------------------------------------------\n")
cat("Distribuição da afiliação\n")
cat("--------------------------------------------------------------\n")

print(
  table(
    dados$afiliacao,
    useNA = "ifany"
  )
)


cat("\n--------------------------------------------------------------\n")
cat("Escore Geral – escala 1 a 4\n")
cat("--------------------------------------------------------------\n")

print(
  summary(
    dados$escore_geral_1a4
  )
)


cat("\n--------------------------------------------------------------\n")
cat("Escore Geral – escala 0 a 10\n")
cat("--------------------------------------------------------------\n")

print(
  summary(
    dados$escore_geral_0a10
  )
)


cat("\n==============================================================\n")


# =============================================================================
#  PARTE XVII — SALVAR BANCO FINAL
# =============================================================================

# O banco original NÃO é sobrescrito.
#
# Será criado:
#
#   pcatool_criancas_resultado.dta


#arquivo_saida <- "pcatool_criancas_resultado.dta"

#write_dta(
#  dados,
#  arquivo_saida
#)


#cat("\n")
#cat("==============================================================\n")
#cat("BANCO FINAL SALVO COM SUCESSO:\n")
#cat(arquivo_saida, "\n")
#cat("==============================================================\n")


# =============================================================================
# PARTE XVIII — EXPORTAÇÃO OPCIONAL PARA EXCEL
# =============================================================================

# Para exportar uma planilha Excel com as principais variáveis,
# retire o comentário (#) das linhas abaixo.
#
# IMPORTANTE:
# "record_id" somente deve ser incluído se existir no seu banco.


# variaveis_excel <- c(
#   "record_id",
#   "a1", "a2", "a3",
#   "afiliacao",
#   "afiliacao_0a10",
#   "n_missing_pcat",
#   "perc_missing_pcat",
#   "pcat_calculavel",
#   "escore_geral_1a4",
#   "escore_geral_0a10"
# )
#
# variaveis_excel <- intersect(
#   variaveis_excel,
#   names(dados)
# )
#
# write_xlsx(
#   dados[, variaveis_excel],
#   "pcatool_criancas_resultados.xlsx"
# )


###############################################################################
###############################################################################
#                           FIM DO SCRIPT
###############################################################################
###############################################################################
