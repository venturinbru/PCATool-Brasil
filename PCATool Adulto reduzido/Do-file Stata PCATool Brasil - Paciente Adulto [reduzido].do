/****************************************************************************************
* PCATOOL-BRASIL – ADULTOS – VERSÃO REDUZIDA
*
* Instrumento REDCap:
* form_pcatooladultoreduzido
*
* Variáveis:
* Afiliação: a1 a2 a3
* Itens do escore:
* b2 c4 c11 d1 d6 d9 d14 e2 e6 e7 e9 f3
* g9 g17 g20
* h1 h5 h7 h11
* i1 i3
* j4
*
* Regras:
* - C11 e D14 são invertidos
* - Código 9 = Não sabe/Não lembro
* - Missing também é ausência
* - Se ausência >= 50% dos 23 itens -> escore missing
* - Se ausência < 50% -> códigos 9 são imputados para 2
* - Escore Geral = soma dos 23 itens / 23
* - Transformação 0–10 = (escore - 1) / 3 * 10
* - Alto: escore 0–10 >= 6,6
* - Baixo: escore 0–10 < 6,6
****************************************************************************************/


/****************************************************************************************
* PREPARAÇÃO
****************************************************************************************/

**clear all
**set more off

* --------------------------------------------------------------------
* Se o banco ainda não estiver carregado, carregue-o aqui.
* Exemplo:
*
* use "C:\caminho\seu_banco.dta", clear
*
* Caso o banco já esteja aberto no Stata, mantenha esta parte comentada.
* --------------------------------------------------------------------


/****************************************************************************************
* CONFERÊNCIA DAS VARIÁVEIS
****************************************************************************************/

describe record_id a1 a2 a3 ///
    b2 c4 c11 d1 d6 d9 d14 ///
    e1 e2 e6 e7 e9 f3 ///
    g9 g17 g20 ///
    h1 h5 h7 h11 ///
    i1 i3 j4


/****************************************************************************************
* DEFINIR OS 23 ITENS DO ESCORE
****************************************************************************************/

local itens b2 c4 c11 d1 d6 d9 d14 e2 e6 e7 e9 f3 ///
            g9 g17 g20 h1 h5 h7 h11 i1 i3 j4

display "Itens utilizados no Escore Geral da APS:"
display "`itens'"

local n_itens : word count `itens'
display "Número de itens do instrumento = `n_itens'"

assert `n_itens' == 22


/****************************************************************************************
* IMPORTANTE SOBRE O NÚMERO DE ITENS
*
* A fórmula apresentada no manual é:
*
* Afiliação + B2 + C4 + C11 + D1 + D6 + D9 + D14 + E2 + E6
* + E7 + E9 + F3 + G9 + G17 + G20 + H1 + H5 + H7 + H11
* + I1 + I3 + J4
*
* Isso corresponde a:
*
* 1 item de Afiliação
* +
* 22 itens do questionário
* =
* 23 componentes.
*
* Portanto, a lista acima contém 22 itens além da Afiliação.
****************************************************************************************/


/****************************************************************************************
* VERIFICAR CODIFICAÇÃO DOS ITENS
****************************************************************************************/

foreach var of local itens {
    
    display "--------------------------------------------------"
    display "Variável: `var'"
    tab `var', missing
}


/****************************************************************************************
* VERIFICAÇÃO DA AFILIAÇÃO
****************************************************************************************/

display "=================================================="
display "DISTRIBUIÇÃO DE A1"
display "=================================================="
tab a1, missing

display "=================================================="
display "DISTRIBUIÇÃO DE A2"
display "=================================================="
tab a2, missing

display "=================================================="
display "DISTRIBUIÇÃO DE A3"
display "=================================================="
tab a3, missing


/****************************************************************************************
* CÁLCULO DO GRAU DE AFILIAÇÃO
*
* A1:
*   0 = Não
*   1 = Sim
*
* A2:
*   0 = Não
*   1 = Sim, mesmo serviço de A1
*   2 = Sim, serviço diferente de A1
*
* A3:
*   0 = Não
*   1 = Sim, mesmo serviço de A1 e A2
*   2 = Sim, somente o mesmo serviço de A1
*   3 = Sim, somente o mesmo serviço de A2
*   4 = Sim, serviço diferente de A1 e A2
*
* Resultado:
*   1 = todas as respostas NÃO
*   2 = serviços diferentes / apenas um SIM
*   3 = dois SIM referentes ao mesmo serviço
*   4 = três SIM referentes ao mesmo serviço
****************************************************************************************/

capture drop afiliacao

gen byte afiliacao = .

label variable afiliacao ///
    "Grau de Afiliação ao serviço de referência"


/****************************************************************************************
* GRAU 1
*
* A1 = NÃO
* A2 = NÃO
* A3 = NÃO
****************************************************************************************/

replace afiliacao = 1 if ///
    a1 == 0 & a2 == 0 & a3 == 0


/****************************************************************************************
* GRAU 4
*
* Os três indicam o mesmo serviço:
*
* A1 = SIM
* A2 = SIM, mesmo serviço de A1
* A3 = SIM, mesmo serviço de A1 e A2
****************************************************************************************/

replace afiliacao = 4 if ///
    a1 == 1 & a2 == 1 & a3 == 1


/****************************************************************************************
* GRAU 3 – DUAS RESPOSTAS SIM PARA O MESMO SERVIÇO
****************************************************************************************/

* A1 e A2 são o mesmo serviço; A3 = NÃO
replace afiliacao = 3 if ///
    a1 == 1 & a2 == 1 & a3 == 0

* A1 e A3 são o mesmo serviço; A2 = NÃO
replace afiliacao = 3 if ///
    a1 == 1 & a2 == 0 & a3 == 2

* A2 e A3 são o mesmo serviço; A1 = NÃO
replace afiliacao = 3 if ///
    a1 == 0 & a2 == 1 & a3 == 3


/****************************************************************************************
* GRAU 2 – APENAS UM SERVIÇO É INDICADO
****************************************************************************************/

* Somente A1 = SIM
replace afiliacao = 2 if ///
    a1 == 1 & a2 == 0 & a3 == 0

* Somente A2 = SIM
replace afiliacao = 2 if ///
    a1 == 0 & a2 == 1 & a3 == 0

* Somente A3 = SIM
*
* Dependendo da resposta de A3, o serviço pode estar relacionado
* a A1 ou A2. Neste cenário A1 e A2 são NÃO, portanto o A3
* representa o único serviço indicado.
replace afiliacao = 2 if ///
    a1 == 0 & a2 == 0 & a3 != 0 & !missing(a3)


/****************************************************************************************
* GRAU 2 – SERVIÇOS DIFERENTES
****************************************************************************************/

* A1 = SIM
* A2 = SIM, diferente de A1
* A3 = SIM, diferente dos anteriores
replace afiliacao = 2 if ///
    a1 == 1 & a2 == 2 & a3 == 4


/****************************************************************************************
* VERIFICAR SE EXISTEM COMBINAÇÕES NÃO CLASSIFICADAS
****************************************************************************************/

display "============================================================"
display "COMBINAÇÕES A1/A2/A3 NÃO CLASSIFICADAS"
display "============================================================"

tabulate a1 a2, missing

tabulate a3, missing

list record_id a1 a2 a3 afiliacao ///
    if missing(afiliacao), noobs


/****************************************************************************************
* DISTRIBUIÇÃO FINAL DA AFILIAÇÃO
****************************************************************************************/

display "============================================================"
display "DISTRIBUIÇÃO DO GRAU DE AFILIAÇÃO"
display "============================================================"

tabulate afiliacao, missing



/****************************************************************************************
* CRIAR CÓPIA DOS 22 ITENS DO ESCORE
*
* Não modificaremos os dados originais.
****************************************************************************************/

foreach var of local itens {
    capture drop pc_`var'
    clonevar pc_`var' = `var'
}


/****************************************************************************************
* CONTAGEM DE VALORES AUSENTES
*
* Ausentes = código 9 + missing.
****************************************************************************************/

capture drop n_missing_pcatool

gen int n_missing_pcatool = 0

foreach var of local itens {
    
    replace n_missing_pcatool = ///
        n_missing_pcatool + 1 ///
        if missing(pc_`var') | pc_`var' == 9
}

label variable n_missing_pcatool ///
    "Número de itens ausentes no PCATool antes da imputação"


/****************************************************************************************
* PERCENTUAL DE ITENS AUSENTES
****************************************************************************************/

capture drop pct_missing_pcatool

gen double pct_missing_pcatool = ///
    (n_missing_pcatool / 23) * 100

label variable pct_missing_pcatool ///
    "Percentual de ausência considerando 23 componentes"


/****************************************************************************************
* INDICADOR DE ELEGIBILIDADE PARA CÁLCULO
*
* O escore possui 23 componentes:
*
* 1 = Afiliação
* 22 = itens do instrumento
*
* Portanto:
*
* ausência >= 50% de 23 = não calcular.
*
* 50% de 23 = 11,5
*
* Como o número de ausentes é inteiro:
* 12 ou mais ausências -> não calcula.
* 11 ou menos -> calcula.
****************************************************************************************/

capture drop pcatool_elegivel

gen byte pcatool_elegivel = .

replace pcatool_elegivel = 0 if ///
    n_missing_pcatool >= 12

replace pcatool_elegivel = 1 if ///
    n_missing_pcatool < 12

label define elig ///
    0 "Não elegível - >=50% ausentes" ///
    1 "Elegível - <50% ausentes"

label values pcatool_elegivel elig

label variable pcatool_elegivel ///
    "Elegibilidade para cálculo do Escore Geral da APS"

tab pcatool_elegivel, missing


/****************************************************************************************
* IMPUTAÇÃO DOS VALORES 9
*
* Para participantes elegíveis:
*
* 9 -> 2
*
* Missing verdadeiro NÃO é imputado.
*
* IMPORTANTE:
* O manual diz que valores 9 devem ser transformados em 2.
* Os itens missing permanecem missing.
****************************************************************************************/

foreach var of local itens {
    
    replace pc_`var' = 2 ///
        if pcatool_elegivel == 1 & pc_`var' == 9
}


/****************************************************************************************
* INVERSÃO DE C11
*
* Escala original:
*
* 4 -> 1
* 3 -> 2
* 2 -> 3
* 1 -> 4
*
* Somente valores válidos 1–4.
****************************************************************************************/

capture drop pc_c11_inv

gen byte pc_c11_inv = .

replace pc_c11_inv = 5 - pc_c11 ///
    if inrange(pc_c11, 1, 4)

label variable pc_c11_inv ///
    "C11 invertido"


/****************************************************************************************
* INVERSÃO DE D14
****************************************************************************************/

capture drop pc_d14_inv

gen byte pc_d14_inv = .

replace pc_d14_inv = 5 - pc_d14 ///
    if inrange(pc_d14, 1, 4)

label variable pc_d14_inv ///
    "D14 invertido"


/****************************************************************************************
* VERIFICAR A INVERSÃO
****************************************************************************************/

display "=================================================="
display "C11 ORIGINAL x C11 INVERTIDO"
display "=================================================="

tab pc_c11 pc_c11_inv, missing

display "=================================================="
display "D14 ORIGINAL x D14 INVERTIDO"
display "=================================================="

tab pc_d14 pc_d14_inv, missing


/****************************************************************************************
* VERIFICAR VALORES INVÁLIDOS
*
* Depois da preparação, os itens devem conter somente:
* 1, 2, 3, 4 ou missing.
****************************************************************************************/

foreach var of local itens {
    
    quietly count if !missing(pc_`var') & ///
        !inrange(pc_`var',1,4)
    
    if r(N) > 0 {
        display as error ///
            "ATENÇÃO: `var' possui " r(N) " valor(es) fora da escala 1-4."
        tab pc_`var' if !missing(pc_`var') & ///
            !inrange(pc_`var',1,4), missing
    }
}


/****************************************************************************************
* CRIAR OS 23 COMPONENTES DO ESCORE
*
* Primeiro componente = Afiliação
* Demais 22 = itens do instrumento
****************************************************************************************/

capture drop pcatool_sum
capture drop pcatool_n_valid

gen double pcatool_sum = .

gen int pcatool_n_valid = 0


/****************************************************************************************
* SOMA DOS 22 ITENS
****************************************************************************************/

egen double soma_22_itens = rowtotal( ///
    pc_b2 ///
    pc_c4 ///
    pc_c11_inv ///
    pc_d1 ///
    pc_d6 ///
    pc_d9 ///
    pc_d14_inv ///
    pc_e2 ///
    pc_e6 ///
    pc_e7 ///
    pc_e9 ///
    pc_f3 ///
    pc_g9 ///
    pc_g17 ///
    pc_g20 ///
    pc_h1 ///
    pc_h5 ///
    pc_h7 ///
    pc_h11 ///
    pc_i1 ///
    pc_i3 ///
    pc_j4 ///
)


/****************************************************************************************
* CONTAR QUANTOS DOS 22 ITENS POSSUEM VALORES
****************************************************************************************/

egen int n_valid_22 = rownonmiss( ///
    pc_b2 ///
    pc_c4 ///
    pc_c11_inv ///
    pc_d1 ///
    pc_d6 ///
    pc_d9 ///
    pc_d14_inv ///
    pc_e2 ///
    pc_e6 ///
    pc_e7 ///
    pc_e9 ///
    pc_f3 ///
    pc_g9 ///
    pc_g17 ///
    pc_g20 ///
    pc_h1 ///
    pc_h5 ///
    pc_h7 ///
    pc_h11 ///
    pc_i1 ///
    pc_i3 ///
    pc_j4 ///
)


/****************************************************************************************
* CÁLCULO DO ESCORE GERAL
*
* Regra:
*
* Escore = (Afiliação + 22 itens) / 23
*
* Se algum dos 22 itens estiver missing após a preparação,
* o escore será missing.
****************************************************************************************/

replace pcatool_sum = ///
    afiliacao + soma_22_itens ///
    if pcatool_elegivel == 1 & ///
       !missing(afiliacao) & ///
       n_valid_22 == 22

capture drop pcatool_geral

gen double pcatool_geral = .

replace pcatool_geral = ///
    pcatool_sum / 23 ///
    if pcatool_elegivel == 1 & ///
       n_valid_22 == 22 & ///
       !missing(afiliacao)

label variable pcatool_geral ///
    "Escore Geral da APS - PCATool Adulto Reduzido (1-4)"


/****************************************************************************************
* TRANSFORMAÇÃO DO ESCORE GERAL PARA 0–10
*
* Fórmula:
*
* (Escore obtido - 1) / (4 - 1) * 10
*
* = (Escore - 1) / 3 * 10
****************************************************************************************/

capture drop pcatool_0_10

gen double pcatool_0_10 = .

replace pcatool_0_10 = ///
    ((pcatool_geral - 1) / 3) * 10 ///
    if !missing(pcatool_geral)

label variable pcatool_0_10 ///
    "Escore Geral da APS - PCATool transformado (0-10)"


/****************************************************************************************
* CLASSIFICAÇÃO: BAIXO x ALTO
*
* Alto: >= 6,6
* Baixo: < 6,6
****************************************************************************************/

capture drop pcatool_class

gen byte pcatool_class = .

replace pcatool_class = 0 ///
    if !missing(pcatool_0_10) & ///
       pcatool_0_10 < 6.6

replace pcatool_class = 1 ///
    if !missing(pcatool_0_10) & ///
       pcatool_0_10 >= 6.6

label define pcatool_class_lbl ///
    0 "Baixo (<6,6)" ///
    1 "Alto (>=6,6)"

label values pcatool_class pcatool_class_lbl

label variable pcatool_class ///
    "Classificação do Escore Geral da APS"


/****************************************************************************************
* RESUMO DOS RESULTADOS
****************************************************************************************/

display "============================================================"
display "RESUMO – PCATOOL BRASIL ADULTO REDUZIDO"
display "============================================================"

summarize afiliacao ///
    n_missing_pcatool ///
    pct_missing_pcatool ///
    pcatool_geral ///
    pcatool_0_10

tab pcatool_elegivel, missing
tab afiliacao, missing
tab pcatool_class, missing


/****************************************************************************************
* DISTRIBUIÇÃO DOS ESCORES
****************************************************************************************/

display "============================================================"
display "DISTRIBUIÇÃO DO ESCORE GERAL – 1 A 4"
display "============================================================"

summarize pcatool_geral, detail

display "============================================================"
display "DISTRIBUIÇÃO DO ESCORE GERAL – 0 A 10"
display "============================================================"

summarize pcatool_0_10, detail


/****************************************************************************************
* FREQUÊNCIA DOS ITENS UTILIZADOS NO ESCORE
****************************************************************************************/

foreach var of local itens {
    
    display "=================================================="
    display "`var'"
    tab pc_`var', missing
}


/****************************************************************************************
* VERIFICAÇÃO DA QUANTIDADE DE MISSING APÓS IMPUTAÇÃO
****************************************************************************************/

display "============================================================"
display "MISSING NOS ITENS APÓS IMPUTAÇÃO"
display "============================================================"

foreach var of local itens {
    quietly count if missing(pc_`var')
    display "`var' : " r(N) " missing"
}


/****************************************************************************************
* VERIFICAR CASOS COM ESCORE MISSING
****************************************************************************************/

display "============================================================"
display "CASOS SEM ESCORE"
display "============================================================"

count if missing(pcatool_geral)

display "Número de casos sem Escore Geral = " r(N)

list record_id ///
    a1 a2 a3 ///
    afiliacao ///
    n_missing_pcatool ///
    pct_missing_pcatool ///
    pcatool_elegivel ///
    if missing(pcatool_geral), noobs


/****************************************************************************************
* ESCORES INDIVIDUAIS DOS ITENS – ESCALA 0 A 10
*
* Fórmula:
*
*     (valor - 1) / 3 * 10
*
* Os itens C11 e D14 já estão invertidos nas variáveis:
*     pc_c11_inv
*     pc_d14_inv
****************************************************************************************/

capture drop score_b2
gen double score_b2 = ((pc_b2 - 1) / 3) * 10 if inrange(pc_b2,1,4)
label variable score_b2 "B2 - Escore 0 a 10"

capture drop score_c4
gen double score_c4 = ((pc_c4 - 1) / 3) * 10 if inrange(pc_c4,1,4)
label variable score_c4 "C4 - Escore 0 a 10"

capture drop score_c11
gen double score_c11 = ((pc_c11_inv - 1) / 3) * 10 if inrange(pc_c11_inv,1,4)
label variable score_c11 "C11 invertido - Escore 0 a 10"

capture drop score_d1
gen double score_d1 = ((pc_d1 - 1) / 3) * 10 if inrange(pc_d1,1,4)
label variable score_d1 "D1 - Escore 0 a 10"

capture drop score_d6
gen double score_d6 = ((pc_d6 - 1) / 3) * 10 if inrange(pc_d6,1,4)
label variable score_d6 "D6 - Escore 0 a 10"

capture drop score_d9
gen double score_d9 = ((pc_d9 - 1) / 3) * 10 if inrange(pc_d9,1,4)
label variable score_d9 "D9 - Escore 0 a 10"

capture drop score_d14
gen double score_d14 = ((pc_d14_inv - 1) / 3) * 10 if inrange(pc_d14_inv,1,4)
label variable score_d14 "D14 invertido - Escore 0 a 10"

capture drop score_e2
gen double score_e2 = ((pc_e2 - 1) / 3) * 10 if inrange(pc_e2,1,4)
label variable score_e2 "E2 - Escore 0 a 10"

capture drop score_e6
gen double score_e6 = ((pc_e6 - 1) / 3) * 10 if inrange(pc_e6,1,4)
label variable score_e6 "E6 - Escore 0 a 10"

capture drop score_e7
gen double score_e7 = ((pc_e7 - 1) / 3) * 10 if inrange(pc_e7,1,4)
label variable score_e7 "E7 - Escore 0 a 10"

capture drop score_e9
gen double score_e9 = ((pc_e9 - 1) / 3) * 10 if inrange(pc_e9,1,4)
label variable score_e9 "E9 - Escore 0 a 10"

capture drop score_f3
gen double score_f3 = ((pc_f3 - 1) / 3) * 10 if inrange(pc_f3,1,4)
label variable score_f3 "F3 - Escore 0 a 10"

capture drop score_g9
gen double score_g9 = ((pc_g9 - 1) / 3) * 10 if inrange(pc_g9,1,4)
label variable score_g9 "G9 - Escore 0 a 10"

capture drop score_g17
gen double score_g17 = ((pc_g17 - 1) / 3) * 10 if inrange(pc_g17,1,4)
label variable score_g17 "G17 - Escore 0 a 10"

capture drop score_g20
gen double score_g20 = ((pc_g20 - 1) / 3) * 10 if inrange(pc_g20,1,4)
label variable score_g20 "G20 - Escore 0 a 10"

capture drop score_h1
gen double score_h1 = ((pc_h1 - 1) / 3) * 10 if inrange(pc_h1,1,4)
label variable score_h1 "H1 - Escore 0 a 10"

capture drop score_h5
gen double score_h5 = ((pc_h5 - 1) / 3) * 10 if inrange(pc_h5,1,4)
label variable score_h5 "H5 - Escore 0 a 10"

capture drop score_h7
gen double score_h7 = ((pc_h7 - 1) / 3) * 10 if inrange(pc_h7,1,4)
label variable score_h7 "H7 - Escore 0 a 10"

capture drop score_h11
gen double score_h11 = ((pc_h11 - 1) / 3) * 10 if inrange(pc_h11,1,4)
label variable score_h11 "H11 - Escore 0 a 10"

capture drop score_i1
gen double score_i1 = ((pc_i1 - 1) / 3) * 10 if inrange(pc_i1,1,4)
label variable score_i1 "I1 - Escore 0 a 10"

capture drop score_i3
gen double score_i3 = ((pc_i3 - 1) / 3) * 10 if inrange(pc_i3,1,4)
label variable score_i3 "I3 - Escore 0 a 10"

capture drop score_j4
gen double score_j4 = ((pc_j4 - 1) / 3) * 10 if inrange(pc_j4,1,4)
label variable score_j4 "J4 - Escore 0 a 10"


/****************************************************************************************
* ESCORE DE AFILIAÇÃO – ESCALA 0 A 10
****************************************************************************************/

capture drop afiliacao_0_10

gen double afiliacao_0_10 = ///
    ((afiliacao - 1) / 3) * 10 ///
    if inrange(afiliacao,1,4)

label variable afiliacao_0_10 ///
    "Afiliação - Escore 0 a 10"


/****************************************************************************************
* CONFERIR OS ESCORES INDIVIDUAIS
****************************************************************************************/

summarize ///
    score_b2 ///
    score_c4 ///
    score_c11 ///
    score_d1 ///
    score_d6 ///
    score_d9 ///
    score_d14 ///
    score_e2 ///
    score_e6 ///
    score_e7 ///
    score_e9 ///
    score_f3 ///
    score_g9 ///
    score_g17 ///
    score_g20 ///
    score_h1 ///
    score_h5 ///
    score_h7 ///
    score_h11 ///
    score_i1 ///
    score_i3 ///
    score_j4 ///
    afiliacao_0_10


/****************************************************************************************
* MÉDIA E DESVIO-PADRÃO DOS ITENS – ESCALA 0 A 10
****************************************************************************************/

display "============================================================"
display "MÉDIAS DOS ITENS – ESCALA 0 A 10"
display "============================================================"

foreach var in ///
    score_b2 ///
    score_c4 ///
    score_c11 ///
    score_d1 ///
    score_d6 ///
    score_d9 ///
    score_d14 ///
    score_e2 ///
    score_e6 ///
    score_e7 ///
    score_e9 ///
    score_f3 ///
    score_g9 ///
    score_g17 ///
    score_g20 ///
    score_h1 ///
    score_h5 ///
    score_h7 ///
    score_h11 ///
    score_i1 ///
    score_i3 ///
    score_j4 {

    quietly summarize `var'

    display "`var' | N = " r(N) ///
        " | Média = " %6.2f r(mean) ///
        " | DP = " %6.2f r(sd)
}


/****************************************************************************************
* RESUMO DO ESCORE GERAL
****************************************************************************************/

display "============================================================"
display "ESCORE GERAL DA APS"
display "============================================================"

summarize pcatool_geral, detail

display "============================================================"
display "ESCORE GERAL DA APS – 0 A 10"
display "============================================================"

summarize pcatool_0_10, detail


/****************************************************************************************
* CLASSIFICAÇÃO BAIXO / ALTO
****************************************************************************************/

display "============================================================"
display "CLASSIFICAÇÃO DO ESCORE"
display "============================================================"

tabulate pcatool_class, missing

/****************************************************************************************
* SALVAR BANCO FINAL
* tire o ** para salvar
****************************************************************************************/

** save "pcatool_adulto_reduzido_analisado.dta", replace
