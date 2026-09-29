/********************************************************************
*********************************************************************
                    PCATOOL - BRASIL
					MÉDICOS E ENFERMEIROS
					VERSAO EXTENSA
*********************************************************************

ESTRUTURA DO INSTRUMENTO

A = A1-A9
B = B1-B13
C = C1-C6
D = D1-D8
E = E1-E22
F = depende do tipo de atendimento
G = G1-G14
H = H1-H21

REGRA DE MISSING

Código 9 = missing.

Para cada componente:
- se houver menos de 50% de itens missing:
      os missing são imputados para 2
- se houver 50% ou mais de itens missing:
      o escore permanece missing

ESCALA ORIGINAL:
1 a 4

ESCALA TRANSFORMADA:
0 a 10

*********************************************************************
*********************************************************************/


/********************************************************************
LISTA DE TODOS OS ITENS
********************************************************************/

local itens ///
    a1 a2 a3 a4 a5 a6 a7 a8 a9 ///
    b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 ///
    c1 c2 c3 c4 c5 c6 ///
    d1 d2 d3 d4 d5 d6 d7 d8 ///
    e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13 e14 e15 e16 e17 e18 e19 e20 e21 e22 ///
    f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 ///
    g1 g2 g3 g4 g5 g6 g7 g8 g9 g10 g11 g12 g13 g14 ///
    h1 h2 h3 h4 h5 h6 h7 h8 h9 h10 h11 h12 h13 h14 h15 h16 h17 h18 h19 h20 h21


/********************************************************************
CRIAR CÓPIAS DOS ITENS

As variáveis originais não serão modificadas.

p_a1, p_a2 etc. serão utilizadas nos cálculos.
********************************************************************/

foreach v of local itens {

    capture drop p_`v'

    clonevar p_`v' = `v'

}


/********************************************************************
TRANSFORMAR CÓDIGO 9 EM MISSING

9 = Não sei / Não lembro

Não fazemos imputação neste momento.
********************************************************************/

foreach v of local itens {

    replace p_`v' = . if p_`v' == 9

}


/********************************************************************
INVERSÃO DO ITEM A9

4 -> 1
3 -> 2
2 -> 3
1 -> 4

Fórmula:
novo valor = 5 - valor
********************************************************************/

replace p_a9 = 5 - p_a9 if inrange(p_a9,1,4)

label variable p_a9 ///
    "A9 - invertido para cálculo do PCATool"
tab p_a9

/********************************************************************
PROGRAMA PARA CÁLCULO DOS COMPONENTES

IMPORTANTE: O programa NÃO altera os itens originais. A imputação é feita em uma variável temporária.

Regra:
missing < 50% -> missing recebe 2
missing >= 50% -> escore permanece missing
********************************************************************/

capture program drop pcat_score

program define pcat_score

    syntax varlist(min=1 numeric) [if], SCORE(name) MISS(name)

    marksample touse

    local n : word count `varlist'

    /**************************************************************
    Número de missing ANTES da imputação
    **************************************************************/

    capture drop `miss'

    egen byte `miss' = rowmiss(`varlist') if `touse'


    /**************************************************************
    Cria escore
    **************************************************************/

    capture drop `score'

    gen double `score' = .


    /**************************************************************
    Cria cópias temporárias dos itens

    Não modifica p_a1, p_b1, p_f1 etc.
    **************************************************************/

    local tempvars

    foreach v of local varlist {

        tempvar x_`v'

        gen double `x_`v'' = `v' if `touse'

        replace `x_`v'' = 2 ///
            if `touse' ///
            & missing(`x_`v'') ///
            & `miss' < (`n'/2)

        local tempvars `tempvars' `x_`v''

    }


    /**************************************************************
    Calcula média
    **************************************************************/

    tempvar temp_score

    egen double `temp_score' = rowmean(`tempvars') if `touse'


    /**************************************************************
    Se >= 50% missing, mantém missing
    **************************************************************/

    replace `score' = `temp_score' ///
        if `touse' ///
        & `miss' < (`n'/2)

end


/********************************************************************
COMPONENTE A

A1-A9
********************************************************************/

pcat_score ///
    p_a1 p_a2 p_a3 p_a4 p_a5 p_a6 p_a7 p_a8 p_a9, ///
    score(escore_A) ///
    miss(nmiss_A)

label variable escore_A ///
    "PCATool A - Acesso de Primeiro Contato - Acessibilidade"

label variable nmiss_A ///
    "Número de itens ausentes - Componente A"


/********************************************************************
COMPONENTE B

B1-B13
********************************************************************/

pcat_score ///
    p_b1 p_b2 p_b3 p_b4 p_b5 p_b6 p_b7 ///
    p_b8 p_b9 p_b10 p_b11 p_b12 p_b13, ///
    score(escore_B) ///
    miss(nmiss_B)

label variable escore_B ///
    "PCATool B - Longitudinalidade"

label variable nmiss_B ///
    "Número de itens ausentes - Componente B"


/********************************************************************
COMPONENTE C

C1-C6
********************************************************************/

pcat_score ///
    p_c1 p_c2 p_c3 p_c4 p_c5 p_c6, ///
    score(escore_C) ///
    miss(nmiss_C)

label variable escore_C ///
    "PCATool C - Coordenação - Integração de Cuidados"

label variable nmiss_C ///
    "Número de itens ausentes - Componente C"


/********************************************************************
COMPONENTE D

D1-D8
********************************************************************/

pcat_score ///
    p_d1 p_d2 p_d3 p_d4 p_d5 p_d6 p_d7 p_d8, ///
    score(escore_D) ///
    miss(nmiss_D)

label variable escore_D ///
    "PCATool D - Coordenação - Sistemas de Informações"

label variable nmiss_D ///
    "Número de itens ausentes - Componente D"


/********************************************************************
COMPONENTE E

E1-E22
********************************************************************/

pcat_score ///
    p_e1 p_e2 p_e3 p_e4 p_e5 p_e6 p_e7 p_e8 p_e9 p_e10 p_e11 ///
    p_e12 p_e13 p_e14 p_e15 p_e16 p_e17 p_e18 p_e19 p_e20 p_e21 p_e22, ///
    score(escore_E) ///
    miss(nmiss_E)

label variable escore_E ///
    "PCATool E - Integralidade - Serviços Disponíveis"

label variable nmiss_E ///
    "Número de itens ausentes - Componente E"


/********************************************************************
*********************************************************************
BLOCO F

ESTRUTURA CONFIRMADA

F1-F3:
    todas as idades
    adultos
    crianças

F4-F13:
    todas as idades
    adultos

F14:
    todas as idades

F15-F18:
    todas as idades
    crianças


SITUAÇÕES POSSÍVEIS:

tipo_F = 1
    Todas as idades
    -> F1-F18

tipo_F = 2
    Somente adultos
    -> F1-F13

tipo_F = 3
    Somente crianças
    -> F1-F3 + F15-F18

tipo_F = 4
    Adultos + crianças
    sem "todas as idades"
    -> F1-F3

*********************************************************************
*********************************************************************/


/********************************************************************
CRIAR TIPO DE F
********************************************************************/

capture drop tipo_F

gen byte tipo_F = .


/********************************************************************
CRIAR INDICADOR DE INCONSISTÊNCIA

Fizemos uma adaptação criando 3 variáveis; atende todas as idades; atende apenas crianças; atende apenas adultos

IMPORTANTE: Adulto = 1 E criança = 1 NÃO É INCONSISTÊNCIA.
É uma situação válida: "atende adultos e crianças, mas não marcou todas as idades".
********************************************************************/

capture drop filtro_F_inconsistente

gen byte filtro_F_inconsistente = 0


/********************************************************************
VERIFICAR APENAS SITUAÇÕES REALMENTE INCONSISTENTES
********************************************************************/

/*
Se marcou "todas as idades", não deveria haver
marcação exclusiva de adulto ou criança.

Aqui assumimos que marcar todas as idades significa
que adulto/criança não devem ser simultaneamente
selecionados.
*/

replace filtro_F_inconsistente = 1 ///
    if atendepcttodasasidades == 1 ///
    & (atendeapenasadulto == 1 | atendeapenascriancas == 1)


/*
Nenhuma opção marcada também é inconsistente.
*/

replace filtro_F_inconsistente = 1 ///
    if atendepcttodasasidades == 0 ///
    & atendeapenasadulto == 0 ///
    & atendeapenascriancas == 0


/********************************************************************
CLASSIFICAÇÃO DO TIPO F
********************************************************************/

/*--------------------------------------------------------------
TIPO 1 = TODAS AS IDADES
--------------------------------------------------------------*/

replace tipo_F = 1 ///
    if atendepcttodasasidades == 1 ///
    & filtro_F_inconsistente == 0


/*--------------------------------------------------------------
TIPO 2 = SOMENTE ADULTOS
--------------------------------------------------------------*/

replace tipo_F = 2 ///
    if atendepcttodasasidades == 0 ///
    & atendeapenasadulto == 1 ///
    & atendeapenascriancas == 0 ///
    & filtro_F_inconsistente == 0


/*--------------------------------------------------------------
TIPO 3 = SOMENTE CRIANÇAS
--------------------------------------------------------------*/

replace tipo_F = 3 ///
    if atendepcttodasasidades == 0 ///
    & atendeapenascriancas == 1 ///
    & atendeapenasadulto == 0 ///
    & filtro_F_inconsistente == 0


/*--------------------------------------------------------------
TIPO 4 = ADULTOS + CRIANÇAS

Não marcou todas as idades.

Essa é uma situação VÁLIDA.

Como somente F1-F3 são comuns às duas populações,
o cálculo utiliza F1-F3.
--------------------------------------------------------------*/

replace tipo_F = 4 ///
    if atendepcttodasasidades == 0 ///
    & atendeapenasadulto == 1 ///
    & atendeapenascriancas == 1 ///
    & filtro_F_inconsistente == 0


/********************************************************************
LABEL DO TIPO F
********************************************************************/

capture label drop tipoF

label define tipoF ///
    1 "Todas as idades" ///
    2 "Somente adultos" ///
    3 "Somente crianças" ///
    4 "Adultos e crianças"

label values tipo_F tipoF

label variable tipo_F ///
    "Tipo de atendimento para cálculo do componente F"


/********************************************************************
CONFERÊNCIA DO FILTRO F
********************************************************************/

display "============================================================"
display "CONFERÊNCIA DO BLOCO F"
display "============================================================"

tab atendepcttodasasidades, missing

tab atendeapenasadulto, missing

tab atendeapenascriancas, missing

tab tipo_F, missing

tab filtro_F_inconsistente, missing


/********************************************************************
F - TODAS AS IDADES

F1-F18
********************************************************************/

pcat_score ///
    p_f1 p_f2 p_f3 p_f4 p_f5 p_f6 p_f7 p_f8 p_f9 ///
    p_f10 p_f11 p_f12 p_f13 p_f14 p_f15 p_f16 p_f17 p_f18 ///
    if tipo_F == 1, ///
    score(escore_FT) ///
    miss(nmiss_FT)

label variable escore_FT ///
    "PCATool F - Todas as idades"

label variable nmiss_FT ///
    "Número de itens ausentes - F todas as idades"


/********************************************************************
F - SOMENTE ADULTOS

F1-F13
********************************************************************/

pcat_score ///
    p_f1 p_f2 p_f3 p_f4 p_f5 p_f6 p_f7 ///
    p_f8 p_f9 p_f10 p_f11 p_f12 p_f13 ///
    if tipo_F == 2, ///
    score(escore_FA) ///
    miss(nmiss_FA)

label variable escore_FA ///
    "PCATool F - Somente adultos"

label variable nmiss_FA ///
    "Número de itens ausentes - F adultos"


/********************************************************************
F - SOMENTE CRIANÇAS

F1-F3 + F15-F18

Total = 7 itens
********************************************************************/

pcat_score ///
    p_f1 p_f2 p_f3 p_f15 p_f16 p_f17 p_f18 ///
    if tipo_F == 3, ///
    score(escore_FC) ///
    miss(nmiss_FC)

label variable escore_FC ///
    "PCATool F - Somente crianças"

label variable nmiss_FC ///
    "Número de itens ausentes - F crianças"


/********************************************************************
F - ADULTOS + CRIANÇAS

F1-F3

Esses são os únicos itens comuns às duas populações
quando não foi marcada a opção "todas as idades".
********************************************************************/

pcat_score ///
    p_f1 p_f2 p_f3 ///
    if tipo_F == 4, ///
    score(escore_FAC) ///
    miss(nmiss_FAC)

label variable escore_FAC ///
    "PCATool F - Adultos e crianças"

label variable nmiss_FAC ///
    "Número de itens ausentes - F adultos e crianças"


/********************************************************************
ESCORE F FINAL
********************************************************************/

capture drop escore_F

gen double escore_F = .


/* Todas as idades */

replace escore_F = escore_FT ///
    if tipo_F == 1


/* Somente adultos */

replace escore_F = escore_FA ///
    if tipo_F == 2


/* Somente crianças */

replace escore_F = escore_FC ///
    if tipo_F == 3


/* Adultos + crianças */

replace escore_F = escore_FAC ///
    if tipo_F == 4


label variable escore_F ///
    "PCATool F - Integralidade - Serviços Prestados"


/********************************************************************
NÚMERO DE MISSING DO COMPONENTE F
********************************************************************/

capture drop nmiss_F

gen byte nmiss_F = .


replace nmiss_F = nmiss_FT ///
    if tipo_F == 1

replace nmiss_F = nmiss_FA ///
    if tipo_F == 2

replace nmiss_F = nmiss_FC ///
    if tipo_F == 3

replace nmiss_F = nmiss_FAC ///
    if tipo_F == 4


label variable nmiss_F ///
    "Número de itens ausentes - Componente F"


/********************************************************************
*********************************************************************
COMPONENTE G

G1-G14
*********************************************************************
********************************************************************/

pcat_score ///
    p_g1 p_g2 p_g3 p_g4 p_g5 p_g6 p_g7 ///
    p_g8 p_g9 p_g10 p_g11 p_g12 p_g13 p_g14, ///
    score(escore_G) ///
    miss(nmiss_G)

label variable escore_G ///
    "PCATool G - Orientação Familiar"

label variable nmiss_G ///
    "Número de itens ausentes - Componente G"


/********************************************************************
*********************************************************************
COMPONENTE H

H1-H21
*********************************************************************
********************************************************************/

pcat_score ///
    p_h1 p_h2 p_h3 p_h4 p_h5 p_h6 p_h7 p_h8 p_h9 p_h10 ///
    p_h11 p_h12 p_h13 p_h14 p_h15 p_h16 p_h17 p_h18 ///
    p_h19 p_h20 p_h21, ///
    score(escore_H) ///
    miss(nmiss_H)

label variable escore_H ///
    "PCATool H - Orientação Comunitária"

label variable nmiss_H ///
    "Número de itens ausentes - Componente H"


/********************************************************************
*********************************************************************
ESCORE ESSENCIAL

Componentes:
A B C D E F

Se 3 ou mais forem missing:
    escore essencial = missing

Caso contrário:
    média dos componentes disponíveis
*********************************************************************
*********************************************************************/

capture drop ncomp_missing_essencial

egen byte ncomp_missing_essencial = rowmiss( ///
    escore_A ///
    escore_B ///
    escore_C ///
    escore_D ///
    escore_E ///
    escore_F ///
)

label variable ncomp_missing_essencial ///
    "Número de componentes essenciais ausentes"


capture drop escore_essencial

egen double escore_essencial = rowmean( ///
    escore_A ///
    escore_B ///
    escore_C ///
    escore_D ///
    escore_E ///
    escore_F ///
)

replace escore_essencial = . ///
    if ncomp_missing_essencial >= 3

label variable escore_essencial ///
    "PCATool - Escore Essencial da APS - escala 1 a 4"


/********************************************************************
ESCORE DERIVADO

G + H
********************************************************************/

capture drop ncomp_missing_derivado

egen byte ncomp_missing_derivado = rowmiss( ///
    escore_G ///
    escore_H ///
)

capture drop escore_derivado

egen double escore_derivado = rowmean( ///
    escore_G ///
    escore_H ///
)

replace escore_derivado = . ///
    if ncomp_missing_derivado >= 2

label variable ncomp_missing_derivado ///
    "Número de componentes derivados ausentes"

label variable escore_derivado ///
    "PCATool - Escore Derivado da APS - escala 1 a 4"


/********************************************************************
*********************************************************************
ESCORE GERAL

A B C D E F G H

Se 4 ou mais componentes forem missing:
    escore geral = missing

Caso contrário:
    média dos componentes disponíveis
*********************************************************************
*********************************************************************/

capture drop ncomp_missing_geral

egen byte ncomp_missing_geral = rowmiss( ///
    escore_A ///
    escore_B ///
    escore_C ///
    escore_D ///
    escore_E ///
    escore_F ///
    escore_G ///
    escore_H ///
)

label variable ncomp_missing_geral ///
    "Número de componentes ausentes no Escore Geral"


capture drop escore_geral

egen double escore_geral = rowmean( ///
    escore_A ///
    escore_B ///
    escore_C ///
    escore_D ///
    escore_E ///
    escore_F ///
    escore_G ///
    escore_H ///
)

replace escore_geral = . ///
    if ncomp_missing_geral >= 4

label variable escore_geral ///
    "PCATool - Escore Geral da APS - escala 1 a 4"


/********************************************************************
*********************************************************************
TRANSFORMAÇÃO DOS COMPONENTES PARA 0-10

Fórmula:

(escore - 1) / 3 * 10

1 = 0
2 = 3,33
3 = 6,67
4 = 10
*********************************************************************
*********************************************************************/

foreach x in A B C D E F G H {

    capture drop escore_`x'_10

    gen double escore_`x'_10 = ///
        (escore_`x' - 1) / 3 * 10 ///
        if !missing(escore_`x')

    label variable escore_`x'_10 ///
        "PCATool `x' - Escala 0 a 10"

}


/********************************************************************
ESCORE ESSENCIAL 0-10
********************************************************************/

capture drop escore_essencial_10

gen double escore_essencial_10 = ///
    (escore_essencial - 1) / 3 * 10 ///
    if !missing(escore_essencial)

label variable escore_essencial_10 ///
    "PCATool - Escore Essencial APS - Escala 0 a 10"


/********************************************************************
ESCORE DERIVADO 0-10
********************************************************************/

capture drop escore_derivado_10

gen double escore_derivado_10 = ///
    (escore_derivado - 1) / 3 * 10 ///
    if !missing(escore_derivado)

label variable escore_derivado_10 ///
    "PCATool - Escore Derivado APS - Escala 0 a 10"


/********************************************************************
ESCORE GERAL 0-10
********************************************************************/

capture drop escore_geral_10

gen double escore_geral_10 = ///
    (escore_geral - 1) / 3 * 10 ///
    if !missing(escore_geral)

label variable escore_geral_10 ///
    "PCATool - Escore Geral APS - Escala 0 a 10"


/********************************************************************
TRANSFORMAÇÃO DOS ITENS PARA 0-10
********************************************************************/

foreach v of local itens {

    capture drop p10_`v'

    gen double p10_`v' = ///
        (p_`v' - 1) / 3 * 10 ///
        if !missing(p_`v')

    label variable p10_`v' ///
        "PCATool `v' - Escala 0 a 10"

}


/********************************************************************
FORMATAÇÃO
********************************************************************/

format escore_A ///
       escore_B ///
       escore_C ///
       escore_D ///
       escore_E ///
       escore_F ///
       escore_G ///
       escore_H ///
       escore_essencial ///
       escore_derivado ///
       escore_geral ///
       %9.2f


format escore_A_10 ///
       escore_B_10 ///
       escore_C_10 ///
       escore_D_10 ///
       escore_E_10 ///
       escore_F_10 ///
       escore_G_10 ///
       escore_H_10 ///
       escore_essencial_10 ///
       escore_derivado_10 ///
       escore_geral_10 ///
       %9.2f


/********************************************************************
*********************************************************************
CONFERÊNCIA FINAL DOS COMPONENTES
*********************************************************************
*********************************************************************/

display "============================================================"
display "RESUMO DOS COMPONENTES - ESCALA 1 A 4"
display "============================================================"

summarize ///
    escore_A ///
    escore_B ///
    escore_C ///
    escore_D ///
    escore_E ///
    escore_F ///
    escore_G ///
    escore_H


/********************************************************************
CONFERÊNCIA DOS ESCORES PRINCIPAIS
********************************************************************/

display "============================================================"
display "ESCORES PRINCIPAIS"
display "============================================================"

summarize ///
    escore_essencial ///
    escore_geral ///
    escore_essencial_10 ///
    escore_geral_10


/********************************************************************
CONFERÊNCIA DOS MISSING POR COMPONENTE
********************************************************************/

display "============================================================"
display "MISSING POR COMPONENTE"
display "============================================================"

summarize ///
    nmiss_A ///
    nmiss_B ///
    nmiss_C ///
    nmiss_D ///
    nmiss_E ///
    nmiss_F ///
    nmiss_G ///
    nmiss_H


/********************************************************************
CONFERÊNCIA DO BLOCO F
********************************************************************/

display "============================================================"
display "CONFERÊNCIA DO BLOCO F"
display "============================================================"

tab tipo_F, missing

tab filtro_F_inconsistente, missing


/********************************************************************
QUANTIDADE DE PROFISSIONAIS POR TIPO DE F
********************************************************************/

display "============================================================"
display "TIPO DE ATENDIMENTO - BLOCO F"
display "============================================================"

count if tipo_F == 1
display "Todas as idades"

count if tipo_F == 2
display "Somente adultos"

count if tipo_F == 3
display "Somente crianças"

count if tipo_F == 4
display "Adultos e crianças"


/********************************************************************
CONFERÊNCIA DOS ESCORES F
********************************************************************/

display "============================================================"
display "ESCORES DO BLOCO F"
display "============================================================"

summarize ///
    escore_FT ///
    escore_FA ///
    escore_FC ///
    escore_FAC ///
    escore_F, detail


/********************************************************************
COMPARAÇÃO DO F POR TIPO DE ATENDIMENTO
********************************************************************/

display "============================================================"
display "ESCORE F POR TIPO DE ATENDIMENTO"
display "============================================================"

tabstat ///
    escore_F ///
    escore_F_10, ///
    by(tipo_F) ///
    statistics(n mean sd median min max)


/********************************************************************
CONFERÊNCIA DOS COMPONENTES AUSENTES
********************************************************************/

display "============================================================"
display "COMPONENTES ESSENCIAIS AUSENTES"
display "============================================================"

tab ncomp_missing_essencial, missing


display "============================================================"
display "COMPONENTES AUSENTES NO ESCORE GERAL"
display "============================================================"

tab ncomp_missing_geral, missing


/********************************************************************
CONFERÊNCIA ESPECÍFICA DO CASO ADULTOS + CRIANÇAS
********************************************************************/

display "============================================================"
display "ADULTOS + CRIANÇAS"
display "============================================================"

count if tipo_F == 4

summarize ///
    p_f1 p_f2 p_f3 ///
    escore_FAC ///
    nmiss_FAC ///
    if tipo_F == 4


/********************************************************************
FIM
********************************************************************/

display "============================================================"
display "CÁLCULO DO PCATOOL CONCLUÍDO"
display "============================================================"
