/***********************************************************************
***********************************************************************
      PCATOOL-BRASIL - CRIANÇAS - VERSÃO EXTENSA
      ANÁLISE COMPLETA DOS ESCORES - STATA

      Instrumento:
      PCATool Brasil - Para Pacientes Crianças - Versão Extensa

      COMPONENTES:

      Afiliação
      B = Acesso de Primeiro Contato - Utilização
      C = Acesso de Primeiro Contato - Acessibilidade
      D = Longitudinalidade
      E = Coordenação - Integração de Cuidados
      F = Coordenação - Sistemas de Informações
      G = Integralidade - Serviços Disponíveis
      H = Integralidade - Serviços Prestados
      I = Orientação Familiar
      J = Orientação Comunitária

      DATA:
      ____/____/________

***********************************************************************
***********************************************************************/


**clear all
**set more off


/***********************************************************************
 CARREGANDO O BANCO DE DADOS
***********************************************************************/

* Se o banco já estiver aberto, deixe esta linha comentada.

* use "seu_banco.dta", clear

* Exemplo:
* use "C:\PCATool\dados_pcatool_criancas.dta", clear


/***********************************************************************
 VERIFICANDO AS VARIÁVEIS
***********************************************************************/

describe a1 a2 a3 ///
         b1 b2 b3 ///
         c1 c2 c3 c4 c5 c6 ///
         d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 ///
         e1 e2 e3 e4 e5 e6 ///
         f1 f2 f3 ///
         g1 g2 g3 g4 g5 g6 g7 g8 g9 ///
         h1 h2 h3 h4 h5 ///
         i1 i2 i3 ///
         j1 j2 j3 j4


/***********************************************************************
 CONFERÊNCIA DOS CÓDIGOS
***********************************************************************/

display "=============================================================="
display "CONFERÊNCIA DOS CÓDIGOS DOS ITENS"
display "=============================================================="

foreach var of varlist ///
    a1 a2 a3 ///
    b1 b2 b3 ///
    c1 c2 c3 c4 c5 c6 ///
    d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 ///
    e1 e2 e3 e4 e5 e6 ///
    f1 f2 f3 ///
    g1 g2 g3 g4 g5 g6 g7 g8 g9 ///
    h1 h2 h3 h4 h5 ///
    i1 i2 i3 ///
    j1 j2 j3 j4 {

    display "----------------------------------------------------------"
    display "`var'"
    tab `var', missing
}


/***********************************************************************
 AFILIAÇÃO
***********************************************************************

 A1:
   0 = Não
   1 = Sim

 A2:
   0 = Não
   1 = Sim, mesmo serviço/profissional de A1
   2 = Sim, serviço/profissional diferente de A1

 A3:
   0 = Não
   1 = Sim, mesmo serviço/profissional referido em A1 e A2
   2 = Sim, mesmo serviço/profissional referido em A1
   3 = Sim, mesmo serviço/profissional referido em A2
   4 = Sim, diferente dos referidos em A1 e A2

 ESCORE:

   1 = nenhuma afiliação / todos NÃO

   2 = serviços/profissionais diferentes

   3 = dois itens identificam o mesmo serviço/profissional

   4 = os três itens identificam o mesmo serviço/profissional

***********************************************************************/


gen afiliacao = .


/*----------------------------------------------------------------------
 CASO 1
 A1 = NÃO
 A2 = NÃO
 A3 = NÃO

 Nenhum serviço/profissional de referência identificado.
 Escore = 1
----------------------------------------------------------------------*/

replace afiliacao = 1 if ///
    a1 == 0 & ///
    a2 == 0 & ///
    a3 == 0


/*----------------------------------------------------------------------
 CASO 2
 Os três apontam para o mesmo serviço/profissional.

 A1 = SIM
 A2 = SIM, mesmo A1
 A3 = SIM, mesmo A1 e A2

 Escore = 4
----------------------------------------------------------------------*/

replace afiliacao = 4 if ///
    a1 == 1 & ///
    a2 == 1 & ///
    a3 == 1


/*----------------------------------------------------------------------
 CASO 3
 A1 = SIM
 A2 = NÃO
 A3 = SIM, mesmo A1

 Dois itens apontam para o mesmo serviço.

 Escore = 3
----------------------------------------------------------------------*/

replace afiliacao = 3 if ///
    a1 == 1 & ///
    a2 == 0 & ///
    a3 == 2


/*----------------------------------------------------------------------
 CASO 4
 A1 = SIM
 A2 = serviço diferente
 A3 = mesmo serviço de A1

 A1 e A3 são iguais.

 Escore = 3
----------------------------------------------------------------------*/

replace afiliacao = 3 if ///
    a1 == 1 & ///
    a2 == 2 & ///
    a3 == 2


/*----------------------------------------------------------------------
 CASO 5
 A1 = SIM
 A2 = serviço diferente
 A3 = mesmo serviço de A2

 A2 e A3 são iguais.

 Escore = 3
----------------------------------------------------------------------*/

replace afiliacao = 3 if ///
    a1 == 1 & ///
    a2 == 2 & ///
    a3 == 3


/*----------------------------------------------------------------------
 CASO 6
 A1 = NÃO
 A2 = SIM
 A3 = mesmo serviço de A2

 A2 e A3 identificam o mesmo serviço.

 Escore = 3
----------------------------------------------------------------------*/

replace afiliacao = 3 if ///
    a1 == 0 & ///
    a2 == 2 & ///
    a3 == 3


/*----------------------------------------------------------------------
 CASO 7
 A1 = SIM
 A2 = serviço diferente
 A3 = serviço diferente de A1 e A2

 Três serviços diferentes.

 Escore = 2
----------------------------------------------------------------------*/

replace afiliacao = 2 if ///
    a1 == 1 & ///
    a2 == 2 & ///
    a3 == 4


/*----------------------------------------------------------------------
 CASO 8
 A1 = NÃO
 A2 = SIM
 A3 = serviço diferente de A1 e A2

 Apenas A2 identifica um serviço.

 Escore = 2
----------------------------------------------------------------------*/

replace afiliacao = 2 if ///
    a1 == 0 & ///
    a2 == 2 & ///
    a3 == 4


/*----------------------------------------------------------------------
 CASO 9
 A1 = SIM
 A2 = NÃO
 A3 = serviço diferente

 A1 e A3 são serviços diferentes.

 Escore = 2
----------------------------------------------------------------------*/

replace afiliacao = 2 if ///
    a1 == 1 & ///
    a2 == 0 & ///
    a3 == 4


/*----------------------------------------------------------------------
 CASO 10
 A1 = NÃO
 A2 = NÃO
 A3 = SIM

 Situação teoricamente inconsistente com o fluxo esperado do instrumento.
 Mantém missing para inspeção.
----------------------------------------------------------------------*/


/*----------------------------------------------------------------------
 CASO 11
 A1 = NÃO
 A2 = SIM
 A3 = mesmo A1

 Situação inconsistente, pois A1 = NÃO.
 Mantém missing.
----------------------------------------------------------------------*/


/*----------------------------------------------------------------------
 CASO 12
 A1 = SIM
 A2 = SIM, mesmo A1
 A3 = NÃO

 Situação inconsistente com a lógica da afiliação.
 Mantém missing.
----------------------------------------------------------------------*/


label variable afiliacao ///
    "Grau de afiliacao - PCATool"

label define lab_afiliacao ///
    1 "Grau 1 - sem afiliacao" ///
    2 "Grau 2 - servicos diferentes" ///
    3 "Grau 3 - dois itens mesmo servico" ///
    4 "Grau 4 - tres itens mesmo servico"

label values afiliacao lab_afiliacao


/***********************************************************************
 CONFERIR AFILIAÇÃO
***********************************************************************/

tab afiliacao, missing

tab a1 a2, missing
tab a1 a3, missing
tab a2 a3, missing

display "=============================================================="
display "CASOS DE AFILIACAO NÃO CLASSIFICADOS"
display "=============================================================="

list record_id a1 a2 a3 afiliacao ///
    if missing(afiliacao), noobs


/***********************************************************************
 CRIANDO CÓPIAS DAS VARIAVEIS/ ITENS
***********************************************************************/

foreach var of varlist ///
    b1 b2 b3 ///
    c1 c2 c3 c4 c5 c6 ///
    d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 ///
    e2 e3 e4 e5 e6 ///
    f1 f2 f3 ///
    g1 g2 g3 g4 g5 g6 g7 g8 g9 ///
    h1 h2 h3 h4 h5 ///
    i1 i2 i3 ///
    j1 j2 j3 j4 {

    clonevar `var'_t = `var'
}


/***********************************************************************
 INVERSÃO DOS ITENS

 C2
 C4
 C5
 D10

 4 -> 1
 3 -> 2
 2 -> 3
 1 -> 4

 O valor 9 permanece 9 nesta etapa.
***********************************************************************/

foreach var in c2 c4 c5 d10 {

    replace `var'_t = 5 - `var' ///
        if inrange(`var',1,4)

}


/***********************************************************************
 CONFERIR INVERSÃO
***********************************************************************/

display "=============================================================="
display "CONFERÊNCIA DA INVERSÃO"
display "=============================================================="

tab c2 c2_t, missing
tab c4 c4_t, missing
tab c5 c5_t, missing
tab d10 d10_t, missing


/***********************************************************************
 B - ACESSO DE PRIMEIRO CONTATO - UTILIZAÇÃO

 B1 B2 B3

 Total = 3 itens
 50% = 1,5

 Portanto:
 0 ou 1 ausente -> calcula
 2 ou 3 ausentes -> não calcula
***********************************************************************/

egen b_nmiss = rowmiss(b1_t b2_t b3_t)

foreach var in b1_t b2_t b3_t {

    replace b_nmiss = b_nmiss + ///
        (`var' == 9) if !missing(`var')

}

foreach var in b1_t b2_t b3_t {

    replace `var' = 2 ///
        if `var' == 9 & b_nmiss < 1.5

}

egen b_nvalid = rownonmiss(b1_t b2_t b3_t)

gen escore_b = .

replace escore_b = ///
    (b1_t + b2_t + b3_t) / 3 ///
    if b_nmiss < 1.5 & b_nvalid == 3

label variable escore_b ///
    "B - Acesso primeiro contato/utilizacao"


/***********************************************************************
 C - ACESSO DE PRIMEIRO CONTATO - ACESSIBILIDADE

 C1 C2 C3 C4 C5 C6

 Total = 6
 50% = 3

 Portanto:
 0, 1 ou 2 ausentes -> calcula
 3 ou mais -> não calcula
***********************************************************************/

egen c_nmiss = rowmiss( ///
    c1_t c2_t c3_t c4_t c5_t c6_t)

foreach var in ///
    c1_t c2_t c3_t c4_t c5_t c6_t {

    replace c_nmiss = c_nmiss + ///
        (`var' == 9) if !missing(`var')

}

foreach var in ///
    c1_t c2_t c3_t c4_t c5_t c6_t {

    replace `var' = 2 ///
        if `var' == 9 & c_nmiss < 3

}

egen c_nvalid = rownonmiss( ///
    c1_t c2_t c3_t c4_t c5_t c6_t)

gen escore_c = .

replace escore_c = ///
    (c1_t + c2_t + c3_t + c4_t + c5_t + c6_t) / 6 ///
    if c_nmiss < 3 & c_nvalid == 6

label variable escore_c ///
    "C - Acesso primeiro contato/acessibilidade"


/***********************************************************************
 D - LONGITUDINALIDADE

 D1-D14

 Total = 14
 50% = 7

 Portanto:
 0 a 6 ausentes -> calcula
 7 ou mais -> não calcula
***********************************************************************/

egen d_nmiss = rowmiss( ///
    d1_t d2_t d3_t d4_t d5_t d6_t d7_t ///
    d8_t d9_t d10_t d11_t d12_t d13_t d14_t)

foreach var in ///
    d1_t d2_t d3_t d4_t d5_t d6_t d7_t ///
    d8_t d9_t d10_t d11_t d12_t d13_t d14_t {

    replace d_nmiss = d_nmiss + ///
        (`var' == 9) if !missing(`var')

}

foreach var in ///
    d1_t d2_t d3_t d4_t d5_t d6_t d7_t ///
    d8_t d9_t d10_t d11_t d12_t d13_t d14_t {

    replace `var' = 2 ///
        if `var' == 9 & d_nmiss < 7

}

egen d_nvalid = rownonmiss( ///
    d1_t d2_t d3_t d4_t d5_t d6_t d7_t ///
    d8_t d9_t d10_t d11_t d12_t d13_t d14_t)

gen escore_d = .

replace escore_d = ///
    (d1_t + d2_t + d3_t + d4_t + d5_t + d6_t + d7_t + ///
     d8_t + d9_t + d10_t + d11_t + d12_t + d13_t + d14_t) / 14 ///
     if d_nmiss < 7 & d_nvalid == 14

label variable escore_d ///
    "D - Longitudinalidade"


/***********************************************************************
 E - COORDENAÇÃO - INTEGRAÇÃO DE CUIDADOS

 E1 NÃO entra no cálculo.

 E2 E3 E4 E5 E6

 Total = 5
 50% = 2,5

 0, 1 ou 2 ausentes -> calcula
 3 ou mais -> não calcula
***********************************************************************/

egen e_nmiss = rowmiss( ///
    e2_t e3_t e4_t e5_t e6_t)

foreach var in e2_t e3_t e4_t e5_t e6_t {

    replace e_nmiss = e_nmiss + ///
        (`var' == 9) if !missing(`var')

}

foreach var in e2_t e3_t e4_t e5_t e6_t {

    replace `var' = 2 ///
        if `var' == 9 & e_nmiss < 2.5

}

egen e_nvalid = rownonmiss( ///
    e2_t e3_t e4_t e5_t e6_t)

gen escore_e = .

replace escore_e = ///
    (e2_t + e3_t + e4_t + e5_t + e6_t) / 5 ///
    if e_nmiss < 2.5 & e_nvalid == 5

label variable escore_e ///
    "E - Coordenacao/integracao de cuidados"


/***********************************************************************
 F - COORDENAÇÃO - SISTEMA DE INFORMAÇÕES

 F1 F2 F3

 Total = 3
 50% = 1,5
***********************************************************************/

egen f_nmiss = rowmiss(f1_t f2_t f3_t)

foreach var in f1_t f2_t f3_t {

    replace f_nmiss = f_nmiss + ///
        (`var' == 9) if !missing(`var')

}

foreach var in f1_t f2_t f3_t {

    replace `var' = 2 ///
        if `var' == 9 & f_nmiss < 1.5

}

egen f_nvalid = rownonmiss(f1_t f2_t f3_t)

gen escore_f = .

replace escore_f = ///
    (f1_t + f2_t + f3_t) / 3 ///
    if f_nmiss < 1.5 & f_nvalid == 3

label variable escore_f ///
    "F - Coordenacao/sistema de informacoes"


/***********************************************************************
 G - INTEGRALIDADE - SERVIÇOS DISPONÍVEIS

 G1-G9

 Total = 9
 50% = 4,5

 0 a 4 ausentes -> calcula
 5 ou mais -> não calcula
***********************************************************************/

egen g_nmiss = rowmiss( ///
    g1_t g2_t g3_t g4_t g5_t g6_t g7_t g8_t g9_t)

foreach var in ///
    g1_t g2_t g3_t g4_t g5_t g6_t g7_t g8_t g9_t {

    replace g_nmiss = g_nmiss + ///
        (`var' == 9) if !missing(`var')

}

foreach var in ///
    g1_t g2_t g3_t g4_t g5_t g6_t g7_t g8_t g9_t {

    replace `var' = 2 ///
        if `var' == 9 & g_nmiss < 4.5

}

egen g_nvalid = rownonmiss( ///
    g1_t g2_t g3_t g4_t g5_t g6_t g7_t g8_t g9_t)

gen escore_g = .

replace escore_g = ///
    (g1_t + g2_t + g3_t + g4_t + g5_t + ///
     g6_t + g7_t + g8_t + g9_t) / 9 ///
     if g_nmiss < 4.5 & g_nvalid == 9

label variable escore_g ///
    "G - Integralidade/servicos disponiveis"


/***********************************************************************
 H - INTEGRALIDADE - SERVIÇOS PRESTADOS

 H1-H5

 Total = 5
 50% = 2,5
***********************************************************************/

egen h_nmiss = rowmiss( ///
    h1_t h2_t h3_t h4_t h5_t)

foreach var in h1_t h2_t h3_t h4_t h5_t {

    replace h_nmiss = h_nmiss + ///
        (`var' == 9) if !missing(`var')

}

foreach var in h1_t h2_t h3_t h4_t h5_t {

    replace `var' = 2 ///
        if `var' == 9 & h_nmiss < 2.5

}

egen h_nvalid = rownonmiss( ///
    h1_t h2_t h3_t h4_t h5_t)

gen escore_h = .

replace escore_h = ///
    (h1_t + h2_t + h3_t + h4_t + h5_t) / 5 ///
    if h_nmiss < 2.5 & h_nvalid == 5

label variable escore_h ///
    "H - Integralidade/servicos prestados"


/***********************************************************************
 I - ORIENTAÇÃO FAMILIAR

 I1 I2 I3

 Total = 3
 50% = 1,5
***********************************************************************/

egen i_nmiss = rowmiss(i1_t i2_t i3_t)

foreach var in i1_t i2_t i3_t {

    replace i_nmiss = i_nmiss + ///
        (`var' == 9) if !missing(`var')

}

foreach var in i1_t i2_t i3_t {

    replace `var' = 2 ///
        if `var' == 9 & i_nmiss < 1.5

}

egen i_nvalid = rownonmiss(i1_t i2_t i3_t)

gen escore_i = .

replace escore_i = ///
    (i1_t + i2_t + i3_t) / 3 ///
    if i_nmiss < 1.5 & i_nvalid == 3

label variable escore_i ///
    "I - Orientacao Familiar"


/***********************************************************************
 J - ORIENTAÇÃO COMUNITÁRIA

 J1 J2 J3 J4

 Total = 4
 50% = 2

 0 ou 1 ausente -> calcula
 2 ou mais -> não calcula
***********************************************************************/

egen j_nmiss = rowmiss( ///
    j1_t j2_t j3_t j4_t)

foreach var in j1_t j2_t j3_t j4_t {

    replace j_nmiss = j_nmiss + ///
        (`var' == 9) if !missing(`var')

}

foreach var in j1_t j2_t j3_t j4_t {

    replace `var' = 2 ///
        if `var' == 9 & j_nmiss < 2

}

egen j_nvalid = rownonmiss( ///
    j1_t j2_t j3_t j4_t)

gen escore_j = .

replace escore_j = ///
    (j1_t + j2_t + j3_t + j4_t) / 4 ///
    if j_nmiss < 2 & j_nvalid == 4

label variable escore_j ///
    "J - Orientacao Comunitaria"


/***********************************************************************
 ESCALA DOS COMPONENTES
***********************************************************************/

display "=============================================================="
display "ESCORE DOS COMPONENTES - ESCALA ORIGINAL 1 A 4"
display "=============================================================="

summarize ///
    afiliacao ///
    escore_b ///
    escore_c ///
    escore_d ///
    escore_e ///
    escore_f ///
    escore_g ///
    escore_h ///
    escore_i ///
    escore_j


/***********************************************************************
 ESCORE ESSENCIAL DA APS

 Componentes:

 Afiliação
 B
 C
 D
 E
 F
 G
 H

 Total = 8

 Se 4 ou mais componentes missing:
     não calcula.

 Se 0 a 3 componentes missing:
     calcula média dos componentes disponíveis.
***********************************************************************/

egen essencial_nmiss = rowmiss( ///
    afiliacao ///
    escore_b ///
    escore_c ///
    escore_d ///
    escore_e ///
    escore_f ///
    escore_g ///
    escore_h)

egen essencial_nvalid = rownonmiss( ///
    afiliacao ///
    escore_b ///
    escore_c ///
    escore_d ///
    escore_e ///
    escore_f ///
    escore_g ///
    escore_h)

gen escore_essencial = .

replace escore_essencial = ///
    (afiliacao + ///
     escore_b + ///
     escore_c + ///
     escore_d + ///
     escore_e + ///
     escore_f + ///
     escore_g + ///
     escore_h) / essencial_nvalid ///
     if essencial_nmiss <= 3

label variable escore_essencial ///
    "Escore Essencial da APS - 1 a 4"


/***********************************************************************
 ESCORE GERAL DA APS

 Componentes:

 Afiliação
 B
 C
 D
 E
 F
 G
 H
 I
 J

 Total = 10

 Se 5 ou mais componentes missing:
     não calcula.

 Se 0 a 4 componentes missing:
     calcula média dos componentes disponíveis.
***********************************************************************/

egen geral_nmiss = rowmiss( ///
    afiliacao ///
    escore_b ///
    escore_c ///
    escore_d ///
    escore_e ///
    escore_f ///
    escore_g ///
    escore_h ///
    escore_i ///
    escore_j)

egen geral_nvalid = rownonmiss( ///
    afiliacao ///
    escore_b ///
    escore_c ///
    escore_d ///
    escore_e ///
    escore_f ///
    escore_g ///
    escore_h ///
    escore_i ///
    escore_j)

gen escore_geral = .

replace escore_geral = ///
    (afiliacao + ///
     escore_b + ///
     escore_c + ///
     escore_d + ///
     escore_e + ///
     escore_f + ///
     escore_g + ///
     escore_h + ///
     escore_i + ///
     escore_j) / geral_nvalid ///
     if geral_nmiss <= 4

label variable escore_geral ///
    "Escore Geral da APS - 1 a 4"


/***********************************************************************
 TRANSFORMAÇÃO DOS ESCORES PARA 0-10

 Fórmula:

 ((escore - 1) / (4 - 1)) * 10

 Portanto:

 1 = 0
 2 = 3,33
 3 = 6,67
 4 = 10
***********************************************************************/


gen afiliacao_0_10 = ///
    ((afiliacao - 1) / 3) * 10 ///
    if !missing(afiliacao)

label variable afiliacao_0_10 ///
    "Afiliação - 0 a 10"


gen escore_b_0_10 = ///
    ((escore_b - 1) / 3) * 10 ///
    if !missing(escore_b)

gen escore_c_0_10 = ///
    ((escore_c - 1) / 3) * 10 ///
    if !missing(escore_c)

gen escore_d_0_10 = ///
    ((escore_d - 1) / 3) * 10 ///
    if !missing(escore_d)

gen escore_e_0_10 = ///
    ((escore_e - 1) / 3) * 10 ///
    if !missing(escore_e)

gen escore_f_0_10 = ///
    ((escore_f - 1) / 3) * 10 ///
    if !missing(escore_f)

gen escore_g_0_10 = ///
    ((escore_g - 1) / 3) * 10 ///
    if !missing(escore_g)

gen escore_h_0_10 = ///
    ((escore_h - 1) / 3) * 10 ///
    if !missing(escore_h)

gen escore_i_0_10 = ///
    ((escore_i - 1) / 3) * 10 ///
    if !missing(escore_i)

gen escore_j_0_10 = ///
    ((escore_j - 1) / 3) * 10 ///
    if !missing(escore_j)


gen escore_essencial_0_10 = ///
    ((escore_essencial - 1) / 3) * 10 ///
    if !missing(escore_essencial)

gen escore_geral_0_10 = ///
    ((escore_geral - 1) / 3) * 10 ///
    if !missing(escore_geral)


label variable escore_b_0_10 ///
    "B - 0 a 10"

label variable escore_c_0_10 ///
    "C - 0 a 10"

label variable escore_d_0_10 ///
    "D - 0 a 10"

label variable escore_e_0_10 ///
    "E - 0 a 10"

label variable escore_f_0_10 ///
    "F - 0 a 10"

label variable escore_g_0_10 ///
    "G - 0 a 10"

label variable escore_h_0_10 ///
    "H - 0 a 10"

label variable escore_i_0_10 ///
    "I - 0 a 10"

label variable escore_j_0_10 ///
    "J - 0 a 10"

label variable escore_essencial_0_10 ///
    "Escore Essencial APS - 0 a 10"

label variable escore_geral_0_10 ///
    "Escore Geral APS - 0 a 10"


/***********************************************************************
 TRANSFORMAR ITENS INDIVIDUAIS PARA 0-10
***********************************************************************/

foreach var of varlist ///
    b1_t b2_t b3_t ///
    c1_t c2_t c3_t c4_t c5_t c6_t ///
    d1_t d2_t d3_t d4_t d5_t d6_t d7_t ///
    d8_t d9_t d10_t d11_t d12_t d13_t d14_t ///
    e2_t e3_t e4_t e5_t e6_t ///
    f1_t f2_t f3_t ///
    g1_t g2_t g3_t g4_t g5_t g6_t g7_t g8_t g9_t ///
    h1_t h2_t h3_t h4_t h5_t ///
    i1_t i2_t i3_t ///
    j1_t j2_t j3_t j4_t {

    gen `var'_0_10 = .

    replace `var'_0_10 = ///
        ((`var' - 1) / 3) * 10 ///
        if inrange(`var',1,4)
}


/***********************************************************************
 FORMATAR ESCORES
***********************************************************************/

format afiliacao ///
       escore_b escore_c escore_d escore_e escore_f ///
       escore_g escore_h escore_i escore_j ///
       escore_essencial escore_geral ///
       afiliacao_0_10 ///
       escore_b_0_10 escore_c_0_10 escore_d_0_10 ///
       escore_e_0_10 escore_f_0_10 escore_g_0_10 ///
       escore_h_0_10 escore_i_0_10 escore_j_0_10 ///
       escore_essencial_0_10 escore_geral_0_10 %9.2f


/***********************************************************************
 RESUMO DOS ESCORES
***********************************************************************/

display "=============================================================="
display "ESCORE ORIGINAL - 1 A 4"
display "=============================================================="

summarize ///
    afiliacao ///
    escore_b ///
    escore_c ///
    escore_d ///
    escore_e ///
    escore_f ///
    escore_g ///
    escore_h ///
    escore_i ///
    escore_j ///
    escore_essencial ///
    escore_geral


display "=============================================================="
display "ESCORE TRANSFORMADO - 0 A 10"
display "=============================================================="

summarize ///
    afiliacao_0_10 ///
    escore_b_0_10 ///
    escore_c_0_10 ///
    escore_d_0_10 ///
    escore_e_0_10 ///
    escore_f_0_10 ///
    escore_g_0_10 ///
    escore_h_0_10 ///
    escore_i_0_10 ///
    escore_j_0_10 ///
    escore_essencial_0_10 ///
    escore_geral_0_10


/***********************************************************************
 NÚMERO DE COMPONENTES DISPONÍVEIS
***********************************************************************/

display "=============================================================="
display "NÚMERO DE COMPONENTES VÁLIDOS"
display "=============================================================="

tab essencial_nmiss, missing
tab geral_nmiss, missing


/***********************************************************************
 PERCENTUAL DE MISSING DOS COMPONENTES
***********************************************************************/

display "=============================================================="
display "PERCENTUAL DE MISSING POR COMPONENTE"
display "=============================================================="

foreach var in ///
    afiliacao ///
    escore_b ///
    escore_c ///
    escore_d ///
    escore_e ///
    escore_f ///
    escore_g ///
    escore_h ///
    escore_i ///
    escore_j ///
    escore_essencial ///
    escore_geral {

    quietly count
    local Ntotal = r(N)

    quietly count if missing(`var')
    local Nmiss = r(N)

    display "`var' : " ///
        %6.2f (100*`Nmiss'/`Ntotal') "%"
}


/***********************************************************************
 VERIFICANDO LIMITES DOS ESCORES
***********************************************************************/

display "=============================================================="
display "VERIFICAÇÃO DOS ESCORES 1-4"
display "=============================================================="

foreach var in ///
    afiliacao ///
    escore_b ///
    escore_c ///
    escore_d ///
    escore_e ///
    escore_f ///
    escore_g ///
    escore_h ///
    escore_i ///
    escore_j ///
    escore_essencial ///
    escore_geral {

    quietly count if ///
        !missing(`var') & ///
        (`var' < 1 | `var' > 4)

    if r(N) > 0 {

        display as error ///
            "`var': ATENÇÃO - valores fora de 1 a 4"

        list record_id `var' ///
            if !missing(`var') & ///
            (`var' < 1 | `var' > 4), noobs

    }
    else {

        display "`var': OK"

    }
}


/***********************************************************************
 VERIFICANDO LIMITES DOS ESCORES 0-10
***********************************************************************/

display "=============================================================="
display "VERIFICAÇÃO DOS ESCORES 0-10"
display "=============================================================="

foreach var in ///
    afiliacao_0_10 ///
    escore_b_0_10 ///
    escore_c_0_10 ///
    escore_d_0_10 ///
    escore_e_0_10 ///
    escore_f_0_10 ///
    escore_g_0_10 ///
    escore_h_0_10 ///
    escore_i_0_10 ///
    escore_j_0_10 ///
    escore_essencial_0_10 ///
    escore_geral_0_10 {

    quietly count if ///
        !missing(`var') & ///
        (`var' < 0 | `var' > 10)

    if r(N) > 0 {

        display as error ///
            "`var': ATENÇÃO - valores fora de 0 a 10"

    }
    else {

        display "`var': OK"

    }
}


/***********************************************************************
 + ESTATÍSTICAS DESCRITIVAS
***********************************************************************/

tabstat ///
    afiliacao_0_10 ///
    escore_b_0_10 ///
    escore_c_0_10 ///
    escore_d_0_10 ///
    escore_e_0_10 ///
    escore_f_0_10 ///
    escore_g_0_10 ///
    escore_h_0_10 ///
    escore_i_0_10 ///
    escore_j_0_10 ///
    escore_essencial_0_10 ///
    escore_geral_0_10, ///
    statistics(n mean sd p25 p50 p75 min max) ///
    columns(statistics)


/***********************************************************************
 MEDIANA E INTERVALO INTERQUARTIL
***********************************************************************/

foreach var in ///
    afiliacao_0_10 ///
    escore_b_0_10 ///
    escore_c_0_10 ///
    escore_d_0_10 ///
    escore_e_0_10 ///
    escore_f_0_10 ///
    escore_g_0_10 ///
    escore_h_0_10 ///
    escore_i_0_10 ///
    escore_j_0_10 ///
    escore_essencial_0_10 ///
    escore_geral_0_10 {

    quietly summarize `var', detail

    display "-----------------------------------------------"
    display "`var'"
    display "N       = " r(N)
    display "Mediana = " %9.2f r(p50)
    display "P25     = " %9.2f r(p25)
    display "P75     = " %9.2f r(p75)
    display "IQR     = " %9.2f (r(p75)-r(p25))
}

/***********************************************************************
 HISTOGRAMAS
***********************************************************************/

histogram escore_essencial_0_10, ///
    frequency ///
    normal ///
    title("Escore Essencial da APS") ///
    xtitle("Escore 0-10") ///
    ytitle("Frequência")

histogram escore_geral_0_10, ///
    frequency ///
    normal ///
    title("Escore Geral da APS") ///
    xtitle("Escore 0-10") ///
    ytitle("Frequência")


/***********************************************************************
 BOX PLOT DOS COMPONENTES
***********************************************************************/

graph box ///
    escore_b_0_10 ///
    escore_c_0_10 ///
    escore_d_0_10 ///
    escore_e_0_10 ///
    escore_f_0_10 ///
    escore_g_0_10 ///
    escore_h_0_10 ///
    escore_i_0_10 ///
    escore_j_0_10, ///
    title("Escores dos componentes da APS") ///
    ytitle("Escore 0-10")


/***********************************************************************
 SALVAR BANCO FINAL
***********************************************************************/

** save "pcatool_criancas_escores_final.dta", replace


/***********************************************************************
 EXPORTAR RESUMO DOS ESCORES PARA EXCEL
***********************************************************************/

**preserve

**keep record_id ///
**     afiliacao ///
**     escore_b escore_c escore_d escore_e ///
**     escore_f escore_g escore_h escore_i escore_j ///
**     escore_essencial escore_geral ///
**     afiliacao_0_10 ///
**     escore_b_0_10 escore_c_0_10 escore_d_0_10 ///
**     escore_e_0_10 escore_f_0_10 escore_g_0_10 ///
**     escore_h_0_10 escore_i_0_10 escore_j_0_10 ///
**     escore_essencial_0_10 ///
**     escore_geral_0_10

** export excel using "pcatool_criancas_escores.xlsx", ///
**    firstrow(variables) ///
**    replace

**restore


/***********************************************************************
 FINAL - PARA FICAR BONITO NO LOG
***********************************************************************/

display ""
display "=============================================================="
display "ANÁLISE PCATOOL CONCLUÍDA"
display "=============================================================="

display "Banco final:"
display "pcatool_criancas_escores_final.dta"

display "Arquivo Excel:"
display "pcatool_criancas_escores.xlsx"

display ""
display "Escores principais:"
display "  - afiliacao"
display "  - escore_b"
display "  - escore_c"
display "  - escore_d"
display "  - escore_e"
display "  - escore_f"
display "  - escore_g"
display "  - escore_h"
display "  - escore_i"
display "  - escore_j"
display "  - escore_essencial"
display "  - escore_geral"

display ""
display "Versões 0-10:"
display "  - *_0_10"

display ""
display "=============================================================="
display "FIM"
display "=============================================================="
