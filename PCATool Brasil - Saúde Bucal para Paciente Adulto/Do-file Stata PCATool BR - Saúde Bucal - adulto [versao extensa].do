/**************************************************************************
***************************************************************************
*
*     PCATOOL-BRASIL - SAÚDE BUCAL - PACIENTE ADULTO
*     VERSÃO EXTENSA
*
*     DO-FILE COMPLETO PARA ANÁLISE
*
***************************************************************************
*
*     Instrumento:
*     PCATool-Brasil - Saúde Bucal para paciente adulto, segundo Manual MS
*
*     Banco REDCap:
*     form_pcatoolbrasilsaudebucaladulto
*
*     Variáveis utilizadas:
*
*     Afiliação:
*       a1 a2 a3
*
*     Acesso - Utilização:
*       b1 b2 b3
*
*     Acesso - Acessibilidade:
*       c1-c12
*
*     Longitudinalidade:
*       d1-d15
*
*     Coordenação - Integração:
*       e1 e3-e10
*       e2 NÃO entra no escore
*
*     Coordenação - Sistemas de Informação:
*       f1-f3
*
*     Integralidade - Serviços Disponíveis:
*       g1-g23
*
*     Integralidade - Serviços Prestados:
*       h1-h9
*
*     Orientação Familiar:
*       i1-i3
*
*     Orientação Comunitária:
*       j1-j6
*
***************************************************************************
*
*     REGRAS DO MANUAL
*
*     Escala:
*       4 = Com certeza sim
*       3 = Provavelmente sim
*       2 = Provavelmente não
*       1 = Com certeza não
*       9 = Não sei / Não lembro
*
*     Valores ausentes:
*       9 + missing
*
*     Se ausentes >= 50% dos itens do componente:
*       escore = missing
*
*     Se ausentes < 50%:
*       9 é imputado como 2
*
*     Itens invertidos:
*       C9 C10 C11 C12
*       D14 D15
*
*     Inversão:
*       4 -> 1
*       3 -> 2
*       2 -> 3
*       1 -> 4
*
*     Transformação 1-4 para 0-10:
*
*       ((escore - 1) / 3) * 10
*
*     Classificação:
*       Alto = >= 6,6
*       Baixo = < 6,6
*
***************************************************************************
*
*     ESCORES
*
*     Afiliação = algoritmo específico
*
*     B = (B1+B2+B3)/3
*
*     C = (C1+...+C12)/12
*
*     D = (D1+...+D15)/15
*
*     E = (E1+E3+...+E10)/9
*
*     F = (F1+F2+F3)/3
*
*     G = (G1+...+G23)/23
*
*     H = (H1+...+H9)/9
*
*     I = (I1+I2+I3)/3
*
*     J = (J1+...+J6)/6
*
*     Essencial = média dos 8 componentes:
*       Afiliação B C D E F G H
*
*     Geral = média dos 10 componentes:
*       Afiliação B C D E F G H I J
*
***************************************************************************
***************************************************************************/


**version 17.0
**clear matrix
**set more off
**set linesize 120


/**************************************************************************
* DEFINA O CAMINHO DO BANCO
*
* ADAPTE ESTA PARTE AO SEU COMPUTADOR.
**************************************************************************/

* Exemplo:
*
* cd "C:\Users\SeuNome\Documents\PCATool"
*
* use "pcatool_bucal_adulto.dta", clear


/**************************************************************************
* IMPORTAÇÃO DO BANCO
*
* DESCOMENTE UMA DAS OPÇÕES ABAIXO.
**************************************************************************/

* ---------- Opção A: banco .dta ----------
*
* use "C:\CAMINHO\pcatool_bucal_adulto.dta", clear


* ---------- Opção B: banco .csv ----------
*
* import delimited ///
*     "C:\CAMINHO\pcatool_bucal_adulto.csv", ///
*     clear ///
*     varnames(1) ///
*     encoding(UTF-8)


/**************************************************************************
* IDENTIFICAÇÃO DA BASE
**************************************************************************/

display " "
display "=============================================================="
display " PCATOOL-BRASIL - SAÚDE BUCAL - ADULTO"
display " Início da análise"
display "=============================================================="
display " "


/**************************************************************************
* CONFERÊNCIA DAS VARIÁVEIS PRINCIPAIS
**************************************************************************/

describe record_id a1 a2 a3


/**************************************************************************
* LISTA DE ITENS LIKERT
**************************************************************************/

local B_items ///
    b1 b2 b3

local C_items ///
    c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12

local D_items ///
    d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 ///
    d11 d12 d13 d14 d15

local E_items ///
    e1 e3 e4 e5 e6 e7 e8 e9 e10

local F_items ///
    f1 f2 f3

local G_items ///
    g1 g2 g3 g4 g5 g6 g7 g8 g9 g10 ///
    g11 g12 g13 g14 g15 g16 g17 g18 g19 ///
    g20 g21 g22 g23

local H_items ///
    h1 h2 h3 h4 h5 h6 h7 h8 h9

local I_items ///
    i1 i2 i3

local J_items ///
    j1 j2 j3 j4 j5 j6


/**************************************************************************
* VERIFICAÇÃO DOS CÓDIGOS
**************************************************************************/

display " "
display "=============================================================="
display " VERIFICAÇÃO DOS ITENS"
display "=============================================================="

foreach v of local B_items {
    tab `v', missing
}

foreach v of local C_items {
    tab `v', missing
}

foreach v of local D_items {
    tab `v', missing
}

foreach v of local E_items {
    tab `v', missing
}

foreach v of local F_items {
    tab `v', missing
}

foreach v of local G_items {
    tab `v', missing
}

foreach v of local H_items {
    tab `v', missing
}

foreach v of local I_items {
    tab `v', missing
}

foreach v of local J_items {
    tab `v', missing
}


/**************************************************************************
* VERIFICAÇÃO DE VALORES INVÁLIDOS
*
* Para os itens Likert, somente 1,2,3,4,9 ou missing são esperados.
**************************************************************************/

display " "
display "=============================================================="
display " VALORES INVÁLIDOS NOS ITENS LIKERT"
display "=============================================================="

foreach v of local B_items {
    count if !missing(`v') & !inlist(`v',1,2,3,4,9)
    if r(N)>0 {
        display as error "ERRO: `v' possui valores inválidos."
        list record_id `v' if !missing(`v') & !inlist(`v',1,2,3,4,9)
    }
}

foreach v of local C_items {
    count if !missing(`v') & !inlist(`v',1,2,3,4,9)
    if r(N)>0 {
        display as error "ERRO: `v' possui valores inválidos."
        list record_id `v' if !missing(`v') & !inlist(`v',1,2,3,4,9)
    }
}

foreach v of local D_items {
    count if !missing(`v') & !inlist(`v',1,2,3,4,9)
    if r(N)>0 {
        display as error "ERRO: `v' possui valores inválidos."
        list record_id `v' if !missing(`v') & !inlist(`v',1,2,3,4,9)
    }
}

foreach v of local E_items {
    count if !missing(`v') & !inlist(`v',1,2,3,4,9)
    if r(N)>0 {
        display as error "ERRO: `v' possui valores inválidos."
        list record_id `v' if !missing(`v') & !inlist(`v',1,2,3,4,9)
    }
}

foreach v of local F_items {
    count if !missing(`v') & !inlist(`v',1,2,3,4,9)
    if r(N)>0 {
        display as error "ERRO: `v' possui valores inválidos."
        list record_id `v' if !missing(`v') & !inlist(`v',1,2,3,4,9)
    }
}

foreach v of local G_items {
    count if !missing(`v') & !inlist(`v',1,2,3,4,9)
    if r(N)>0 {
        display as error "ERRO: `v' possui valores inválidos."
        list record_id `v' if !missing(`v') & !inlist(`v',1,2,3,4,9)
    }
}

foreach v of local H_items {
    count if !missing(`v') & !inlist(`v',1,2,3,4,9)
    if r(N)>0 {
        display as error "ERRO: `v' possui valores inválidos."
        list record_id `v' if !missing(`v') & !inlist(`v',1,2,3,4,9)
    }
}

foreach v of local I_items {
    count if !missing(`v') & !inlist(`v',1,2,3,4,9)
    if r(N)>0 {
        display as error "ERRO: `v' possui valores inválidos."
        list record_id `v' if !missing(`v') & !inlist(`v',1,2,3,4,9)
    }
}

foreach v of local J_items {
    count if !missing(`v') & !inlist(`v',1,2,3,4,9)
    if r(N)>0 {
        display as error "ERRO: `v' possui valores inválidos."
        list record_id `v' if !missing(`v') & !inlist(`v',1,2,3,4,9)
    }
}


/**************************************************************************
* AFILIAÇÃO
*
* Afiliação não é calculada como média.
* É utilizado o algoritmo específico do manual.
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
*   1 = mesmo serviço de A1 e A2
*   2 = mesmo serviço de A1
*   3 = mesmo serviço de A2
*   4 = serviço diferente de A1 e A2
*
* Resultado:
*
*   1 = todas NÃO
*   2 = SIM relacionados a serviços diferentes
*   3 = dois ou mais SIM relacionados ao mesmo serviço
*   4 = todos SIM relacionados ao mesmo serviço
**************************************************************************/

capture drop esc_afiliacao
capture drop afiliacao_nsim
capture drop afiliacao_ndist
capture drop afiliacao_inconsistente

gen esc_afiliacao = .
gen afiliacao_nsim = .
gen afiliacao_ndist = .
gen afiliacao_inconsistente = 0


/**************************************************************************
* Verificar códigos inválidos de A1
**************************************************************************/

count if !missing(a1) & !inlist(a1,0,1)

if r(N)>0 {
    display as error "Atenção: A1 possui códigos inválidos."
    list record_id a1 if !missing(a1) & !inlist(a1,0,1)
    replace afiliacao_inconsistente = 1 ///
        if !missing(a1) & !inlist(a1,0,1)
}


/**************************************************************************
* Verificar códigos inválidos de A2
**************************************************************************/

count if !missing(a2) & !inlist(a2,0,1,2)

if r(N)>0 {
    display as error "Atenção: A2 possui códigos inválidos."
    list record_id a2 if !missing(a2) & !inlist(a2,0,1,2)
    replace afiliacao_inconsistente = 1 ///
        if !missing(a2) & !inlist(a2,0,1,2)
}


/**************************************************************************
* Verificar códigos inválidos de A3
**************************************************************************/

count if !missing(a3) & !inlist(a3,0,1,2,3,4)

if r(N)>0 {
    display as error "Atenção: A3 possui códigos inválidos."
    list record_id a3 if !missing(a3) & !inlist(a3,0,1,2,3,4)
    replace afiliacao_inconsistente = 1 ///
        if !missing(a3) & !inlist(a3,0,1,2,3,4)
}


/**************************************************************************
* Número de respostas SIM
**************************************************************************/

replace afiliacao_nsim = ///
    (a1 == 1) + ///
    inlist(a2,1,2) + ///
    inlist(a3,1,2,3,4) ///
    if !missing(a1,a2,a3)


/**************************************************************************
* Identificação dos serviços
*
* Criamos códigos internos apenas para calcular a quantidade de serviços
* diferentes. Eles não representam os nomes reais dos serviços.
**************************************************************************/

capture drop _afil_s1
capture drop _afil_s2
capture drop _afil_s3

gen _afil_s1 = .
gen _afil_s2 = .
gen _afil_s3 = .


* Serviço indicado em A1
replace _afil_s1 = 1 if a1 == 1


* Serviço indicado em A2
*
* A2=1 -> mesmo serviço de A1
* A2=2 -> serviço diferente

replace _afil_s2 = 1 if a2 == 1 & a1 == 1
replace _afil_s2 = 2 if a2 == 2


* Serviço indicado em A3
*
* A3=1 -> mesmo serviço de A1/A2
* A3=2 -> mesmo serviço de A1
* A3=3 -> mesmo serviço de A2
* A3=4 -> novo serviço


replace _afil_s3 = 1 if a3 == 1 & a1 == 1 & a2 == 1

replace _afil_s3 = 1 if ///
    a3 == 2 & a1 == 1

replace _afil_s3 = 1 if ///
    a3 == 3 & a2 == 1

replace _afil_s3 = 2 if ///
    a3 == 3 & a2 == 2

replace _afil_s3 = 3 if ///
    a3 == 4


/**************************************************************************
* Casos em que A1 não possui serviço
**************************************************************************/

* A1=0 e A2=1 é conceitualmente inconsistente:
* A2=1 significa "mesmo serviço de A1", mas A1 respondeu não.

replace afiliacao_inconsistente = 1 ///
    if a1 == 0 & a2 == 1

* A1=0 e A3=1/2 também são inconsistentes.
replace afiliacao_inconsistente = 1 ///
    if a1 == 0 & inlist(a3,1,2)

* A2=0 e A3=1/3 são inconsistentes.
replace afiliacao_inconsistente = 1 ///
    if a2 == 0 & inlist(a3,1,3)


/**************************************************************************
* Corrigir a representação dos serviços para situações em que
*     A1 ou A2 são NÃO.
**************************************************************************/

* Se A1=0 e A2=2:
* A2 é o primeiro serviço existente.
replace _afil_s2 = 1 if a1 == 0 & a2 == 2

* Se A1=0 e A2=0 e A3=4:
* A3 é o único serviço.
replace _afil_s3 = 1 if ///
    a1 == 0 & a2 == 0 & a3 == 4

* Se A1=0 e A2=2 e A3=3:
* A3 é o mesmo serviço de A2.
replace _afil_s3 = 1 if ///
    a1 == 0 & a2 == 2 & a3 == 3

* Se A1=0 e A2=2 e A3=4:
* A3 é um segundo serviço.
replace _afil_s3 = 2 if ///
    a1 == 0 & a2 == 2 & a3 == 4

* Se A1=1 e A2=0 e A3=2:
* A3 é o mesmo serviço de A1.
replace _afil_s3 = 1 if ///
    a1 == 1 & a2 == 0 & a3 == 2

* Se A1=1 e A2=0 e A3=4:
* A3 é serviço diferente de A1.
replace _afil_s3 = 2 if ///
    a1 == 1 & a2 == 0 & a3 == 4


/**************************************************************************
* CONTAR NÚMERO DE SERVIÇOS DISTINTOS
**************************************************************************/

capture drop afiliacao_ndist

gen byte afiliacao_ndist = .

* Nenhum serviço identificado
replace afiliacao_ndist = 0 if afiliacao_nsim == 0

* Apenas um SIM
replace afiliacao_ndist = 1 if afiliacao_nsim == 1

* Dois SIM:
* Se os dois serviços são iguais -> 1 serviço
* Se são diferentes -> 2 serviços

replace afiliacao_ndist = 1 if ///
    afiliacao_nsim == 2 & ///
    _afil_s1 == _afil_s2

replace afiliacao_ndist = 2 if ///
    afiliacao_nsim == 2 & ///
    _afil_s1 != _afil_s2

* Três SIM:
* Todos iguais -> 1 serviço
* Exatamente dois iguais -> 2 serviços
* Todos diferentes -> 3 serviços

replace afiliacao_ndist = 1 if ///
    afiliacao_nsim == 3 & ///
    _afil_s1 == _afil_s2 & ///
    _afil_s1 == _afil_s3

replace afiliacao_ndist = 2 if ///
    afiliacao_nsim == 3 & ///
    ( ///
        (_afil_s1 == _afil_s2 & _afil_s1 != _afil_s3) | ///
        (_afil_s1 == _afil_s3 & _afil_s1 != _afil_s2) | ///
        (_afil_s2 == _afil_s3 & _afil_s1 != _afil_s2) ///
    )

replace afiliacao_ndist = 3 if ///
    afiliacao_nsim == 3 & ///
    _afil_s1 != _afil_s2 & ///
    _afil_s1 != _afil_s3 & ///
    _afil_s2 != _afil_s3

label variable afiliacao_ndist ///
    "Número de serviços/dentistas distintos na afiliação"

tab afiliacao_ndist, missing



/**************************************************************************
* Escore de Afiliação
**************************************************************************/

* Todas NÃO
replace esc_afiliacao = 1 if ///
    a1==0 & a2==0 & a3==0


* Todos SIM e mesmo serviço
replace esc_afiliacao = 4 if ///
    a1==1 & a2==1 & a3==1


* Dois ou mais SIM e pelo menos dois pertencem ao mesmo serviço
replace esc_afiliacao = 3 if ///
    missing(esc_afiliacao) & ///
    afiliacao_nsim >= 2 & ///
    afiliacao_ndist < afiliacao_nsim


* SIM relacionados a serviços diferentes
replace esc_afiliacao = 2 if ///
    missing(esc_afiliacao) & ///
    afiliacao_nsim >= 1 & ///
    afiliacao_ndist == afiliacao_nsim


/**************************************************************************
* Afiliação missing
**************************************************************************/

replace esc_afiliacao = . ///
    if missing(a1) | missing(a2) | missing(a3)


label variable esc_afiliacao ///
    "Afiliação - escore 1 a 4"

label variable afiliacao_nsim ///
    "Afiliação - número de respostas SIM"

label variable afiliacao_ndist ///
    "Afiliação - número de serviços distintos"

label variable afiliacao_inconsistente ///
    "Afiliação - combinação potencialmente inconsistente"


/**************************************************************************
* Conferência da Afiliação
**************************************************************************/

tab esc_afiliacao, missing
tab afiliacao_nsim, missing
tab afiliacao_ndist, missing

list record_id a1 a2 a3 ///
    afiliacao_nsim afiliacao_ndist esc_afiliacao ///
    if afiliacao_inconsistente == 1


/**************************************************************************
* PROGRAMA AUXILIAR PARA PROCESSAR COMPONENTES
*
* O programa:
*
* 1. copia os itens originais para _sc;
* 2. inverte os itens especificados;
* 3. conta 9 + missing;
* 4. se >=50%, escore missing;
* 5. se <50%, transforma 9 em 2;
* 6. calcula média dos itens válidos.
**************************************************************************/

capture program drop pcat_component

program define pcat_component
    syntax, PREFIX(string) ITEMS(string) NITEMS(integer) ///
        [INVERT(string)]

    local prefix "`prefix'"
    local items "`items'"
    local invert "`invert'"

    *--------------------------------------------------------------
    * Criar cópias dos itens
    *--------------------------------------------------------------

    foreach v of local items {

        capture drop `prefix'_`v'_sc

        gen `prefix'_`v'_sc = `v'

        * Inversão
        if strpos(" `invert' ", " `v' ") > 0 {

            replace `prefix'_`v'_sc = ///
                5 - `v' ///
                if inrange(`v',1,4)

            replace `prefix'_`v'_sc = 9 ///
                if `v' == 9
        }
    }


    *--------------------------------------------------------------
    * Lista das variáveis recodificadas
    *--------------------------------------------------------------

    local sclist

    foreach v of local items {
        local sclist `sclist' `prefix'_`v'_sc
    }


    *--------------------------------------------------------------
    * Número de ausentes
    *
    * rowmiss conta missing, inclusive os 9 NÃO são missing.
    * Por isso precisamos transformar temporariamente 9 em missing
    * para contar 9 + missing.
    *--------------------------------------------------------------

    capture drop nmiss_`prefix'

    egen nmiss_`prefix' = rowmiss(`sclist')

    foreach v of local items {

        replace `prefix'_`v'_sc = . ///
            if `prefix'_`v'_sc == 9

    }

    * Agora rowmiss conta apenas missing, mas perdemos a distinção
    * dos 9. Por isso a contagem deve ser refeita usando as originais.
    *
    * Recriaremos a contagem de 9 + missing.

    drop nmiss_`prefix'

    gen nmiss_`prefix' = 0

    foreach v of local items {

        replace nmiss_`prefix' = ///
            nmiss_`prefix' + 1 ///
            if missing(`v') | `v' == 9
    }


    *--------------------------------------------------------------
    * Regra de 50%
    *
    * se ausentes >= 50%:
    * escore = missing
    *
    * se ausentes < 50%:
    * 9 -> 2
    *--------------------------------------------------------------

    local limite = `nitems'/2

    foreach v of local items {

        replace `prefix'_`v'_sc = 2 ///
            if `v' == 9 & nmiss_`prefix' < `limite'
    }


    *--------------------------------------------------------------
    * Média
    *--------------------------------------------------------------

    capture drop esc_`prefix'

    egen esc_`prefix' = rowmean(`sclist')


    *--------------------------------------------------------------
    * Se >=50% ausentes, escore = missing
    *--------------------------------------------------------------

    replace esc_`prefix' = . ///
        if nmiss_`prefix' >= `limite'


    label variable esc_`prefix' ///
        "Escore `prefix' - escala 1 a 4"

    label variable nmiss_`prefix' ///
        "Número de ausentes - componente `prefix'"

end


/**************************************************************************
* COMPONENTE B
*
* Acesso de Primeiro Contato - Utilização
*
* 3 itens
*
* >=1,5 ausentes = 50% ou mais
* portanto 2 ou 3 ausentes -> missing
**************************************************************************/

pcat_component, ///
    prefix(b) ///
    items("b1 b2 b3") ///
    nitems(3)


/**************************************************************************
* COMPONENTE C
*
* Acesso de Primeiro Contato - Acessibilidade
*
* 12 itens
*
* C9-C12 invertidos
*
* >=6 ausentes -> missing
**************************************************************************/

pcat_component, ///
    prefix(c) ///
    items("c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12") ///
    nitems(12) ///
    invert("c9 c10 c11 c12")


/**************************************************************************
* COMPONENTE D
*
* Longitudinalidade
*
* 15 itens
*
* D14-D15 invertidos
*
* >=7,5 ausentes -> missing
* Portanto 8 ou mais ausentes -> missing.
**************************************************************************/

pcat_component, ///
    prefix(d) ///
    items("d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15") ///
    nitems(15) ///
    invert("d14 d15")


/**************************************************************************
* COMPONENTE E
*
* Coordenação - Integração de Cuidados
*
* 9 itens:
*
* E1
* E3
* E4
* E5
* E6
* E7
* E8
* E9
* E10
*
* E2 NÃO participa.
*
* >=4,5 ausentes -> missing
* Portanto 5 ou mais ausentes -> missing.
**************************************************************************/

pcat_component, ///
    prefix(e) ///
    items("e1 e3 e4 e5 e6 e7 e8 e9 e10") ///
    nitems(9)


/**************************************************************************
* COMPONENTE F
*
* Coordenação - Sistemas de Informações
*
* 3 itens
**************************************************************************/

pcat_component, ///
    prefix(f) ///
    items("f1 f2 f3") ///
    nitems(3)


/**************************************************************************
* COMPONENTE G
*
* Integralidade - Serviços Disponíveis
*
* 23 itens
*
* 50% = 11,5
*
* 11 ausentes -> calcula
* 12 ou mais -> missing
**************************************************************************/

pcat_component, ///
    prefix(g) ///
    items("g1 g2 g3 g4 g5 g6 g7 g8 g9 g10 g11 g12 g13 g14 g15 g16 g17 g18 g19 g20 g21 g22 g23") ///
    nitems(23)


/**************************************************************************
* COMPONENTE H
*
* Integralidade - Serviços Prestados
*
* 9 itens
**************************************************************************/

pcat_component, ///
    prefix(h) ///
    items("h1 h2 h3 h4 h5 h6 h7 h8 h9") ///
    nitems(9)


/**************************************************************************
* COMPONENTE I
*
* Orientação Familiar
*
* 3 itens
**************************************************************************/

pcat_component, ///
    prefix(i) ///
    items("i1 i2 i3") ///
    nitems(3)


/**************************************************************************
* COMPONENTE J
*
* Orientação Comunitária
*
* 6 itens
*
* 50% = 3
*
* 3 ausentes ou mais -> missing
**************************************************************************/

pcat_component, ///
    prefix(j) ///
    items("j1 j2 j3 j4 j5 j6") ///
    nitems(6)


/**************************************************************************
* ESCORES DOS COMPONENTES
**************************************************************************/

display " "
display "=============================================================="
display " ESCORES DOS COMPONENTES - ESCALA ORIGINAL 1 A 4"
display "=============================================================="

summarize ///
    esc_afiliacao ///
    esc_b ///
    esc_c ///
    esc_d ///
    esc_e ///
    esc_f ///
    esc_g ///
    esc_h ///
    esc_i ///
    esc_j


/**************************************************************************
* TRANSFORMAÇÃO DOS COMPONENTES PARA 0-10
*
* Fórmula:
*
*     (escore - 1) / (4 - 1) * 10
*
*     = (escore - 1) / 3 * 10
**************************************************************************/

capture drop esc_afiliacao_10
capture drop esc_b_10
capture drop esc_c_10
capture drop esc_d_10
capture drop esc_e_10
capture drop esc_f_10
capture drop esc_g_10
capture drop esc_h_10
capture drop esc_i_10
capture drop esc_j_10

gen esc_afiliacao_10 = ///
    ((esc_afiliacao - 1) / 3) * 10 ///
    if !missing(esc_afiliacao)

gen esc_b_10 = ///
    ((esc_b - 1) / 3) * 10 ///
    if !missing(esc_b)

gen esc_c_10 = ///
    ((esc_c - 1) / 3) * 10 ///
    if !missing(esc_c)

gen esc_d_10 = ///
    ((esc_d - 1) / 3) * 10 ///
    if !missing(esc_d)

gen esc_e_10 = ///
    ((esc_e - 1) / 3) * 10 ///
    if !missing(esc_e)

gen esc_f_10 = ///
    ((esc_f - 1) / 3) * 10 ///
    if !missing(esc_f)

gen esc_g_10 = ///
    ((esc_g - 1) / 3) * 10 ///
    if !missing(esc_g)

gen esc_h_10 = ///
    ((esc_h - 1) / 3) * 10 ///
    if !missing(esc_h)

gen esc_i_10 = ///
    ((esc_i - 1) / 3) * 10 ///
    if !missing(esc_i)

gen esc_j_10 = ///
    ((esc_j - 1) / 3) * 10 ///
    if !missing(esc_j)


/**************************************************************************
* CLASSIFICAÇÃO DOS COMPONENTES
*
* 0 = Baixo
* 1 = Alto
**************************************************************************/

capture drop class_afiliacao
capture drop class_b
capture drop class_c
capture drop class_d
capture drop class_e
capture drop class_f
capture drop class_g
capture drop class_h
capture drop class_i
capture drop class_j


gen class_afiliacao = .
replace class_afiliacao = 0 ///
    if esc_afiliacao_10 < 6.6 & !missing(esc_afiliacao_10)
replace class_afiliacao = 1 ///
    if esc_afiliacao_10 >= 6.6 & !missing(esc_afiliacao_10)


gen class_b = .
replace class_b = 0 ///
    if esc_b_10 < 6.6 & !missing(esc_b_10)
replace class_b = 1 ///
    if esc_b_10 >= 6.6 & !missing(esc_b_10)


gen class_c = .
replace class_c = 0 ///
    if esc_c_10 < 6.6 & !missing(esc_c_10)
replace class_c = 1 ///
    if esc_c_10 >= 6.6 & !missing(esc_c_10)


gen class_d = .
replace class_d = 0 ///
    if esc_d_10 < 6.6 & !missing(esc_d_10)
replace class_d = 1 ///
    if esc_d_10 >= 6.6 & !missing(esc_d_10)


gen class_e = .
replace class_e = 0 ///
    if esc_e_10 < 6.6 & !missing(esc_e_10)
replace class_e = 1 ///
    if esc_e_10 >= 6.6 & !missing(esc_e_10)


gen class_f = .
replace class_f = 0 ///
    if esc_f_10 < 6.6 & !missing(esc_f_10)
replace class_f = 1 ///
    if esc_f_10 >= 6.6 & !missing(esc_f_10)


gen class_g = .
replace class_g = 0 ///
    if esc_g_10 < 6.6 & !missing(esc_g_10)
replace class_g = 1 ///
    if esc_g_10 >= 6.6 & !missing(esc_g_10)


gen class_h = .
replace class_h = 0 ///
    if esc_h_10 < 6.6 & !missing(esc_h_10)
replace class_h = 1 ///
    if esc_h_10 >= 6.6 & !missing(esc_h_10)


gen class_i = .
replace class_i = 0 ///
    if esc_i_10 < 6.6 & !missing(esc_i_10)
replace class_i = 1 ///
    if esc_i_10 >= 6.6 & !missing(esc_i_10)


gen class_j = .
replace class_j = 0 ///
    if esc_j_10 < 6.6 & !missing(esc_j_10)
replace class_j = 1 ///
    if esc_j_10 >= 6.6 & !missing(esc_j_10)


/**************************************************************************
* LABELS DAS CLASSIFICAÇÕES
**************************************************************************/

label define pcat_class ///
    0 "Baixo (<6,6)" ///
    1 "Alto (>=6,6)", replace

label values class_afiliacao pcat_class
label values class_b pcat_class
label values class_c pcat_class
label values class_d pcat_class
label values class_e pcat_class
label values class_f pcat_class
label values class_g pcat_class
label values class_h pcat_class
label values class_i pcat_class
label values class_j pcat_class


/**************************************************************************
* ESCORE ESSENCIAL
*
* Componentes essenciais:
*
* Afiliação
* B
* C
* D
* E
* F
* G
* H
*
* Total = 8 componentes
*
* Se 4 ou mais componentes forem missing:
*   Escore Essencial = missing
*
* Se 3 ou menos forem missing:
*   média dos componentes disponíveis
**************************************************************************/

capture drop nmiss_essencial
capture drop esc_essencial
capture drop esc_essencial_10
capture drop class_essencial

egen nmiss_essencial = rowmiss( ///
    esc_afiliacao ///
    esc_b ///
    esc_c ///
    esc_d ///
    esc_e ///
    esc_f ///
    esc_g ///
    esc_h ///
)


egen esc_essencial = rowmean( ///
    esc_afiliacao ///
    esc_b ///
    esc_c ///
    esc_d ///
    esc_e ///
    esc_f ///
    esc_g ///
    esc_h ///
)


replace esc_essencial = . ///
    if nmiss_essencial >= 4


/**************************************************************************
* ESCORE ESSENCIAL 0-10
**************************************************************************/

gen esc_essencial_10 = ///
    ((esc_essencial - 1) / 3) * 10 ///
    if !missing(esc_essencial)


/**************************************************************************
* CLASSIFICAÇÃO ESCORE ESSENCIAL
**************************************************************************/

gen class_essencial = .

replace class_essencial = 0 ///
    if esc_essencial_10 < 6.6 & !missing(esc_essencial_10)

replace class_essencial = 1 ///
    if esc_essencial_10 >= 6.6 & !missing(esc_essencial_10)

label values class_essencial pcat_class


/**************************************************************************
* ESCORE GERAL
*
* Componentes:
*
* Afiliação
* B
* C
* D
* E
* F
* G
* H
* I
* J
*
* Total = 10 componentes
*
* Se 5 ou mais componentes forem missing:
*   Escore Geral = missing
*
* Se 4 ou menos forem missing:
*   média dos componentes disponíveis
**************************************************************************/

capture drop nmiss_geral
capture drop esc_geral
capture drop esc_geral_10
capture drop class_geral

egen nmiss_geral = rowmiss( ///
    esc_afiliacao ///
    esc_b ///
    esc_c ///
    esc_d ///
    esc_e ///
    esc_f ///
    esc_g ///
    esc_h ///
    esc_i ///
    esc_j ///
)


egen esc_geral = rowmean( ///
    esc_afiliacao ///
    esc_b ///
    esc_c ///
    esc_d ///
    esc_e ///
    esc_f ///
    esc_g ///
    esc_h ///
    esc_i ///
    esc_j ///
)


replace esc_geral = . ///
    if nmiss_geral >= 5


/**************************************************************************
* ESCORE GERAL 0-10
**************************************************************************/

gen esc_geral_10 = ///
    ((esc_geral - 1) / 3) * 10 ///
    if !missing(esc_geral)


/**************************************************************************
* CLASSIFICAÇÃO ESCORE GERAL
**************************************************************************/

gen class_geral = .

replace class_geral = 0 ///
    if esc_geral_10 < 6.6 & !missing(esc_geral_10)

replace class_geral = 1 ///
    if esc_geral_10 >= 6.6 & !missing(esc_geral_10)

label values class_geral pcat_class


/**************************************************************************
* LABELS DOS ESCORES
**************************************************************************/

label variable esc_b ///
    "Acesso primeiro contato - utilização (1-4)"

label variable esc_c ///
    "Acesso primeiro contato - acessibilidade (1-4)"

label variable esc_d ///
    "Longitudinalidade (1-4)"

label variable esc_e ///
    "Coordenação - integração de cuidados (1-4)"

label variable esc_f ///
    "Coordenação - sistemas de informações (1-4)"

label variable esc_g ///
    "Integralidade - serviços disponíveis (1-4)"

label variable esc_h ///
    "Integralidade - serviços prestados (1-4)"

label variable esc_i ///
    "Orientação familiar (1-4)"

label variable esc_j ///
    "Orientação comunitária (1-4)"

label variable esc_b_10 ///
    "Acesso utilização (0-10)"

label variable esc_c_10 ///
    "Acesso acessibilidade (0-10)"

label variable esc_d_10 ///
    "Longitudinalidade (0-10)"

label variable esc_e_10 ///
    "Coordenação integração (0-10)"

label variable esc_f_10 ///
    "Coordenação informação (0-10)"

label variable esc_g_10 ///
    "Integralidade serviços disponíveis (0-10)"

label variable esc_h_10 ///
    "Integralidade serviços prestados (0-10)"

label variable esc_i_10 ///
    "Orientação familiar (0-10)"

label variable esc_j_10 ///
    "Orientação comunitária (0-10)"

label variable esc_essencial ///
    "Escore Essencial APS Saúde Bucal (1-4)"

label variable esc_essencial_10 ///
    "Escore Essencial APS Saúde Bucal (0-10)"

label variable esc_geral ///
    "Escore Geral APS Saúde Bucal (1-4)"

label variable esc_geral_10 ///
    "Escore Geral APS Saúde Bucal (0-10)"

label variable nmiss_essencial ///
    "Número de componentes essenciais missing"

label variable nmiss_geral ///
    "Número de componentes gerais missing"


/**************************************************************************
* TABELA DESCRITIVA - ESCORES 1-4
**************************************************************************/

display " "
display "=============================================================="
display " ESTATÍSTICAS DESCRITIVAS - ESCALA 1 A 4"
display "=============================================================="

tabstat ///
    esc_afiliacao ///
    esc_b ///
    esc_c ///
    esc_d ///
    esc_e ///
    esc_f ///
    esc_g ///
    esc_h ///
    esc_i ///
    esc_j ///
    esc_essencial ///
    esc_geral, ///
    statistics(n mean sd p50 min max) ///
    columns(statistics)


/**************************************************************************
* TABELA DESCRITIVA - ESCORES 0-10
**************************************************************************/

display " "
display "=============================================================="
display " ESTATÍSTICAS DESCRITIVAS - ESCALA 0 A 10"
display "=============================================================="

tabstat ///
    esc_afiliacao_10 ///
    esc_b_10 ///
    esc_c_10 ///
    esc_d_10 ///
    esc_e_10 ///
    esc_f_10 ///
    esc_g_10 ///
    esc_h_10 ///
    esc_i_10 ///
    esc_j_10 ///
    esc_essencial_10 ///
    esc_geral_10, ///
    statistics(n mean sd p50 min max) ///
    columns(statistics)


/**************************************************************************
* DISTRIBUIÇÃO DOS COMPONENTES
**************************************************************************/

display " "
display "=============================================================="
display " CLASSIFICAÇÃO DOS COMPONENTES"
display "=============================================================="

tab class_afiliacao, missing
tab class_b, missing
tab class_c, missing
tab class_d, missing
tab class_e, missing
tab class_f, missing
tab class_g, missing
tab class_h, missing
tab class_i, missing
tab class_j, missing


/**************************************************************************
* CLASSIFICAÇÃO DO ESCORE ESSENCIAL
**************************************************************************/

display " "
display "=============================================================="
display " CLASSIFICAÇÃO - ESCORE ESSENCIAL"
display "=============================================================="

tab class_essencial, missing


/**************************************************************************
* CLASSIFICAÇÃO DO ESCORE GERAL
**************************************************************************/

display " "
display "=============================================================="
display " CLASSIFICAÇÃO - ESCORE GERAL"
display "=============================================================="

tab class_geral, missing


/**************************************************************************
* QUANTIDADE DE COMPONENTES MISSING
**************************************************************************/

display " "
display "=============================================================="
display " COMPONENTES MISSING"
display "=============================================================="

tab nmiss_essencial, missing
tab nmiss_geral, missing


/**************************************************************************
* PERCENTUAL DE RESPONDENTES COM ESCORE CALCULADO
**************************************************************************/

display " "
display "=============================================================="
display " COMPLETUDE DOS ESCORES"
display "=============================================================="

foreach v in ///
    esc_afiliacao ///
    esc_b ///
    esc_c ///
    esc_d ///
    esc_e ///
    esc_f ///
    esc_g ///
    esc_h ///
    esc_i ///
    esc_j ///
    esc_essencial ///
    esc_geral {

    quietly count if !missing(`v')
    local n_ok = r(N)

    quietly count
    local n_total = r(N)

    local perc = 100 * `n_ok' / `n_total'

    display "`v' : " ///
        %8.2f `n_ok' " de " `n_total' ///
        " (" %6.2f `perc' "%)"
}


/**************************************************************************
* VERIFICAÇÃO DA FAIXA DOS ESCORES
*
* Todos os componentes devem ficar entre 1 e 4.
**************************************************************************/

display " "
display "=============================================================="
display " VERIFICAÇÃO DA FAIXA DOS ESCORES"
display "=============================================================="

foreach v in ///
    esc_afiliacao ///
    esc_b ///
    esc_c ///
    esc_d ///
    esc_e ///
    esc_f ///
    esc_g ///
    esc_h ///
    esc_i ///
    esc_j ///
    esc_essencial ///
    esc_geral {

    count if (`v' < 1 | `v' > 4) & !missing(`v')

    if r(N)>0 {
        display as error ///
            "ERRO: `v' possui valores fora de 1-4."
        list record_id `v' ///
            if (`v' < 1 | `v' > 4) & !missing(`v')
    }
}


/**************************************************************************
* VERIFICAÇÃO DA FAIXA 0-10
**************************************************************************/

foreach v in ///
    esc_afiliacao_10 ///
    esc_b_10 ///
    esc_c_10 ///
    esc_d_10 ///
    esc_e_10 ///
    esc_f_10 ///
    esc_g_10 ///
    esc_h_10 ///
    esc_i_10 ///
    esc_j_10 ///
    esc_essencial_10 ///
    esc_geral_10 {

    count if (`v' < 0 | `v' > 10) & !missing(`v')

    if r(N)>0 {
        display as error ///
            "ERRO: `v' possui valores fora de 0-10."
    }
}

/**************************************************************************
* TABELA FINAL DOS ESCORES
**************************************************************************/

display " "
display "=============================================================="
display " TABELA FINAL - PCATOOL"
display "=============================================================="

tabstat ///
    esc_afiliacao_10 ///
    esc_b_10 ///
    esc_c_10 ///
    esc_d_10 ///
    esc_e_10 ///
    esc_f_10 ///
    esc_g_10 ///
    esc_h_10 ///
    esc_i_10 ///
    esc_j_10 ///
    esc_essencial_10 ///
    esc_geral_10, ///
    statistics(n mean sd min max) ///
    columns(statistics)


/**************************************************************************
* EXPORTAÇÃO DOS DADOS COM ESCORES
*
* O banco original permanece preservado durante todo o cálculo.
**************************************************************************/

save ///
    "pcatool_bucal_adulto_com_escores.dta", ///
    replace


/**************************************************************************
* RELATÓRIO FINAL NO LOG
**************************************************************************/

display " "
display "=============================================================="
display " ANÁLISE FINALIZADA"
display "=============================================================="
display " "
display "Variáveis principais geradas:"
display " "
display "esc_afiliacao      = Afiliação 1-4"
display "esc_afiliacao_10  = Afiliação 0-10"
display "esc_b              = Utilização 1-4"
display "esc_c              = Acessibilidade 1-4"
display "esc_d              = Longitudinalidade 1-4"
display "esc_e              = Coordenação/Integração 1-4"
display "esc_f              = Coordenação/Informação 1-4"
display "esc_g              = Integralidade/Disponíveis 1-4"
display "esc_h              = Integralidade/Prestados 1-4"
display "esc_i              = Orientação Familiar 1-4"
display "esc_j              = Orientação Comunitária 1-4"
display "esc_essencial      = Escore Essencial 1-4"
display "esc_essencial_10   = Escore Essencial 0-10"
display "esc_geral          = Escore Geral 1-4"
display "esc_geral_10       = Escore Geral 0-10"
display " "
display "Classificações:"
display "0 = Baixo (<6,6)"
display "1 = Alto (>=6,6)"
display " "
display "Banco salvo como:"
display "pcatool_bucal_adulto_com_escores.dta"
display " "
display "=============================================================="
display " FIM"
display "=============================================================="

**************************************************************************
* FIM DO DO-FILE
**************************************************************************
