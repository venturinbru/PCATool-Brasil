/**************************************************************************
***************************************************************************
              PCATOOL-BRASIL – PACIENTES CRIANÇAS
                    VERSÃO REDUZIDA

                  ANÁLISE E CÁLCULO DOS ESCORES

***************************************************************************

Este do-file:

1. Mantém as variáveis originais intactas;
2. Remove automaticamente variáveis derivadas de execuções anteriores;
3. Calcula o grau de afiliação;
4. Inverte o item C4;
5. Identifica missing e respostas 9;
6. Aplica a regra de 50%;
7. Imputa 9 para 2 quando o escore é calculável;
8. Calcula o Escore Geral da APS de 1 a 4;
9. Transforma o Escore Geral para 0 a 10;
10. Calcula os escores individuais dos itens em 0 a 10;
11. Faz verificações de consistência;
12. Salva o banco final.

***************************************************************************

ESCORE GERAL:

(Afiliação + B1 + B2 + C1 + C3 + C4 +
 D1 + D2 + D6 + D8 + D11 +
 E4 + E5 + E6 + F2 +
 G3 + G4 + G5 + G6 + G9 +
 H3 + H4 + H5 +
 I1 + I2 + I3 +
 J2 + J4) / 28

TOTAL = 28 COMPONENTES

E1 NÃO entra no cálculo.

***************************************************************************

INVERSÃO DO C4:

4 -> 1
3 -> 2
2 -> 3
1 -> 4

***************************************************************************

MISSING:

9 = Não sei / Não lembro
missing = sem resposta

Se >= 50% dos 28 componentes forem ausentes:

    Escore Geral = missing

Como 50% de 28 = 14:

    0 a 13 ausentes = calculável
    14 a 28 ausentes = não calculável

Quando calculável:

    9 -> 2

***************************************************************************

TRANSFORMAÇÃO 0-10:

((Escore - 1) / (4 - 1)) * 10

ou

((Escore - 1) / 3) * 10

**************************************************************************/

** version 17.0
** clear all
** set more off


/**************************************************************************
***************************************************************************
  ABRIR O BANCO DE DADOS
***************************************************************************
**************************************************************************/

* >>> ALTERE SOMENTE ESTA LINHA PARA O CAMINHO DO SEU BANCO <<<

** use "seu_banco.dta", clear


/**************************************************************************
***************************************************************************
  LIMPAR VARIÁVEIS DERIVADAS DE EXECUÇÕES ANTERIORES
***************************************************************************
**************************************************************************/

/*
  IMPORTANTE:

  Este bloco NÃO exclui nenhuma variável original do questionário.

  Ele exclui somente variáveis que este do-file cria.

  Assim, o código pode ser executado novamente sem gerar:

      variable already defined
      r(110)
*/


/* Variáveis utilizadas na afiliação */

capture drop serv1
capture drop serv2
capture drop serv3
capture drop n_sim_afil
capture drop n_servicos_afil
capture drop afiliacao
capture drop afiliacao_0a10


/* Variáveis de controle de missing */

capture drop n_missing_pcat
capture drop perc_missing_pcat
capture drop pcat_calculavel


/* Variáveis do escore geral */

capture drop soma_pcat
capture drop escore_geral_1a4
capture drop escore_geral_0a10


/* Variáveis de controle da imputação */

capture drop n_nove_orig
capture drop houve_imputacao


/* Variável de inconsistência */

capture drop inconsistencia_afil


/* Variáveis derivadas dos 27 itens */

foreach v in ///
    b1 b2 ///
    c1 c3 c4 ///
    d1 d2 d6 d8 d11 ///
    e4 e5 e6 ///
    f2 ///
    g3 g4 g5 g6 g9 ///
    h3 h4 h5 ///
    i1 i2 i3 ///
    j2 j4 {

    capture drop `v'_pcat
    capture drop `v'_pcat_0a10
}


/**************************************************************************
***************************************************************************
  CONFERIR AS VARIÁVEIS ORIGINAIS
***************************************************************************
**************************************************************************/

/* Variáveis de afiliação */

local vars_afil ///
    a1 a2 a3


/* 27 itens utilizados no escore */

local vars_itens ///
    b1 b2 ///
    c1 c3 c4 ///
    d1 d2 d6 d8 d11 ///
    e4 e5 e6 ///
    f2 ///
    g3 g4 g5 g6 g9 ///
    h3 h4 h5 ///
    i1 i2 i3 ///
    j2 j4


/* Conferir A1-A3 */

foreach v of local vars_afil {

    capture confirm variable `v'

    if _rc {

        di as error "ERRO: a variável `v' não foi encontrada."

        exit 111
    }
}


/* Conferir os 27 itens */

foreach v of local vars_itens {

    capture confirm variable `v'

    if _rc {

        di as error "ERRO: a variável `v' não foi encontrada."

        exit 111
    }
}


di as result ""
di as result "Todas as variáveis necessárias foram encontradas."


/**************************************************************************
***************************************************************************
  CONFERIR NÚMERO DE ITENS
***************************************************************************
**************************************************************************/

local n_itens : word count `vars_itens'

di as text ""
di as text "Número de itens B-J = `n_itens'"

assert `n_itens' == 27


/**************************************************************************
***************************************************************************
  PARTE I
  CÁLCULO DO GRAU DE AFILIAÇÃO
***************************************************************************
**************************************************************************/

/**************************************************************************
  ESTRUTURA DE A1, A2 E A3

  A1:

      0 = Não
      1 = Sim


  A2:

      0 = Não
      1 = Sim, mesmo serviço referido em A1
      2 = Sim, serviço diferente de A1


  A3:

      0 = Não
      1 = Sim, mesmo serviço de A1 e A2
      2 = Sim, mesmo serviço de A1
      3 = Sim, mesmo serviço de A2
      4 = Sim, serviço diferente de A1 e A2

**************************************************************************/


/**************************************************************************
  IDENTIFICADOR DO SERVIÇO DE A1
**************************************************************************/

gen byte serv1 = .

replace serv1 = 0 if a1 == 0
replace serv1 = 1 if a1 == 1

label variable serv1 ///
    "Identificador do serviço indicado em A1"


/**************************************************************************
  IDENTIFICADOR DO SERVIÇO DE A2
**************************************************************************/

gen byte serv2 = .

replace serv2 = 0 if a2 == 0
replace serv2 = 1 if a2 == 1
replace serv2 = 2 if a2 == 2

label variable serv2 ///
    "Identificador do serviço indicado em A2"


/**************************************************************************
 IDENTIFICADOR DO SERVIÇO DE A3

  A3 = 0:
      Não há serviço

  A3 = 1:
      Mesmo serviço de A1 e A2

  A3 = 2:
      Mesmo serviço de A1

  A3 = 3:
      Mesmo serviço de A2

  A3 = 4:
      Serviço diferente dos anteriores
**************************************************************************/

gen byte serv3 = .

/* A3 = 0: nenhum serviço */

replace serv3 = 0 ///
    if a3 == 0


/* A3 = 1: mesmo serviço de A1 e A2 */

replace serv3 = 1 ///
    if a3 == 1 ///
    & a1 == 1 ///
    & a2 == 1


/* A3 = 2: mesmo serviço de A1 */

replace serv3 = 1 ///
    if a3 == 2 ///
    & a1 == 1


/* A3 = 3: mesmo serviço de A2 */

replace serv3 = serv2 ///
    if a3 == 3 ///
    & a2 > 0


/* A3 = 4: novo serviço */

replace serv3 = 3 ///
    if a3 == 4


label variable serv3 ///
    "Identificador do serviço indicado em A3"


/**************************************************************************
  NÚMERO DE RESPOSTAS SIM
**************************************************************************/

gen byte n_sim_afil = .

replace n_sim_afil = ///
    (a1 == 1) + ///
    (a2 > 0) + ///
    (a3 > 0) ///
    if !missing(a1,a2,a3)

label variable n_sim_afil ///
    "Número de respostas SIM em A1-A3"


/**************************************************************************
  NÚMERO DE SERVIÇOS DIFERENTES
**************************************************************************/

gen byte n_servicos_afil = .


/* Nenhum serviço */

replace n_servicos_afil = 0 ///
    if serv1 == 0 ///
    & serv2 == 0 ///
    & serv3 == 0


/* Primeiro serviço */

replace n_servicos_afil = 1 ///
    if serv1 > 0


/* Segundo serviço diferente */

replace n_servicos_afil = n_servicos_afil + 1 ///
    if serv2 > 0 ///
    & serv2 != serv1 ///
    & !missing(n_servicos_afil)


/* Terceiro serviço diferente */

replace n_servicos_afil = n_servicos_afil + 1 ///
    if serv3 > 0 ///
    & serv3 != serv1 ///
    & serv3 != serv2 ///
    & !missing(n_servicos_afil)


label variable n_servicos_afil ///
    "Número de serviços diferentes em A1-A3"


/**************************************************************************
  CÁLCULO DO GRAU DE AFILIAÇÃO
**************************************************************************/

gen byte afiliacao = .


/*
  GRAU 1

  Todas as respostas são NÃO.
*/

replace afiliacao = 1 ///
    if a1 == 0 ///
    & a2 == 0 ///
    & a3 == 0


/*
  GRAU 4

  Todas são SIM e todas se referem ao mesmo serviço.
*/

replace afiliacao = 4 ///
    if n_sim_afil == 3 ///
    & n_servicos_afil == 1


/*
  GRAU 3

  Duas ou mais respostas são SIM
  e estão relacionadas ao mesmo serviço.
*/

replace afiliacao = 3 ///
    if n_sim_afil >= 2 ///
    & n_servicos_afil == 1 ///
    & missing(afiliacao)


/*
  GRAU 2

  Há pelo menos uma resposta SIM,
  mas não existe um único serviço comum.

  Também inclui situações com somente uma resposta SIM.
*/

replace afiliacao = 2 ///
    if n_sim_afil >= 1 ///
    & missing(afiliacao)


/*
  Caso haja missing em A1, A2 ou A3,
  não é possível determinar a afiliação.
*/

replace afiliacao = . ///
    if missing(a1) ///
    | missing(a2) ///
    | missing(a3)


/**************************************************************************
  RÓTULOS DA AFILIAÇÃO
**************************************************************************/

label variable afiliacao ///
    "Grau de afiliação PCATool"


capture label drop lab_afiliacao

label define lab_afiliacao ///
    1 "1 - Todas NÃO" ///
    2 "2 - Serviços diferentes" ///
    3 "3 - Dois ou mais SIM mesmo serviço" ///
    4 "4 - Todos SIM mesmo serviço"

label values afiliacao lab_afiliacao


/**************************************************************************
  VERIFICAR AFILIAÇÃO
**************************************************************************/

di ""
di "=============================================================="
di "DISTRIBUIÇÃO DA AFILIAÇÃO"
di "=============================================================="

tab a1, missing
tab a2, missing
tab a3, missing

tab afiliacao, missing

tab n_sim_afil, missing
tab n_servicos_afil, missing


/**************************************************************************
***************************************************************************
  PARTE II
  CRIAÇÃO DAS CÓPIAS DOS 27 ITENS
***************************************************************************
**************************************************************************/

/**************************************************************************
  CRIAR VARIÁVEIS *_pcat
**************************************************************************/

foreach v of local vars_itens {

    gen double `v'_pcat = `v'

    label variable `v'_pcat ///
        "`v' - variável utilizada no cálculo PCATool"
}


/**************************************************************************
***************************************************************************
  PARTE III
  INVERSÃO DO ITEM C4
***************************************************************************
**************************************************************************/

/**************************************************************************
  INVERTER C4

  Original:

      4 = Com certeza sim
      3 = Provavelmente sim
      2 = Provavelmente não
      1 = Com certeza não
      9 = Não sei / Não lembro


  Invertido:

      4 -> 1
      3 -> 2
      2 -> 3
      1 -> 4

  O 9 permanece 9 até a etapa de imputação.
**************************************************************************/

replace c4_pcat = 1 if c4 == 4
replace c4_pcat = 2 if c4 == 3
replace c4_pcat = 3 if c4 == 2
replace c4_pcat = 4 if c4 == 1

replace c4_pcat = 9 if c4 == 9

replace c4_pcat = . if missing(c4)


label variable c4_pcat ///
    "C4 - escala invertida"


/**************************************************************************
  CONFERIR A INVERSÃO DO C4
**************************************************************************/

di ""
di "=============================================================="
di "VERIFICAÇÃO DO C4"
di "=============================================================="

tab c4 c4_pcat, missing


/**************************************************************************
***************************************************************************
  PARTE IV
  DEFINIÇÃO DOS 28 COMPONENTES
***************************************************************************
**************************************************************************/

/**************************************************************************
  LISTA DOS 28 COMPONENTES
**************************************************************************/

local componentes ///
    afiliacao ///
    b1_pcat b2_pcat ///
    c1_pcat c3_pcat c4_pcat ///
    d1_pcat d2_pcat d6_pcat d8_pcat d11_pcat ///
    e4_pcat e5_pcat e6_pcat ///
    f2_pcat ///
    g3_pcat g4_pcat g5_pcat g6_pcat g9_pcat ///
    h3_pcat h4_pcat h5_pcat ///
    i1_pcat i2_pcat i3_pcat ///
    j2_pcat j4_pcat


local n_componentes : word count `componentes'

di ""
di as text "Número de componentes do Escore Geral = `n_componentes'"

assert `n_componentes' == 28


/**************************************************************************
***************************************************************************
  PARTE V
  MISSING E VALORES 9
***************************************************************************
**************************************************************************/

/**************************************************************************
  CONTAGEM DE COMPONENTES AUSENTES

  São considerados ausentes:

      - missing
      - valor 9

  São contados os 28 componentes.
**************************************************************************/

gen byte n_missing_pcat = 0

foreach v of local componentes {

    replace n_missing_pcat = ///
        n_missing_pcat + 1 ///
        if missing(`v') | `v' == 9
}


label variable n_missing_pcat ///
    "Número de componentes ausentes ou 9"


/**************************************************************************
  PERCENTUAL DE AUSENTES
**************************************************************************/

gen double perc_missing_pcat = ///
    (n_missing_pcat / 28) * 100


label variable perc_missing_pcat ///
    "Percentual de componentes ausentes"


/**************************************************************************
  ESCORE CALCULÁVEL

  < 50%:
      calculável

  >= 50%:
      não calculável

  50% de 28 = 14

  Portanto:

      0-13 = calculável
      14-28 = não calculável
**************************************************************************/

gen byte pcat_calculavel = .

replace pcat_calculavel = 1 ///
    if n_missing_pcat < 14

replace pcat_calculavel = 0 ///
    if n_missing_pcat >= 14


label variable pcat_calculavel ///
    "Escore calculável (<50% de ausentes)"


capture label drop lab_calculavel

label define lab_calculavel ///
    0 "Não calculável - >=50% ausentes" ///
    1 "Calculável - <50% ausentes"

label values pcat_calculavel lab_calculavel


/**************************************************************************
  VERIFICAR MISSING
**************************************************************************/

di ""
di "=============================================================="
di "MISSING DO PCATOOL"
di "=============================================================="

tab n_missing_pcat, missing

tab pcat_calculavel, missing

summ n_missing_pcat perc_missing_pcat, detail


/**************************************************************************
***************************************************************************
  PARTE VI
  IMPUTAÇÃO DE 9 PARA 2
***************************************************************************
**************************************************************************/

/**************************************************************************
  IMPUTAÇÃO

  Somente para participantes com:

      pcat_calculavel = 1

  Ou seja:

      menos de 50% de componentes ausentes.

  Regra:

      9 -> 2
**************************************************************************/

foreach v of local componentes {

    replace `v' = 2 ///
        if `v' == 9 ///
        & pcat_calculavel == 1
}


/**************************************************************************
***************************************************************************
  PARTE VII
  ESCORE GERAL DA APS – 1 A 4
***************************************************************************
**************************************************************************/

/**************************************************************************
  SOMA DOS 28 COMPONENTES
**************************************************************************/

egen double soma_pcat = ///
    rowtotal(`componentes') ///
    if pcat_calculavel == 1


label variable soma_pcat ///
    "Soma dos 28 componentes do PCATool"


/**************************************************************************
  ESCORE GERAL – ESCALA 1 A 4
**************************************************************************/

gen double escore_geral_1a4 = ///
    soma_pcat / 28 ///
    if pcat_calculavel == 1


label variable escore_geral_1a4 ///
    "Escore Geral da APS - escala 1 a 4"


/**************************************************************************
***************************************************************************
  PARTE VIII
  ESCORE GERAL – 0 A 10
***************************************************************************
**************************************************************************/

/**************************************************************************
  TRANSFORMAÇÃO PARA 0-10

      ((Escore - 1) / 3) * 10
**************************************************************************/

gen double escore_geral_0a10 = ///
    ((escore_geral_1a4 - 1) / 3) * 10 ///
    if !missing(escore_geral_1a4)


label variable escore_geral_0a10 ///
    "Escore Geral da APS - escala 0 a 10"


/**************************************************************************
***************************************************************************
  PARTE IX
  ESCORES INDIVIDUAIS DOS COMPONENTES – 0 A 10
***************************************************************************
**************************************************************************/

/**************************************************************************
  TRANSFORMAÇÃO DOS 28 COMPONENTES PARA 0-10
**************************************************************************/

foreach v of local componentes {

    gen double `v'_0a10 = ///
        ((`v' - 1) / 3) * 10 ///
        if pcat_calculavel == 1

}


/**************************************************************************
  ESCORE DE AFILIAÇÃO 0-10

  IMPORTANTE:

  capture drop foi colocado antes do gen.

  Isso impede o erro:

      variable afiliacao_0a10 already defined
      r(110)
**************************************************************************/

capture drop afiliacao_0a10

gen double afiliacao_0a10 = ///
    ((afiliacao - 1) / 3) * 10 ///
    if pcat_calculavel == 1


label variable afiliacao_0a10 ///
    "Grau de afiliação - escala 0 a 10"


/**************************************************************************
***************************************************************************
  PARTE X
  LISTA DOS COMPONENTES EM 0-10
***************************************************************************
**************************************************************************/

local componentes_010 ///
    afiliacao_0a10 ///
    b1_pcat_0a10 b2_pcat_0a10 ///
    c1_pcat_0a10 c3_pcat_0a10 c4_pcat_0a10 ///
    d1_pcat_0a10 d2_pcat_0a10 d6_pcat_0a10 ///
    d8_pcat_0a10 d11_pcat_0a10 ///
    e4_pcat_0a10 e5_pcat_0a10 e6_pcat_0a10 ///
    f2_pcat_0a10 ///
    g3_pcat_0a10 g4_pcat_0a10 g5_pcat_0a10 ///
    g6_pcat_0a10 g9_pcat_0a10 ///
    h3_pcat_0a10 h4_pcat_0a10 h5_pcat_0a10 ///
    i1_pcat_0a10 i2_pcat_0a10 i3_pcat_0a10 ///
    j2_pcat_0a10 j4_pcat_0a10


/**************************************************************************
***************************************************************************
  PARTE XI
  ESTATÍSTICAS DESCRITIVAS
***************************************************************************
**************************************************************************/

/**************************************************************************
  ESCORE GERAL 1-4
**************************************************************************/

di ""
di "=============================================================="
di "ESCORE GERAL DA APS – ESCALA 1 A 4"
di "=============================================================="

summ escore_geral_1a4, detail


/**************************************************************************
  ESCORE GERAL 0-10
**************************************************************************/

di ""
di "=============================================================="
di "ESCORE GERAL DA APS – ESCALA 0 A 10"
di "=============================================================="

summ escore_geral_0a10, detail


/**************************************************************************
  ESTATÍSTICAS PRINCIPAIS
**************************************************************************/

tabstat ///
    escore_geral_1a4 ///
    escore_geral_0a10, ///
    statistics(n mean sd median p25 p75 min max) ///
    columns(statistics)


/**************************************************************************
  DISTRIBUIÇÃO DA AFILIAÇÃO
**************************************************************************/

di ""
di "=============================================================="
di "GRAU DE AFILIAÇÃO"
di "=============================================================="

tab afiliacao, missing

tabstat ///
    afiliacao ///
    afiliacao_0a10, ///
    statistics(n mean sd median min max) ///
    columns(statistics)


/**************************************************************************
***************************************************************************
  PARTE XII
  ESTATÍSTICAS DOS 28 COMPONENTES
***************************************************************************
**************************************************************************/

/**************************************************************************
  COMPONENTES – ESCALA ORIGINAL
**************************************************************************/

di ""
di "=============================================================="
di "ESTATÍSTICAS DOS 28 COMPONENTES – ESCALA 1 A 4"
di "=============================================================="

tabstat `componentes', ///
    statistics(n mean sd median p25 p75 min max) ///
    columns(statistics)


/**************************************************************************
  COMPONENTES – ESCALA 0 A 10
**************************************************************************/

di ""
di "=============================================================="
di "ESTATÍSTICAS DOS COMPONENTES – ESCALA 0 A 10"
di "=============================================================="

tabstat `componentes_010', ///
    statistics(n mean sd median p25 p75 min max) ///
    columns(statistics)


/**************************************************************************
***************************************************************************
  PARTE XIII
  CONTROLE DA IMPUTAÇÃO
***************************************************************************
**************************************************************************/

/**************************************************************************
  CONTAGEM DOS VALORES 9 ORIGINAIS

  Aqui utilizamos as variáveis originais.

  Isso permite saber quantos "Não sei/Não lembro"
  cada entrevistado apresentou antes da imputação.
**************************************************************************/

gen byte n_nove_orig = 0

foreach v of local vars_itens {

    replace n_nove_orig = ///
        n_nove_orig + 1 ///
        if `v' == 9

}


label variable n_nove_orig ///
    "Número de respostas 9 originais nos 27 itens"


/**************************************************************************
  INDICADOR DE IMPUTAÇÃO
**************************************************************************/

gen byte houve_imputacao = 0

replace houve_imputacao = 1 ///
    if n_nove_orig > 0 ///
    & pcat_calculavel == 1


label variable houve_imputacao ///
    "Houve imputação de 9 para 2"


capture label drop lab_imputacao

label define lab_imputacao ///
    0 "Não" ///
    1 "Sim"

label values houve_imputacao lab_imputacao


tab houve_imputacao, missing


/**************************************************************************
***************************************************************************
  PARTE XIV
  VERIFICAÇÃO DE VALORES INVÁLIDOS
***************************************************************************
**************************************************************************/

/**************************************************************************
  VALORES VÁLIDOS NOS 27 ITENS

  Esperados:

      1
      2
      3
      4
      9
      missing
**************************************************************************/

foreach v of local vars_itens {

    count if ///
        !inlist(`v',1,2,3,4,9) ///
        & !missing(`v')

    if r(N) > 0 {

        di as error ///
            "ATENÇÃO: `v' possui " r(N) ///
            " valor(es) inválido(s)."

    }
}


/**************************************************************************
  VALORES VÁLIDOS EM A1
**************************************************************************/

count if ///
    !inlist(a1,0,1) ///
    & !missing(a1)

if r(N) > 0 {

    di as error ///
        "ATENÇÃO: A1 possui valores inválidos."

}


/**************************************************************************
  VALORES VÁLIDOS EM A2
**************************************************************************/

count if ///
    !inlist(a2,0,1,2) ///
    & !missing(a2)

if r(N) > 0 {

    di as error ///
        "ATENÇÃO: A2 possui valores inválidos."

}


/**************************************************************************
  VALORES VÁLIDOS EM A3
**************************************************************************/

count if ///
    !inlist(a3,0,1,2,3,4) ///
    & !missing(a3)

if r(N) > 0 {

    di as error ///
        "ATENÇÃO: A3 possui valores inválidos."

}


/**************************************************************************
***************************************************************************
  PARTE XV
  CONSISTÊNCIA LÓGICA DA AFILIAÇÃO
***************************************************************************
**************************************************************************/

/**************************************************************************
  CRIAR INDICADOR DE INCONSISTÊNCIA
**************************************************************************/

gen byte inconsistencia_afil = 0


/**************************************************************************
  A2 = 1

  "Mesmo serviço referido em A1"

  Portanto A1 deveria ser SIM.
**************************************************************************/

replace inconsistencia_afil = 1 ///
    if a2 == 1 ///
    & a1 != 1


/**************************************************************************
  A3 = 1

  "Mesmo serviço de A1 e A2"

  Portanto A1 e A2 deveriam ser SIM.
**************************************************************************/

replace inconsistencia_afil = 1 ///
    if a3 == 1 ///
    & !(a1 == 1 & a2 == 1)


/**************************************************************************
  A3 = 2

  "Mesmo serviço de A1"

  Portanto A1 deveria ser SIM.
**************************************************************************/

replace inconsistencia_afil = 1 ///
    if a3 == 2 ///
    & a1 != 1


/**************************************************************************
  A3 = 3

  "Mesmo serviço de A2"

  Portanto A2 deveria ser SIM.
**************************************************************************/

replace inconsistencia_afil = 1 ///
    if a3 == 3 ///
    & a2 == 0


label variable inconsistencia_afil ///
    "Possível inconsistência lógica A1-A3"


capture label drop lab_inconsistencia

label define lab_inconsistencia ///
    0 "Sem inconsistência identificada" ///
    1 "Possível inconsistência"

label values inconsistencia_afil lab_inconsistencia


tab inconsistencia_afil, missing


/**************************************************************************
***************************************************************************
  PARTE XVI
  RELATÓRIO FINAL
***************************************************************************
**************************************************************************/

/**************************************************************************
  NÚMERO TOTAL DE ENTREVISTADOS
**************************************************************************/

count

local N_total = r(N)


/**************************************************************************
  NÚMERO CALCULÁVEL
**************************************************************************/

count if pcat_calculavel == 1

local N_calculavel = r(N)


/**************************************************************************
  NÚMERO NÃO CALCULÁVEL
**************************************************************************/

count if pcat_calculavel == 0

local N_nao_calculavel = r(N)


/**************************************************************************
 RELATÓRIO
**************************************************************************/

di ""
di ""
di "=============================================================="
di "        PCATOOL-BRASIL – CRIANÇAS – VERSÃO REDUZIDA"
di "                 RELATÓRIO FINAL"
di "=============================================================="

di ""
di "Total de entrevistados:        `N_total'"
di "Escore calculável:             `N_calculavel'"
di "Escore não calculável:         `N_nao_calculavel'"

if `N_total' > 0 {

    di "Percentual calculável:         " ///
        %6.2f (100 * `N_calculavel' / `N_total') "%"

}

di ""
di "--------------------------------------------------------------"
di "Distribuição do número de componentes ausentes"
di "--------------------------------------------------------------"

tab n_missing_pcat, missing

di ""
di "--------------------------------------------------------------"
di "Distribuição da afiliação"
di "--------------------------------------------------------------"

tab afiliacao, missing

di ""
di "--------------------------------------------------------------"
di "Escore Geral – escala 1 a 4"
di "--------------------------------------------------------------"

summ escore_geral_1a4, detail

di ""
di "--------------------------------------------------------------"
di "Escore Geral – escala 0 a 10"
di "--------------------------------------------------------------"

summ escore_geral_0a10, detail

di ""
di "=============================================================="


/**************************************************************************
***************************************************************************
  PARTE XVII
  SALVAR BANCO FINAL
***************************************************************************
**************************************************************************/

/**************************************************************************
SALVAR

  O banco original NÃO é sobrescrito.

  Será criado:

      pcatool_criancas_resultado.dta
**************************************************************************/

** save "pcatool_criancas_resultado.dta", replace


di ""
di as result "=============================================================="
di as result "BANCO FINAL SALVO COM SUCESSO:"
di as result "pcatool_criancas_resultado.dta"
di as result "=============================================================="


/**************************************************************************
***************************************************************************
  PARTE XVIII
  EXPORTAÇÃO OPCIONAL PARA EXCEL
***************************************************************************
**************************************************************************/

/*
  Para criar uma planilha Excel, retire os asteriscos (*) abaixo.
*/

* export excel ///
*     record_id ///
*     a1 a2 a3 ///
*     afiliacao afiliacao_0a10 ///
*     n_missing_pcat ///
*     perc_missing_pcat ///
*     pcat_calculavel ///
*     escore_geral_1a4 ///
*     escore_geral_0a10 ///
*     using "pcatool_criancas_resultados.xlsx", ///
*     firstrow(variables) replace


/**************************************************************************
***************************************************************************
                         FIM DO DO-FILE
***************************************************************************
**************************************************************************/
