/********************************************************************
* PCATOOL-BRASIL 2020 - ADULTOS - questionário extenso
* Cálculo dos escores segundo o Manual PCATool-Brasil 2020 (Ministério da saúde)
*
* Componentes:
* A = Afiliação
* B = Acesso de Primeiro Contato - Utilização
* C = Acesso de Primeiro Contato - Acessibilidade
* D = Longitudinalidade
* E = Coordenação - Integração de Cuidados
* F = Coordenação - Sistema de Informações
* G = Integralidade - Serviços Disponíveis
* H = Integralidade - Serviços Prestados
* I = Orientação Familiar
* J = Orientação Comunitária
*
* Regras implementadas:
* 1. Inversão C9-C12 e D14
* 2. Valor 9 = "Não sei/Não lembro"
* 3. Missing + 9 para regra de perda >/= 50%
* 4. Se <50% ausentes: 9 -> 2
* 5. Se >=50% ausentes: escore do componente = missing
* 6. H depende do sexo
* 7. Escore Essencial e Geral com regras de perda de componentes
* 8. Transformação final para escala 0-10
********************************************************************/

clear all
set more off

********************************************************************
* IMPORTAR BANCO DE DADOS
********************************************************************

use "/Users/brunaventurin/Downloads/pcatooladultoextenso.dta"

/********************************************************************
* VERIFICAÇÃO DAS VARIÁVEIS
********************************************************************/

local itens ///
    a1 a2 a3 ///
    b1 b2 b3 ///
    c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 ///
    d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 ///
    e1 e2 e3 e4 e5 e6 e7 e8 e9 ///
    f1 f2 f3 ///
    g1 g2 g3 g4 g5 g6 g7 g8 g9 g10 g11 g12 g13 g14 g15 g16 g17 g18 g19 g20 g21 g22 ///
    h1 h2 h3 h4 h5 h6 h7 h8 h9 h10 h11 h12 h13 ///
    i1 i2 i3 ///
    j1 j2 j3 j4 j5 j6

foreach var of local itens {
    capture confirm variable `var'
    if _rc {
        display as error "ERRO: variável `var' não encontrada no banco."
    }
}


/********************************************************************
* CÓPIA DAS VARIÁVEIS ORIGINAIS
*
* Não alteramos as respostas originais.
* São criadas variáveis auxiliares para o cálculo.
********************************************************************/

foreach var of local itens {
    capture drop calc_`var'
    gen double calc_`var' = `var'
}


/********************************************************************
* INVERSÃO DOS ITENS
*
* C9, C10, C11, C12 e D14
*
* Escala:
* 4 -> 1
* 3 -> 2
* 2 -> 3
* 1 -> 4
*
* O valor 9 permanece 9 neste momento.
********************************************************************/

foreach var in c9 c10 c11 c12 d14 {
    replace calc_`var' = 5 - calc_`var' if inrange(calc_`var',1,4)
}


/********************************************************************
* FUNÇÃO / ROTINA PARA CÁLCULO DOS COMPONENTES
*
* Regra:
*
* Ausentes = 9 + missing
*
* Se ausentes >= 50%:
*       escore = missing
*
* Se ausentes < 50%:
*       9 -> 2
*       escore = média dos itens válidos
*
* IMPORTANTE:
* Não transformamos o 9 original diretamente no banco.
********************************************************************/


/********************************************************************
* ACESSO DE PRIMEIRO CONTATO - UTILIZAÇÃO
* B = (B1 + B2 + B3) / 3
*
* Regra de missing:
* - 9 = Não sei/Não lembro
* - missing = sem resposta
* - se >= 50% dos 3 itens forem ausentes -> escore B = missing
* - se < 50% forem ausentes -> 9 é imputado como 2
********************************************************************/

* Número de itens ausentes
gen byte nmiss_b = ///
    missing(calc_b1) + (calc_b1==9) + ///
    missing(calc_b2) + (calc_b2==9) + ///
    missing(calc_b3) + (calc_b3==9)

label variable nmiss_b "Número de itens ausentes - Componente B"

* Criar escore B
gen double esc_b = .

label variable esc_b ///
    "Acesso de Primeiro Contato - Utilização (1-4)"

* Criar cópias temporárias para imputação
gen double b1_calc = calc_b1
gen double b2_calc = calc_b2
gen double b3_calc = calc_b3

* Se houver menos de 50% de ausentes:
* transformar 9 em 2
replace b1_calc = 2 if b1_calc == 9 & nmiss_b < 1.5
replace b2_calc = 2 if b2_calc == 9 & nmiss_b < 1.5
replace b3_calc = 2 if b3_calc == 9 & nmiss_b < 1.5

* 3 itens:
* 50% = 1,5
* Portanto:
* 0 ou 1 ausente -> calcula
* 2 ou 3 ausentes -> missing
replace esc_b = ///
    (b1_calc + b2_calc + b3_calc) / 3 ///
    if nmiss_b < 1.5

* Limpar variáveis temporárias
drop b1_calc b2_calc b3_calc

/********************************************************************
* ACESSO DE PRIMEIRO CONTATO - ACESSIBILIDADE
* C = média dos 12 itens
********************************************************************/

gen byte nmiss_c = 0

forvalues k = 1/12 {
    replace nmiss_c = nmiss_c + missing(calc_c`k') + (calc_c`k'==9)
}

gen double esc_c = .

* 12 itens; 50% = 6
replace esc_c = . if nmiss_c >= 6

forvalues k = 1/12 {
    replace calc_c`k' = 2 if calc_c`k'==9 & nmiss_c < 6
}

egen soma_c = rowtotal(calc_c1-calc_c12)
replace esc_c = soma_c/12 if nmiss_c < 6

drop soma_c


/********************************************************************
* LONGITUDINALIDADE
* D = média dos 14 itens
********************************************************************/

gen byte nmiss_d = 0

forvalues k = 1/14 {
    replace nmiss_d = nmiss_d + missing(calc_d`k') + (calc_d`k'==9)
}

gen double esc_d = .

* 14 itens; 50% = 7
replace esc_d = . if nmiss_d >= 7

forvalues k = 1/14 {
    replace calc_d`k' = 2 if calc_d`k'==9 & nmiss_d < 7
}

egen soma_d = rowtotal(calc_d1-calc_d14)
replace esc_d = soma_d/14 if nmiss_d < 7

drop soma_d


/********************************************************************
* COORDENAÇÃO - INTEGRAÇÃO DE CUIDADOS
*
* E1 NÃO entra no cálculo.
* São utilizados E2-E9.
*
* Importante:
* E2-E9 só existem quando E1 = 1.
********************************************************************/

gen byte nmiss_e = 0

forvalues k = 2/9 {
    replace nmiss_e = nmiss_e + missing(calc_e`k') + (calc_e`k'==9)
}

gen double esc_e = .

* 8 itens; 50% = 4
replace esc_e = . if nmiss_e >= 4

forvalues k = 2/9 {
    replace calc_e`k' = 2 if calc_e`k'==9 & nmiss_e < 4
}

egen soma_e = rowtotal(calc_e2-calc_e9)
replace esc_e = soma_e/8 if nmiss_e < 4

drop soma_e


/********************************************************************
* COORDENAÇÃO - SISTEMA DE INFORMAÇÕES
* F = (F1+F2+F3)/3
********************************************************************/

gen byte nmiss_f = 0

forvalues k = 1/3 {
    replace nmiss_f = nmiss_f + missing(calc_f`k') + (calc_f`k'==9)
}

gen double esc_f = .

* 3 itens; 50% = 1.5
replace esc_f = . if nmiss_f >= 1.5

forvalues k = 1/3 {
    replace calc_f`k' = 2 if calc_f`k'==9 & nmiss_f < 1.5
}

egen soma_f = rowtotal(calc_f1-calc_f3)
replace esc_f = soma_f/3 if nmiss_f < 1.5

drop soma_f


/********************************************************************
* INTEGRALIDADE - SERVIÇOS DISPONÍVEIS
* G = média dos 22 itens
********************************************************************/

gen byte nmiss_g = 0

forvalues k = 1/22 {
    replace nmiss_g = nmiss_g + missing(calc_g`k') + (calc_g`k'==9)
}

gen double esc_g = .

* 22 itens; 50% = 11
replace esc_g = . if nmiss_g >= 11

forvalues k = 1/22 {
    replace calc_g`k' = 2 if calc_g`k'==9 & nmiss_g < 11
}

egen soma_g = rowtotal(calc_g1-calc_g22)
replace esc_g = soma_g/22 if nmiss_g < 11

drop soma_g


/********************************************************************
* INTEGRALIDADE - SERVIÇOS PRESTADOS
*
* Sexo feminino: H1-H13
* Sexo masculino: H1-H11
*
* ATENÇÃO:
* É necessário identificar o nome/codificação da variável de sexo
* no seu banco.
*
* Neste exemplo será utilizada a variável "sexo":
*   1 = feminino
*   2 = masculino
*
* Se no seu banco for diferente, ALTERE AQUI.
********************************************************************/

capture confirm variable sexo
if _rc {
    display as error "ATENÇÃO: variável 'sexo' não encontrada."
    display as error "Altere o código da seção H para o nome correto da variável de sexo."
}


/*--------------------------------------------------------------
  H - MULHERES
--------------------------------------------------------------*/

gen byte nmiss_hf = 0

forvalues k = 1/13 {
    replace nmiss_hf = nmiss_hf + missing(calc_h`k') + (calc_h`k'==9)
}

gen double esc_hf = .

* 13 itens; 50% = 6.5
replace esc_hf = . if nmiss_hf >= 6.5

forvalues k = 1/13 {
    replace calc_h`k' = 2 if calc_h`k'==9 & nmiss_hf < 6.5
}

egen soma_hf = rowtotal(calc_h1-calc_h13)
replace esc_hf = soma_hf/13 if nmiss_hf < 6.5

drop soma_hf


/*--------------------------------------------------------------
  H - HOMENS
--------------------------------------------------------------*/

gen byte nmiss_hm = 0

forvalues k = 1/11 {
    replace nmiss_hm = nmiss_hm + missing(calc_h`k') + (calc_h`k'==9)
}

gen double esc_hm = .

* 11 itens; 50% = 5.5
replace esc_hm = . if nmiss_hm >= 5.5

forvalues k = 1/11 {
    replace calc_h`k' = 2 if calc_h`k'==9 & nmiss_hm < 5.5
}

egen soma_hm = rowtotal(calc_h1-calc_h11)
replace esc_hm = soma_hm/11 if nmiss_hm < 5.5

drop soma_hm


/*--------------------------------------------------------------
  ESCOLHA DO ESCORE H SEGUNDO SEXO
--------------------------------------------------------------*/

gen double esc_h = .

replace esc_h = esc_hf if sexo == 1
replace esc_h = esc_hm if sexo == 2

label variable esc_h  "Integralidade - Serviços Prestados"
label variable esc_hf "H - Serviços Prestados - Feminino"
label variable esc_hm "H - Serviços Prestados - Masculino"


/********************************************************************
* ORIENTAÇÃO FAMILIAR
* I = (I1+I2+I3)/3
********************************************************************/

gen byte nmiss_i = 0

forvalues k = 1/3 {
    replace nmiss_i = nmiss_i + missing(calc_i`k') + (calc_i`k'==9)
}

gen double esc_i = .

* 3 itens; 50% = 1.5
replace esc_i = . if nmiss_i >= 1.5

forvalues k = 1/3 {
    replace calc_i`k' = 2 if calc_i`k'==9 & nmiss_i < 1.5
}

egen soma_i = rowtotal(calc_i1-calc_i3)
replace esc_i = soma_i/3 if nmiss_i < 1.5

drop soma_i


/********************************************************************
* ORIENTAÇÃO COMUNITÁRIA
* J = (J1+J2+J3+J4+J5+J6)/6
********************************************************************/

gen byte nmiss_j = 0

forvalues k = 1/6 {
    replace nmiss_j = nmiss_j + missing(calc_j`k') + (calc_j`k'==9)
}

gen double esc_j = .

* 6 itens; 50% = 3
replace esc_j = . if nmiss_j >= 3

forvalues k = 1/6 {
    replace calc_j`k' = 2 if calc_j`k'==9 & nmiss_j < 3
}

egen soma_j = rowtotal(calc_j1-calc_j6)
replace esc_j = soma_j/6 if nmiss_j < 3

drop soma_j


/********************************************************************
* AFILIAÇÃO
*
* A1:
*   0 = Não
*   1 = Sim
*
* A2:
*   0 = Não
*   1 = Sim, mesmo serviço do A1
*   2 = Sim, serviço diferente do A1
*
* A3:
*   0 = Não
*   1 = Sim, mesmo serviço de A1 e A2
*   2 = Sim, somente mesmo de A1
*   3 = Sim, somente mesmo de A2
*   4 = Sim, diferente de A1 e A2
*
* O algoritmo do manual deve ser traduzido considerando que A2/A3
* possuem códigos que identificam a relação entre os serviços.
********************************************************************/

gen double esc_a = .

/*
---------------------------------------------------------------
CASO 1:
A1 = NÃO, A2 = NÃO, A3 = NÃO
---------------------------------------------------------------
*/

replace esc_a = 1 if a1==0 & a2==0 & a3==0


/*
---------------------------------------------------------------
CASO 2:
Todas as respostas são NÃO?
Já tratado acima.

Caso exista UMA resposta SIM:
grau de afiliação = 2

Isso ocorre quando apenas um dos três itens é positivo.
---------------------------------------------------------------
*/

replace esc_a = 2 if missing(esc_a) & ///
    ( ///
        (a1==1 & a2==0 & a3==0) | ///
        (a1==0 & a2>0 & a3==0) | ///
        (a1==0 & a2==0 & a3>0) ///
    )


/*
---------------------------------------------------------------
DUAS OU TRÊS respostas SIM referentes a serviços DIFERENTES
= 2

A identificação exata depende da lógica de A1/A2/A3.
---------------------------------------------------------------
*/

/*
A1 = SIM.

Se A2 = 2:
A2 aponta para serviço diferente de A1.

Se A3 = 3:
A3 aponta para o serviço de A2.

Portanto, A1 e A2 são diferentes, mas A2=A3.
Nesse caso grau = 3.

Se A3 = 1:
A3 é o mesmo serviço de A1 e A2.
Então os três são iguais -> grau 4.

Se A3 = 2:
A3 é somente A1.
A1=A3, A2 diferente -> grau 3.

Se A3 = 4:
A3 é diferente de A1 e A2.
Todos diferentes -> grau 2.
*/

replace esc_a = 4 if a1==1 & a2==1 & a3==1

replace esc_a = 3 if a1==1 & a2==2 & a3==1
replace esc_a = 3 if a1==1 & a2==2 & a3==2
replace esc_a = 3 if a1==1 & a2==1 & a3==2

replace esc_a = 2 if a1==1 & a2==2 & a3==4


/*
---------------------------------------------------------------
A1 = NÃO
A2 = SIM
A3 pode indicar:

A2=1 não é coerente semanticamente se A1=0, mas caso exista,
deve ser tratado conforme o banco.

A2=2 = serviço diferente de A1.
A3:
  3 = somente o mesmo de A2 -> A2=A3 -> grau 3
  4 = diferente de A1 e A2 -> três referências conceituais,
      mas A1 é NÃO -> os serviços SIM são diferentes -> grau 2
---------------------------------------------------------------
*/

replace esc_a = 3 if a1==0 & a2==2 & a3==3
replace esc_a = 2 if a1==0 & a2==2 & a3==4

/*
A1=0, A2=0, A3=2:
A3 indica o mesmo serviço de A1, mas A1 é NÃO.
Situação logicamente inconsistente segundo a codificação.
Não atribuir automaticamente sem verificar dados.
*/


/*
---------------------------------------------------------------
A1 = SIM
A2 = NÃO
A3 = 2
A3 = somente o mesmo de A1
=> apenas A1 e A3 referem-se ao mesmo serviço
=> grau 3
---------------------------------------------------------------
*/

replace esc_a = 3 if a1==1 & a2==0 & a3==2


/*
A1 = SIM
A2 = NÃO
A3 = 4
=> A3 é diferente dos serviços A1/A2.
Há dois serviços SIM diferentes.
=> grau 2
*/

replace esc_a = 2 if a1==1 & a2==0 & a3==4


/*
---------------------------------------------------------------
A1 = NÃO
A2 = SIM
A3 = 4
=> A2 e A3 são diferentes
=> dois serviços diferentes
=> grau 2
---------------------------------------------------------------
*/

replace esc_a = 2 if a1==0 & a2>0 & a3==4


/*
---------------------------------------------------------------
A1 = SIM
A2 = 2
A3 = 3
=> A2=A3
=> grau 3
---------------------------------------------------------------
*/

replace esc_a = 3 if a1==1 & a2==2 & a3==3


/*
---------------------------------------------------------------
A1 = NÃO
A2 = NÃO
A3 = qualquer SIM
=> somente uma resposta SIM
=> grau 2
---------------------------------------------------------------
*/

replace esc_a = 2 if a1==0 & a2==0 & a3>0


/*
---------------------------------------------------------------
A1 = SIM
A2 = NÃO
A3 = 1
Se A3 diz que é o mesmo de A1 e A2, mas A2 é NÃO.
Situação inconsistente. Não forçar classificação.
---------------------------------------------------------------
*/


label variable esc_a "Afiliação"


/********************************************************************
* ESCORE ESSENCIAL DA APS
*
* A + B + C + D + E + F + G + H
*
* Se 4 ou mais componentes essenciais forem missing:
*   escore essencial = missing
*
* Se 3 ou menos missing:
*   média dos componentes disponíveis.
********************************************************************/

egen nmiss_essencial = rowmiss(esc_a esc_b esc_c esc_d esc_e esc_f esc_g esc_h)

egen soma_essencial = rowtotal(esc_a esc_b esc_c esc_d esc_e esc_f esc_g esc_h)

gen double esc_essencial = .

replace esc_essencial = . if nmiss_essencial >= 4

replace esc_essencial = soma_essencial / ///
    (8 - nmiss_essencial) if nmiss_essencial <= 3

label variable esc_essencial "Escore Essencial da APS - escala 1 a 4"
sum esc_essencial, d
hist esc_essencial

/********************************************************************
* ESCORE GERAL DA APS
*
* A + B + C + D + E + F + G + H + I + J
*
* Se 5 ou mais componentes missing:
*   missing
*
* Se 4 ou menos missing:
*   média dos componentes disponíveis.
********************************************************************/

egen nmiss_geral = rowmiss( ///
    esc_a esc_b esc_c esc_d esc_e esc_f esc_g esc_h esc_i esc_j ///
)

egen soma_geral = rowtotal( ///
    esc_a esc_b esc_c esc_d esc_e esc_f esc_g esc_h esc_i esc_j ///
)

gen double esc_geral = .

replace esc_geral = . if nmiss_geral >= 5

replace esc_geral = soma_geral / ///
    (10 - nmiss_geral) if nmiss_geral <= 4

label variable esc_geral "Escore Geral da APS - escala 1 a 4"
sum esc_geral, d
hist esc_geral

/********************************************************************
* TRANSFORMAÇÃO DOS ESCORES PARA 0-10
*
* Fórmula:
*
* (escore obtido - 1)/(4 - 1) * 10
*
* = (escore - 1)/3 * 10
********************************************************************/

gen double esc_a_10 = ((esc_a - 1)/3)*10 if !missing(esc_a)
gen double esc_b_10 = ((esc_b - 1)/3)*10 if !missing(esc_b)
gen double esc_c_10 = ((esc_c - 1)/3)*10 if !missing(esc_c)
gen double esc_d_10 = ((esc_d - 1)/3)*10 if !missing(esc_d)
gen double esc_e_10 = ((esc_e - 1)/3)*10 if !missing(esc_e)
gen double esc_f_10 = ((esc_f - 1)/3)*10 if !missing(esc_f)
gen double esc_g_10 = ((esc_g - 1)/3)*10 if !missing(esc_g)
gen double esc_h_10 = ((esc_h - 1)/3)*10 if !missing(esc_h)
gen double esc_i_10 = ((esc_i - 1)/3)*10 if !missing(esc_i)
gen double esc_j_10 = ((esc_j - 1)/3)*10 if !missing(esc_j)

gen double esc_essencial_10 = ///
    ((esc_essencial - 1)/3)*10 if !missing(esc_essencial)

gen double esc_geral_10 = ///
    ((esc_geral - 1)/3)*10 if !missing(esc_geral)


/********************************************************************
* RÓTULOS DOS ESCORES
********************************************************************/

label variable esc_a_10 "Afiliação - escala 0 a 10"
label variable esc_b_10 "Acesso Utilização - escala 0 a 10"
label variable esc_c_10 "Acesso Acessibilidade - escala 0 a 10"
label variable esc_d_10 "Longitudinalidade - escala 0 a 10"
label variable esc_e_10 "Coordenação Integração - escala 0 a 10"
label variable esc_f_10 "Coordenação Informação - escala 0 a 10"
label variable esc_g_10 "Integralidade Serviços Disponíveis - escala 0 a 10"
label variable esc_h_10 "Integralidade Serviços Prestados - escala 0 a 10"
label variable esc_i_10 "Orientação Familiar - escala 0 a 10"
label variable esc_j_10 "Orientação Comunitária - escala 0 a 10"

label variable esc_essencial_10 ///
    "Escore Essencial APS - escala 0 a 10"

label variable esc_geral_10 ///
    "Escore Geral APS - escala 0 a 10"


/********************************************************************
* DIAGNÓSTICO DOS COMPONENTES
********************************************************************/

label variable nmiss_b "N ausentes - B"
label variable nmiss_c "N ausentes - C"
label variable nmiss_d "N ausentes - D"
label variable nmiss_e "N ausentes - E"
label variable nmiss_f "N ausentes - F"
label variable nmiss_g "N ausentes - G"
label variable nmiss_hf "N ausentes - H feminino"
label variable nmiss_hm "N ausentes - H masculino"
label variable nmiss_i "N ausentes - I"
label variable nmiss_j "N ausentes - J"

label variable nmiss_essencial ///
    "N componentes essenciais missing"

label variable nmiss_geral ///
    "N componentes gerais missing"


/********************************************************************
* CHECAGENS DE CONSISTÊNCIA
********************************************************************/

display "=========================================================="
display "CHECAGEM DOS ESCORES"
display "=========================================================="

summarize ///
    esc_a esc_b esc_c esc_d esc_e esc_f esc_g esc_h esc_i esc_j ///
    esc_essencial esc_geral

display "----------------------------------------------------------"
display "ESCORE 0-10"
display "----------------------------------------------------------"

summarize ///
    esc_a_10 esc_b_10 esc_c_10 esc_d_10 esc_e_10 ///
    esc_f_10 esc_g_10 esc_h_10 esc_i_10 esc_j_10 ///
    esc_essencial_10 esc_geral_10


/********************************************************************
* CHECAR VALORES FORA DA ESCALA
********************************************************************/

foreach var in ///
    esc_a esc_b esc_c esc_d esc_e esc_f esc_g esc_h esc_i esc_j ///
    esc_essencial esc_geral {

    count if `var' < 1 & !missing(`var')
    if r(N) > 0 {
        display as error "`var': existem valores menores que 1."
    }

    count if `var' > 4 & !missing(`var')
    if r(N) > 0 {
        display as error "`var': existem valores maiores que 4."
    }
}


/********************************************************************
* DISTRIBUIÇÃO DA AFILIAÇÃO
********************************************************************/

tab esc_a, missing


/********************************************************************
* QUANTIDADE DE COMPONENTES MISSING
********************************************************************/

tab nmiss_essencial, missing
tab nmiss_geral, missing


/********************************************************************
* TABELA RESUMIDA DOS ESCORES
********************************************************************/

tabstat ///
    esc_a_10 esc_b_10 esc_c_10 esc_d_10 ///
    esc_e_10 esc_f_10 esc_g_10 esc_h_10 ///
    esc_i_10 esc_j_10 ///
    esc_essencial_10 esc_geral_10, ///
    statistics(n mean sd min p50 max) ///
    columns(statistics)


/********************************************************************
* FIM
********************************************************************/
