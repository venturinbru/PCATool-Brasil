*******************************************************************************
******** Do-file analises PCATool Saúde bucal para profissionais dentistas ****
************* QUESTIONARIO COM TODOS OS CAMPOS OBRIGATORIOS

** Importar o banco de dados para o stata
use "caminho do banco"

** Excluir variaveis
drop excluir excluir2 excluir3 excluir4 excluir5 excluir6 excluir7 excluir8 excluir9 

** Descrever
describe
codebook


*============================================================*
* SEGUINDO O MANUAL
* 1o PASSO - INVERSÃO DO ITEM A7
* Escala original:
* 4 = 1
* 3 = 2
* 2 = 3
* 1 = 4
* pode ser tambem recode a7 (1 = 4) (2 = 3) (3 = 2) (4 = 1), generate(a7inv)
*============================================================*

replace a7 = 5 - a7 if inlist(a7, 1, 2, 3, 4)


*============================================================*
* 2º PASSO - TRATAMENTO DOS VALORES AUSENTES
*
* Valores ausentes:
* 9 = Não sei/Não lembro
* . = missing/sem resposta
*
* Regra:
* - Se >= 50% dos itens forem ausentes:
*       escore = missing
*
* - Se < 50% forem ausentes:
*       transformar 9 em 2
*       calcular o escore normalmente
*
*============================================================*


*------------------------------------------------------------*
* ESCALA A - Acesso de Primeiro Contato - Acessibilidade
* 7 itens: A1-A7
*------------------------------------------------------------*
* Criando variavel de nmiss_A, a1-a7
egen nmiss_A = rowmiss(a1-a7)
** Tabulando variavel nmiss_A
tab nmiss_A
** Listando para conferir
list a1-a7 nmiss_A
** Contalizando Variavel de . e 9 como missing
foreach var of varlist a1-a7 {
    replace nmiss_A = nmiss_A + (`var' == 9) if !missing(`var')
}
** Listando para conferir
list a1-a7 nmiss_A
** tabulando
tab nmiss_A
** Criando escore_A para calcular o percentual de missing e 9, para transformar
gen escore_A = .
** Aplicando a regra do passo 2 do manual - se menos de 50% na linha, transformar em 2
foreach var of varlist a1-a7 {
    replace `var' = 2 if `var' == 9 & nmiss_A < 3.5
}
replace escore_A = (a1+a2+a3+a4+a5+a6+a7)/7 if nmiss_A < 3.5
tab escore_A
sum escore_A, d
** Histograma do escore
hist escore_A

*------------------------------------------------------------*
* ESCALA B - Longitudinalidade
* 13 itens: B1-B13
*------------------------------------------------------------*
** Criando variavel de nmiss_D
egen nmiss_B = rowmiss(b1-b13)
tab nmiss_B
** Listando para conferir
list b1-b13 nmiss_B
** Contabilizando Variavel de . e 9 como missing
foreach var of varlist b1-b13 {
    replace nmiss_B = nmiss_B + (`var' == 9) if !missing(`var')
}
** listando para conferir
list b1-b13 nmiss_B
** tabulando
tab nmiss_B
** Criando escore de numero de missing
gen escore_B = .
** Aplicando a regra do passo 2 do manual - se menos de 50% na linha, transformar em 2
foreach var of varlist b1-b13 {
    replace `var' = 2 if `var' == 9 & nmiss_B < 6.5
}
replace escore_B = (b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9+ b10 + b11 + b12 + b13)/13 if nmiss_B < 6.5
** tabulando
tab escore_B
sum escore_B, d
** criando histograma basico do escore
hist escore_B

*------------------------------------------------------------*
* ESCALA C - Coordenação - Integração de Cuidados
* 5 itens: C1-C5
*------------------------------------------------------------*
* Criando var de numero de missing para a escala c, c1-5
egen nmiss_C = rowmiss(c1-c5)
tab nmiss_C
** listando para conferir
list c1-c5 nmiss_C
** Contabilizando Variavel de . e 9 como missing
foreach var of varlist c1-c5 {
    replace nmiss_C = nmiss_C + (`var' == 9) if !missing(`var')
}
** listando para conferir
list c1-c5 nmiss_C
** tabulando
tab nmiss_C
** Criando escore de numero de missing
gen escore_C = .
** Aplicando a regra do passo 2 do manual - se menos de 50% na linha, transformar em 2
foreach var of varlist c1-c5 {
    replace `var' = 2 if `var' == 9 & nmiss_C < 2.5
}
replace escore_C = (c1 + c2 + c3 + c4 + c5)/5 if nmiss_C < 2.5
tab escore_C
sum escore_C, d
** criando histograma basico do escore
hist escore_C

*------------------------------------------------------------*
* ESCALA D - Coordenação - Sistema de Informações
* 3 itens: D1-D3
*------------------------------------------------------------*
** Criar variavel de nmiss_D
egen nmiss_D = rowmiss(d1-d3)
tab nmiss_D
** listando para conferir
list d1-d3 nmiss_D
** Variavel de . e 9 como missing
foreach var of varlist d1-d3 {
    replace nmiss_D = nmiss_D + (`var' == 9) if !missing(`var')
}
** listando para conferir
list d1-d3 nmiss_D
** tabulando
tab nmiss_D
** Criando escore de numero de missing
gen escore_D = .
** Aplicando a regra do passo 2 do manual - se menos de 50% na linha, transformar em 2
foreach var of varlist d1-d3 {
    replace `var' = 2 if `var' == 9 & nmiss_D < 1.5
}
replace escore_D = (d1 + d2 + d3)/3 if nmiss_D < 1.5
tab escore_D
sum escore_D, d
** Histograma do escore D1-D3
hist escore_D

*------------------------------------------------------------*
* ESCALA E - Integralidade - Serviços Disponíveis
* 23 itens: E1-E23
*------------------------------------------------------------*
* Criando variavel de nmiss_E
egen nmiss_E = rowmiss(e1-e23)
tab nmiss_E
** listando para conferir
list e1-e23 nmiss_E
** Contabilizando Variavel de . e 9 como missing
foreach var of varlist e1-e23 {
    replace nmiss_E = nmiss_E + (`var' == 9) if !missing(`var')
}
** listando para conferir
list e1-e23 nmiss_E
** tabulando
tab nmiss_E
** Criando escore de numero de missing
gen escore_E = .
** Aplicando a regra do passo 2 do manual - se menos de 50% na linha, transformar em 2
foreach var of varlist e1-e23 {
    replace `var' = 2 if `var' == 9 & nmiss_E < 11.5
}
replace escore_E = (e1 + e2 + e3 + e4 + e5 + e6 + e7 + e8 + e9 + e10 + e11 + e12 + e13 + e14 + e15 + e16 + e17 + e18 + e19 + e20 + e21 + e22 + e23)/23 if nmiss_E < 11.5
tab escore_E
sum escore_E, d
** criando histograma basico do escore
hist escore_E

*------------------------------------------------------------*
* ESCALA F - Integralidade - Serviços Prestados
* 7 itens: F1-F7
*------------------------------------------------------------*
** Criando variavel nmiss_F
egen nmiss_F = rowmiss(f1-f7)
tab nmiss_F
** listando para conferir
list f1-f7 nmiss_F
** Contabilizando Variavel de . e 9 como missing
foreach var of varlist f1-f7 {
    replace nmiss_F = nmiss_F + (`var' == 9) if !missing(`var')
}
** listando para conferir
list f1-f7 nmiss_F
** tabulando
tab nmiss_F
** Criando escore de numero de missing
gen escore_F = .
** Aplicando a regra do passo 2 do manual - se menos de 50% na linha, transformar em 2
foreach var of varlist f1-f7 {
    replace `var' = 2 if `var' == 9 & nmiss_F < 3.5
}

replace escore_F = (f1 + f2 + f3 + f4 + f5 + f6 + f7)/7 if nmiss_F < 3.5
tab escore_F
sum escore_F, d
** histograma do escore
histo escore_F

*------------------------------------------------------------*
* ESCALA G - Orientação Familiar
* 4 itens: G1-G4
*------------------------------------------------------------*
** criar variavel de nmiss_G
egen nmiss_G = rowmiss(g1-g4)
tab nmiss_G
** listando para conferir
list g1-g4 nmiss_G
**C ontabilizando Variavel de . e 9 como missing
foreach var of varlist g1-g4 {
    replace nmiss_G = nmiss_G + (`var' == 9) if !missing(`var')
}
** listando para conferir
list g1-g4 nmiss_G
* tabulando
tab nmiss_G
** Criando escore de numero de missing
gen escore_G = .
**  Aplicando a regra do passo 2 do manual - se menos de 50% na linha, transformar em 2
foreach var of varlist g1-g4 {
    replace `var' = 2 if `var' == 9 & nmiss_G < 2
}

replace escore_G = (g1 + g2 + g3 + g4)/4 if nmiss_G < 2
tab escore_G
sum escore_G, d
** criando histograma do escore
histo escore_G

*------------------------------------------------------------*
* ESCALA H - Orientação Comunitária
* 13 itens: H1-H13
*------------------------------------------------------------*
** Criar variavel nmiss_H
egen nmiss_H = rowmiss(h1-h13)
tab nmiss_H
** listando para conferir
list h1-h13 nmiss_H
** Contabilizando Variavel de . e 9 como missing
foreach var of varlist h1-h13 {
    replace nmiss_H = nmiss_H + (`var' == 9) if !missing(`var')
}
** listando para conferir
list h1-h13 nmiss_H
** tabulando
tab nmiss_H
** Criando escore do numero de missing
gen escore_H = .
**  Aplicando a regra do passo 2 do manual - se menos de 50% na linha, transformar em 2
foreach var of varlist h1-h13 {
    replace `var' = 2 if `var' == 9 & nmiss_H < 6.5
}

replace escore_H = (h1 + h2 + h3 + h4 + h5 + h6 + h7 + h8 + h9 + h10 + h11 + h12 + h13)/13 if nmiss_H < 6.5
tab escore_H
sum escore_H, d
** Histograma do escore
hist escore_H

*------------------------------------------------------------*
* ESCALA I - Competência Cultural
* 6 itens: I1-I6
*------------------------------------------------------------*
** Criar variavel nmiss_I
egen nmiss_I = rowmiss(i1-i6)
tab nmiss_I
** listando para conferir
list i1-i6 nmiss_I
** Contabilizando Variavel de . e 9 como missing
foreach var of varlist i1-i6 {
    replace nmiss_I = nmiss_I + (`var' == 9) if !missing(`var')
}
** listando para conferir
list i1-i6 nmiss_I
** tabulando
tab nmiss_I
** Criando escore de numero de missing
gen escore_I = .
** Aplicando a regra do passo 2 do manual - se menos de 50% na linha, transformar em 2
foreach var of varlist i1-i6 {
    replace `var' = 2 if `var' == 9 & nmiss_I < 3
}

replace escore_I = (i1 + i2 + i3 + i4 + i5 + i6)/6 if nmiss_I < 3
tab escore_I
sum escore_I, d
** criando histograma do escore
hist escore_I

*============================================================*
* VISUALIZAR OS ESCORES
*============================================================*

summarize escore_A escore_B escore_C escore_D escore_E ///
          escore_F escore_G escore_H escore_I


*============================================================*
* PCATOOL - SAUDE BUCAL
* ESCORES ESSENCIAL E GERAL
* ESCORE ESSENCIAL DA APS EM SAÚDE BUCAL
* Componentes: A, B, C, D, E, F
*============================================================*

egen essencial_nmiss = rowmiss(escore_A escore_B escore_C escore_D escore_E escore_F)

tab essencial_nmiss
sum essencial_nmiss, d

egen essencial_soma = rowtotal(escore_A escore_B escore_C escore_D escore_E escore_F)

tab essencial_soma
sum essencial_soma, d

gen escore_essencial = essencial_soma/(6 - essencial_nmiss) ///
    if essencial_nmiss <= 2

tab essencial_nmiss
summ escore_essencial, d

*============================================================*
* ESCORE GERAL DA APS EM SAÚDE BUCAL
* Componentes: A, B, C, D, E, F, G, H, I
*============================================================*

egen geral_nmiss = rowmiss(escore_A escore_B escore_C escore_D ///
                           escore_E escore_F escore_G escore_H escore_I)
tab geral_nmiss
sum geral_nmiss, d
						   
egen geral_soma = rowtotal(escore_A escore_B escore_C escore_D ///
                           escore_E escore_F escore_G escore_H escore_I)

tab geral_soma
sum geral_soma, d
						   
gen escore_geral = geral_soma/(9 - geral_nmiss) ///
    if geral_nmiss <= 3

tab escore_geral
sum escore_geral, d	

*============================================================*
* TRANSFORMAÇÃO DOS COMPONENTES PARA ESCALA 0 A 10
*============================================================*	

foreach x in a b c d e f g h i {
    gen escore_`x'_0_10 = ((escore_`x' - 1)/3)*10
}
	
*============================================================*
* TRANSFORMAÇÃO PARA ESCALA 0 A 10
*============================================================*

gen escore_essencial_0_10 = ((escore_essencial - 1)/3)*10

tab escore_essencial_0_10
sum escore_essencial_0_10, d

gen escore_geral_0_10 = ((escore_geral - 1)/3)*10

tab escore_geral_0_10
sum escore_geral_0_10, d

*============================================================*
* VERIFICACAO DOS ESCORES DOS COMPONENTES NA ESCALA 0 A 10
*============================================================*

summ escore_A escore_B escore_C escore_D escore_E escore_F ///
      escore_G escore_H escore_I escore_essencial escore_geral

summ escore_A_0_10 escore_B_0_10 escore_C_0_10 ///
      escore_D_0_10 escore_E_0_10 escore_F_0_10 ///
      escore_G_0_10 escore_H_0_10 escore_I_0_10 ///
      escore_essencial_0_10 escore_geral_0_10
