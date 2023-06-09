#Econometria com Microdados da PNAD cont�nua de 2018 com R::PNADcIBGE, de BRAGA (2017)
#Trabalho produzido por Paulo Roberto Carneiro de S�
#Produzido em: 03/05/2020

##############################################################################################
#Introdu��o
#pacotes necess�rios: PNADcIBGE (BRAGA, 2018) e survey (LUMLEY, 2019).
library(PNADcIBGE)
help("get_pnadc")

#A importa��o dos microdados atrav�s da fun��o get_pnadc � 
#feita indicando-se o ano e o trimestre dos dados desejados nos argumentos da fun��o. 
#Por exemplo, para o 3� trimestre de 2017:

#para todas as vari�veis
dadosPNADc = get_pnadc(year = 2017, quarter = 3) ou 

#para algumas vari�veis apenas (de nomes �VD4001� e �VD4002�, por exemplo)
dadosPNADc <- get_pnadc(year = 2017, quarter = 3, 
                        vars=c("VD4001","VD4002"))
dadosPNADc
class(dadosPNADc)

### Microdados do Ano de 2018 ###

### Temas e t�picos suplementares pesquisados em trimestres espec�ficos do
### ano: Educa��o (2o trimestre); e Acesso � televis�o e � Internet e posse de
### telefone m�vel celular para uso pessoal (4o trimestre).

# Temas e t�picos pesquisados ao longo do ano em determinada visita:
# Habita��o (1a visita); Caracter�sticas gerais dos moradores (1a visita);
# Informa��es adicionais da for�a de trabalho (1a visita); Outras formas de
# trabalho (afazeres dom�sticos, cuidados de pessoas, produ��o para o
# pr�prio consumo e trabalho volunt�rio) (5a visita); Trabalho de crian�as e
# adolescentes (5a visita); e Rendimentos de outras fontes (1a e 5a
# visitas).  UF Unidade da Federa��o V2007 Sexo V2009 Idade do morador na
# data de refer�ncia V2010 Cor ou ra�a V3007 j� concluiu algum outro curso
# de gradua��o?  VD3004 N�vel de instru��o mais elevado alcan�ado (pessoas
# de 5 anos ou mais de idade) padronizado para o Ensino fundamental sistema
# de 9 anos VD4001 Condi��o em rela��o � for�a de trabalho na semana de
# refer�ncia para pessoas de 14 anos ou mais de idade VD4002 Condi��o de
# ocupa��o na semana de refer�ncia para pessoas de 14 anos ou mais de idade
# VD4020 Rendimento mensal efetivo de todos os trabalhos para pessoas de 14
# anos ou mais de idade (apenas para pessoas que receberam em dinheiro,
# produtos ou mercadorias em qualquer trabalho) VD4035 Horas efetivamente
# trabalhadas na semana de refer�ncia em todos os trabalhos para pessoas de
# 14 anos ou mais de idade

#Download dos dados selecionados

library(PNADcIBGE)
# variaveis selecionadas
variaveis_selecionadas <- c("UF", "V2007", "V2009", "V2010", "V3007", "VD3004", 
                            "VD4001", "VD4002", "VD4020", "VD4035")
dadosPNADc <- get_pnadc(year = 2018, interview = 5, vars = variaveis_selecionadas)
# nos dados trimestrais, posso usar labels, nos anuais n�o!  se indicar
# labels = FALSE, os dados vir�o, por exemplo, com 1 e 2 para homem e mulher
# se indicar labels = TRUE, vir�o Homem e Mulher inv�s de n�meros
class(dadosPNADc)

### Primeiros resultados ###
library(convey)
library(survey)
library(PNADcIBGE)
totalrenda <- svytotal(~VD4020, dadosPNADc, na.rm = T)
options(scipen = 999)  # desativando notacao cientifica
totalrenda

cv(totalrenda)

confint(totalrenda)  #intervalo de confian�a de 95% (padr�o)

confint(totalrenda, level = 0.99)  #intervalo de confian�a de 99%

### Contagens com svytotal

# contagem por sexo 1 Homem e 2 Mulher
totalsexo <- svytotal(~V2007, dadosPNADc, na.rm = T)
totalsexo

# contagem por sexo e raca sem interacao 1 Branca 2 Preta 3 Amarela 4 Parda
# 5 Ind�gena 9 Ignorado

totalsexoraca <- svytotal(~V2007 + V2010, dadosPNADc, na.rm = T)
totalsexoraca

# contagem por sexo, raca e com interacao
totalsexoEraca <- svytotal(~interaction(V2007, V2010), dadosPNADc, na.rm = T)
ftable(totalsexoEraca)  # coluna A com valor e B com SE

### M�dias com svymean

# m�dia da renda
mediarenda <- svymean(~VD4020, dadosPNADc, na.rm = T)
mediarenda

cv(mediarenda)

confint(mediarenda)

# propor��o em vari�vel categ�rica sexo
propsexo <- svymean(~V2007, dadosPNADc, na.rm = T)
propsexo

# propor��o em vari�vel categ�rica sexo e raca sem interacao
propsexoraca <- svymean(~V2007 + V2010, dadosPNADc, na.rm = T)
propsexoraca

# propor��o em vari�vel categ�rica sexo e raca com cruzamento (interaction)
propsexoEraca <- svymean(~interaction(V2007, V2010), dadosPNADc, na.rm = T)
ftable(propsexoEraca)

### Teste entre m�dias de renda para grupos de sexo

# ttest entre m�dias de VD4020 (renda) para os grupos de V2007 (sexo)
tt <- svyttest(VD4020 ~ V2007, dadosPNADc)
tt

confint(tt, level = 0.9)

# m�dia com crit�rio/dom�nio (# 1 Homem e 2 Mulher)
mediarendaM <- svymean(~VD4020, subset(dadosPNADc, V2007 == "2"), na.rm = T)
mediarendaM

mediarendaH <- svymean(~VD4020, subset(dadosPNADc, V2007 == "1"), na.rm = T)
mediarendaH

# Outro exemplo para testar diferen�as entre m�dias de horas trabalhadas,
# entre concluintes e n�o concluintes de gradua��o:
svyttest(as.numeric(VD4035) ~ V3007, dadosPNADc)

### Taxa de desocupa��o com svyratio
# taxa de desocupa��o VD4002 1 Pessoas ocupadas 2 Pessoas desocupadas ' '
# N�o aplic�vel VD4001 1 Pessoas na for�a de trabalho 2 Pessoas fora da
# for�a de trabalho ' ' N�o aplic�vel

txdesocup <- svyratio(~VD4002 == "2", ~VD4001 == "1", dadosPNADc, na.rm = T)
txdesocup

cv(txdesocup)

confint(txdesocup)

# taxa de desocupa��o com desigualdade (V2009 idade >= 25 anos)
txdesocup25 <- svyratio(~VD4002 == "2", ~VD4001 == "1", subset(dadosPNADc, V2009 >= 
                                                                 25), na.rm = T)
txdesocup25

dadosPNADc_mulheres <- subset(dadosPNADc, V2007 == "2")
dadosPNADc_mulheres

# Se desejamos estimar a frequ�ncia relativa de homens e mulheres em cada
# n�vel de instru��o, usamos o seguinte c�digo:

freqSexoInstr <- svyby(~V2007, ~VD3004, dadosPNADc, svymean, na.rm = T)
freqSexoInstr

### Renda de cada estado (Unidade da Federa��o, UF)
# m�dia da renda por estado
mediaRendaUF <- svyby(~VD4020, ~UF, dadosPNADc, svymean, na.rm = T)
mediaRendaUF

confint(mediaRendaUF)

# taxa de desocupa��o com sexo (1 e 2) e ra�a (1 a 5 e 9) e interacao
txdesocupSexoRaca <- svyby(~VD4002 == "2", ~interaction(V2007, V2010), dadosPNADc, 
                           svyratio, denominator = ~VD4001 == "1", na.rm = T, vartype = "cv")
txdesocupSexoRaca

### Regress�o para determinar a renda
# o sistema escolar mudou da VD3001 (8 anos) para a VD3004 (9 anos) modelo
# linear
modeloLin <- svyglm(VD4020 ~ VD3004 + V2010 + V2009, dadosPNADc)
(summary(modeloLin))

confint(modeloLin)

library(stargazer)
stargazer(modeloLin, title = "T�tulo: Resultado da Regress�o com Survey Data", 
          align = TRUE, type = "text", style = "all")

### Indice Gini

# para os calculos de gini preciso preparar pelo convey
library(convey)
dados_pnadcc <- convey_prep(dadosPNADc)
giniHab <- svygini(~VD4020, dados_pnadcc, na.rm = TRUE)
giniHab

cv(giniHab)

giniUF <- svyby(~VD4020, by = ~UF, dados_pnadcc, svygini, na.rm = TRUE)

giniUF

confint(giniUF)

### Curva de lorenz
curvaLorenz <- svylorenz(~VD4020, dados_pnadcc, quantiles = seq(0, 1, 0.05), 
                         na.rm = TRUE)









































































