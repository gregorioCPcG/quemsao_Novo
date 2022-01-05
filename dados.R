#seleção #####
library(haven)
ESEB2018 <- read_sav("C:/Users/grego/Dropbox/doutorado/2021.1 disciplinas/Metodologia Quantitativa avançada/paper/04622.sav")
library (shiny)
library(tidyverse)
options(scipen = 1000)
library(haven)
library(memisc)
library(sjPlot)
library(Zelig)
library(knitr)
library(car)
library(kableExtra)
BASET <- subset(ESEB2018, select = c(D1A_FAIXAID, D2_SEXO, D3_ESCOLA, Q12P1_B, Q12P2_B, D10, REG, D12A))
BASET$MULTI_T1<- memisc::recode(as.factor(BASET$Q12P1_B), 1 <- c (9), 2 <- c(5), 
                                3 <- c(3), 4 <- c(6), 5 <-c(10),
                                6 <-c(1,2,4,7,8,12,13,14), 7 <-c(50,60),
                                NA <- c(97,98,99))
table(BASET$Q12P2_B)
# 10 amoedo - 35 pessoas
# https://rpubs.com/gregogregogregogregogrego/819255 - amoedo 1.99 %

BASET <- subset(BASET, select = c(D1A_FAIXAID, D2_SEXO, D3_ESCOLA, MULTI_T1, Q12P1_B, Q12P2_B, D10, REG, D12A)) %>% na.omit() 

BASET$BOLS_HADD_T2<- memisc::recode(as.factor(BASET$Q12P2_B), 1 <- c (2), 2 <- c(1,50,60,97,98,99))
table(BASET$BOLS_HADD_T2)
table(BASET$Q12P2_B) #para comparar
BASET <- within(BASET, {
  turno2 <- Recode(BOLS_HADD_T2, '1 = "Bolsonaro"; 2 = "N"', as.factor=FALSE)
})
table(BASET$BOLS_HADD_T2) #para conferir
table(BASET$turno2) # para conferir
BASET$BOLS_HADDa_T2<- memisc::recode(as.factor(BASET$Q12P2_B), 1 <- c (2), 0 <- c(1), NA <- c(50,60,97,98,99))
table(BASET$BOLS_HADDa_T2)
table(BASET$Q12P2_B) #para comparar
BASET <- within(BASET, {
  turno2_ <- Recode(BOLS_HADDa_T2, '1 = "Bolsonaro"; 2 = "Haddad"', as.factor=FALSE)
})
BASET$faixa_idade <- as.numeric(BASET$D1A_FAIXAID) # necessário em todos os dados
BASET$escolaridade <- as.numeric(BASET$D3_ESCOLA)
BASET$sexo <- as.numeric(BASET$D2_SEXO)
BASET <- BASET %>%
  mutate(Feminino = case_when(sexo == "2" ~ 1,
                              TRUE ~0)) 
BASET <- BASET %>%
  dplyr::mutate(Amoedo = case_when(MULTI_T1 == "5" ~ 1,
                                   TRUE ~ 0)) 
table(BASET$Amoedo)#para conferir
BASET$D10 <- as.numeric(BASET$D10)

BASET <- BASET %>%
  mutate(Evangelico = case_when(D10 == "5" ~ 1,
                                TRUE ~ 0))%>%
  mutate(Catolico = case_when(D10 == "3" ~ 1,
                              TRUE ~ 0))%>%
  mutate(Ateu_Agnostico = case_when(D10 == "96" ~ 1,
                                    TRUE ~ 0))
BASET$REG <- as.numeric(BASET$REG)

BASET <- BASET %>%
  mutate(Sudeste = case_when(REG == "3" ~ 1,
                             TRUE ~ 0))%>%
  mutate(Norte = case_when(REG == "1" ~ 1,
                           TRUE ~ 0))%>%
  mutate(Sul = case_when(REG == "4" ~ 1,
                         TRUE ~ 0))%>%
  mutate(Nordeste = case_when(REG == "2" ~1,TRUE ~ 0)) 


BASET$D12A <- as.numeric(BASET$D12A)

BASET <- BASET %>%
  mutate(Branco = case_when(D12A == "3" ~ 1,
                            TRUE ~0)) 

BASET <- subset(BASET, select = c(faixa_idade, Feminino, escolaridade, Amoedo, Evangelico, Catolico, Ateu_Agnostico, Sul,
                                  Sudeste, Norte, Nordeste, Branco, turno2, turno2_))

Amoedo <- BASET %>%
  filter(Amoedo == "1")
rm(ESEB2018)






str(Amoedo)

mean(Amoedo$faixa_idade)
mean(BASET$faixa_idade)
mean(Amoedo$Feminino)
mean(BASET$Feminino)
mean(Amoedo$escolaridade)
mean(BASET$escolaridade)
mean(Amoedo$Evangelico)
mean(BASET$Evangelico)
mean(Amoedo$Sul)
mean(BASET$Sul)
prop.table(table(Amoedo$turno2))
prop.table(table(BASET$turno2))
mean(Amoedo$Branco)
mean(BASET$Branco)
prop.table(table(Amoedo$turno2_))
prop.table(table(BASET$turno2_))


ESEB2018 <- read_sav("C:/Users/grego/Dropbox/doutorado/2021.1 disciplinas/Metodologia Quantitativa avançada/paper/04622.sav")
Amoedo2 <- ESEB2018 %>%
  filter(Q12P1_B == 10)
table(ESEB2018$Q10A)
prop.table(table(ESEB2018$Q10A))
prop.table(table(Amoedo2$Q10A))


summary(ESEB2018$D1A_ID)
summary(Amoedo2$D1A_ID)


table(ESEB2018$Q1)
prop.table(table(ESEB2018$Q1))
prop.table(table(Amoedo2$Q1))

table(ESEB2018$Q1501)
prop.table(table(ESEB2018$Q1501))
prop.table(table(Amoedo2$Q1501))

table(ESEB2018$Q1701)
prop.table(table(ESEB2018$Q1701))
0.330007981
prop.table(table(Amoedo2$Q1701))
0.45714286


table(ESEB2018$P19)
prop.table(table(ESEB2018$P19))
prop.table(table(Amoedo2$P19))

table(ESEB2018$P20)
prop.table(table(ESEB2018$P20))
prop.table(table(Amoedo2$P20))


table(ESEB2018$P22)
prop.table(table(ESEB2018$P22))
prop.table(table(Amoedo2$P22))

table(ESEB2018$P24)
prop.table(table(ESEB2018$P24))
prop.table(table(Amoedo2$P24))


str(ESEB2018$D9A)
prop.table(table(ESEB2018$D9A))
prop.table(table(Amoedo2$D9A))


str(ESEB2018$P1102)
prop.table(table(ESEB2018$P1102))
prop.table(table(Amoedo2$P1102))


str(ESEB2018$D4)
prop.table(table(ESEB2018$D4))
prop.table(table(Amoedo2$D4))

prop.table(table(ESEB2018$OCUPA))
prop.table(table(Amoedo2$OCUPA))


str(ESEB2018$localidade)
prop.table(table(ESEB2018$localidade))
prop.table(table(Amoedo2$localidade))



MODELO <- glm(Amoedo ~ escolaridade + Sul + Nordeste + Sudeste + Norte + Branco, data = BASET,
              family=binomial(link=logit))
tab_model(MODELO, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

z.out1 <- zelig(Amoedo ~ escolaridade + Sul + Nordeste + Sudeste + Norte + Branco,
                model = "logit", data = BASET, cite = FALSE)
x.out1 <- Zelig::setx(z.out1,escolaridade = 9)
s.out1 <- sim(z.out1, x = x.out1)
summary(s.out1)



MODELO2 <- glm(Amoedo ~ escolaridade + Branco, data = BASET,
              family=binomial(link=logit))
tab_model(MODELO2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")


MODELO3 <- glm(Amoedo ~ escolaridade, data = BASET,
               family=binomial(link=logit))
tab_model(MODELO3, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
library(huxtable)
huxreg(MODELO3, MODELO2, MODELO)
MODELO4 <- glm(Amoedo ~ escolaridade + turno2, data = BASET,
               family=binomial(link=logit))
tab_model(MODELO4, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")



table(ESEB2018$Q10B)
table(Amoedo2$Q10B)
prop.table(table(Amoedo2$Q10B))
prop.table(table(ESEB2018$Q10B))

table(ESEB2018$Q12P1_B)

Novo <- ESEB2018 %>%
  filter(Q10B == 30)



table(Novo$Q12P1_B)
prop.table(table(Novo$Q12P1_B))
prop.table(table(ESEB2018$localidade))
prop.table(table(Novo$localidade))

#Novo <- Novo + Amoedo2

Novo <- full_join(Novo, Amoedo2)

prop.table(table(ESEB2018$Q10B))
prop.table(table(Novo$Q10B))
prop.table(table(Novo$Q12P1_B))
prop.table(table(ESEB2018$Q12P1_B))
prop.table(table(ESEB2018$localidade))
prop.table(table(Novo$localidade))           


ESEB2018$Novox <- ESEB2018$Q10B == 30
table(ESEB2018$Novox)
ESEB2018$Novoy <- ESEB2018$Q12P1_B == 10
table(ESEB2018$Novoy)
ESEB2018$Novo <- ESEB2018$Novox + ESEB2018$Novoy
table(ESEB2018$Novo)
ESEB2018$Novo<- memisc::recode(as.factor(ESEB2018$Novo), 0 <- c (0), 1 <- c(1,2))
table(ESEB2018$Novo)

rm(Amoedo, Amoedo2, BASET, MODELO, MODELO2, MODELO3, MODELO4, Novo, s.out1, x.out1, z.out1)


BASET <- subset(ESEB2018, select = c(D1A_FAIXAID, D2_SEXO, D3_ESCOLA, Novo, Q12P2_B, D10, D12A, localidade, Q1501, P20))

table(BASET$P20)
str(BASET$P20)#1 apoio a lava-jato

BASET$lava<- memisc::recode(as.factor(BASET$P20), 1 <- c (1), 2 <- c(2,8,9))
BASET <- within(BASET, {
  lavajato_combatecorrupcao <- Recode(lava, '1 = "Combate"; 2 = "NaoCombate"', as.factor=TRUE)
})


BASET$BOLS_HADD_T2<- memisc::recode(as.factor(BASET$Q12P2_B), 1 <- c (2), 2 <- c(1,50,60,97,98,99))
table(BASET$BOLS_HADD_T2)
table(BASET$Q12P2_B) #para comparar
BASET <- within(BASET, {
  turno2 <- Recode(BOLS_HADD_T2, '1 = "Bolsonaro"; 2 = "N"', as.factor=TRUE)
})

BASET$faixa_idade <- as.numeric(BASET$D1A_FAIXAID) # necessário em todos os dados
BASET$escolaridade <- as.numeric(BASET$D3_ESCOLA)
BASET$sexo <- as.numeric(BASET$D2_SEXO)
BASET <- BASET %>%
  mutate(Feminino = case_when(sexo == "2" ~ 1,
                              TRUE ~0)) 
BASET$D10 <- as.numeric(BASET$D10)

BASET$anti<- memisc::recode(as.factor(BASET$Q1501), 1 <- c (0), 0 <- c(1,2,3,4,5,6,7,8,9,10,96,97,98,99))

BASET <- BASET %>%
  mutate(AntiPT = case_when(anti == "1" ~ 1,
                              TRUE ~0))

BASET$D12A <- as.numeric(BASET$D12A)

BASET <- BASET %>%
  mutate(Branco = case_when(D12A == "3" ~ 1,
                            TRUE ~0)) 

BASET <- subset(BASET, select = c(faixa_idade, Feminino, escolaridade, localidade, turno2, Novo, AntiPT, Branco, lavajato_combatecorrupcao))
table(ESEB2018$localidade)
dados <- BASET
rm(BASET, ESEB2018)

# analise previa ####
library(writexl)
write_xlsx(dados, "D:/ATUALIZA_PASTA_d/Amoedistas no Brasil/merge.xlsx" )

str(dados)

MODELO1 <- glm(Novo ~ escolaridade, data = dados,
              family=binomial(link=logit))
tab_model(MODELO1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

MODELO2 <- glm(Novo ~ escolaridade + turno2 + Branco, data = dados,
               family=binomial(link=logit))
tab_model(MODELO2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

MODELO2 <- glm(Novo ~ escolaridade + turno2 + Branco +, data = dados,
               family=binomial(link=logit))
tab_model(MODELO2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

library(coefplot)
coefplot(MODELO2, intercept = F)


# analise ####
dados$turno2 <- relevel(dados$turno2, "N")
detach("package:memisc", unload = TRUE)

# modelo demografico
escolaridade <- glm(Novo ~ escolaridade, data = dados,
               family=binomial(link=logit))
tab_model(escolaridade, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

idade <- glm(Novo ~ faixa_idade, data = dados,
             family=binomial(link=logit))
tab_model(idade, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

Feminino <- glm(Novo ~ Feminino, data = dados,
             family=binomial(link=logit))
tab_model(Feminino, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

Branco <- glm(Novo ~ Branco, data = dados,
                family=binomial(link=logit))
tab_model(Branco, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

URBANO <- glm(Novo ~ localidade, data = dados,
             family=binomial(link=logit))
tab_model(URBANO, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

demografia1 <- glm(Novo ~ escolaridade + faixa_idade, data = dados,
                   family=binomial(link=logit))
tab_model(demografia1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")


demografia2 <- glm(Novo ~ escolaridade + faixa_idade + Branco + Feminino + localidade, data = dados,
                      family=binomial(link=logit))
tab_model(demografia2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

tab_model(escolaridade, demografia1, demografia2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

# modelo político
bolsonaro <- glm(Novo ~ turno2, data = dados,
                 family=binomial(link=logit))
tab_model(bolsonaro, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

prop.table(table(dados$Novo, dados$AntiPT))

antipt <- glm(Novo ~ AntiPT, data = dados,
              family=binomial(link=logit))
tab_model(antipt, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

dados$lavajato_combatecorrupcao <- relevel(dados$lavajato_combatecorrupcao, "NaoCombate")
lavajato <- glm(Novo ~ lavajato_combatecorrupcao, data = dados,
              family=binomial(link=logit))
tab_model(lavajato, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")


politico <- glm(Novo ~ AntiPT + lavajato_combatecorrupcao + turno2, data = dados,
              family=binomial(link=logit))
tab_model(politico, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")



# modelo full (com as categoriais sig)

full <- glm(Novo ~ AntiPT + lavajato_combatecorrupcao + escolaridade, data = dados,
            family=binomial(link=logit))
tab_model(full, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
#(odds-1)*100=prob%
 #AntiPt
(2.9-1)*100# Ser AntiPT aumenta as chances de se gostar do Novo e/ou 
#ter votado em Amoedo em 190%
(2.36-1)*100 
#Achar que a Lava Jato combate a corrupção aumenta as chances 
#de se gostar do Novo e/ou ter votado em Amoedo em 136%
(1.35-1)*100
#AUm nível a mais de escolaridade aumenta(são nove) as chances 
#de se gostar do Novo e/ou ter votado em Amoedo em 35%

coefplot(full, intercept = FALSE)

