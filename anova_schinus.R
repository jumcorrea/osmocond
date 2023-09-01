dados<-read.csv2("anova_schinus.csv")

summary(dados)

names(dados)

#install.packages("dplyr")
library(dplyr)

#mostra uma amostra aleatoria dos seus dados
dplyr::sample_n(dados, 10)

#checa a sua estrutura de dados
str(dados)

#constroi o modelo de %g pra ver se a distribuicao dos residuos eh normal
modeloxg<-lm(X.g~trat+temb+trat:temb, data=dados)
summary(modeloxg)

shapiro.test(modeloxg$residuals)
#quando p-value > 0.05, seus dados (ou seus residuos) tem distribuicao normal, 
  #entao voce pode usar analise de variancia


#analise de variancia do %g sem interacao, para porcentagem de germinacao
res.aov<-aov(X.g ~ trat + temb, data=dados)
res.aov
summary(res.aov)

#anova do %g com interacao
res.aov2 <- aov(X.g ~ trat + temb + trat:temb , data=dados)
res.aov2
summary(res.aov2)
#nao teve significancia, nem com e nem sem interacao

#constroi o modelo de tmg pra ver se a distribuicao dos residuos eh normal
modelotmg<-lm(tmg~trat+temb+trat:temb, data=dados)
summary(modelotmg)

shapiro.test(modelotmg$residuals)
#quando p-value > 0.05, seus dados (ou seus residuos) tem distribuicao normal, 
  #entao voce pode usar analise de variancia

# anova do tmg sem interacao
res.aov.tmg <- aov(tmg ~ trat+temb, data=dados)
res.aov.tmg
summary(res.aov.tmg)

#anova do tmg com interacao
res.aov.tmg2 <- aov(tmg ~ trat + temb + trat:temb, data=dados)
res.aov.tmg2
summary(res.aov.tmg2)
  #nao teve significancia, nem com e nem sem interacao

#constroi o modelo de ivg pra ver se a distribuicao dos residuos eh normal
modeloivg<-lm(ivg~trat+temb+trat:temb, data=dados)
summary(modeloivg)

shapiro.test(modelotmg$residuals)
#quando p-value > 0.05, seus dados (ou seus residuos) tem distribuicao normal, 
  #entao voce pode usar analise de variancia

# anova do ivg sem interacao
res.aov.ivg <- aov(ivg ~ trat+temb, data=dados)
res.aov.ivg
summary(res.aov.ivg)

#desdobramento da variancia em ivg = houve significancia a 1% para temb
des_ivg<-aov(ivg~temb, data=dados)
des_ivg
summary(des_ivg, split=list('temb'= list('12'=1, '24' =2,'48' =3,'72' =4, '96' =5,'120' =6)))

# anova do ivg sem interacao
res.aov.ivg2 <- aov(ivg ~ trat+temb+ trat:temb, data=dados)
res.aov.ivg2
summary(res.aov.ivg2)

#desdobramento da variancia em ivg = houve significancia a 1% para temb
des_ivgi<-aov(ivg~temb, data=dados)
des_ivgi
summary(des_ivgi, split=list('temb'= list('12'=1, '24' =2,'48' =3,'72' =4, '96' =5,'120' =6)))
