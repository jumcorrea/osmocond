dados<-read.csv2("anova_paudalho.csv")

dados$tmg<-as.numeric(dados$tmg)
summary(dados)

names(dados)
#dados$X<-NULL

#install.packages("dplyr")
library(dplyr)

#mostra uma amostra aleat?ria dos seus dados
dplyr::sample_n(dados, 10)

#checa a sua estrutura de dados
str(dados)

#teste de normalidade dos dados
shapiro.test(dados$X.g)
#quando p-value > 0.05, seus dados (ou seus res?duos) tem distribui??o normal, ent?o voc? pode usar an?lise de vari?ncia
shapiro.test(dados$tmg)



#construindo o modelo para an?lise de vari?ncia
#modelo<-X.g~trat+temb

#an?lise de vari?ncia do %g sem intera??o, para porcentagem de germinacao
res.aov<-aov(X.g ~ trat + temb, data=dados)
res.aov
summary(res.aov)

#anova do %g com intera??o
res.aov2 <- aov(X.g ~ trat + temb + trat:temb , data=dados)
res.aov2
summary(res.aov2)

# anova do tmg sem intera??o
res.aov.tmg <- aov(tmg ~ trat+temb, data=dados)
res.aov.tmg
summary(res.aov.tmg)

#desdobramento da variancia em tmg = houve significancia para temb
des_tmg<-aov(tmg~temb, data=dados)
des_tmg
summary(des_tmg, split=list('temb'= list('12h'=1, '24h' =2,'48h' =3,'72h' =4, '96h' =5,'120h' =6)))

#anova do tmg com intera??o
res.aov.tmg2 <- aov(tmg ~ trat + temb + trat:temb, data=dados)
res.aov.tmg2
summary(res.aov.tmg2)

