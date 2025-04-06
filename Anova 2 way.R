#Passo 1: Baixar os pacotes no R

##Manipulação de dados

install.packages("dplyr")
library(dplyr)

##Teste levene

install.packages("car")
library(car)

##Ver estatistícas descritivas e identificar possíveis Outliers

install.packages("rstatix")
library(rstatix)

##Fazer pós-hocs

install.packages("DescTools")
library(DescTools)

##Médias marginais estimadas

install.packages("emmeans")
library(emmeans)

##Criação do gráfico

install.packages("ggplot2")
library(ggplot2)

#Passo 2: Setar o diretório de trabalho e carregar o banco de dados

##Importante saber o diretório em que está o banco de dados
##Pode fazer isso manualmente: Session > Set Working Directory > Choose Directory

setwd("C:/Users/Lenovo/OneDrive/Área de Trabalho/Levi/Nova pasta")

dados <- read.csv2('exemplo_anova_2_w.csv')
View(dados)
glimpse(dados)

#Passo 3: Verificação dos pressupostos nos dados brutos

##Verificação da normalidade - Shapiro por grupo:

dados %>% group_by(Ambiente, Tamanho) %>% 
  shapiro_test(Nsementes)

##Verificação da presença de Outliers por grupo
###Cálculo feito excluindo os quartis cálculados excluindo a mediana
boxplot(dados$Nsementes ~ dados$Ambiente:dados$Tamanho)

###Essa função inclui a mediana
library(rstatix)
dados %>% group_by(Ambiente, Tamanho) %>% 
  identify_outliers(Nsementes)

##Verificação da homogeneidade de variância - teste de Levene (pacote car)

library(car)
leveneTest(Nsementes ~ Ambiente*Tamanho, dados, center = mean)

#Observação:
#Por default, o teste realizado pelo pacote car tem como base a mediana (median)
#O teste baseado na mediana é mais robusto
#Mudado para ser baseado na média (comparado ao SPSS)

#Passo 5: Verificação dos pressupostos nos resíduos

##Construção do modelo:
modelo <- aov(Nsementes ~ Ambiente*Tamanho, dados)

##teste de normalidade para os resíduos
shapiro.test(modelo$residuals)

##Verificação da presença de outliers entre os resíduos:
boxplot(modelo$residuals)

dados$Residuos <- modelo$residuals

dados %>% group_by(Ambiente, Tamanho) %>% 
  identify_outliers(Residuos)

##Verificação da homogeneidade das variâncias - teste de Levene (pacote car)
leveneTest(Residuos ~ Ambiente*Tamanho, dados, center = mean)

#Passo 5: Realização da ANOVA

##Mudança no contraste para equivaler ao SPSS:
options(contrast = c("contr.sum", "contr.poly"))

##criação do modelo:
modelo <- aov(Nsementes ~ Ambiente*Tamanho, dados)
summary(modelo)
Anova(modelo, type='III')

#Passo 6: Estimated Marginal Means (Pacote emmeans)

dados %>% 
  group_by(Ambiente) %>% 
  emmeans_test(Nsementes ~ Tamanho, p.adjust.method = "hsd")
#Outra opção: correção de Sidak

dados %>% 
  group_by(Tamanho) %>% 
  emmeans_test(Nsementes ~ Ambiente, p.adjust.method = "hsd")

#Passo 7: Análise de post-hoc (Pacote DescTools)
#Post-hocs permitidos: 'hsd', 'bonferroni', 'lsd', 'scheffe', 'newmankeuls', 'duncan'
library(DescTools)
##uso do ducan
PostHocTest(modelo, method = "duncan")

#Uso do TukeyHSD
PostHocTest(modelo, method = "hsd")

#Uso do Bonferroni
PostHocTest(modelo, method = "bonf")


#Passo 8: Gráfico de interação
library(ggplot2)
##Com Ambientes com cores diferentes
ggplot(dados, aes(x = Tamanho, y = Nsementes, group = Ambiente, color = Ambiente)) + 
  geom_line(stat = "summary", fun.data = "mean_se", size = 0.6) +
  geom_point(stat = "summary", fun.y = "mean") + 
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2)

##Com Ambientes com linhas diferentes
ggplot(dados, aes(x = Tamanho, y = Nsementes, group = Ambiente)) + 
  geom_line(stat = "summary", fun.data = "mean_se", size = 0.6, aes(linetype = Ambiente)) +
  geom_point(stat = "summary", fun.y = "mean", size = 2, aes(shape = Ambiente)) + 
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2)

