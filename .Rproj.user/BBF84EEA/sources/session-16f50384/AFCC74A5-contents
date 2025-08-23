#=========================================================================#
#                         MODELAGEM ESTATÍSTICA                           #
#                                AULA 2                                   #
#-------------------------------------------------------------------------#
#                                                                         #                                                   
#-------------------------------------------------------------------------#

#------------------------------------------------------------------------
# limpar variáveis
rm(list=ls())
#------------------------------------------------------------------------
# Pacotes Necessários

install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
install.packages("nima")

library("ggplot2")
library("dplyr")
library("broom")
library("ggpubr")
library("nima")

#------------------------------------------------------------------------
# Definir diretório onde está o arquivo de dados.
setwd("/cloud/project")

# "chamar" conjunto de dados
data = read.csv(file ='renda.data.csv', 
                header = T, 
                fileEncoding="UTF-8-BOM",
                sep = ";", 
                na.strings = '..')


# como esta base de dados não e nativa do R, fica a explicação:
# Pesquisa com 498 pessoas sobre a renda (entre $15k/ano e $75k/ano)
# e Felicidade (medido entre 1 e 10)

#------------------------------------------------------------------------

# Análise Exploratória dos dados

head(data)


str(data)

summary(data)


descritivas = function(x)list(
  Média=mean(x),
  Mediana=median(x),
  Max.Min=range(x),
  Amplitude=max(x) - min(x),
  Variância=var(x),
  DesvioPadrão=sd(x),
  CoeficienteVariação=sd(x)/mean(x)*100,
  Quantis=quantile(x)
)


attach(data)

descritivas(Renda)
descritivas(Felicidade)


hist(Felicidade)



x11()
ggplot(data = data, aes(x = Felicidade)) +
  geom_histogram(fill = "steelblue", color = "black") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(2) )) +
  labs(title = "Histograma Felicidade", x = "Felicidade", y = "Frequência")


hist(Renda)

x11()
ggplot(data = data, aes(x = Renda)) +
  geom_histogram(fill = "steelblue", color = "black") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(2) )) +
  labs(title = "Histograma Renda", x = "Renda", y = "Frequência")



plot(Renda, Felicidade)

ggplot(data=data, aes(x=Felicidade, y=Renda)) + 
  geom_point() +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(2) )) +
  labs(title = "Diagrama de Dispersão", x = "Felicidade",
       y = "Renda")




  
4#------------------------------------------------------------------------
# Construção do Modelo

Modelo1 = lm(Felicidade ~ Renda, data = data)

summary(Modelo1)

#------------------------------------------------------------------------
# Qualidade do Ajuste - Resíduos


# Histograma dos Resíduos - Normalidade
x11()
ggplot(data = data, aes(x = Modelo1$residuals)) +
  geom_histogram(fill = "steelblue", color = "black") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(2) )) +
  labs(title = "Histograma dos Resíduos", x = "Resíduos", y = "Frequência")


# QQ-Plot - Normalidade

x11()
qq_plot(Modelo1$residuals)+ theme_bw() +
  theme(axis.text.x=element_text(size=rel(1)), 
        text = element_text(size = 20)) +
  labs(title = 'QQ-Plot - Resíduos', 
       x = 'Quantis Teóricos', y = 'Quantis dos Resíduos do Modelo')

# Teste Shapiro Wilk - Normalidade
shapiro.test(Modelo1$residuals)

#Gráficos do Modelo - Homoscedasticidade
x11()
par(mfrow=c(2,2))
plot(Modelo1)


# Gráfico de Dispersão com a Reta de Regressão e a equação
x11()
ggplot(data, aes(x=Renda, y=Felicidade))+
  geom_point() + 
  geom_smooth(method="lm", col="black") +
  stat_regline_equation(label.x = 3, label.y = 7, size=10) +
  theme_bw() +
  labs(title = "Felicidade medida em função da renda",
       x = "Renda (x$10,000)",
       y = "Felicidade (0 a 10)")






# Criação do gráfico de dispersão com a inserção da reta que 
# representa o modelo e os resíduos marcados em vermelho - LIVRO (p.65)



#criação de informações de diagnóstico do modelo (chamado de model)
model.diag.metrics <- augment(Modelo1)
head(model.diag.metrics)
#criação do gráfico de dispersão com a inserção da reta que representa o modelo e os resíduos
marcados em vermelho


x11()
ggplot(model.diag.metrics, aes(Renda, Felicidade)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  geom_segment(aes(xend = Renda, yend = .fitted), color = "red", size = 1) +
  theme(axis.text.x=element_text(size=rel(1)), text = element_text(size = 20)) +
  labs(title = 'Gráfico de Dispersão', x = 'Renda', y = 'Felicidade')






summary(Modelo1)


#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------
# Ir de Bicileta para o Trabalho, fumar e doença do coração 
# em 500 cidades

# Taxa de Bike entre 1 e 75%
# Taxa de Fumar entre 0.5 e 30%
# Taxa de Doenças entre 0.5 e 20.5%



coracao = read.csv(file ='coracao.data.csv', 
                 header = T, 
                 fileEncoding="UTF-8-BOM",
                 sep = ";", 
                 na.strings = '..')
#------------------------------------------------------------------------
head(coracao)


str(coracao)

summary(coracao)
names(coracao)



descritivas(Bike)

#------------------------------------------------------------------------
# Histograma das variáveis

# Bike
x11()
ggplot(data=coracao, aes(x=Bike)) +
  geom_histogram(fill="steelblue", color="black", binwidth = 5) +
  ggtitle("Histograma - Bike")

# Fumar



# Doenca



#------------------------------------------------------------------------
# Relacionar as Variáveis

plot(Bike, Doenca)
plot(Fumar, Doenca)





# Verificar se há correlação entre as variáveis EXLICATIVAS

plot(Bike,Fumar)

# Calcula o Coeficiente de Correlação de Pearson

cor(Bike, Fumar, method="pearson")




#------------------------------------------------------------------------
# Ajustar Modelo

Modelo2 = lm(Doenca ~ Bike + Fumar, data = coracao)

summary(Modelo2)

# Outros Modelos?












#------------------------------------------------------------------------
# Qualidade do Ajuste - Resíduos


# Histograma dos Reíduos - Normalidade
x11()
ggplot(data = coracao, aes(x = Modelo2$residuals)) +
  geom_histogram(fill = "steelblue", color = "black") +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(2) )) +
  labs(title = "Histograma dos Resíduos", x = "Resíduos", 
       y = "Frequência")


# QQ-Plot - Normalidade
library("nima")

x11()
qq_plot(Modelo2$residuals)+ theme_bw() +
  theme(axis.text.x=element_text(size=rel(1)), 
        text = element_text(size = 20)) +
  labs(title = 'QQ-Plot - Resíduos', 
       x = 'Quantis Teóricos', y = 'Quantis dos Resíduos do Modelo')

# Teste Shapiro Wilk - Normalidade
shapiro.test(Modelo2$residuals)




#Gráficos do Modelo - Homoscedasticidade
x11()
par(mfrow=c(2,2))
plot(Modelo2)



# Gráfico de Dispersão + Modelo
x11()
ggplot(coracao, aes(x=Bike, y=Doenca)) +
  geom_point() +
  geom_smooth(method="lm", col="red")+
  theme_bw() +
  labs(title = "Taxas de Doença de Coração (% da população)",
       x = "Bike p/ Trabalho (% da população)",
       y = "Doença Coração (% da População)")






#------------------------------------------------------------------------
#------------------------------------------------------------------------


#------------------------------------------------------------------------
#------------------------------------------------------------------------
x11()
par(mfrow=c(2,1))

p = 0.5
q = seq(0,100,1)
y = p*q
plot(q,y,type='l',col='red',main='Relação Linear')

y = 450 + p*(q-10)^3
plot(q,y,type='l',col='navy',main='Relação NÃO-LINEAR',lwd=3)


#------------------------------------------------------------------------
#------------------------------------------------------------------------

# set.seed() para gerar valores PSEUDO ALEATÓRIOS

set.seed(20)

q = seq(from=0, to=20, by=0.1)

y = 500 + 0.4 * (q-10)^3

plot(q,y)


# Criar "barulho" nos dados
noise = rnorm(length(q), mean=10, sd=80)
noisy.y = y + noise


# Gráfico de Dispersão
plot(q,noisy.y,col='deepskyblue4',xlab='q',main='Dados Observados')
lines(q,y,col='firebrick1',lwd=3)


# Nosso modelo deve ser algo como:
# y = a*q + b*q² + c*q³

Modeloa = lm(noisy.y ~ q + I(q^2) + I(q^3))

Modelob = lm(noisy.y ~ poly(q,3))

# Note que  I(q^2) e I(q^3) irá gerar variáveis correlacionadas


summary(Modelob)

hist(Modelob$residuals)

confint(Modelob, level=0.95)
x11()
par(mfrow=c(2,2))
plot(Modelob)



#Gráfico de Dispersão com o Modelo

Preditos = predict(Modelob,data.frame(x=q),interval='confidence',
                               level=0.99)

x11()
plot(q,noisy.y,col='deepskyblue4',xlab='q',main='Dados Observados')
lines(q,Preditos[,1],col='green',lwd=3)
lines(q,Preditos[,2],col='red',lwd=1)
lines(q,Preditos[,3],col='black',lwd=1)
legend("bottomright",c("Observ.","Signal","Predicted"), 
       col=c("deepskyblue4","red","green"), lwd=3)

