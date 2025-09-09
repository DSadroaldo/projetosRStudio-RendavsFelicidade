#=========================================================================#
#                         MODELAGEM ESTATÍSTICA                           #
#                        Mapa - Cars                                      #
#-------------------------------------------------------------------------#
#                                                                         #                                                   
#-------------------------------------------------------------------------#

#------------------------------------------------------------------------
# limpar variáveis
rm(list=ls())
#------------------------------------------------------------------------

# Definir diretório onde está o arquivo de dados.
#setwd("/cloud/project")
# 
# 
# data = read.csv(file ='PISA2015a.csv', 
#                 header = T, 
#                 fileEncoding="UTF-8-BOM",
#                 sep = ";", 
#                 na.strings = '..')

# carregando nosso banco cars, como eh nativo não precisa não read.csv() nem setwd().
data('cars')
# atribuindo uma variavel ao nosso banco
df <- cars
# apresentando cabeçalho
kable(head(df))

# comando importante, a 'Str' dos dados vai me trazer a estrutura de cada uma das variáveis
# em nossas Analise temos tipo de informações do tipo texto, e isso, não é interessante para nossa análise. Precisamos, então, transformá-las gerar variáveis do tipo 'Fatores'.

str(df) # antes das transformações

# A função names(data) no R é de fundamental importância para entender e manipular um conjunto de dados (um data frame ou lista). Ela retorna os nomes das colunas (ou elementos, no caso de uma lista) do objeto data.
names(df)

# aqui estamos pegando a variavel (country/chr) do dataframe e colocando igual a um fator com mesmo nome. Mesma lógica utilizada na variável (Series)
data$Country     = as.factor(data$Country)
data$Series.Code = as.factor(data$Series.Code)

str(data) # validamos agora as transformações
#------------------------------------------------------------------------


#------------------------------------------------------------------------
#Os pacotes do Tidyverse no R são uma coleção de pacotes inter-relacionados para ciência de dados. Eles são projetados para trabalhar juntos de forma consistente, tornando a manipulação, visualização e análise de dados mais fácil e intuitiva. A filosofia central do Tidyverse é a de "dados arrumados" (tidy data), onde cada variável tem sua própria coluna, cada observação tem sua própria linha e cada tipo de unidade observacional tem sua própria tabela.

# install.packages("tidyverse") - > como ja temos instalado
library(tidyverse)

# conseguimos perceber que há colunas q não são importantes pra nossa analise.
view(df)


# podemos filtrar esse nosso dataset, onde colocamos todas linhas mas colunas especificas. OUtra coisa podemos transformar em alias as colunas de forma a ficar mais fascilitado a leitura delas.
#Depois de todas as operações de spread e rename, a função drop_na() será aplicada ao df. Isso significa que, se alguma linha em df tiver um NA em qualquer uma das colunas resultantes (MAT, MAT.F, etc.), essa linha será completamente removida do df final.

df <- data[1:1161, c(1, 4, 7)] %>%  
  spread(key=Series.Code, value=YR2015)  %>%  
  rename(MAT = LO.PISA.MAT,                        
         MAT.F = LO.PISA.MAT.FE,
         MAT.M = LO.PISA.MAT.MA,
         LEIT = LO.PISA.REA,
         LEIT.F = LO.PISA.REA.FE,
         LEIT.M = LO.PISA.REA.MA,
         CIENCIA = LO.PISA.SCI,
         CIENCIA.F = LO.PISA.SCI.FE,
         CIENCIA.M = LO.PISA.SCI.MA
  ) %>%
  drop_na()


head(df)

view(df)

#------------------------------------------------------------------------
#------------------------------------------------------------------------
# Medidas Descritivas
# 
# attach(df)
# 
# descritivas = function(x)list(
#   Média=mean(x),
#   Mediana=median(x),
#   Max.Min=range(x),
#   Amplitude=max(x) - min(x),
#   Variância=var(x),
#   DesvioPadrão=sd(x),
#   CoeficienteVariação=sd(x)/mean(x)*100,
#   Quantis=quantile(x)
# )
# 
# 
# descritivas(MAT)

# -----------------------------------------------------------------------
# Medidas Descritivas variaveis numericas speed dist()
# Função de medidas descritivas
# Função de medidas descritivas

# Para melhor analise das variaveis me atentei as duas medidas(speed, dist) para cada variavel (speed, dist) transformando-as em fatores, isto é atentei a condição do tempo que cada carro levou para todo o processo de frenagem considerando sua velocidade.
# 
# Algumas importâncias a considerarmos nos dados de teste da base
# # a velocidade possuem como unidade de medida MPH (milhas por hora) enquanto a distância fora utilizado (distancia de frenagem em pés)

descritivas <- function(x) list(
  Média = mean(x),                 
  Mediana = median(x),             
  Máximo = max(x),
  Mínimo = min(x),
  Amplitude = max(x) - min(x),
  Variância = var(x),
  Desvio_Padrão = sd(x),
  Coef_Variação = sd(x)/mean(x) * 100,
  Quantis = quantile(x)
)

# Aplicando nas variáveis
descritivas(df$speed)
descritivas(df$dist)

#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Visualisar os dados!

install.packages("ggplot2")
library("ggplot2")


#Rank de Matemática por Pais
# x11 é uma funcionalidade voltada ao windows, dando automonomia para visualizarmos em uma aba fora, o q fascilita a analise dos dados.
# nesse nosso caso temos uma dificuldade maior pq no rstudio cloud No RStudio Cloud  não funciona, porque o servidor não tem acesso a um "display gráfico" externo.Então comentamos o x11()

# #x11()
# ggplot(data=df,aes(x=reorder(Country,MAT),y=MAT)) + 
#   geom_bar(stat ='identity',aes(fill=MAT))+
#   coord_flip() + 
#   theme_grey() + 
#   scale_fill_gradient(name="Nível Pontuação")+
#   labs(title = 'Rank de Países por pontuação em Matemática',
#        y='Pontuação',x='Países')+ 
#   geom_hline(yintercept = mean(df$MAT),size = 1, color = 'blue')



ggplot(data=df,aes(x=dist,y=speed)) + 
  geom_bar(stat ='identity',aes(fill=speed))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="Nível Pontuação")+
  labs(title = 'Representa os carros em função da velocidade',
       y='velocidade(speed)',x='distância(dist)')+ 
  geom_hline(yintercept = mean(df$speed),size = 1, color = 'blue')

# Gráfico de dispersão com linha de tendência
ggplot(df, aes(x = speed, y = dist)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.7) +   # pontos
  geom_smooth(method = "lm", se = TRUE, color = "red") +    # linha de regressão
  theme_minimal() +
  labs(
    title = "Relação entre Velocidade e Distância de Frenagem",
    x = "Velocidade (mph)",
    y = "Distância de Frenagem (pés)"
  )

ggplot(data = df, aes(x = reorder(speed, dist), y = dist, fill = speed)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Deixa o gráfico horizontal
  theme_grey() +
  scale_fill_gradient(name = "Velocidade (mph)") +
  labs(
    title = "Distância de Frenagem por Velocidade",
    x = "Velocidade (mph)",
    y = "Distância de Frenagem (pés)"
  ) +
  geom_hline(yintercept = mean(df$dist), size = 1, color = "blue") # linha média


#Rank de Matemática por Pais
# obs, tudo q temos para MAT alteramos para outra variavel
#x11()
ggplot(data=df,aes(x=reorder(Country,LEIT),y=LEIT)) + 
  geom_bar(stat ='identity',aes(fill=LEIT))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="Nível Pontuação")+
  labs(title = 'Rank de Países por pontuação em Leitura',
       y='Pontuação',x='Países')+ 
  geom_hline(yintercept = mean(df$LEIT),size = 1, color = 'blue')

#------------------------------------------------------------------------
# BOX PLOT

# O BoxPlot precisa de dúas variáveis

df2 = df[,c(1,3,4,6,7,9,10)] %>%   # Colunas relevantes 
  pivot_longer(c(2,3,4,5,6,7),names_to = 'Score')
view(df2) 



x11()
ggplot(data = df2, aes(x=Score,y=value, color=Score)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Maior pontuação Masculino x Feminino',
       y='Pontuação',x='Tipo do Teste')

#------------------------------------------------------------------------
# Calcular a porcentagem de diferença entre Masculino e Feminino

df = df %>% mutate(MAT.Diff = ((MAT.M - MAT.F)/MAT.F)*100,
                   LEIT.Diff = ((LEIT.M - LEIT.F)/LEIT.F)*100,
                   CIENCIA.Diff = ((CIENCIA.M - CIENCIA.F)/CIENCIA.F)*100,
                   Total.Score = MAT + LEIT + CIENCIA,
                   Avg.Diff = (MAT.Diff+LEIT.Diff+CIENCIA.Diff)/3
)
view(df)


##### MATHS SCORE #####
x11()
ggplot(data=df, aes(x=reorder(Country, MAT.Diff), y=MAT.Diff)) +
  geom_bar(stat = "identity", aes(fill=MAT.Diff)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = mean(df$MAT.Diff), size=1, color="black") +
  scale_fill_gradient(name="% Nível de Diferença") +
  labs(title="Masculino x Feminino em Matemática", x="", 
       y="% diferença para Feminino")


##### Reading SCORE #####
x11()
ggplot(data=df, aes(x=reorder(Country, LEIT.Diff), y=LEIT.Diff)) +
  geom_bar(stat = "identity", aes(fill=LEIT.Diff)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = mean(df$LEIT.Diff), size=1, color="black") +
  scale_fill_gradient(name="% Nível de Diferença") +
  labs(title="Masculino x Feminino em Leitura", x="", 
       y="% diferença para Feminino")






#------------------------------------------------------------------------
#------------------------------------------------------------------------
install.packages("tidyverse")
library("tidyverse")

# Banco de Dados diamonds
data(diamonds)


head(diamonds)

summary(diamonds)

attach(diamonds)

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

descritivas(carat)
descritivas(price)

#HISTOGRAMA - Preço
x11()
ggplot(data=diamonds, aes(x=price)) +
  geom_histogram(fill="steelblue", color="black", binwidth = 2000) +
  ggtitle("Histogram of Price Values")



x11()
ggplot(data=diamonds, aes(x=carat)) +
  geom_histogram(fill="steelblue", color="black", binwidth = 0.05) +
  ggtitle("Histogram of Price Values")


#DISPERSÃO CARAT x Preço
x11()
ggplot(data=diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_point()


#BOXPLOT PREÇO, separado pelo CORTE
x11()
ggplot(data=diamonds, aes(x=cut, y=price)) + 
  geom_boxplot(fill="steelblue")


# HISTOGRAMA POR CORTE
ggplot(data = diamonds, aes(price,fill= cut))+
  scale_fill_brewer(type = 'qual')+
  geom_histogram(bins = 100)+
  scale_x_log10()+
  facet_wrap(~color, ncol=3)



#modelo utilizado para regressao lienar

# Ajustar modelo de regressão linear
modelo <- lm(dist ~ speed, data = df)


# Resumo do modelo
summary(modelo)


# model estatistico usando regressão   
library(ggplot2)

ggplot(df, aes(x = speed, y = dist)) +
  geom_point(color = "blue", size = 2) +   # pontos reais
  geom_smooth(method = "lm", col = "red") + # reta do modelo
  labs(
    title = "Relação entre Velocidade e Distância de Frenagem",
    x = "Velocidade (mph)",
    y = "Distância de Frenagem (pés)"
  ) +
  theme_minimal()