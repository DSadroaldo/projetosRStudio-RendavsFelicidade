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
