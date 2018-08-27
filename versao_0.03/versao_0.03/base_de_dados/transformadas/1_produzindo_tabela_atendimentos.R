options(encoding = "UTF-8")
#Pacotes
library(tidyverse)
library(readr)
library(data.table)
library(zoo)
library(Kmisc) #pacote para manipulação de string
library(lubridate)
library(dplyr)
library(reshape2)


#PREPARANDO A BASE PARA O CÁLCULO DO NÚMERO DE ATENDIMENTOS
atendimentos <- read_csv("base_de_dados/extraidas/painel_upa.csv", locale = locale(encoding = 'UTF-8'))


#Serão calculados o número de atendimentos médicos, na clínica, na pediatria, na cirurgia, na odontologia e classificações de risco por mês, dia da semana e hora do dia.
#Juntando data e hora do atendimento do início do atendimento
atendimentos$INI_ATD <- paste(atendimentos$DIA_INICIO_ATD, atendimentos$HORA_INICIO_ATD)
atendimentos$INI_ATD <- as.POSIXct(atendimentos$INI_ATD, format = "%d/%m/%Y %H:%M:%S")
#Só será utilizado o ini_atd
atendimentos <- select(atendimentos,-c(DIA_CHEGADA, HORA_CHEGADA, DIA_INICIO_ATD, HORA_INICIO_ATD, DIA_FIM_ATD, HORA_FIM_ATD, CD_PROFISSIONAL, CD_USUARIO))

#Trocando código por nome das UPAS
atendimentos$UNIDADE <- ifelse(atendimentos$UNIDADE == 55, "UPA Sul", "UPA Norte")

#Criando a tabela de médicos e odontológicos e especialidades médicas
atendimentos_med_odont <- select(atendimentos, -c(PROCEDIMENTO, CLASSIFICACAO_RISCO))
atendimentos_med_odont <- unique(atendimentos_med_odont)#Retirando as células que são repetidas por causa dos procedimentos (uma consulta pode ter mais de um procedimento)


#Importando tabela de especialidades
especialidade <- read_csv("base_de_dados/extraidas/especialidade.csv")
#Transformando as várias especialidades em clíncia, cirurgia, pediatria e odontologia
atendimentos_med_odont <- merge(atendimentos_med_odont, especialidade, by = "ESPECIALIDADE_ATD", all.x = T)
atendimentos_med_odont <- select(atendimentos_med_odont, -c(ESPECIALIDADE_ATD ,CD_CONSULTA))
atendimentos_med_odont <- na.omit(atendimentos_med_odont)


#Componto os bancos de atendimentos de médicos e enfermeiros
#Médico
atendimentos_med <- subset(atendimentos_med_odont, atendimentos_med_odont$ESPECIALIDADE != "Odontologia")

#Odontologia
atendimentos_odont <- subset(atendimentos_med_odont, atendimentos_med_odont$ESPECIALIDADE == "Odontologia")
atendimentos_odont <- select(atendimentos_odont,-c(ESPECIALIDADE))

#Componto os bancos de classificações
#Serão analiados apenas as classificações cujo código do procedimento é 0301060118, que é o código aceito pela 
#Portaria Nº 10, de 3 de janeiro de 2017
atendimentos$PROCEDIMENTO <- as.character(atendimentos$PROCEDIMENTO)
atendimentos_classificacao <- subset(atendimentos, atendimentos$PROCEDIMENTO == "0301060118")
atendimentos_classificacao <- unique(atendimentos_classificacao)#Retirando a possibilidade de duplicação nas observações
atendimentos_classificacao <- select(atendimentos_classificacao,-c(PROCEDIMENTO, ESPECIALIDADE_ATD, CD_CONSULTA))
atendimentos_classificacao <- na.omit(atendimentos_classificacao)

#Escrever bancos de dados
write.csv(atendimentos_med, "base_de_dados/transformadas/atendimentos_med.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_odont, "base_de_dados/transformadas/atendimentos_odont.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_classificacao, "base_de_dados/transformadas/atendimentos_classificacao.csv", fileEncoding = "UTF-8", row.names = F)





