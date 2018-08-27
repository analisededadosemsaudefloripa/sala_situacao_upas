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


#PREPARANDO A BASE PARA O CÁLCULO DO NÚMERO Do tempo
tempo <- read_csv("base_de_dados/extraidas/painel_upa.csv", locale = locale(encoding = 'UTF-8'))


#Serão calculados o tempo de tempo médicos, na clínica, na pediatria, na cirurgia, na odontologia e classificações de risco por mês, dia da semana e hora do dia.
#Juntando data e hora do atendimento do início e do fim do atendimento
tempo$INI_ATD <- paste(tempo$DIA_INICIO_ATD, tempo$HORA_INICIO_ATD)
tempo$INI_ATD <- as.POSIXct(tempo$INI_ATD, format = "%d/%m/%Y %H:%M:%S")
tempo$FIM_ATD <- paste(tempo$DIA_FIM_ATD, tempo$HORA_FIM_ATD)
tempo$FIM_ATD <- as.POSIXct(tempo$FIM_ATD, format = "%d/%m/%Y %H:%M:%S")
#Calculando o tempo de atendimento em minutos
tempo$TEMPO_ATD <- (tempo$FIM_ATD - tempo$INI_ATD)/60
#Só será utilizado o TEMPO_ATD
tempo <- select(tempo,-c(FIM_ATD,DIA_CHEGADA, HORA_CHEGADA, DIA_INICIO_ATD, HORA_INICIO_ATD, DIA_FIM_ATD, HORA_FIM_ATD, CD_PROFISSIONAL, CD_USUARIO))

#Trocando código por nome das UPAS
tempo$UNIDADE <- ifelse(tempo$UNIDADE == 55, "UPA Sul", "UPA Norte")

#Criando a tabela de médicos e odontológicos e especialidades médicas
tempo_med_odont <- select(tempo, -c(PROCEDIMENTO, CLASSIFICACAO_RISCO))
tempo_med_odont <- unique(tempo_med_odont)#Retirando as células que são repetidas por causa dos procedimentos (uma consulta pode ter mais de um procedimento)


#Importando tabela de especialidades
especialidade <- read_csv("base_de_dados/extraidas/especialidade.csv")
#Transformando as várias especialidades em clíncia, cirurgia, pediatria e odontologia
tempo_med_odont <- merge(tempo_med_odont, especialidade, by = "ESPECIALIDADE_ATD", all.x = T)
tempo_med_odont <- select(tempo_med_odont, -c(ESPECIALIDADE_ATD ,CD_CONSULTA))



#Componto os bancos de tempo de médicos e enfermeiros
#Médico
tempo_med <- subset(tempo_med_odont, tempo_med_odont$ESPECIALIDADE != "Odontologia")

#Odontologia
tempo_odont <- subset(tempo_med_odont, tempo_med_odont$ESPECIALIDADE == "Odontologia")
tempo_odont <- select(tempo_odont,-c(ESPECIALIDADE))

#Componto os bancos de classificações
#Serão analiados apenas as classificações cujo código do procedimento é 0301060118, que é o código aceito pela 
#Portaria Nº 10, de 3 de janeiro de 2017
tempo$PROCEDIMENTO <- as.character(tempo$PROCEDIMENTO)
tempo_classificacao <- subset(tempo, tempo$PROCEDIMENTO == "0301060118")
tempo_classificacao <- unique(tempo_classificacao)#Retirando a possibilidade de duplicação nas observações
tempo_classificacao <- select(tempo_classificacao,-c(PROCEDIMENTO, ESPECIALIDADE_ATD, CD_CONSULTA))


#Escrever bancos de dados
write.csv(tempo_med, "base_de_dados/transformadas/tempo_med.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_odont, "base_de_dados/transformadas/tempo_odont.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_classificacao, "base_de_dados/transformadas/tempo_classificacao.csv", fileEncoding = "UTF-8", row.names = F)





