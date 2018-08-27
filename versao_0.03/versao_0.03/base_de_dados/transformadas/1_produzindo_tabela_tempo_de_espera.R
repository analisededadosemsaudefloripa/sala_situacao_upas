options(encoding = "UTF-8")
#Pacotes
library(tidyverse)
library(readr)
library(data.table)
library(zoo)
library(Kmisc) #pacote para manipulação de string
library(lubridate)
library(dplyr)
library(plyr)
library(reshape2)


#PREPARANDO A BASE PARA O CÁLCULO  Do tempo
espera <- read_csv("base_de_dados/extraidas/upa_tempo_espera.csv", locale = locale(encoding = 'UTF-8'))
espera <- subset(espera, espera$CLASSIFICACAO_RISCO != is.na(espera$CLASSIFICACAO_RISCO))
#O formato das datas precisam de padronização serão utilizados a DATA_HORA_CHEGADA e a DATA_HORA_INICIO_1 (chegada do paciente à UPA até o início do atendimento)
#Ajustando DATA_HORA_CHEGADA
#Separando data da hora
espera$DATA_CHEGADA <- gsub( " .*$", "", espera$DATA_HORA_CHEGADA)
#Formata data
espera$SPLIT_CHEGADA <- strsplit(espera$DATA_CHEGADA, split='[//]')
espera$DIA_CHEGADA <- sapply(espera$SPLIT_CHEGADA,function(x) x[1])
espera$MES_CHEGADA <- sapply(espera$SPLIT_CHEGADA,function(x) x[2])
espera$ANO_CHEGADA <- sapply(espera$SPLIT_CHEGADA,function(x) x[3])
espera$DATA_A <- paste(espera$DIA_CHEGADA,"/", espera$MES_CHEGADA,"/",espera$ANO_CHEGADA, sep = "")
espera$DATA_A <- as.Date(espera$DATA_A, format = "%d/%m/%Y")
#Separando hora da data
espera$HORA_CHEGADA <- gsub( ".* ", "", espera$DATA_HORA_CHEGADA)
#Formata hora
espera$SPLIT_HORA_CHEGADA <- strsplit(espera$HORA_CHEGADA, split='[::]')
espera$HORA_CHEGADA <- sapply(espera$SPLIT_HORA_CHEGADA,function(x) x[1])
espera$MINUTO_CHEGADA <- sapply(espera$SPLIT_HORA_CHEGADA,function(x) x[2])
espera$SEGUNDO_CHEGADA <- sapply(espera$SPLIT_HORA_CHEGADA,function(x) x[3])
espera$SEGUNDO_CHEGADA[which(is.na(espera$SEGUNDO_CHEGADA))] <- "00"
espera$HORA_A <- paste(espera$HORA_CHEGADA,":", espera$MINUTO_CHEGADA,":",espera$SEGUNDO_CHEGADA, sep = "")
espera$DATA_HORA_CHEGADA <- paste(espera$DATA_A," ",espera$HORA_A, sep = "")
espera$DATA_HORA_CHEGADA <- as.POSIXct(espera$DATA_HORA_CHEGADA, format = "%Y-%m-%d %H:%M:%S")

#Ajustando DATA_HORA_INICIO - início da classificacao
#Separando data da hora
espera$DATA_INICIO <- gsub( " .*$", "", espera$DATA_HORA_INICIO)
#Formata data
espera$SPLIT_INICIO <- strsplit(espera$DATA_INICIO, split='[//]')
espera$DIA_INICIO <- sapply(espera$SPLIT_INICIO,function(x) x[1])
espera$MES_INICIO <- sapply(espera$SPLIT_INICIO,function(x) x[2])
espera$ANO_INICIO <- sapply(espera$SPLIT_INICIO,function(x) x[3])
espera$DATA_A <- paste(espera$DIA_INICIO,"/", espera$MES_INICIO,"/",espera$ANO_INICIO, sep = "")
espera$DATA_A <- as.Date(espera$DATA_A, format = "%d/%m/%Y")
#Separando hora da data
espera$HORA_INICIO <- gsub( ".* ", "", espera$DATA_HORA_INICIO)
#Formata hora
espera$SPLIT_HORA_INICIO <- strsplit(espera$HORA_INICIO, split='[::]')
espera$HORA_INICIO <- sapply(espera$SPLIT_HORA_INICIO,function(x) x[1])
espera$MINUTO_INICIO <- sapply(espera$SPLIT_HORA_INICIO,function(x) x[2])
espera$SEGUNDO_INICIO <- sapply(espera$SPLIT_HORA_INICIO,function(x) x[3])
espera$SEGUNDO_INICIO[which(is.na(espera$SEGUNDO_INICIO))] <- "00"
espera$HORA_A <- paste(espera$HORA_INICIO,":", espera$MINUTO_INICIO,":",espera$SEGUNDO_INICIO, sep = "")
espera$DATA_HORA_INICIO <- paste(espera$DATA_A," ",espera$HORA_A, sep = "")
espera$DATA_HORA_INICIO <- as.POSIXct(espera$DATA_HORA_INICIO, format = "%Y-%m-%d %H:%M:%S")


#Ajustando DATA_HORA_INICIO_1 - início do atendimento
#Separando data da hora
espera$DATA_INICIO <- gsub( " .*$", "", espera$DATA_HORA_INICIO_1)
#Formata data
espera$SPLIT_INICIO <- strsplit(espera$DATA_INICIO, split='[//]')
espera$DIA_INICIO <- sapply(espera$SPLIT_INICIO,function(x) x[1])
espera$MES_INICIO <- sapply(espera$SPLIT_INICIO,function(x) x[2])
espera$ANO_INICIO <- sapply(espera$SPLIT_INICIO,function(x) x[3])
espera$DATA_A <- paste(espera$DIA_INICIO,"/", espera$MES_INICIO,"/",espera$ANO_INICIO, sep = "")
espera$DATA_A <- as.Date(espera$DATA_A, format = "%d/%m/%Y")
#Separando hora da data
espera$HORA_INICIO <- gsub( ".* ", "", espera$DATA_HORA_INICIO_1)
#Formata hora
espera$SPLIT_HORA_INICIO <- strsplit(espera$HORA_INICIO, split='[::]')
espera$HORA_INICIO <- sapply(espera$SPLIT_HORA_INICIO,function(x) x[1])
espera$MINUTO_INICIO <- sapply(espera$SPLIT_HORA_INICIO,function(x) x[2])
espera$SEGUNDO_INICIO <- sapply(espera$SPLIT_HORA_INICIO,function(x) x[3])
espera$SEGUNDO_INICIO[which(is.na(espera$SEGUNDO_INICIO))] <- "00"
espera$HORA_A <- paste(espera$HORA_INICIO,":", espera$MINUTO_INICIO,":",espera$SEGUNDO_INICIO, sep = "")
espera$DATA_HORA_INICIO_1 <- paste(espera$DATA_A," ",espera$HORA_A, sep = "")
espera$DATA_HORA_INICIO_1 <- as.POSIXct(espera$DATA_HORA_INICIO_1, format = "%Y-%m-%d %H:%M:%S")

#Componto tabela apenas com as variáveis de interesse
espera <- espera[,c("DATA_HORA_CHEGADA", "DATA_HORA_INICIO", "DATA_HORA_INICIO_1", "CD_USUARIO", "CD_CONSULTA_1", "UNIDADE_1", "CLASSIFICACAO_RISCO", "ESPECIALIDADE_ATD_1", "CD_PROFISSIONAL_1")]


#Calculando o tempo de espera em minutos
espera$TEMPO_ESPERA_MED <- (espera$DATA_HORA_INICIO_1 - espera$DATA_HORA_CHEGADA)/60
espera$TEMPO_ESPERA_CLASS <- (espera$DATA_HORA_INICIO - espera$DATA_HORA_CHEGADA)/60

#Trocando código por nome das UPAS
espera$UNIDADE_1 <- ifelse(espera$UNIDADE_1 == 55, "UPA Sul", "UPA Norte")


#Importando tabela de especialidades
especialidade <- read_csv("base_de_dados/extraidas/especialidade.csv")
#Transformando as várias especialidades em clíncia, cirurgia, pediatria e odontologia
colnames(espera)[8] <- "ESPECIALIDADE_ATD" 
espera <- merge(espera, especialidade, by = "ESPECIALIDADE_ATD", all.x = T)
espera <- select(espera, -c(ESPECIALIDADE_ATD ,CD_CONSULTA_1))



#Escrever banco de dados
write.csv(espera, "base_de_dados/transformadas/espera.csv", fileEncoding = "UTF-8", row.names = F)

#####################################################################################################

#PREPARANDO A BASE PARA O CÁLCULO DO tempo de espera para odonto
espera_1 <- read_csv("base_de_dados/extraidas/painel_upa.csv", locale = locale(encoding = 'UTF-8'))


#Criando a tabela odontológos
#Importando tabela de especialidades
especialidade <- read_csv("base_de_dados/extraidas/especialidade.csv")
#Transformando as várias especialidades em clíncia, cirurgia, pediatria e odontologia
espera_odonto <- merge(espera_1, especialidade, by = "ESPECIALIDADE_ATD", all.x = T)
espera_odonto <- subset(espera_odonto, espera_odonto$ESPECIALIDADE == "Odontologia")
espera_odonto <- unique(espera_odonto)#Retirando as células que são repetidas por causa dos procedimentos (uma consulta pode ter mais de um procedimento)

#Serão calculados o espera de espera médicos, na clínica, na pediatria, na cirurgia, na odontologia e classificações de risco por mês, dia da semana e hora do dia.
#Juntando data e hora do atendimento do início e do fim do atendimento
espera_odonto$DATA_HORA_CHEGADA <- paste(espera_odonto$DIA_CHEGADA, espera_odonto$HORA_CHEGADA)
espera_odonto$DATA_HORA_CHEGADA <- as.POSIXct(espera_odonto$DATA_HORA_CHEGADA, format = "%d/%m/%Y %H:%M:%S")
espera_odonto$INI_ATD <- paste(espera_odonto$DIA_INICIO_ATD, espera_odonto$HORA_INICIO_ATD)
espera_odonto$INI_ATD <- as.POSIXct(espera_odonto$INI_ATD, format = "%d/%m/%Y %H:%M:%S")


#Calculando o espera de atendimento em minutos
espera_odonto$ESPERA_ODONTO <- (espera_odonto$INI_ATD - espera_odonto$DATA_HORA_CHEGADA)/60
#Só será utilizado o espera_odonto e hora_dia_chegada
espera_odonto <- select(espera_odonto,c(DATA_HORA_CHEGADA, ESPERA_ODONTO, UNIDADE))

#Trocando código por nome das UPAS
espera_odonto$UNIDADE <- ifelse(espera_odonto$UNIDADE == 55, "UPA Sul", "UPA Norte")


write.csv(espera_odonto, "base_de_dados/transformadas/espera_odonto.csv", fileEncoding = "UTF-8", row.names = F)
