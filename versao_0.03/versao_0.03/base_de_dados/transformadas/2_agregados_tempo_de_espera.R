options(encoding = "UTF-8")
#Tabela de contagem por áreas
library(tidyverse)
library(readr)
library(data.table)
library(zoo)
library(Kmisc) #pacote para manipulação de string
library(lubridate)
library(dplyr)
library(reshape2)


##############################################################################################################

#Agregando os dados - médicos
#Importando a base
espera_new <- fread("base_de_dados/transformadas/espera.csv", encoding = 'UTF-8')

#Lendo as bases antigas
espera_old <- fread("base_de_dados/transformadas/espera_old.csv", encoding = 'UTF-8')

espera <- rbind(espera_new, espera_old) 

#Salvando as bases antigas como old
write.csv(espera, "base_de_dados/transformadas/espera_old.csv", fileEncoding = "UTF-8", row.names = F)

names(espera) <- c("CHEGADA", "INI_CLASS", "INI_ATD", "CD_USUARIO", "UNIDADE", "CLASSIFICACAO_RISCO", "CD_PROFISSIONAL", "ESPERA_ATD", "ESPERA_CLASS", "ESPECIALIDADE")
#Separando as upas
espera_med_sul <- subset(espera, espera$UNIDADE == "UPA Sul")
espera_med_sul <- select(espera_med_sul, -c(UNIDADE))
espera_med_sul$CHEGADA <- as.POSIXct(espera_med_sul$CHEGADA, format = "%Y-%m-%d %H:%M:%S")


espera_med_norte <- subset(espera, espera$UNIDADE == "UPA Norte")
espera_med_norte <- select(espera_med_norte, -c(UNIDADE))
espera_med_norte$CHEGADA <- as.POSIXct(espera_med_norte$CHEGADA, format = "%Y-%m-%d %H:%M:%S")




#Mês-Ano
espera_med_sul_mes <- espera_med_sul
espera_med_sul_mes$CHEGADA <- substr(espera_med_sul_mes$CHEGADA, 0, 10)
espera_med_sul_mes$CHEGADA <- as.yearmon(espera_med_sul_mes$CHEGADA, format = "%Y-%m-%d")
espera_med_sul_mes <- aggregate(espera_med_sul_mes$ESPERA_ATD, by = list(espera_med_sul_mes$CHEGADA, espera_med_sul_mes$ESPECIALIDADE), FUN = median) %>% as.data.frame()
names(espera_med_sul_mes) <- c("DATA", "ESPECIALIDADE", "MINUTOS")
a <- aggregate(espera_med_sul_mes$MINUTOS, by = list(espera_med_sul_mes$DATA), FUN = median)
a$ESPECIALIDADE <- "Total"
names(a) <- c("DATA", "MINUTOS", "ESPECIALIDADE")
espera_med_sul_mes <- rbind(espera_med_sul_mes, a)
espera_med_sul_mes$MINUTOS <- round(espera_med_sul_mes$MINUTOS, 2)
espera_med_sul_mes$DATA <- as.Date(espera_med_sul_mes$DATA, format = "%d-%m-%Y")


#Dia-da-Semana
#Será a média do último semestre
espera_med_sul_dia <- na.omit(espera_med_sul)
espera_med_sul_dia <- subset(espera_med_sul_dia, as.yearmon(espera_med_sul_dia$CHEGADA) >= max(as.yearmon(espera_med_sul_dia$CHEGADA)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
espera_med_sul_dia$DIA <- format.Date(as.Date(espera_med_sul_dia$CHEGADA), "%a")
espera_med_sul_dia$CHEGADA <- NULL
espera_med_sul_dia <- aggregate(espera_med_sul_dia$ESPERA_ATD, by = list(espera_med_sul_dia$ESPECIALIDADE, espera_med_sul_dia$DIA), FUN = median, na.rm = T)
names(espera_med_sul_dia) <- c("ESPECIALIDADE", "DIA", "MINUTOS")
#espera_med_sul_dia$DIA <- factor(espera_med_sul_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#espera_med_sul_dia$DIA <- revalue(espera_med_sul_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
espera_med_sul_dia$DIA <- revalue(espera_med_sul_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))
a <- aggregate(espera_med_sul_dia$MINUTOS, by = list(espera_med_sul_dia$DIA), FUN = median)
a$ESPECIALIDADE <- "Total"
names(a) <- c("DIA", "MINUTOS", "ESPECIALIDADE")
espera_med_sul_dia <- rbind(espera_med_sul_dia, a)
espera_med_sul_dia$MINUTOS <- round(espera_med_sul_dia$MINUTOS,2) 

#Hora do dia
espera_med_sul_hora <- na.omit(espera_med_sul)
espera_med_sul_hora <- subset(espera_med_sul_hora, as.yearmon(espera_med_sul_hora$CHEGADA) >= max(as.yearmon(espera_med_sul_hora$CHEGADA)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
espera_med_sul_hora$HORA <- format.POSIXct(as.POSIXct(espera_med_sul_hora$CHEGADA), "%H")
espera_med_sul_hora$CHEGADA <- NULL
espera_med_sul_hora <- aggregate(espera_med_sul_hora$ESPERA_ATD, by = list(espera_med_sul_hora$ESPECIALIDADE, espera_med_sul_hora$HORA), FUN = median, na.rm = T)
names(espera_med_sul_hora) <- c("ESPECIALIDADE", "HORA", "MINUTOS")
a <- aggregate(espera_med_sul_hora$MINUTOS, by = list(espera_med_sul_hora$HORA), FUN = median)
a$ESPECIALIDADE <- "Total"
names(a) <- c("HORA", "MINUTOS", "ESPECIALIDADE")
espera_med_sul_hora <- rbind(espera_med_sul_hora, a)
espera_med_sul_hora$MINUTOS <- round(espera_med_sul_hora$MINUTOS, 2) 

#Mês-Ano
espera_med_norte_mes <- espera_med_norte
espera_med_norte_mes$CHEGADA <- substr(espera_med_norte_mes$CHEGADA, 0, 10)
espera_med_norte_mes$CHEGADA <- as.yearmon(espera_med_norte_mes$CHEGADA, format = "%Y-%m-%d")
espera_med_norte_mes <- aggregate(espera_med_norte_mes$ESPERA_ATD, by = list(espera_med_norte_mes$CHEGADA, espera_med_norte_mes$ESPECIALIDADE), FUN = median) %>% as.data.frame()
names(espera_med_norte_mes) <- c("DATA", "ESPECIALIDADE", "MINUTOS")
a <- aggregate(espera_med_norte_mes$MINUTOS, by = list(espera_med_norte_mes$DATA), FUN = median)
a$ESPECIALIDADE <- "Total"
names(a) <- c("DATA", "MINUTOS", "ESPECIALIDADE")
espera_med_norte_mes <- rbind(espera_med_norte_mes, a)
espera_med_norte_mes$MINUTOS <- round(espera_med_norte_mes$MINUTOS, 2)
espera_med_norte_mes$DATA <- as.Date(espera_med_norte_mes$DATA, format = "%d-%m-%Y")


#Dia-da-Semana
#Será a média do último semestre
espera_med_norte_dia <- na.omit(espera_med_norte)
espera_med_norte_dia <- subset(espera_med_norte_dia, as.yearmon(espera_med_norte_dia$CHEGADA) >= max(as.yearmon(espera_med_norte_dia$CHEGADA)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
espera_med_norte_dia$DIA <- format.Date(as.Date(espera_med_norte_dia$CHEGADA), "%a")
espera_med_norte_dia$CHEGADA <- NULL
espera_med_norte_dia <- aggregate(espera_med_norte_dia$ESPERA_ATD, by = list(espera_med_norte_dia$ESPECIALIDADE, espera_med_norte_dia$DIA), FUN = median, na.rm = T)
names(espera_med_norte_dia) <- c("ESPECIALIDADE", "DIA", "MINUTOS")
#espera_med_norte_dia$DIA <- factor(espera_med_norte_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#espera_med_norte_dia$DIA <- revalue(espera_med_norte_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
espera_med_norte_dia$DIA <- revalue(espera_med_norte_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))

a <- aggregate(espera_med_norte_dia$MINUTOS, by = list(espera_med_norte_dia$DIA), FUN = median)
a$ESPECIALIDADE <- "Total"
names(a) <- c("DIA", "MINUTOS", "ESPECIALIDADE")
espera_med_norte_dia <- rbind(espera_med_norte_dia, a)
espera_med_norte_dia$MINUTOS <- round(espera_med_norte_dia$MINUTOS,2) 

#Hora do dia
espera_med_norte_hora <- na.omit(espera_med_norte)
espera_med_norte_hora <- subset(espera_med_norte_hora, as.yearmon(espera_med_norte_hora$CHEGADA) >= max(as.yearmon(espera_med_norte_hora$CHEGADA)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
espera_med_norte_hora$HORA <- format.POSIXct(as.POSIXct(espera_med_norte_hora$CHEGADA), "%H")
espera_med_norte_hora$CHEGADA <- NULL
espera_med_norte_hora <- aggregate(espera_med_norte_hora$ESPERA_ATD, by = list(espera_med_norte_hora$ESPECIALIDADE, espera_med_norte_hora$HORA), FUN = median, na.rm = T)
names(espera_med_norte_hora) <- c("ESPECIALIDADE", "HORA", "MINUTOS")
a <- aggregate(espera_med_norte_hora$MINUTOS, by = list(espera_med_norte_hora$HORA), FUN = median)
a$ESPECIALIDADE <- "Total"
names(a) <- c("HORA", "MINUTOS", "ESPECIALIDADE")
espera_med_norte_hora <- rbind(espera_med_norte_hora, a)
espera_med_norte_hora$MINUTOS <- round(espera_med_norte_hora$MINUTOS, 2) 




#Escrever bancos de dados
write.csv(espera_med_sul_mes, "base_de_dados/transformadas/espera_med_sul_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(espera_med_sul_dia, "base_de_dados/transformadas/espera_med_sul_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(espera_med_sul_hora, "base_de_dados/transformadas/espera_med_sul_hora.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(espera_med_norte_mes, "base_de_dados/transformadas/espera_med_norte_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(espera_med_norte_dia, "base_de_dados/transformadas/espera_med_norte_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(espera_med_norte_hora, "base_de_dados/transformadas/espera_med_norte_hora.csv", fileEncoding = "UTF-8", row.names = F)

##############################################################################################################                   
#CLASSIFICACAO
#Separando as upas
espera_classificacao_sul <- subset(espera, espera$UNIDADE == "UPA Sul")
espera_classificacao_sul <- select(espera_classificacao_sul, -c(UNIDADE))
espera_classificacao_sul$CHEGADA <- as.POSIXct(espera_classificacao_sul$CHEGADA, format = "%Y-%m-%d %H:%M:%S")

espera_classificacao_norte <- subset(espera, espera$UNIDADE == "UPA Norte")
espera_classificacao_norte <- select(espera_classificacao_norte, -c(UNIDADE))
espera_classificacao_norte$CHEGADA <- as.POSIXct(espera_classificacao_norte$CHEGADA, format = "%Y-%m-%d %H:%M:%S")


#Mês-Ano
espera_classificacao_sul_mes <- espera_classificacao_sul
espera_classificacao_sul_mes$CHEGADA <- substr(espera_classificacao_sul_mes$CHEGADA, 0, 10)
espera_classificacao_sul_mes$CHEGADA <- as.yearmon(espera_classificacao_sul_mes$CHEGADA, format = "%Y-%m-%d")
espera_classificacao_sul_mes <- aggregate(espera_classificacao_sul_mes$ESPERA_CLASS, by = list(espera_classificacao_sul_mes$CHEGADA, espera_classificacao_sul_mes$CLASSIFICACAO_RISCO), FUN = median) %>% as.data.frame()
names(espera_classificacao_sul_mes) <- c("DATA", "CLASSIFICACAO", "MINUTOS")
a <- aggregate(espera_classificacao_sul_mes$MINUTOS, by = list(espera_classificacao_sul_mes$DATA), FUN = median)
a$CLASSIFICACAO <- "Total"
names(a) <- c("DATA", "MINUTOS", "CLASSIFICACAO")
espera_classificacao_sul_mes <- rbind(espera_classificacao_sul_mes, a)
espera_classificacao_sul_mes$MINUTOS <- round(espera_classificacao_sul_mes$MINUTOS, 2)
espera_classificacao_sul_mes$DATA <- as.Date(espera_classificacao_sul_mes$DATA, format = "%d-%m-%Y")


#Dia-da-Semana
#Será a média do último semestre
espera_classificacao_sul_dia <- na.omit(espera_classificacao_sul)
espera_classificacao_sul_dia <- subset(espera_classificacao_sul_dia, as.yearmon(espera_classificacao_sul_dia$CHEGADA) >= max(as.yearmon(espera_classificacao_sul_dia$CHEGADA)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
espera_classificacao_sul_dia$DIA <- format.Date(as.Date(espera_classificacao_sul_dia$CHEGADA), "%a")
espera_classificacao_sul_dia$CHEGADA <- NULL
espera_classificacao_sul_dia <- aggregate(espera_classificacao_sul_dia$ESPERA_CLASS, by = list(espera_classificacao_sul_dia$CLASSIFICACAO_RISCO, espera_classificacao_sul_dia$DIA), FUN = median, na.rm = T)
names(espera_classificacao_sul_dia) <- c("CLASSIFICACAO", "DIA", "MINUTOS")
#espera_classificacao_sul_dia$DIA <- factor(espera_classificacao_sul_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#espera_classificacao_sul_dia$DIA <- revalue(espera_classificacao_sul_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
espera_classificacao_sul_dia$DIA <- revalue(espera_classificacao_sul_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))
a <- aggregate(espera_classificacao_sul_dia$MINUTOS, by = list(espera_classificacao_sul_dia$DIA), FUN = median)
a$CLASSIFICACAO <- "Total"
names(a) <- c("DIA", "MINUTOS", "CLASSIFICACAO")
espera_classificacao_sul_dia <- rbind(espera_classificacao_sul_dia, a)
espera_classificacao_sul_dia$MINUTOS <- round(espera_classificacao_sul_dia$MINUTOS,2) 

#Hora do dia
espera_classificacao_sul_hora <- na.omit(espera_classificacao_sul)
espera_classificacao_sul_hora <- subset(espera_classificacao_sul_hora, as.yearmon(espera_classificacao_sul_hora$CHEGADA) >= max(as.yearmon(espera_classificacao_sul_hora$CHEGADA)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
espera_classificacao_sul_hora$HORA <- format.POSIXct(as.POSIXct(espera_classificacao_sul_hora$CHEGADA), "%H")
espera_classificacao_sul_hora$CHEGADA <- NULL
espera_classificacao_sul_hora <- aggregate(espera_classificacao_sul_hora$ESPERA_CLASS, by = list(espera_classificacao_sul_hora$CLASSIFICACAO_RISCO, espera_classificacao_sul_hora$HORA), FUN = median, na.rm = T)
names(espera_classificacao_sul_hora) <- c("CLASSIFICACAO", "HORA", "MINUTOS")
a <- aggregate(espera_classificacao_sul_hora$MINUTOS, by = list(espera_classificacao_sul_hora$HORA), FUN = median)
a$CLASSIFICACAO <- "Total"
names(a) <- c("HORA", "MINUTOS", "CLASSIFICACAO")
espera_classificacao_sul_hora <- rbind(espera_classificacao_sul_hora, a)
espera_classificacao_sul_hora$MINUTOS <- round(espera_classificacao_sul_hora$MINUTOS, 2) 

#Mês-Ano
espera_classificacao_norte_mes <- espera_classificacao_norte
espera_classificacao_norte_mes$CHEGADA <- substr(espera_classificacao_norte_mes$CHEGADA, 0, 10)
espera_classificacao_norte_mes$CHEGADA <- as.yearmon(espera_classificacao_norte_mes$CHEGADA, format = "%Y-%m-%d")
espera_classificacao_norte_mes <- aggregate(espera_classificacao_norte_mes$ESPERA_CLASS, by = list(espera_classificacao_norte_mes$CHEGADA, espera_classificacao_norte_mes$CLASSIFICACAO_RISCO), FUN = median) %>% as.data.frame()
names(espera_classificacao_norte_mes) <- c("DATA", "CLASSIFICACAO", "MINUTOS")
a <- aggregate(espera_classificacao_norte_mes$MINUTOS, by = list(espera_classificacao_norte_mes$DATA), FUN = median)
a$CLASSIFICACAO <- "Total"
names(a) <- c("DATA", "MINUTOS", "CLASSIFICACAO")
espera_classificacao_norte_mes <- rbind(espera_classificacao_norte_mes, a)
espera_classificacao_norte_mes$MINUTOS <- round(espera_classificacao_norte_mes$MINUTOS, 2)
espera_classificacao_norte_mes$DATA <- as.Date(espera_classificacao_norte_mes$DATA, format = "%d-%m-%Y")

#Dia-da-Semana
#Será a média do último semestre
espera_classificacao_norte_dia <- na.omit(espera_classificacao_norte)
espera_classificacao_norte_dia <- subset(espera_classificacao_norte_dia, as.yearmon(espera_classificacao_norte_dia$CHEGADA) >= max(as.yearmon(espera_classificacao_norte_dia$CHEGADA)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
espera_classificacao_norte_dia$DIA <- format.Date(as.Date(espera_classificacao_norte_dia$CHEGADA), "%a")
espera_classificacao_norte_dia$CHEGADA <- NULL
espera_classificacao_norte_dia <- aggregate(espera_classificacao_norte_dia$ESPERA_CLASS, by = list(espera_classificacao_norte_dia$CLASSIFICACAO_RISCO, espera_classificacao_norte_dia$DIA), FUN = median, na.rm = T)
names(espera_classificacao_norte_dia) <- c("CLASSIFICACAO", "DIA", "MINUTOS")
#espera_classificacao_norte_dia$DIA <- factor(espera_classificacao_norte_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#espera_classificacao_norte_dia$DIA <- revalue(espera_classificacao_norte_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
espera_classificacao_norte_dia$DIA <- revalue(espera_classificacao_norte_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))
a <- aggregate(espera_classificacao_norte_dia$MINUTOS, by = list(espera_classificacao_norte_dia$DIA), FUN = median)
a$CLASSIFICACAO <- "Total"
names(a) <- c("DIA", "MINUTOS", "CLASSIFICACAO")
espera_classificacao_norte_dia <- rbind(espera_classificacao_norte_dia, a)
espera_classificacao_norte_dia$MINUTOS <- round(espera_classificacao_norte_dia$MINUTOS,2) 

#Hora do dia
espera_classificacao_norte_hora <- na.omit(espera_classificacao_norte)
espera_classificacao_norte_hora <- subset(espera_classificacao_norte_hora, as.yearmon(espera_classificacao_norte_hora$CHEGADA) >= max(as.yearmon(espera_classificacao_norte_hora$CHEGADA)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
espera_classificacao_norte_hora$HORA <- format.POSIXct(as.POSIXct(espera_classificacao_norte_hora$CHEGADA), "%H")
espera_classificacao_norte_hora$CHEGADA <- NULL
espera_classificacao_norte_hora <- aggregate(espera_classificacao_norte_hora$ESPERA_CLASS, by = list(espera_classificacao_norte_hora$CLASSIFICACAO_RISCO, espera_classificacao_norte_hora$HORA), FUN = median, na.rm = T)
names(espera_classificacao_norte_hora) <- c("CLASSIFICACAO", "HORA", "MINUTOS")
a <- aggregate(espera_classificacao_norte_hora$MINUTOS, by = list(espera_classificacao_norte_hora$HORA), FUN = median)
a$CLASSIFICACAO <- "Total"
names(a) <- c("HORA", "MINUTOS", "CLASSIFICACAO")
espera_classificacao_norte_hora <- rbind(espera_classificacao_norte_hora, a)
espera_classificacao_norte_hora$MINUTOS <- round(espera_classificacao_norte_hora$MINUTOS, 2) 



#Escrever bancos de dados
write.csv(espera_classificacao_sul_mes, "base_de_dados/transformadas/espera_classificacao_sul_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(espera_classificacao_sul_dia, "base_de_dados/transformadas/espera_classificacao_sul_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(espera_classificacao_sul_hora, "base_de_dados/transformadas/espera_classificacao_sul_hora.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(espera_classificacao_norte_mes, "base_de_dados/transformadas/espera_classificacao_norte_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(espera_classificacao_norte_dia, "base_de_dados/transformadas/espera_classificacao_norte_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(espera_classificacao_norte_hora, "base_de_dados/transformadas/espera_classificacao_norte_hora.csv", fileEncoding = "UTF-8", row.names = F)

##############################################################################################################                   

#Agregando os dados - odontologia
#Importando a base
espera_odontologia_new <- fread("base_de_dados/transformadas/espera_odonto.csv", encoding = 'UTF-8')

#Lendo as bases antigas
espera_odontologia_old <- fread("base_de_dados/transformadas/espera_odonto_old.csv", encoding = 'UTF-8')

espera_odontologia <- rbind(espera_odontologia_new, espera_odontologia_old) 

#Salvando as bases antigas como old
write.csv(espera_odontologia, "base_de_dados/transformadas/espera_odonto_old.csv", fileEncoding = "UTF-8", row.names = F)

names(espera_odontologia) <- c("CHEGADA", "ESPERA_ODONTO", "UNIDADE")

#Separando as upas
espera_odontologia_sul <- subset(espera_odontologia, espera_odontologia$UNIDADE == "UPA Sul")
espera_odontologia_sul <- select(espera_odontologia_sul, -c(UNIDADE))
espera_odontologia_sul$CHEGADA <- as.POSIXct(espera_odontologia_sul$CHEGADA, format = "%Y-%m-%d %H:%M:%S")

espera_odontologia_norte <- subset(espera_odontologia, espera_odontologia$UNIDADE == "UPA Norte")
espera_odontologia_norte <- select(espera_odontologia_norte, -c(UNIDADE))
espera_odontologia_norte$CHEGADA <- as.POSIXct(espera_odontologia_norte$CHEGADA, format = "%Y-%m-%d %H:%M:%S")


#Mês-Ano
espera_odontologia_sul_mes <- espera_odontologia_sul
espera_odontologia_sul_mes$CHEGADA <- substr(espera_odontologia_sul_mes$CHEGADA, 0, 10)
espera_odontologia_sul_mes$CHEGADA <- as.yearmon(espera_odontologia_sul_mes$CHEGADA, format = "%Y-%m-%d")
espera_odontologia_sul_mes <- aggregate(espera_odontologia_sul_mes$ESPERA_ODONTO, by = list(espera_odontologia_sul_mes$CHEGADA), FUN = median) %>% as.data.frame()
names(espera_odontologia_sul_mes) <- c("DATA", "MINUTOS")
espera_odontologia_sul_mes$MINUTOS <- round(espera_odontologia_sul_mes$MINUTOS, 2)
espera_odontologia_sul_mes$DATA <- as.yearmon(espera_odontologia_sul_mes$DATA, format = "%b %Y")
espera_odontologia_sul_mes$DATA <- as.Date(espera_odontologia_sul_mes$DATA, format = "%d-%m-%Y")


#Dia-da-Semana
#Será a média do último semestre
espera_odontologia_sul_dia <- na.omit(espera_odontologia_sul)
espera_odontologia_sul_dia <- subset(espera_odontologia_sul_dia, as.yearmon(espera_odontologia_sul_dia$CHEGADA) >= max(as.yearmon(espera_odontologia_sul_dia$CHEGADA)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
espera_odontologia_sul_dia$DIA <- format.Date(as.Date(espera_odontologia_sul_dia$CHEGADA), "%a")
espera_odontologia_sul_dia$CHEGADA <- NULL
espera_odontologia_sul_dia <- aggregate(espera_odontologia_sul_dia$ESPERA_ODONTO, by = list(espera_odontologia_sul_dia$DIA), FUN = median, na.rm = T)
names(espera_odontologia_sul_dia) <- c("DIA", "MINUTOS")
espera_odontologia_sul_dia$MINUTOS <- round(espera_odontologia_sul_dia$MINUTOS,2) 
#espera_odontologia_sul_dia$DIA <- factor(espera_odontologia_sul_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#espera_odontologia_sul_dia$DIA <- revalue(espera_odontologia_sul_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
espera_odontologia_sul_dia$DIA <- revalue(espera_odontologia_sul_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))


#Hora do dia
espera_odontologia_sul_hora <- na.omit(espera_odontologia_sul)
espera_odontologia_sul_hora <- subset(espera_odontologia_sul_hora, as.yearmon(espera_odontologia_sul_hora$CHEGADA) >= max(as.yearmon(espera_odontologia_sul_hora$CHEGADA)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
espera_odontologia_sul_hora$HORA <- format.POSIXct(as.POSIXct(espera_odontologia_sul_hora$CHEGADA), "%H")
espera_odontologia_sul_hora$CHEGADA <- NULL
espera_odontologia_sul_hora <- aggregate(espera_odontologia_sul_hora$ESPERA_ODONTO, by = list(espera_odontologia_sul_hora$HORA), FUN = median, na.rm = T)
names(espera_odontologia_sul_hora) <- c("HORA", "MINUTOS")
espera_odontologia_sul_hora$MINUTOS <- round(espera_odontologia_sul_hora$MINUTOS,2) 

#Mês-Ano
espera_odontologia_norte_mes <- espera_odontologia_norte
espera_odontologia_norte_mes$CHEGADA <- substr(espera_odontologia_norte_mes$CHEGADA, 0, 10)
espera_odontologia_norte_mes$CHEGADA <- as.yearmon(espera_odontologia_norte_mes$CHEGADA, format = "%Y-%m-%d")
espera_odontologia_norte_mes <- aggregate(espera_odontologia_norte_mes$ESPERA_ODONTO, by = list(espera_odontologia_norte_mes$CHEGADA), FUN = median) %>% as.data.frame()
names(espera_odontologia_norte_mes) <- c("DATA", "MINUTOS")
espera_odontologia_norte_mes$MINUTOS <- round(espera_odontologia_norte_mes$MINUTOS, 2)
espera_odontologia_norte_mes$DATA <- as.yearmon(espera_odontologia_norte_mes$DATA, format = "%b %Y")
espera_odontologia_norte_mes$DATA <- as.Date(espera_odontologia_norte_mes$DATA, format = "%d-%m-%Y")


#Dia-da-Semana
#Será a média do último semestre
espera_odontologia_norte_dia <- na.omit(espera_odontologia_norte)
espera_odontologia_norte_dia <- subset(espera_odontologia_norte_dia, as.yearmon(espera_odontologia_norte_dia$CHEGADA) >= max(as.yearmon(espera_odontologia_norte_dia$CHEGADA)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
espera_odontologia_norte_dia$DIA <- format.Date(as.Date(espera_odontologia_norte_dia$CHEGADA), "%a")
espera_odontologia_norte_dia$CHEGADA <- NULL
espera_odontologia_norte_dia <- aggregate(espera_odontologia_norte_dia$ESPERA_ODONTO, by = list(espera_odontologia_norte_dia$DIA), FUN = median, na.rm = T)
names(espera_odontologia_norte_dia) <- c("DIA", "MINUTOS")
espera_odontologia_norte_dia$MINUTOS <- round(espera_odontologia_norte_dia$MINUTOS,2) 
#espera_odontologia_norte_dia$DIA <- factor(espera_odontologia_norte_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#espera_odontologia_norte_dia$DIA <- revalue(espera_odontologia_norte_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
espera_odontologia_norte_dia$DIA <- revalue(espera_odontologia_norte_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))


#Hora do dia
espera_odontologia_norte_hora <- na.omit(espera_odontologia_norte)
espera_odontologia_norte_hora <- subset(espera_odontologia_norte_hora, as.yearmon(espera_odontologia_norte_hora$CHEGADA) >= max(as.yearmon(espera_odontologia_norte_hora$CHEGADA)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
espera_odontologia_norte_hora$HORA <- format.POSIXct(as.POSIXct(espera_odontologia_norte_hora$CHEGADA), "%H")
espera_odontologia_norte_hora$CHEGADA <- NULL
espera_odontologia_norte_hora <- aggregate(espera_odontologia_norte_hora$ESPERA_ODONTO, by = list(espera_odontologia_norte_hora$HORA), FUN = median, na.rm = T)
names(espera_odontologia_norte_hora) <- c("HORA", "MINUTOS")
espera_odontologia_norte_hora$MINUTOS <- round(espera_odontologia_norte_hora$MINUTOS,2)


#Escrever bancos de dados
write.csv(espera_odontologia_sul_mes, "base_de_dados/transformadas/espera_odontologia_sul_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(espera_odontologia_sul_dia, "base_de_dados/transformadas/espera_odontologia_sul_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(espera_odontologia_sul_hora, "base_de_dados/transformadas/espera_odontologia_sul_hora.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(espera_odontologia_norte_mes, "base_de_dados/transformadas/espera_odontologia_norte_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(espera_odontologia_norte_dia, "base_de_dados/transformadas/espera_odontologia_norte_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(espera_odontologia_norte_hora, "base_de_dados/transformadas/espera_odontologia_norte_hora.csv", fileEncoding = "UTF-8", row.names = F)


