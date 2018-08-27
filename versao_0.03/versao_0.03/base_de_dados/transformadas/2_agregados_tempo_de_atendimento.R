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
library(plyr)


##############################################################################################################

#Agregando os dados - médicos
#Importando a base
tempo_med_new <- fread("base_de_dados/transformadas/tempo_med.csv", encoding = 'UTF-8')

#Lendo as bases antigas
tempo_med_old <- fread("base_de_dados/transformadas/tempo_med_old.csv", encoding = 'UTF-8')

tempo_med <- rbind(tempo_med_new, tempo_med_old) 

#Salvando as bases antigas como old
write.csv(tempo_med, "base_de_dados/transformadas/tempo_med_old.csv", fileEncoding = "UTF-8", row.names = F)

#Separando as upas
tempo_med_sul <- subset(tempo_med, tempo_med$UNIDADE == "UPA Sul")
tempo_med_sul <- select(tempo_med_sul, -c(UNIDADE))


tempo_med_norte <- subset(tempo_med, tempo_med$UNIDADE == "UPA Norte")
tempo_med_norte <- select(tempo_med_norte, -c(UNIDADE))



#Mês-Ano
tempo_med_sul_mes <- tempo_med_sul
tempo_med_sul_mes$INI_ATD <- as.yearmon(tempo_med_sul_mes$INI_ATD, format = "%Y-%m-%d")
tempo_med_sul_mes <- aggregate(tempo_med_sul_mes$TEMPO_ATD, by = list(tempo_med_sul_mes$INI_ATD, tempo_med_sul_mes$ESPECIALIDADE), FUN = median) %>% as.data.frame()
names(tempo_med_sul_mes) <- c("DATA", "ESPECIALIDADE", "MINUTOS")
a <- aggregate(tempo_med_sul_mes$MINUTOS, by = list(tempo_med_sul_mes$DATA), FUN = median)
a$ESPECIALIDADE <- "Total"
names(a) <- c("DATA", "MINUTOS", "ESPECIALIDADE")
tempo_med_sul_mes <- rbind(tempo_med_sul_mes, a)
tempo_med_sul_mes$MINUTOS <- round(tempo_med_sul_mes$MINUTOS, 2)
tempo_med_sul_mes$DATA <- as.Date(tempo_med_sul_mes$DATA, format = "%d-%m-%Y")


#Dia-da-Semana
#Será a média do último semestre
tempo_med_sul_dia <-na.omit(tempo_med_sul)
tempo_med_sul_dia <- subset(tempo_med_sul_dia, as.yearmon(tempo_med_sul_dia$INI_ATD) >= max(as.yearmon(tempo_med_sul_dia$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
tempo_med_sul_dia$DIA <- format.Date(as.Date(tempo_med_sul_dia$INI_ATD), "%a")
tempo_med_sul_dia$INI_ATD <- NULL
tempo_med_sul_dia <- aggregate(tempo_med_sul_dia$TEMPO_ATD, by = list(tempo_med_sul_dia$ESPECIALIDADE, tempo_med_sul_dia$DIA), FUN = median, na.rm = T)
names(tempo_med_sul_dia) <- c("ESPECIALIDADE", "DIA", "MINUTOS")
#tempo_med_sul_dia$DIA <- factor(tempo_med_sul_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#tempo_med_sul_dia$DIA <- revalue(tempo_med_sul_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
tempo_med_sul_dia$DIA <- revalue(tempo_med_sul_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))
a <- aggregate(tempo_med_sul_dia$MINUTOS, by = list(tempo_med_sul_dia$DIA), FUN = median)
a$ESPECIALIDADE <- "Total"
names(a) <- c("DIA", "MINUTOS", "ESPECIALIDADE")
tempo_med_sul_dia <- rbind(tempo_med_sul_dia, a)
tempo_med_sul_dia$MINUTOS <- round(tempo_med_sul_dia$MINUTOS,2) 

#Hora do dia
tempo_med_sul_hora <-na.omit(tempo_med_sul)
tempo_med_sul_hora <- subset(tempo_med_sul_hora, as.yearmon(tempo_med_sul_hora$INI_ATD) >= max(as.yearmon(tempo_med_sul_hora$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
tempo_med_sul_hora$HORA <- format.POSIXct(as.POSIXct(tempo_med_sul_hora$INI_ATD), "%H")
tempo_med_sul_hora$INI_ATD <- NULL
tempo_med_sul_hora <- aggregate(tempo_med_sul_hora$TEMPO_ATD, by = list(tempo_med_sul_hora$ESPECIALIDADE, tempo_med_sul_hora$HORA), FUN = median, na.rm = T)
names(tempo_med_sul_hora) <- c("ESPECIALIDADE", "HORA", "MINUTOS")
a <- aggregate(tempo_med_sul_hora$MINUTOS, by = list(tempo_med_sul_hora$HORA), FUN = median)
a$ESPECIALIDADE <- "Total"
names(a) <- c("HORA", "MINUTOS", "ESPECIALIDADE")
tempo_med_sul_hora <- rbind(tempo_med_sul_hora, a)
tempo_med_sul_hora$MINUTOS <- round(tempo_med_sul_hora$MINUTOS, 2) 

#Mês-Ano
tempo_med_norte_mes <- tempo_med_norte
tempo_med_norte_mes$INI_ATD <- as.yearmon(tempo_med_norte_mes$INI_ATD, format = "%Y-%m-%d")
tempo_med_norte_mes <- aggregate(tempo_med_norte_mes$TEMPO_ATD, by = list(tempo_med_norte_mes$INI_ATD, tempo_med_norte_mes$ESPECIALIDADE), FUN = median) %>% as.data.frame()
names(tempo_med_norte_mes) <- c("DATA", "ESPECIALIDADE", "MINUTOS")
a <- aggregate(tempo_med_norte_mes$MINUTOS, by = list(tempo_med_norte_mes$DATA), FUN = median)
a$ESPECIALIDADE <- "Total"
names(a) <- c("DATA", "MINUTOS", "ESPECIALIDADE")
tempo_med_norte_mes <- rbind(tempo_med_norte_mes, a)
tempo_med_norte_mes$MINUTOS <- round(tempo_med_norte_mes$MINUTOS, 2)
tempo_med_norte_mes$DATA <- as.Date(tempo_med_norte_mes$DATA, format = "%d-%m-%Y")

#Dia-da-Semana
#Será a média do último semestre
tempo_med_norte_dia <-na.omit(tempo_med_norte)
tempo_med_norte_dia <- subset(tempo_med_norte_dia, as.yearmon(tempo_med_norte_dia$INI_ATD) >= max(as.yearmon(tempo_med_norte_dia$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
tempo_med_norte_dia$DIA <- format.Date(as.Date(tempo_med_norte_dia$INI_ATD), "%a")
tempo_med_norte_dia$INI_ATD <- NULL
tempo_med_norte_dia <- aggregate(tempo_med_norte_dia$TEMPO_ATD, by = list(tempo_med_norte_dia$ESPECIALIDADE, tempo_med_norte_dia$DIA), FUN = median, na.rm = T)
names(tempo_med_norte_dia) <- c("ESPECIALIDADE", "DIA", "MINUTOS")
#tempo_med_norte_dia$DIA <- factor(tempo_med_norte_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#tempo_med_norte_dia$DIA <- revalue(tempo_med_norte_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
tempo_med_norte_dia$DIA <- revalue(tempo_med_norte_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))
a <- aggregate(tempo_med_norte_dia$MINUTOS, by = list(tempo_med_norte_dia$DIA), FUN = median)
a$ESPECIALIDADE <- "Total"
names(a) <- c("DIA", "MINUTOS", "ESPECIALIDADE")
tempo_med_norte_dia <- rbind(tempo_med_norte_dia, a)
tempo_med_norte_dia$MINUTOS <- round(tempo_med_norte_dia$MINUTOS,2) 

#Hora do dia
tempo_med_norte_hora <- na.omit(tempo_med_norte)
tempo_med_norte_hora <- subset(tempo_med_norte_hora, as.yearmon(tempo_med_norte_hora$INI_ATD) >= max(as.yearmon(tempo_med_norte_hora$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
tempo_med_norte_hora$HORA <- format.POSIXct(as.POSIXct(tempo_med_norte_hora$INI_ATD), "%H")
tempo_med_norte_hora$INI_ATD <- NULL
tempo_med_norte_hora <- aggregate(tempo_med_norte_hora$TEMPO_ATD, by = list(tempo_med_norte_hora$ESPECIALIDADE, tempo_med_norte_hora$HORA), FUN = median, na.rm = T)
names(tempo_med_norte_hora) <- c("ESPECIALIDADE", "HORA", "MINUTOS")
a <- aggregate(tempo_med_norte_hora$MINUTOS, by = list(tempo_med_norte_hora$HORA), FUN = median)
a$ESPECIALIDADE <- "Total"
names(a) <- c("HORA", "MINUTOS", "ESPECIALIDADE")
tempo_med_norte_hora <- rbind(tempo_med_norte_hora, a)
tempo_med_norte_hora$MINUTOS <- round(tempo_med_norte_hora$MINUTOS, 2) 




#Escrever bancos de dados
write.csv(tempo_med_sul_mes, "base_de_dados/transformadas/tempo_med_sul_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_med_sul_dia, "base_de_dados/transformadas/tempo_med_sul_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_med_sul_hora, "base_de_dados/transformadas/tempo_med_sul_hora.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_med_norte_mes, "base_de_dados/transformadas/tempo_med_norte_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_med_norte_dia, "base_de_dados/transformadas/tempo_med_norte_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_med_norte_hora, "base_de_dados/transformadas/tempo_med_norte_hora.csv", fileEncoding = "UTF-8", row.names = F)

##############################################################################################################                   

#Agregando os dados - classificacao
#Importando a base
tempo_classificacao_new <- fread("base_de_dados/transformadas/tempo_classificacao.csv", encoding = 'UTF-8')

#Lendo as bases antigas
tempo_classificacao_old <- fread("base_de_dados/transformadas/tempo_classificacao_old.csv", encoding = 'UTF-8')

tempo_classificacao <- rbind(tempo_classificacao_new, tempo_classificacao_old) 

#Salvando as bases antigas como old
write.csv(tempo_classificacao, "base_de_dados/transformadas/tempo_classificacao_old.csv", fileEncoding = "UTF-8", row.names = F)



#Separando as upas
tempo_classificacao_sul <- subset(tempo_classificacao, tempo_classificacao$UNIDADE == "UPA Sul")
tempo_classificacao_sul <- select(tempo_classificacao_sul, -c(UNIDADE))
tempo_classificacao_sul$INI_ATD <- as.POSIXct(tempo_classificacao_sul$INI_ATD, format = "%Y-%m-%d %H:%M:%S")

tempo_classificacao_norte <- subset(tempo_classificacao, tempo_classificacao$UNIDADE == "UPA Norte")
tempo_classificacao_norte <- select(tempo_classificacao_norte, -c(UNIDADE))
tempo_classificacao_norte$INI_ATD <- as.POSIXct(tempo_classificacao_norte$INI_ATD, format = "%Y-%m-%d %H:%M:%S")


#Mês-Ano
tempo_classificacao_sul_mes <- tempo_classificacao_sul
tempo_classificacao_sul_mes$INI_ATD <- substr(tempo_classificacao_sul_mes$INI_ATD,0,10)
tempo_classificacao_sul_mes$INI_ATD <- as.yearmon(tempo_classificacao_sul_mes$INI_ATD, format = "%Y-%m-%d")
tempo_classificacao_sul_mes <- aggregate(tempo_classificacao_sul_mes$TEMPO_ATD, by = list(tempo_classificacao_sul_mes$INI_ATD, tempo_classificacao_sul_mes$CLASSIFICACAO_RISCO), FUN = median) %>% as.data.frame()
names(tempo_classificacao_sul_mes) <- c("DATA", "CLASSIFICACAO", "MINUTOS")
a <- aggregate(tempo_classificacao_sul_mes$MINUTOS, by = list(tempo_classificacao_sul_mes$DATA), FUN = median)
a$CLASSIFICACAO <- "Total"
names(a) <- c("DATA", "MINUTOS", "CLASSIFICACAO")
tempo_classificacao_sul_mes <- rbind(tempo_classificacao_sul_mes, a)
tempo_classificacao_sul_mes$MINUTOS <- round(tempo_classificacao_sul_mes$MINUTOS, 2)
tempo_classificacao_sul_mes$DATA <- as.Date(tempo_classificacao_sul_mes$DATA, format = "%d-%m-%Y")

#Dia-da-Semana
#Será a média do último semestre
tempo_classificacao_sul_dia <- na.omit(tempo_classificacao_sul)
tempo_classificacao_sul_dia <- subset(tempo_classificacao_sul_dia, as.yearmon(tempo_classificacao_sul_dia$INI_ATD) >= max(as.yearmon(tempo_classificacao_sul_dia$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
tempo_classificacao_sul_dia$DIA <- format.Date(as.Date(tempo_classificacao_sul_dia$INI_ATD), "%a")
tempo_classificacao_sul_dia$INI_ATD <- NULL
tempo_classificacao_sul_dia <- aggregate(tempo_classificacao_sul_dia$TEMPO_ATD, by = list(tempo_classificacao_sul_dia$CLASSIFICACAO_RISCO, tempo_classificacao_sul_dia$DIA), FUN = median, na.rm = T)
names(tempo_classificacao_sul_dia) <- c("CLASSIFICACAO", "DIA", "MINUTOS")
#tempo_classificacao_sul_dia$DIA <- factor(tempo_classificacao_sul_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#tempo_classificacao_sul_dia$DIA <- revalue(tempo_classificacao_sul_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
tempo_classificacao_sul_dia$DIA <- revalue(tempo_classificacao_sul_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))
a <- aggregate(tempo_classificacao_sul_dia$MINUTOS, by = list(tempo_classificacao_sul_dia$DIA), FUN = median)
a$CLASSIFICACAO <- "Total"
names(a) <- c("DIA", "MINUTOS", "CLASSIFICACAO")
tempo_classificacao_sul_dia <- rbind(tempo_classificacao_sul_dia, a)
tempo_classificacao_sul_dia$MINUTOS <- round(tempo_classificacao_sul_dia$MINUTOS,2) 

#Hora do dia
tempo_classificacao_sul_hora <- na.omit(tempo_classificacao_sul)
tempo_classificacao_sul_hora <- subset(tempo_classificacao_sul_hora, as.yearmon(tempo_classificacao_sul_hora$INI_ATD) >= max(as.yearmon(tempo_classificacao_sul_hora$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
tempo_classificacao_sul_hora$HORA <- format.POSIXct(as.POSIXct(tempo_classificacao_sul_hora$INI_ATD), "%H")
tempo_classificacao_sul_hora$INI_ATD <- NULL
tempo_classificacao_sul_hora <- aggregate(tempo_classificacao_sul_hora$TEMPO_ATD, by = list(tempo_classificacao_sul_hora$CLASSIFICACAO_RISCO, tempo_classificacao_sul_hora$HORA), FUN = median, na.rm = T)
names(tempo_classificacao_sul_hora) <- c("CLASSIFICACAO", "HORA", "MINUTOS")
a <- aggregate(tempo_classificacao_sul_hora$MINUTOS, by = list(tempo_classificacao_sul_hora$HORA), FUN = median)
a$CLASSIFICACAO <- "Total"
names(a) <- c("HORA", "MINUTOS", "CLASSIFICACAO")
tempo_classificacao_sul_hora <- rbind(tempo_classificacao_sul_hora, a)
tempo_classificacao_sul_hora$MINUTOS <- round(tempo_classificacao_sul_hora$MINUTOS, 2) 

#Mês-Ano
tempo_classificacao_norte_mes <- tempo_classificacao_norte
tempo_classificacao_norte_mes$INI_ATD <- substr(tempo_classificacao_norte_mes$INI_ATD,0,10)
tempo_classificacao_norte_mes$INI_ATD <- as.yearmon(tempo_classificacao_norte_mes$INI_ATD, format = "%Y-%m-%d")
tempo_classificacao_norte_mes <- aggregate(tempo_classificacao_norte_mes$TEMPO_ATD, by = list(tempo_classificacao_norte_mes$INI_ATD, tempo_classificacao_norte_mes$CLASSIFICACAO_RISCO), FUN = median) %>% as.data.frame()
names(tempo_classificacao_norte_mes) <- c("DATA", "CLASSIFICACAO", "MINUTOS")
a <- aggregate(tempo_classificacao_norte_mes$MINUTOS, by = list(tempo_classificacao_norte_mes$DATA), FUN = median)
a$CLASSIFICACAO <- "Total"
names(a) <- c("DATA", "MINUTOS", "CLASSIFICACAO")
tempo_classificacao_norte_mes <- rbind(tempo_classificacao_norte_mes, a)
tempo_classificacao_norte_mes$MINUTOS <- round(tempo_classificacao_norte_mes$MINUTOS, 2)
tempo_classificacao_norte_mes$DATA <- as.Date(tempo_classificacao_norte_mes$DATA, format = "%d-%m-%Y")


#Dia-da-Semana
#Será a média do último semestre
tempo_classificacao_norte_dia <- na.omit(tempo_classificacao_norte)
tempo_classificacao_norte_dia <- subset(tempo_classificacao_norte_dia, as.yearmon(tempo_classificacao_norte_dia$INI_ATD) >= max(as.yearmon(tempo_classificacao_norte_dia$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
tempo_classificacao_norte_dia$DIA <- format.Date(as.Date(tempo_classificacao_norte_dia$INI_ATD), "%a")
tempo_classificacao_norte_dia$INI_ATD <- NULL
tempo_classificacao_norte_dia <- aggregate(tempo_classificacao_norte_dia$TEMPO_ATD, by = list(tempo_classificacao_norte_dia$CLASSIFICACAO_RISCO, tempo_classificacao_norte_dia$DIA), FUN = median, na.rm = T)
names(tempo_classificacao_norte_dia) <- c("CLASSIFICACAO", "DIA", "MINUTOS")
#tempo_classificacao_norte_dia$DIA <- factor(tempo_classificacao_norte_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#tempo_classificacao_norte_dia$DIA <- revalue(tempo_classificacao_norte_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
tempo_classificacao_norte_dia$DIA <- revalue(tempo_classificacao_norte_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))
a <- aggregate(tempo_classificacao_norte_dia$MINUTOS, by = list(tempo_classificacao_norte_dia$DIA), FUN = median)
a$CLASSIFICACAO <- "Total"
names(a) <- c("DIA", "MINUTOS", "CLASSIFICACAO")
tempo_classificacao_norte_dia <- rbind(tempo_classificacao_norte_dia, a)
tempo_classificacao_norte_dia$MINUTOS <- round(tempo_classificacao_norte_dia$MINUTOS,2) 

#Hora do dia
tempo_classificacao_norte_hora <- na.omit(tempo_classificacao_norte)
tempo_classificacao_norte_hora <- subset(tempo_classificacao_norte_hora, as.yearmon(tempo_classificacao_norte_hora$INI_ATD) >= max(as.yearmon(tempo_classificacao_norte_hora$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
tempo_classificacao_norte_hora$HORA <- format.POSIXct(as.POSIXct(tempo_classificacao_norte_hora$INI_ATD), "%H")
tempo_classificacao_norte_hora$INI_ATD <- NULL
tempo_classificacao_norte_hora <- aggregate(tempo_classificacao_norte_hora$TEMPO_ATD, by = list(tempo_classificacao_norte_hora$CLASSIFICACAO_RISCO, tempo_classificacao_norte_hora$HORA), FUN = median, na.rm = T)
names(tempo_classificacao_norte_hora) <- c("CLASSIFICACAO", "HORA", "MINUTOS")
a <- aggregate(tempo_classificacao_norte_hora$MINUTOS, by = list(tempo_classificacao_norte_hora$HORA), FUN = median)
a$CLASSIFICACAO <- "Total"
names(a) <- c("HORA", "MINUTOS", "CLASSIFICACAO")
tempo_classificacao_norte_hora <- rbind(tempo_classificacao_norte_hora, a)
tempo_classificacao_norte_hora$MINUTOS <- round(tempo_classificacao_norte_hora$MINUTOS, 2) 


#Escrever bancos de dados
write.csv(tempo_classificacao_sul_mes, "base_de_dados/transformadas/tempo_classificacao_sul_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_classificacao_sul_dia, "base_de_dados/transformadas/tempo_classificacao_sul_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_classificacao_sul_hora, "base_de_dados/transformadas/tempo_classificacao_sul_hora.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_classificacao_norte_mes, "base_de_dados/transformadas/tempo_classificacao_norte_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_classificacao_norte_dia, "base_de_dados/transformadas/tempo_classificacao_norte_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_classificacao_norte_hora, "base_de_dados/transformadas/tempo_classificacao_norte_hora.csv", fileEncoding = "UTF-8", row.names = F)
##############################################################################################################                   

#Agregando os dados - odontologia
#Importando a base
tempo_odontologia_new <- fread("base_de_dados/transformadas/tempo_odont.csv", encoding = 'UTF-8')

#Lendo as bases antigas
tempo_odontologia_old <- fread("base_de_dados/transformadas/tempo_odont_old.csv", encoding = 'UTF-8')

tempo_odontologia <- rbind(tempo_odontologia_new, tempo_odontologia_old) 

#Salvando as bases antigas como old
write.csv(tempo_odontologia, "base_de_dados/transformadas/tempo_odont_old.csv", fileEncoding = "UTF-8", row.names = F)


tempo_odontologia <- na.omit(tempo_odontologia)
#Separando as upas
tempo_odontologia_sul <- subset(tempo_odontologia, tempo_odontologia$UNIDADE == "UPA Sul")
tempo_odontologia_sul <- select(tempo_odontologia_sul, -c(UNIDADE))
tempo_odontologia_sul$INI_ATD <- as.POSIXct(tempo_odontologia_sul$INI_ATD, format = "%Y-%m-%d %H:%M:%S")

tempo_odontologia_norte <- subset(tempo_odontologia, tempo_odontologia$UNIDADE == "UPA Norte")
tempo_odontologia_norte <- select(tempo_odontologia_norte, -c(UNIDADE))
tempo_odontologia_norte$INI_ATD <- as.POSIXct(tempo_odontologia_norte$INI_ATD, format = "%Y-%m-%d %H:%M:%S")


#Mês-Ano
tempo_odontologia_sul_mes <- tempo_odontologia_sul
tempo_odontologia_sul_mes$INI_ATD <- substr(tempo_odontologia_sul_mes$INI_ATD, 0, 10)
tempo_odontologia_sul_mes$INI_ATD <- as.yearmon(tempo_odontologia_sul_mes$INI_ATD, format = "%Y-%m-%d")
tempo_odontologia_sul_mes <- aggregate(tempo_odontologia_sul_mes$TEMPO_ATD, by = list(tempo_odontologia_sul_mes$INI_ATD), FUN = median) %>% as.data.frame()
names(tempo_odontologia_sul_mes) <- c("DATA", "MINUTOS")
tempo_odontologia_sul_mes$MINUTOS <- round(tempo_odontologia_sul_mes$MINUTOS, 2)
tempo_odontologia_sul_mes$DATA <- as.yearmon(tempo_odontologia_sul_mes$DATA, format = "%b %Y")
tempo_odontologia_sul_mes$DATA <- as.Date(tempo_odontologia_sul_mes$DATA, format = "%d-%m-%Y")


#Dia-da-Semana
#Será a média do último semestre
tempo_odontologia_sul_dia <- na.omit(tempo_odontologia_sul)
tempo_odontologia_sul_dia <- subset(tempo_odontologia_sul_dia, as.yearmon(tempo_odontologia_sul_dia$INI_ATD) >= max(as.yearmon(tempo_odontologia_sul_dia$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
tempo_odontologia_sul_dia$DIA <- format.Date(as.Date(tempo_odontologia_sul_dia$INI_ATD), "%a")
tempo_odontologia_sul_dia$INI_ATD <- NULL
tempo_odontologia_sul_dia <- aggregate(tempo_odontologia_sul_dia$TEMPO_ATD, by = list(tempo_odontologia_sul_dia$DIA), FUN = median, na.rm = T)
names(tempo_odontologia_sul_dia) <- c("DIA", "MINUTOS")
tempo_odontologia_sul_dia$MINUTOS <- round(tempo_odontologia_sul_dia$MINUTOS,2) 
#tempo_odontologia_sul_dia$DIA <- factor(tempo_odontologia_sul_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#tempo_odontologia_sul_dia$DIA <- revalue(tempo_odontologia_sul_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
tempo_odontologia_sul_dia$DIA <- revalue(tempo_odontologia_sul_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))


#Hora do dia
tempo_odontologia_sul_hora <- na.omit(tempo_odontologia_sul)
tempo_odontologia_sul_hora <- subset(tempo_odontologia_sul_hora, as.yearmon(tempo_odontologia_sul_hora$INI_ATD) >= max(as.yearmon(tempo_odontologia_sul_hora$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
tempo_odontologia_sul_hora$HORA <- format.POSIXct(as.POSIXct(tempo_odontologia_sul_hora$INI_ATD), "%H")
tempo_odontologia_sul_hora$INI_ATD <- NULL
tempo_odontologia_sul_hora <- aggregate(tempo_odontologia_sul_hora$TEMPO_ATD, by = list(tempo_odontologia_sul_hora$HORA), FUN = median, na.rm = T)
names(tempo_odontologia_sul_hora) <- c("HORA", "MINUTOS")
tempo_odontologia_sul_hora$MINUTOS <- round(tempo_odontologia_sul_hora$MINUTOS,2) 

#Mês-Ano
tempo_odontologia_norte_mes <- tempo_odontologia_norte
tempo_odontologia_norte_mes$INI_ATD <- substr(tempo_odontologia_norte_mes$INI_ATD, 0, 10)
tempo_odontologia_norte_mes$INI_ATD <- as.yearmon(tempo_odontologia_norte_mes$INI_ATD, format = "%Y-%m-%d")
tempo_odontologia_norte_mes <- aggregate(tempo_odontologia_norte_mes$TEMPO_ATD, by = list(tempo_odontologia_norte_mes$INI_ATD), FUN = median) %>% as.data.frame()
names(tempo_odontologia_norte_mes) <- c("DATA", "MINUTOS")
tempo_odontologia_norte_mes$MINUTOS <- round(tempo_odontologia_norte_mes$MINUTOS, 2)
tempo_odontologia_norte_mes$DATA <- as.yearmon(tempo_odontologia_norte_mes$DATA, format = "%b %Y")
tempo_odontologia_norte_mes$DATA <- as.Date(tempo_odontologia_norte_mes$DATA, format = "%d-%m-%Y")


#Dia-da-Semana
#Será a média do último semestre
tempo_odontologia_norte_dia <- na.omit(tempo_odontologia_norte)
tempo_odontologia_norte_dia <- subset(tempo_odontologia_norte_dia, as.yearmon(tempo_odontologia_norte_dia$INI_ATD) >= max(as.yearmon(tempo_odontologia_norte_dia$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
tempo_odontologia_norte_dia$DIA <- format.Date(as.Date(tempo_odontologia_norte_dia$INI_ATD), "%a")
tempo_odontologia_norte_dia$INI_ATD <- NULL
tempo_odontologia_norte_dia <- aggregate(tempo_odontologia_norte_dia$TEMPO_ATD, by = list(tempo_odontologia_norte_dia$DIA), FUN = median, na.rm = T)
names(tempo_odontologia_norte_dia) <- c("DIA", "MINUTOS")
tempo_odontologia_norte_dia$MINUTOS <- round(tempo_odontologia_norte_dia$MINUTOS,2) 
#tempo_odontologia_norte_dia$DIA <- factor(tempo_odontologia_norte_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#tempo_odontologia_norte_dia$DIA <- revalue(tempo_odontologia_norte_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
tempo_odontologia_norte_dia$DIA <- revalue(tempo_odontologia_norte_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))


#Hora do dia
tempo_odontologia_norte_hora <- na.omit(tempo_odontologia_norte)
tempo_odontologia_norte_hora <- subset(tempo_odontologia_norte_hora, as.yearmon(tempo_odontologia_norte_hora$INI_ATD) >= max(as.yearmon(tempo_odontologia_norte_hora$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
tempo_odontologia_norte_hora$HORA <- format.POSIXct(as.POSIXct(tempo_odontologia_norte_hora$INI_ATD), "%H")
tempo_odontologia_norte_hora$INI_ATD <- NULL
tempo_odontologia_norte_hora <- aggregate(tempo_odontologia_norte_hora$TEMPO_ATD, by = list(tempo_odontologia_norte_hora$HORA), FUN = median, na.rm = T)
names(tempo_odontologia_norte_hora) <- c("HORA", "MINUTOS")
tempo_odontologia_norte_hora$MINUTOS <- round(tempo_odontologia_norte_hora$MINUTOS,2)


#Escrever bancos de dados
write.csv(tempo_odontologia_sul_mes, "base_de_dados/transformadas/tempo_odontologia_sul_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_odontologia_sul_dia, "base_de_dados/transformadas/tempo_odontologia_sul_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_odontologia_sul_hora, "base_de_dados/transformadas/tempo_odontologia_sul_hora.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_odontologia_norte_mes, "base_de_dados/transformadas/tempo_odontologia_norte_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_odontologia_norte_dia, "base_de_dados/transformadas/tempo_odontologia_norte_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(tempo_odontologia_norte_hora, "base_de_dados/transformadas/tempo_odontologia_norte_hora.csv", fileEncoding = "UTF-8", row.names = F)


############################################################################################################## 
#analisando o tempo de cirurgia às 3h que está dando muito alto
#cirurgia <- subset(tempo_med_norte,tempo_med_norte$ESPECIALIDADE == "Cirurgia")
#cirurgia <- subset(cirurgia, cirurgia$INI_ATD >= as.POSIXct("2017-02-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"))
#cirurgia$HORA <-format.POSIXct(as.POSIXct(cirurgia$INI_ATD), "%H")
#cirurgia_3h <- subset(cirurgia, cirurgia$HORA == "03")
#summary(cirurgia_3h$TEMPO_ATD)


