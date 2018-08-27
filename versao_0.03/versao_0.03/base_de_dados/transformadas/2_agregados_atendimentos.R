options(encoding = "UTF-8")
library(tidyverse)
library(readr)
library(data.table)
library(zoo)
library(Kmisc) #pacote para manipulação de string
library(lubridate)
library(dplyr)
library(plyr)
library(reshape2)


##############################################################################################################

#Agregando os dados - médicos
#Importando a base
atendimentos_med_new <- fread("base_de_dados/transformadas/atendimentos_med.csv", encoding = 'UTF-8')

#Lendo as bases antigas
atendimentos_med_old <- fread("base_de_dados/transformadas/atendimentos_med_old.csv", encoding = 'UTF-8')

atendimentos_med <- rbind(atendimentos_med_new, atendimentos_med_old) 

#Salvando as bases antigas como old
write.csv(atendimentos_med, "base_de_dados/transformadas/atendimentos_med_old.csv", fileEncoding = "UTF-8", row.names = F)

#Fazendo rbind das bases antigas com as novas


#Separando as upas
atendimentos_med_sul <- subset(atendimentos_med, atendimentos_med$UNIDADE == "UPA Sul")
atendimentos_med_sul <- select(atendimentos_med_sul, -c(UNIDADE))


atendimentos_med_norte <- subset(atendimentos_med, atendimentos_med$UNIDADE == "UPA Norte")
atendimentos_med_norte <- select(atendimentos_med_norte, -c(UNIDADE))



#Mês-Ano
atendimentos_med_sul_mes <- atendimentos_med_sul 
atendimentos_med_sul_mes$INI_ATD <- as.yearmon(atendimentos_med_sul_mes$INI_ATD, format = "%Y-%m-%d")
atendimentos_med_sul_mes$QUANTIDADE <- 1
atendimentos_med_sul_mes <- aggregate(atendimentos_med_sul_mes$QUANTIDADE, 
                                      by = list(atendimentos_med_sul_mes$ESPECIALIDADE, 
                                                atendimentos_med_sul_mes$INI_ATD), FUN = sum) 
names(atendimentos_med_sul_mes) <- c("ESPECIALIDADE","DATA", "QUANTIDADE")
atendimentos_med_sul_mes$VARIAVEL <- NA #Criando variável para dizer se a UPA está atingindo o preconizado pela portaria
a <- aggregate(atendimentos_med_sul_mes$QUANTIDADE, by = list(atendimentos_med_sul_mes$DATA), FUN = sum) 
a$ESPECIALIDADE <- "Total"
names(a) <- c("DATA", "QUANTIDADE", "ESPECIALIDADE")
a$VARIAVEL <- "PRODUZIDO"
b <-a
b$QUANTIDADE <- 10125 - a$QUANTIDADE #Subtraindo o que deveria ser produzido por uma UPA porte 8, como que que foi produzido pela UPA Norte
b$QUANTIDADE[which(b$QUANTIDADE < 0)] <- 0
b$VARIAVEL <- "DEFCIT UPA PORTE 8"
atendimentos_med_sul_mes <- rbind(atendimentos_med_sul_mes, a, b)
atendimentos_med_sul_mes$DATA <- as.Date(atendimentos_med_sul_mes$DATA, format = "%d-%m-%Y")
atendimentos_med_sul_mes$DATA <- as.character(atendimentos_med_sul_mes$DATA)
atendimentos_med_sul_mes$QUANTIDADE <- as.integer(atendimentos_med_sul_mes$QUANTIDADE)

#Dia-da-Semana
#Será a média do último semestre
atendimentos_med_sul_dia <- subset(atendimentos_med_sul, as.yearmon(atendimentos_med_sul$INI_ATD) >= max(as.yearmon(atendimentos_med_sul$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
atendimentos_med_sul_dia$DIA <- format.Date(as.Date(atendimentos_med_sul_dia$INI_ATD), "%a")
atendimentos_med_sul_dia$SEMANA <- format.Date(as.Date(atendimentos_med_sul_dia$INI_ATD), "%W")
atendimentos_med_sul_dia$INI_ATD <- NULL
atendimentos_med_sul_dia$QUANTIDADE <- 1
atendimentos_med_sul_dia <- aggregate(atendimentos_med_sul_dia$QUANTIDADE, by = list(atendimentos_med_sul_dia$ESPECIALIDADE, atendimentos_med_sul_dia$SEMANA, atendimentos_med_sul_dia$DIA), FUN = sum, na.rm = T)#somando os atendimentos por dia da semana
names(atendimentos_med_sul_dia) <- c("ESPECIALIDADE", "SEMANA", "DIA", "QUANTIDADE")
atendimentos_med_sul_dia <- aggregate(atendimentos_med_sul_dia$QUANTIDADE, by = list(atendimentos_med_sul_dia$ESPECIALIDADE, atendimentos_med_sul_dia$DIA), FUN = median, na.rm = T)#fazendo a mediana por semana
names(atendimentos_med_sul_dia) <- c("ESPECIALIDADE", "DIA", "QUANTIDADE")
#atendimentos_med_sul_dia$DIA <- factor(atendimentos_med_sul_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#atendimentos_med_sul_dia$DIA <- revalue(atendimentos_med_sul_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
atendimentos_med_sul_dia$DIA <- revalue(atendimentos_med_sul_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))
a <- aggregate(atendimentos_med_sul_dia$QUANTIDADE, by = list(atendimentos_med_sul_dia$DIA), FUN = sum)#soma de todos os atendimentos médicos
a$ESPECIALIDADE <- "Total"
names(a) <- c("DIA", "QUANTIDADE", "ESPECIALIDADE")
atendimentos_med_sul_dia <- rbind(atendimentos_med_sul_dia, a)

#Hora do dia
atendimentos_med_sul_hora <- subset(atendimentos_med_sul, as.yearmon(atendimentos_med_sul$INI_ATD) >= max(as.yearmon(atendimentos_med_sul$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
atendimentos_med_sul_hora$HORA <- format.POSIXct(as.POSIXct(atendimentos_med_sul_hora$INI_ATD), "%H")
atendimentos_med_sul_hora$DIA <- format.POSIXct(as.POSIXct(atendimentos_med_sul_hora$INI_ATD), "%d")
atendimentos_med_sul_hora$INI_ATD <- NULL
atendimentos_med_sul_hora$QUANTIDADE <- 1
atendimentos_med_sul_hora <- aggregate(atendimentos_med_sul_hora$QUANTIDADE, by = list(atendimentos_med_sul_hora$ESPECIALIDADE,atendimentos_med_sul_hora$DIA, atendimentos_med_sul_hora$HORA), FUN = sum, na.rm = T)#somando os atendimentos por dia do mês
names(atendimentos_med_sul_hora) <- c("ESPECIALIDADE", "DIA", "HORA", "QUANTIDADE")
atendimentos_med_sul_hora <- aggregate(atendimentos_med_sul_hora$QUANTIDADE, by = list(atendimentos_med_sul_hora$ESPECIALIDADE, atendimentos_med_sul_hora$HORA), FUN = median, na.rm = T)#calculando a mediana por dia do mês
names(atendimentos_med_sul_hora) <- c("ESPECIALIDADE", "HORA", "QUANTIDADE")
a <- aggregate(atendimentos_med_sul_hora$QUANTIDADE, by = list(atendimentos_med_sul_hora$HORA), FUN = sum)
a$ESPECIALIDADE <- "Total"
names(a) <- c("HORA", "QUANTIDADE", "ESPECIALIDADE")
atendimentos_med_sul_hora <- rbind(atendimentos_med_sul_hora, a)


#Mês-Ano
atendimentos_med_norte_mes <- atendimentos_med_norte 
atendimentos_med_norte_mes$INI_ATD <- as.yearmon(atendimentos_med_norte_mes$INI_ATD, format = "%Y-%m-%d")
atendimentos_med_norte_mes$QUANTIDADE <- 1
atendimentos_med_norte_mes <- aggregate(atendimentos_med_norte_mes$QUANTIDADE, by = list(atendimentos_med_norte_mes$ESPECIALIDADE, atendimentos_med_norte_mes$INI_ATD), FUN = sum) 
names(atendimentos_med_norte_mes) <- c("ESPECIALIDADE","DATA", "QUANTIDADE")
atendimentos_med_norte_mes$VARIAVEL <- NA #Criando variável para dizer se a UPA está atingindo o preconizado pela portaria
a <- aggregate(atendimentos_med_norte_mes$QUANTIDADE, by = list(atendimentos_med_norte_mes$DATA), FUN = sum) 
a$ESPECIALIDADE <- "Total"
names(a) <- c("DATA", "QUANTIDADE", "ESPECIALIDADE")
a$VARIAVEL <- "PRODUZIDO"
b <-a
b$QUANTIDADE <- 10125 - a$QUANTIDADE #Subtraindo o que deveria ser produzido por uma UPA porte 8, como que que foi produzido pela UPA Norte
b$QUANTIDADE[which(b$QUANTIDADE < 0)] <- 0
b$VARIAVEL <- "DEFCIT UPA PORTE 8"
atendimentos_med_norte_mes <- rbind(atendimentos_med_norte_mes, a, b)
atendimentos_med_norte_mes$DATA <- as.Date(atendimentos_med_norte_mes$DATA, format = "%d-%m-%Y")
atendimentos_med_norte_mes$DATA <- as.character(atendimentos_med_norte_mes$DATA)
atendimentos_med_norte_mes$QUANTIDADE <- as.integer(atendimentos_med_norte_mes$QUANTIDADE)



#Dia-da-Semana
#Será a média do último semestre
atendimentos_med_norte_dia <- subset(atendimentos_med_norte, as.yearmon(atendimentos_med_norte$INI_ATD) >= max(as.yearmon(atendimentos_med_norte$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
atendimentos_med_norte_dia$DIA <- format.Date(as.Date(atendimentos_med_norte_dia$INI_ATD), "%a")
atendimentos_med_norte_dia$SEMANA <- format.Date(as.Date(atendimentos_med_norte_dia$INI_ATD), "%W")
atendimentos_med_norte_dia$INI_ATD <- NULL
atendimentos_med_norte_dia$QUANTIDADE <- 1
atendimentos_med_norte_dia <- aggregate(atendimentos_med_norte_dia$QUANTIDADE, by = list(atendimentos_med_norte_dia$ESPECIALIDADE, atendimentos_med_norte_dia$SEMANA, atendimentos_med_norte_dia$DIA), FUN = sum, na.rm = T)#somando os atendimentos por dia da semana
names(atendimentos_med_norte_dia) <- c("ESPECIALIDADE", "SEMANA", "DIA", "QUANTIDADE")
atendimentos_med_norte_dia <- aggregate(atendimentos_med_norte_dia$QUANTIDADE, by = list(atendimentos_med_norte_dia$ESPECIALIDADE, atendimentos_med_norte_dia$DIA), FUN = median, na.rm = T)#fazendo a mediana por semana
names(atendimentos_med_norte_dia) <- c("ESPECIALIDADE", "DIA", "QUANTIDADE")
#atendimentos_med_norte_dia$DIA <- factor(atendimentos_med_norte_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#atendimentos_med_norte_dia$DIA <- revalue(atendimentos_med_norte_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
atendimentos_med_norte_dia$DIA <- revalue(atendimentos_med_norte_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))
a <- aggregate(atendimentos_med_norte_dia$QUANTIDADE, by = list(atendimentos_med_norte_dia$DIA), FUN = sum)#soma de todos os atendimentos médicos
a$ESPECIALIDADE <- "Total"
names(a) <- c("DIA", "QUANTIDADE", "ESPECIALIDADE")
atendimentos_med_norte_dia <- rbind(atendimentos_med_norte_dia, a)

#Hora do dia
atendimentos_med_norte_hora <- subset(atendimentos_med_norte, as.yearmon(atendimentos_med_norte$INI_ATD) >= max(as.yearmon(atendimentos_med_norte$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
atendimentos_med_norte_hora$HORA <- format.POSIXct(as.POSIXct(atendimentos_med_norte_hora$INI_ATD), "%H")
atendimentos_med_norte_hora$DIA <- format.POSIXct(as.POSIXct(atendimentos_med_norte_hora$INI_ATD), "%d")
atendimentos_med_norte_hora$INI_ATD <- NULL
atendimentos_med_norte_hora$QUANTIDADE <- 1
atendimentos_med_norte_hora <- aggregate(atendimentos_med_norte_hora$QUANTIDADE, by = list(atendimentos_med_norte_hora$ESPECIALIDADE,atendimentos_med_norte_hora$DIA, atendimentos_med_norte_hora$HORA), FUN = sum, na.rm = T)#somando os atendimentos por dia do mês
names(atendimentos_med_norte_hora) <- c("ESPECIALIDADE", "DIA", "HORA", "QUANTIDADE")
atendimentos_med_norte_hora <- aggregate(atendimentos_med_norte_hora$QUANTIDADE, by = list(atendimentos_med_norte_hora$ESPECIALIDADE, atendimentos_med_norte_hora$HORA), FUN = median, na.rm = T)#calculando a mediana por dia do mês
names(atendimentos_med_norte_hora) <- c("ESPECIALIDADE", "HORA", "QUANTIDADE")
a <- aggregate(atendimentos_med_norte_hora$QUANTIDADE, by = list(atendimentos_med_norte_hora$HORA), FUN = sum)
a$ESPECIALIDADE <- "Total"
names(a) <- c("HORA", "QUANTIDADE", "ESPECIALIDADE")
atendimentos_med_norte_hora <- rbind(atendimentos_med_norte_hora, a)


#Escrever bases atualizadas
write.csv(atendimentos_med_sul_mes, "base_de_dados/transformadas/atendimentos_med_sul_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_med_sul_dia, "base_de_dados/transformadas/atendimentos_med_sul_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_med_sul_hora, "base_de_dados/transformadas/atendimentos_med_sul_hora.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_med_norte_mes, "base_de_dados/transformadas/atendimentos_med_norte_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_med_norte_dia, "base_de_dados/transformadas/atendimentos_med_norte_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_med_norte_hora, "base_de_dados/transformadas/atendimentos_med_norte_hora.csv", fileEncoding = "UTF-8", row.names = F)


##############################################################################################################                   

#Agregando os dados - classificação de risco

#Importando a base
atendimentos_classificacao_new <- fread("base_de_dados/transformadas/atendimentos_classificacao.csv", encoding = 'UTF-8')

#Lendo as bases antigas
atendimentos_classificacao_old <- fread("base_de_dados/transformadas/atendimentos_classificacao_old.csv", encoding = 'UTF-8')

atendimentos_classificacao <- rbind(atendimentos_classificacao_new, atendimentos_classificacao_old) 

atendimentos_classificacao <- na.omit(atendimentos_classificacao)
#Salvando as bases antigas como old
write.csv(atendimentos_classificacao, "base_de_dados/transformadas/atendimentos_classificacao_old.csv", fileEncoding = "UTF-8", row.names = F)


#Separando as upas
atendimentos_classificacao_sul <- subset(atendimentos_classificacao, atendimentos_classificacao$UNIDADE == "UPA Sul")
atendimentos_classificacao_sul <- select(atendimentos_classificacao_sul, -c(UNIDADE))
atendimentos_classificacao_sul$INI_ATD <- as.POSIXct(atendimentos_classificacao_sul$INI_ATD, format = "%Y-%m-%d %H:%M:%S")

atendimentos_classificacao_norte <- subset(atendimentos_classificacao, atendimentos_classificacao$UNIDADE == "UPA Norte")
atendimentos_classificacao_norte <- select(atendimentos_classificacao_norte, -c(UNIDADE))
atendimentos_classificacao_norte$INI_ATD <- as.POSIXct(atendimentos_classificacao_norte$INI_ATD, format = "%Y-%m-%d %H:%M:%S")


#Mês-Ano
atendimentos_classificacao_sul_mes <- atendimentos_classificacao_sul 
atendimentos_classificacao_sul_mes$INI_ATD <- substr(atendimentos_classificacao_sul_mes$INI_ATD,0,10)
atendimentos_classificacao_sul_mes$INI_ATD <- as.yearmon(atendimentos_classificacao_sul_mes$INI_ATD, format = "%Y-%m-%d")
atendimentos_classificacao_sul_mes$QUANTIDADE <- 1
atendimentos_classificacao_sul_mes <- aggregate(atendimentos_classificacao_sul_mes$QUANTIDADE, by = list(atendimentos_classificacao_sul_mes$CLASSIFICACAO_RISCO, atendimentos_classificacao_sul_mes$INI_ATD), FUN = sum) 
names(atendimentos_classificacao_sul_mes) <- c("CLASSIFICACAO","DATA", "QUANTIDADE")
atendimentos_classificacao_sul_mes$VARIAVEL <- NA #Criando variável para dizer se a UPA está atingindo o preconizado pela portaria
a <- aggregate(atendimentos_classificacao_sul_mes$QUANTIDADE, by = list(atendimentos_classificacao_sul_mes$DATA), FUN = sum) 
a$CLASSIFICACAO <- "Total"
names(a) <- c("DATA", "QUANTIDADE", "CLASSIFICACAO")
a$VARIAVEL <- "PRODUZIDO"
b <-a
b$QUANTIDADE <- 10125 - a$QUANTIDADE #Subtraindo o que deveria ser produzido por uma UPA porte 8, como que que foi produzido pela UPA Norte
b$QUANTIDADE[which(b$QUANTIDADE < 0)] <- 0
b$VARIAVEL <- "DEFCIT UPA PORTE 8"
atendimentos_classificacao_sul_mes <- rbind(atendimentos_classificacao_sul_mes, a, b)
atendimentos_classificacao_sul_mes$DATA <- as.Date(atendimentos_classificacao_sul_mes$DATA, format = "%d-%m-%Y")
atendimentos_classificacao_sul_mes$DATA <- as.character(atendimentos_classificacao_sul_mes$DATA)
atendimentos_classificacao_sul_mes$QUANTIDADE <- as.integer(atendimentos_classificacao_sul_mes$QUANTIDADE)


#Dia-da-Semana
#Será a média do último semestre
atendimentos_classificacao_sul_dia <- subset(atendimentos_classificacao_sul, as.yearmon(atendimentos_classificacao_sul$INI_ATD) >= max(as.yearmon(atendimentos_classificacao_sul$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
atendimentos_classificacao_sul_dia$DIA <- format.Date(as.Date(atendimentos_classificacao_sul_dia$INI_ATD), "%a")
atendimentos_classificacao_sul_dia$SEMANA <- format.Date(as.Date(atendimentos_classificacao_sul_dia$INI_ATD), "%W")
atendimentos_classificacao_sul_dia$INI_ATD <- NULL
atendimentos_classificacao_sul_dia$QUANTIDADE <- 1
atendimentos_classificacao_sul_dia <- aggregate(atendimentos_classificacao_sul_dia$QUANTIDADE, by = list(atendimentos_classificacao_sul_dia$CLASSIFICACAO_RISCO, atendimentos_classificacao_sul_dia$SEMANA, atendimentos_classificacao_sul_dia$DIA), FUN = sum, na.rm = T)#somando os atendimentos por dia da semana
names(atendimentos_classificacao_sul_dia) <- c("CLASSIFICACAO", "SEMANA", "DIA", "QUANTIDADE")
atendimentos_classificacao_sul_dia <- aggregate(atendimentos_classificacao_sul_dia$QUANTIDADE, by = list(atendimentos_classificacao_sul_dia$CLASSIFICACAO, atendimentos_classificacao_sul_dia$DIA), FUN = median, na.rm = T)#fazendo a classificacaoiana por semana
names(atendimentos_classificacao_sul_dia) <- c("CLASSIFICACAO", "DIA", "QUANTIDADE")
#atendimentos_classificacao_sul_dia$DIA <- factor(atendimentos_classificacao_sul_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#atendimentos_classificacao_sul_dia$DIA <- revalue(atendimentos_classificacao_sul_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
atendimentos_classificacao_sul_dia$DIA <- revalue(atendimentos_classificacao_sul_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))
a <- aggregate(atendimentos_classificacao_sul_dia$QUANTIDADE, by = list(atendimentos_classificacao_sul_dia$DIA), FUN = sum)#soma de todos os atendimentos médicos
a$CLASSIFICACAO <- "Total"
names(a) <- c("DIA", "QUANTIDADE", "CLASSIFICACAO")
atendimentos_classificacao_sul_dia <- rbind(atendimentos_classificacao_sul_dia, a)

#Hora do dia
atendimentos_classificacao_sul_hora <- subset(atendimentos_classificacao_sul, as.yearmon(atendimentos_classificacao_sul$INI_ATD) >= max(as.yearmon(atendimentos_classificacao_sul$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
atendimentos_classificacao_sul_hora$HORA <- format.POSIXct(as.POSIXct(atendimentos_classificacao_sul_hora$INI_ATD), "%H")
atendimentos_classificacao_sul_hora$DIA <- format.POSIXct(as.POSIXct(atendimentos_classificacao_sul_hora$INI_ATD), "%d")
atendimentos_classificacao_sul_hora$INI_ATD <- NULL
atendimentos_classificacao_sul_hora$QUANTIDADE <- 1
atendimentos_classificacao_sul_hora <- aggregate(atendimentos_classificacao_sul_hora$QUANTIDADE, by = list(atendimentos_classificacao_sul_hora$CLASSIFICACAO_RISCO,atendimentos_classificacao_sul_hora$DIA, atendimentos_classificacao_sul_hora$HORA), FUN = sum, na.rm = T)#somando os atendimentos por dia do mês
names(atendimentos_classificacao_sul_hora) <- c("CLASSIFICACAO", "DIA", "HORA", "QUANTIDADE")
atendimentos_classificacao_sul_hora <- aggregate(atendimentos_classificacao_sul_hora$QUANTIDADE, by = list(atendimentos_classificacao_sul_hora$CLASSIFICACAO, atendimentos_classificacao_sul_hora$HORA), FUN = median, na.rm = T)#calculando a classificacaoiana por dia do mês
names(atendimentos_classificacao_sul_hora) <- c("CLASSIFICACAO", "HORA", "QUANTIDADE")
a <- aggregate(atendimentos_classificacao_sul_hora$QUANTIDADE, by = list(atendimentos_classificacao_sul_hora$HORA), FUN = sum)
a$CLASSIFICACAO <- "Total"
names(a) <- c("HORA", "QUANTIDADE", "CLASSIFICACAO")
atendimentos_classificacao_sul_hora <- rbind(atendimentos_classificacao_sul_hora, a)


#Mês-Ano
atendimentos_classificacao_norte_mes <- atendimentos_classificacao_norte
atendimentos_classificacao_norte_mes$INI_ATD <- substr(atendimentos_classificacao_norte_mes$INI_ATD,0,10)
atendimentos_classificacao_norte_mes$INI_ATD <- as.yearmon(atendimentos_classificacao_norte_mes$INI_ATD, format = "%Y-%m-%d")
atendimentos_classificacao_norte_mes$QUANTIDADE <- 1
atendimentos_classificacao_norte_mes <- aggregate(atendimentos_classificacao_norte_mes$QUANTIDADE, by = list(atendimentos_classificacao_norte_mes$CLASSIFICACAO_RISCO, atendimentos_classificacao_norte_mes$INI_ATD), FUN = sum) 
names(atendimentos_classificacao_norte_mes) <- c("CLASSIFICACAO","DATA", "QUANTIDADE")
atendimentos_classificacao_norte_mes$VARIAVEL <- NA #Criando variável para dizer se a UPA está atingindo o preconizado pela portaria
a <- aggregate(atendimentos_classificacao_norte_mes$QUANTIDADE, by = list(atendimentos_classificacao_norte_mes$DATA), FUN = sum) 
a$CLASSIFICACAO <- "Total"
names(a) <- c("DATA", "QUANTIDADE", "CLASSIFICACAO")
a$VARIAVEL <- "PRODUZIDO"
b <-a
b$QUANTIDADE <- 10125 - a$QUANTIDADE #Subtraindo o que deveria ser produzido por uma UPA porte 8, como que que foi produzido pela UPA Norte
b$QUANTIDADE[which(b$QUANTIDADE < 0)] <- 0
b$VARIAVEL <- "DEFCIT UPA PORTE 8"
atendimentos_classificacao_norte_mes <- rbind(atendimentos_classificacao_norte_mes, a, b)
atendimentos_classificacao_norte_mes$DATA <- as.Date(atendimentos_classificacao_norte_mes$DATA, format = "%d-%m-%Y")
atendimentos_classificacao_norte_mes$DATA <- as.character(atendimentos_classificacao_norte_mes$DATA)
atendimentos_classificacao_norte_mes$QUANTIDADE <- as.integer(atendimentos_classificacao_norte_mes$QUANTIDADE)


#Dia-da-Semana
#Será a média do último semestre
atendimentos_classificacao_norte_dia <- subset(atendimentos_classificacao_norte, as.yearmon(atendimentos_classificacao_norte$INI_ATD) >= max(as.yearmon(atendimentos_classificacao_norte$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
atendimentos_classificacao_norte_dia$DIA <- format.Date(as.Date(atendimentos_classificacao_norte_dia$INI_ATD), "%a")
atendimentos_classificacao_norte_dia$SEMANA <- format.Date(as.Date(atendimentos_classificacao_norte_dia$INI_ATD), "%W")
atendimentos_classificacao_norte_dia$INI_ATD <- NULL
atendimentos_classificacao_norte_dia$QUANTIDADE <- 1
atendimentos_classificacao_norte_dia <- aggregate(atendimentos_classificacao_norte_dia$QUANTIDADE, by = list(atendimentos_classificacao_norte_dia$CLASSIFICACAO_RISCO, atendimentos_classificacao_norte_dia$SEMANA, atendimentos_classificacao_norte_dia$DIA), FUN = sum, na.rm = T)#somando os atendimentos por dia da semana
names(atendimentos_classificacao_norte_dia) <- c("CLASSIFICACAO", "SEMANA", "DIA", "QUANTIDADE")
atendimentos_classificacao_norte_dia <- aggregate(atendimentos_classificacao_norte_dia$QUANTIDADE, by = list(atendimentos_classificacao_norte_dia$CLASSIFICACAO, atendimentos_classificacao_norte_dia$DIA), FUN = median, na.rm = T)#fazendo a classificacaoiana por semana
names(atendimentos_classificacao_norte_dia) <- c("CLASSIFICACAO", "DIA", "QUANTIDADE")
#atendimentos_classificacao_norte_dia$DIA <- factor(atendimentos_classificacao_norte_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#atendimentos_classificacao_norte_dia$DIA <- revalue(atendimentos_classificacao_norte_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
atendimentos_classificacao_norte_dia$DIA <- revalue(atendimentos_classificacao_norte_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))
a <- aggregate(atendimentos_classificacao_norte_dia$QUANTIDADE, by = list(atendimentos_classificacao_norte_dia$DIA), FUN = sum)#soma de todos os atendimentos médicos
a$CLASSIFICACAO <- "Total"
names(a) <- c("DIA", "QUANTIDADE", "CLASSIFICACAO")
atendimentos_classificacao_norte_dia <- rbind(atendimentos_classificacao_norte_dia, a)

#Hora do dia
atendimentos_classificacao_norte_hora <- subset(atendimentos_classificacao_norte, as.yearmon(atendimentos_classificacao_norte$INI_ATD) >= max(as.yearmon(atendimentos_classificacao_norte$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
atendimentos_classificacao_norte_hora$HORA <- format.POSIXct(as.POSIXct(atendimentos_classificacao_norte_hora$INI_ATD), "%H")
atendimentos_classificacao_norte_hora$DIA <- format.POSIXct(as.POSIXct(atendimentos_classificacao_norte_hora$INI_ATD), "%d")
atendimentos_classificacao_norte_hora$INI_ATD <- NULL
atendimentos_classificacao_norte_hora$QUANTIDADE <- 1
atendimentos_classificacao_norte_hora <- aggregate(atendimentos_classificacao_norte_hora$QUANTIDADE, by = list(atendimentos_classificacao_norte_hora$CLASSIFICACAO_RISCO,atendimentos_classificacao_norte_hora$DIA, atendimentos_classificacao_norte_hora$HORA), FUN = sum, na.rm = T)#somando os atendimentos por dia do mês
names(atendimentos_classificacao_norte_hora) <- c("CLASSIFICACAO", "DIA", "HORA", "QUANTIDADE")
atendimentos_classificacao_norte_hora <- aggregate(atendimentos_classificacao_norte_hora$QUANTIDADE, by = list(atendimentos_classificacao_norte_hora$CLASSIFICACAO, atendimentos_classificacao_norte_hora$HORA), FUN = median, na.rm = T)#calculando a classificacaoiana por dia do mês
names(atendimentos_classificacao_norte_hora) <- c("CLASSIFICACAO", "HORA", "QUANTIDADE")
a <- aggregate(atendimentos_classificacao_norte_hora$QUANTIDADE, by = list(atendimentos_classificacao_norte_hora$HORA), FUN = sum)
a$CLASSIFICACAO <- "Total"
names(a) <- c("HORA", "QUANTIDADE", "CLASSIFICACAO")
atendimentos_classificacao_norte_hora <- rbind(atendimentos_classificacao_norte_hora, a)

#Escrever bancos de dados
write.csv(atendimentos_classificacao_sul_mes, "base_de_dados/transformadas/atendimentos_classificacao_sul_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_classificacao_sul_dia, "base_de_dados/transformadas/atendimentos_classificacao_sul_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_classificacao_sul_hora, "base_de_dados/transformadas/atendimentos_classificacao_sul_hora.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_classificacao_norte_mes, "base_de_dados/transformadas/atendimentos_classificacao_norte_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_classificacao_norte_dia, "base_de_dados/transformadas/atendimentos_classificacao_norte_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_classificacao_norte_hora, "base_de_dados/transformadas/atendimentos_classificacao_norte_hora.csv", fileEncoding = "UTF-8", row.names = F)




##############################################################################################################                   

#Agregando os dados - odontologia

#Importando a base
atendimentos_odontologia_new <- fread("base_de_dados/transformadas/atendimentos_odont.csv", encoding = 'UTF-8')

#Lendo as bases antigas
atendimentos_odontologia_old <- fread("base_de_dados/transformadas/atendimentos_odont_old.csv", encoding = 'UTF-8')

atendimentos_odontologia <- rbind(atendimentos_odontologia_new, atendimentos_odontologia_old) 

#Salvando as bases antigas como old
write.csv(atendimentos_odontologia, "base_de_dados/transformadas/atendimentos_odont_old.csv", fileEncoding = "UTF-8", row.names = F)

atendimentos_odontologia <- na.omit(atendimentos_odontologia)
#Separando as upas
atendimentos_odontologia_sul <- subset(atendimentos_odontologia, atendimentos_odontologia$UNIDADE == "UPA Sul")
atendimentos_odontologia_sul <- select(atendimentos_odontologia_sul, -c(UNIDADE))
atendimentos_odontologia_sul$INI_ATD <- as.POSIXct(atendimentos_odontologia_sul$INI_ATD, format = "%Y-%m-%d %H:%M:%S")

atendimentos_odontologia_norte <- subset(atendimentos_odontologia, atendimentos_odontologia$UNIDADE == "UPA Norte")
atendimentos_odontologia_norte <- select(atendimentos_odontologia_norte, -c(UNIDADE))
atendimentos_odontologia_norte$INI_ATD <- as.POSIXct(atendimentos_odontologia_norte$INI_ATD, format = "%Y-%m-%d %H:%M:%S")


#Mês-Ano
atendimentos_odontologia_sul_mes <- atendimentos_odontologia_sul
atendimentos_odontologia_sul_mes$INI_ATD <- substr(atendimentos_odontologia_sul_mes$INI_ATD,0,10)
atendimentos_odontologia_sul_mes$INI_ATD <- as.yearmon(atendimentos_odontologia_sul_mes$INI_ATD, format = "%Y-%m-%d")
atendimentos_odontologia_sul_mes <- table(atendimentos_odontologia_sul_mes) %>% as.data.frame()
names(atendimentos_odontologia_sul_mes) <- c("DATA", "QUANTIDADE")
atendimentos_odontologia_sul_mes$DATA <- as.yearmon(atendimentos_odontologia_sul_mes$DATA, format = "%b %Y")
atendimentos_odontologia_sul_mes$DATA <- as.Date(atendimentos_odontologia_sul_mes$DATA, format = "%d-%m-%Y")
atendimentos_odontologia_sul_mes$DATA <- as.character(atendimentos_odontologia_sul_mes$DATA)
atendimentos_odontologia_sul_mes$QUANTIDADE <- as.integer(atendimentos_odontologia_sul_mes$QUANTIDADE)


#Dia-da-Semana
#Será a média do último semestre
atendimentos_odontologia_sul_dia <- subset(atendimentos_odontologia_sul, as.yearmon(atendimentos_odontologia_sul$INI_ATD) >= max(as.yearmon(atendimentos_odontologia_sul$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
atendimentos_odontologia_sul_dia$DIA <- format.Date(as.Date(atendimentos_odontologia_sul_dia$INI_ATD), "%a")
atendimentos_odontologia_sul_dia$SEMANA <- format.Date(as.Date(atendimentos_odontologia_sul_dia$INI_ATD), "%W")
atendimentos_odontologia_sul_dia$INI_ATD <- NULL
atendimentos_odontologia_sul_dia$QUANTIDADE <- 1
atendimentos_odontologia_sul_dia <- aggregate(atendimentos_odontologia_sul_dia$QUANTIDADE, by = list(atendimentos_odontologia_sul_dia$SEMANA, atendimentos_odontologia_sul_dia$DIA), FUN = sum, na.rm = T)#somando os atendimentos por dia da semana
names(atendimentos_odontologia_sul_dia) <- c("SEMANA", "DIA", "QUANTIDADE")
atendimentos_odontologia_sul_dia <- aggregate(atendimentos_odontologia_sul_dia$QUANTIDADE, by = list( atendimentos_odontologia_sul_dia$DIA), FUN = median, na.rm = T)#fazendo a odontologiaiana por semana
names(atendimentos_odontologia_sul_dia) <- c("DIA", "QUANTIDADE")
#atendimentos_odontologia_sul_dia$DIA <- factor(atendimentos_odontologia_sul_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#atendimentos_odontologia_sul_dia$DIA <- revalue(atendimentos_odontologia_sul_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
atendimentos_odontologia_sul_dia$DIA <- revalue(atendimentos_odontologia_sul_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))


#Hora do dia
atendimentos_odontologia_sul_hora <- subset(atendimentos_odontologia_sul, as.yearmon(atendimentos_odontologia_sul$INI_ATD) >= max(as.yearmon(atendimentos_odontologia_sul$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
atendimentos_odontologia_sul_hora$HORA <- format.POSIXct(as.POSIXct(atendimentos_odontologia_sul_hora$INI_ATD), "%H")
atendimentos_odontologia_sul_hora$DIA <- format.POSIXct(as.POSIXct(atendimentos_odontologia_sul_hora$INI_ATD), "%d")
atendimentos_odontologia_sul_hora$INI_ATD <- NULL
atendimentos_odontologia_sul_hora$QUANTIDADE <- 1
atendimentos_odontologia_sul_hora <- aggregate(atendimentos_odontologia_sul_hora$QUANTIDADE, by = list(atendimentos_odontologia_sul_hora$DIA, atendimentos_odontologia_sul_hora$HORA), FUN = sum, na.rm = T)#somando os atendimentos por dia do mês
names(atendimentos_odontologia_sul_hora) <- c("DIA", "HORA", "QUANTIDADE")
atendimentos_odontologia_sul_hora <- aggregate(atendimentos_odontologia_sul_hora$QUANTIDADE, by = list(atendimentos_odontologia_sul_hora$HORA), FUN = median, na.rm = T)#calculando a odontologiaiana por dia do mês
names(atendimentos_odontologia_sul_hora) <- c("HORA", "QUANTIDADE")



#Mês-Ano
atendimentos_odontologia_norte_mes <- atendimentos_odontologia_norte
atendimentos_odontologia_norte_mes$INI_ATD <- substr(atendimentos_odontologia_norte_mes$INI_ATD,0,10)
atendimentos_odontologia_norte_mes$INI_ATD <- as.yearmon(atendimentos_odontologia_norte_mes$INI_ATD, format = "%Y-%m-%d")
atendimentos_odontologia_norte_mes <- table(atendimentos_odontologia_norte_mes) %>% as.data.frame()
names(atendimentos_odontologia_norte_mes) <- c("DATA", "QUANTIDADE")
atendimentos_odontologia_norte_mes$DATA <- as.yearmon(atendimentos_odontologia_norte_mes$DATA, format = "%b %Y")
atendimentos_odontologia_norte_mes$DATA <- as.Date(atendimentos_odontologia_norte_mes$DATA, format = "%d-%m-%Y")
atendimentos_odontologia_norte_mes$DATA <- as.character(atendimentos_odontologia_norte_mes$DATA)
atendimentos_odontologia_norte_mes$QUANTIDADE <- as.integer(atendimentos_odontologia_norte_mes$QUANTIDADE)


#Dia-da-Semana
#Será a média do último semestre
atendimentos_odontologia_norte_dia <- subset(atendimentos_odontologia_norte, as.yearmon(atendimentos_odontologia_norte$INI_ATD) >= max(as.yearmon(atendimentos_odontologia_norte$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
atendimentos_odontologia_norte_dia$DIA <- format.Date(as.Date(atendimentos_odontologia_norte_dia$INI_ATD), "%a")
atendimentos_odontologia_norte_dia$SEMANA <- format.Date(as.Date(atendimentos_odontologia_norte_dia$INI_ATD), "%W")
atendimentos_odontologia_norte_dia$INI_ATD <- NULL
atendimentos_odontologia_norte_dia$QUANTIDADE <- 1
atendimentos_odontologia_norte_dia <- aggregate(atendimentos_odontologia_norte_dia$QUANTIDADE, by = list(atendimentos_odontologia_norte_dia$SEMANA, atendimentos_odontologia_norte_dia$DIA), FUN = sum, na.rm = T)#somando os atendimentos por dia da semana
names(atendimentos_odontologia_norte_dia) <- c("SEMANA", "DIA", "QUANTIDADE")
atendimentos_odontologia_norte_dia <- aggregate(atendimentos_odontologia_norte_dia$QUANTIDADE, by = list( atendimentos_odontologia_norte_dia$DIA), FUN = median, na.rm = T)#fazendo a odontologiaiana por semana
names(atendimentos_odontologia_norte_dia) <- c("DIA", "QUANTIDADE")
#atendimentos_odontologia_norte_dia$DIA <- factor(atendimentos_odontologia_norte_dia$DIA, levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat","Sun"))
#atendimentos_odontologia_norte_dia$DIA <- revalue(atendimentos_odontologia_norte_dia$DIA, c("Mon" = "Seg", "Tue" = "Ter", "Wed" = "Qua", "Thu" = "Qui","Fri" = "Sex", "Sat" = "Sáb","Sun" = "Dom"))
atendimentos_odontologia_norte_dia$DIA <- revalue(atendimentos_odontologia_norte_dia$DIA, c("Seg", "Ter", "Qua", "Qui","Sex","Sáb","Dom"))

#Hora do dia
atendimentos_odontologia_norte_hora <- subset(atendimentos_odontologia_norte, as.yearmon(atendimentos_odontologia_norte$INI_ATD) >= max(as.yearmon(atendimentos_odontologia_norte$INI_ATD)-0.5)) #retirando apenas dos ultimos e meses (0.5 anos)
atendimentos_odontologia_norte_hora$HORA <- format.POSIXct(as.POSIXct(atendimentos_odontologia_norte_hora$INI_ATD), "%H")
atendimentos_odontologia_norte_hora$DIA <- format.POSIXct(as.POSIXct(atendimentos_odontologia_norte_hora$INI_ATD), "%d")
atendimentos_odontologia_norte_hora$INI_ATD <- NULL
atendimentos_odontologia_norte_hora$QUANTIDADE <- 1
atendimentos_odontologia_norte_hora <- aggregate(atendimentos_odontologia_norte_hora$QUANTIDADE, by = list(atendimentos_odontologia_norte_hora$DIA, atendimentos_odontologia_norte_hora$HORA), FUN = sum, na.rm = T)#somando os atendimentos por dia do mês
names(atendimentos_odontologia_norte_hora) <- c("DIA", "HORA", "QUANTIDADE")
atendimentos_odontologia_norte_hora <- aggregate(atendimentos_odontologia_norte_hora$QUANTIDADE, by = list(atendimentos_odontologia_norte_hora$HORA), FUN = median, na.rm = T)#calculando a odontologiaiana por dia do mês
names(atendimentos_odontologia_norte_hora) <- c("HORA", "QUANTIDADE")


#Escrever bases atualizadas
write.csv(atendimentos_odontologia_sul_mes, "base_de_dados/transformadas/atendimentos_odontologia_sul_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_odontologia_sul_dia, "base_de_dados/transformadas/atendimentos_odontologia_sul_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_odontologia_sul_hora, "base_de_dados/transformadas/atendimentos_odontologia_sul_hora.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_odontologia_norte_mes, "base_de_dados/transformadas/atendimentos_odontologia_norte_mes.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_odontologia_norte_dia, "base_de_dados/transformadas/atendimentos_odontologia_norte_dia.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(atendimentos_odontologia_norte_hora, "base_de_dados/transformadas/atendimentos_odontologia_norte_hora.csv", fileEncoding = "UTF-8", row.names = F)
