########################################################################################### 
#pacotes 
library(shiny)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(leaflet)
library(magrittr)
library(htmltools)
library(plyr)
library(readr)
library(stringr)
library(dplyr)
library(DT)
library(ggvis)
library(zoo)
library(scales)

#Setando a localização do sistema
Sys.setlocale("LC_TIME","Portuguese_Brazil.1252")



#Banco de dados para o mapa

########################################################################################### 
#Banco de dados de produção
#Norte
##Médico
atendimentos_med_norte_mes <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_med_norte_mes.csv")
atendimentos_med_norte_mes$DATA <- as.yearmon(atendimentos_med_norte_mes$DATA, format = "%b %Y") 
atendimentos_med_norte_mes$DATA <- as.Date(atendimentos_med_norte_mes$DATA, format = "%d-%m-%Y") 
atendimentos_med_norte_dia <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_med_norte_dia.csv")
atendimentos_med_norte_dia$DIA <- factor(atendimentos_med_norte_dia$DIA, levels = c("seg", "ter", "qua", "qui", "sex", "sáb","dom"))
atendimentos_med_norte_hora <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_med_norte_hora.csv")
atendimentos_med_norte_hora$HORA <- as.numeric(atendimentos_med_norte_hora$HORA)
#Classificação de Risco
atendimentos_classificacao_norte_mes <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_classificacao_norte_mes.csv")
atendimentos_classificacao_norte_mes$DATA <- as.yearmon(atendimentos_classificacao_norte_mes$DATA, format = "%b %Y") 
atendimentos_classificacao_norte_mes$DATA <- as.Date(atendimentos_classificacao_norte_mes$DATA, format = "%d-%m-%Y") 
atendimentos_classificacao_norte_dia <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_classificacao_norte_dia.csv")
atendimentos_classificacao_norte_dia$DIA <- factor(atendimentos_classificacao_norte_dia$DIA, levels = c("seg", "ter", "qua", "qui", "sex", "sáb","dom"))
atendimentos_classificacao_norte_hora <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_classificacao_norte_hora.csv")
atendimentos_classificacao_norte_hora$HORA <- as.numeric(atendimentos_classificacao_norte_hora$HORA)
#Odontologia
atendimentos_odontologia_norte_mes <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_odontologia_norte_mes.csv")
atendimentos_odontologia_norte_mes$DATA <- as.yearmon(atendimentos_odontologia_norte_mes$DATA, format = "%b %Y") 
atendimentos_odontologia_norte_mes$DATA <- as.Date(atendimentos_odontologia_norte_mes$DATA, format = "%d-%m-%Y") 
atendimentos_odontologia_norte_dia <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_odontologia_norte_dia.csv")
atendimentos_odontologia_norte_dia$DIA <- factor(atendimentos_odontologia_norte_dia$DIA, levels = c("seg", "ter", "qua", "qui", "sex", "sáb","dom"))
atendimentos_odontologia_norte_hora <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_odontologia_norte_hora.csv")
atendimentos_odontologia_norte_hora$HORA <- as.numeric(atendimentos_odontologia_norte_hora$HORA)



#Sul
##Médico
atendimentos_med_sul_mes <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_med_sul_mes.csv")
atendimentos_med_sul_mes$DATA <- as.yearmon(atendimentos_med_sul_mes$DATA, format = "%b %Y") 
atendimentos_med_sul_mes$DATA <- as.Date(atendimentos_med_sul_mes$DATA, format = "%d-%m-%Y") 
atendimentos_med_sul_dia <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_med_sul_dia.csv")
atendimentos_med_sul_dia$DIA <- factor(atendimentos_med_sul_dia$DIA, levels = c("seg", "ter", "qua", "qui", "sex", "sáb","dom"))
atendimentos_med_sul_hora <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_med_sul_hora.csv")
atendimentos_med_sul_hora$HORA <- as.numeric(atendimentos_med_sul_hora$HORA)
#Classificação de Risco
atendimentos_classificacao_sul_mes <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_classificacao_sul_mes.csv")
atendimentos_classificacao_sul_mes$DATA <- as.yearmon(atendimentos_classificacao_sul_mes$DATA, format = "%b %Y") 
atendimentos_classificacao_sul_mes$DATA <- as.Date(atendimentos_classificacao_sul_mes$DATA, format = "%d-%m-%Y") 
atendimentos_classificacao_sul_dia <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_classificacao_sul_dia.csv")
atendimentos_classificacao_sul_dia$DIA <- factor(atendimentos_classificacao_sul_dia$DIA, levels = c("seg", "ter", "qua", "qui", "sex", "sáb","dom"))
atendimentos_classificacao_sul_hora <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_classificacao_sul_hora.csv")
atendimentos_classificacao_sul_hora$HORA <- as.numeric(atendimentos_classificacao_sul_hora$HORA)
#Odontologia
atendimentos_odontologia_sul_mes <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_odontologia_sul_mes.csv")
atendimentos_odontologia_sul_mes$DATA <- as.yearmon(atendimentos_odontologia_sul_mes$DATA, format = "%b %Y") 
atendimentos_odontologia_sul_mes$DATA <- as.Date(atendimentos_odontologia_sul_mes$DATA, format = "%d-%m-%Y") 
atendimentos_odontologia_sul_dia <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_odontologia_sul_dia.csv")
atendimentos_odontologia_sul_dia$DIA <- factor(atendimentos_odontologia_sul_dia$DIA, levels = c("seg", "ter", "qua", "qui", "sex", "sáb","dom"))
atendimentos_odontologia_sul_hora <- read_csv("C:/Users/hp1806/Google Drive/RStudio/sala_situacao_upas/sala_situacao_upas/base_de_dados_preparadas/atendimentos_odontologia_sul_hora.csv")
atendimentos_odontologia_sul_hora$HORA <- as.numeric(atendimentos_odontologia_sul_hora$HORA)

########################################################################################### 
ui <- dashboardPage(skin = "blue",
                    ########################################################################################### 
                    dashboardHeader(title = "Sala de Situação da UPAs - Versão para Teste", titleWidth = 550),
                    ########################################################################################### 
                    dashboardSidebar(
                      ########################################################################################### 
                      sidebarMenu(
                        menuItem("Apresentação", tabName = "apresentacao", icon = icon("heartbeat")),
                        menuItem("UPA Norte", icon = icon("dashboard"),   
                                 menuSubItem("Médicos", tabName = "medicos_norte"),
                                 menuSubItem("Classificação", tabName = "classificacao_norte"),
                                 menuSubItem("Odontólogos", tabName = "odontologos_norte")),
                        menuItem("UPA Sul", icon = icon("dashboard"), 
                                 menuSubItem("Médicos", tabName = "medicos_sul"),
                                 menuSubItem("Classificação", tabName = "classificacao_sul"),
                                 menuSubItem("Odontólogos", tabName = "odontologos_sul")),
                        menuItem("Instruções", icon = icon("question-circle"),
                                 href = "https://github.com/analisededadosemsaudefloripa/saladesituacao/wiki/Instru%C3%A7%C3%B5es-para-Utiliza%C3%A7%C3%A3o-das-Salas-de-Situa%C3%A7%C3%A3o-em-Sa%C3%BAde"),
                        menuItem("Dados", tabName = "dados", icon = icon("database")),
                        menuItem("Código-fonte", icon = icon("code"), 
                                 href = "https://github.com/analisededadosemsaudefloripa/saladesituacao/blob/atencao_primaria/app.R"),
                        menuItem("Licença de Uso", icon = icon("cc"), 
                                 href = "https://github.com/analisededadosemsaudefloripa/saladesituacao/blob/atencao_primaria/LICENSE")
                      )
                    ),
                    ########################################################################################### 
                    dashboardBody(
                      tabItems(
                        ########################################################################################### 
                        #Apresentação
                        tabItem(tabName = "apresentacao", h2("Pronto Atendimento em Florianópolis"),
                                fluidRow(
                                  mainPanel(
                                    p("O serviço de pronto atendimento desenvolvido pela Secretaria Municipal de Saúde de Florianópolis se dá por meio de duas Unidades de Pronto Atendimento - UPA"),
                                    p("A UPA Norte instalada no bairro Vargem Grande e a UPA Sul no bairro Rio Tavares.")
                                  )
                                )
                        ),
                        ###########################################################################################
                        #UPA Norte
                        ###########################################################################################                         
                        
                        ###########################################################################################                         
                        #Dashboard Médicos na UPA Norte                                                         
                        ###########################################################################################
                        tabItem(tabName = "medicos_norte", h2("Atendimentos Médicos na UPA Norte"),
                                
                                fluidRow(
                                  box(selectInput(
                                    inputId="medicos_norte_especialidade",
                                    label="Selecione a Especialidade:",
                                    choices=list("Pediatria" = "Pediatria", "Clínica" = "Clínica",
                                                 "Cirurgia" = "Cirurgia", "Total" = "Total", "Comparação" = "Comparação"),
                                    selected="Total"),
                                    width = 12, status = "primary")
                                ),
                                
                                fluidRow(
                                  tabBox(title = "Número de Atendimentos Médicos na UPA Norte por Mês", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "medicos_norte_mes_plot")),
                                         tabPanel("Dados", dataTableOutput("medicos_norte_mes_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("medicos_norte_mes_info"))
                                  ),
                                  tabBox(title = "Média de Atendimentos Médicos na UPA Norte por Dia de Semana", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "medicos_norte_dia_plot")),
                                         tabPanel("Dados", dataTableOutput("medicos_norte_dia_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("medicos_norte_dia_info"))
                                  ),
                                  tabBox(title = "Média de Atendimentos Médicos na UPA Norte por Hora do Dia", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "medicos_norte_hora_plot")),
                                         tabPanel("Dados", dataTableOutput("medicos_norte_hora_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("medicos_norte_hora_info"))
                            )
                          )
                        ),
                        
                        ###########################################################################################
                        #Dashboard Classificação de Risco na UPA Norte 
                        ###########################################################################################
                        tabItem(tabName = "classificacao_norte", h2("Classificações de Risco na UPA Norte"),
                                
                                fluidRow(
                                  box(selectInput(
                                    inputId="classificacao_norte_tipo",
                                    label="Selecione o Tipo de Classificação:",
                                    choices=list("Ambulatorial" = "Ambulatorial", "Intercorrência" = "Intercorrência",
                                                 "Urgência" = "Urgência","Emergência" = "Emergência", "Total" = "Total", "Comparação" = "Comparação"),
                                    selected="Total"),
                                    width = 12, status = "primary")
                                ),
                                
                                fluidRow(
                                  tabBox(title = "Número de Classificações de Risco na UPA Norte por Mês", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "classificacao_norte_mes_plot")),
                                         tabPanel("Dados", dataTableOutput("classificacao_norte_mes_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("classificacao_norte_mes_info"))
                                  ),
                                  tabBox(title = "Média de Classificações de Risco na UPA Norte por Dia de Semana", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "classificacao_norte_dia_plot")),
                                         tabPanel("Dados", dataTableOutput("classificacao_norte_dia_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("classificacao_norte_dia_info"))
                                  ),
                                  tabBox(title = "Média de Classificações de Risco na UPA Norte por Hora do Dia", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "classificacao_norte_hora_plot")),
                                         tabPanel("Dados", dataTableOutput("classificacao_norte_hora_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("classificacao_norte_hora_info"))
                                  )
                                )
                        ),
                        
                        ###########################################################################################
                        #Dashboard Odontologia na UPA Norte 
                        ###########################################################################################
                        tabItem(tabName = "odontologos_norte", h2("Atendimentos Odontológicos na UPA Norte"),
                                
                                fluidRow(
                                  tabBox(title = "Número de Atendimentos Odontológicos na UPA Norte por Mês", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "odontologia_norte_mes_plot")),
                                         tabPanel("Dados", dataTableOutput("odontologia_norte_mes_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("odontologia_norte_mes_info"))
                                  ),
                                  tabBox(title = "Média de Atendimentos Odontológicos na UPA Norte por Dia de Semana", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "odontologia_norte_dia_plot")),
                                         tabPanel("Dados", dataTableOutput("odontologia_norte_dia_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("odontologia_norte_dia_info"))
                                  ),
                                  tabBox(title = "Média de Atendimentos Odontológicos na UPA Norte por Hora do Dia", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "odontologia_norte_hora_plot")),
                                         tabPanel("Dados", dataTableOutput("odontologia_norte_hora_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("odontologia_norte_hora_info"))
                                  )
                                )
                        ),
                        ###########################################################################################                         
                        #Dashboard Médicos na UPA sul                                                         
                        ###########################################################################################
                        tabItem(tabName = "medicos_sul", h2("Atendimentos Médicos na UPA sul"),
                                
                                fluidRow(
                                  box(selectInput(
                                    inputId="medicos_sul_especialidade",
                                    label="Selecione a Especialidade:",
                                    choices=list("Pediatria" = "Pediatria", "Clínica" = "Clínica",
                                                 "Cirurgia" = "Cirurgia", "Total" = "Total", "Comparação" = "Comparação"),
                                    selected="Total"),
                                    width = 12, status = "primary")
                                ),
                                
                                fluidRow(
                                  tabBox(title = "Número de Atendimentos Médicos na UPA sul por Mês", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "medicos_sul_mes_plot")),
                                         tabPanel("Dados", dataTableOutput("medicos_sul_mes_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("medicos_sul_mes_info"))
                                  ),
                                  tabBox(title = "Média de Atendimentos Médicos na UPA sul por Dia de Semana", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "medicos_sul_dia_plot")),
                                         tabPanel("Dados", dataTableOutput("medicos_sul_dia_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("medicos_sul_dia_info"))
                                  ),
                                  tabBox(title = "Média de Atendimentos Médicos na UPA sul por Hora do Dia", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "medicos_sul_hora_plot")),
                                         tabPanel("Dados", dataTableOutput("medicos_sul_hora_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("medicos_sul_hora_info"))
                                  )
                                )
                        ),
                        
                        ###########################################################################################
                        #Dashboard Classificação de Risco na UPA sul 
                        ###########################################################################################
                        tabItem(tabName = "classificacao_sul", h2("Classificações de Risco na UPA sul"),
                                
                                fluidRow(
                                  box(selectInput(
                                    inputId="classificacao_sul_tipo",
                                    label="Selecione o Tipo de Classificação:",
                                    choices=list("Ambulatorial" = "Ambulatorial", "Intercorrência" = "Intercorrência",
                                                 "Urgência" = "Urgência","Emergência" = "Emergência", "Total" = "Total", "Comparação" = "Comparação"),
                                    selected="Total"),
                                    width = 12, status = "primary")
                                ),
                                
                                fluidRow(
                                  tabBox(title = "Número de Classificações de Risco na UPA sul por Mês", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "classificacao_sul_mes_plot")),
                                         tabPanel("Dados", dataTableOutput("classificacao_sul_mes_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("classificacao_sul_mes_info"))
                                  ),
                                  tabBox(title = "Média de Classificações de Risco na UPA sul por Dia de Semana", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "classificacao_sul_dia_plot")),
                                         tabPanel("Dados", dataTableOutput("classificacao_sul_dia_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("classificacao_sul_dia_info"))
                                  ),
                                  tabBox(title = "Média de Classificações de Risco na UPA sul por Hora do Dia", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "classificacao_sul_hora_plot")),
                                         tabPanel("Dados", dataTableOutput("classificacao_sul_hora_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("classificacao_sul_hora_info"))
                                  )
                                )
                        ),
                        
                        ###########################################################################################
                        #Dashboard Odontologia na UPA sul 
                        ###########################################################################################
                        tabItem(tabName = "odontologos_sul", h2("Atendimentos Odontológicos na UPA sul"),
                                
                                fluidRow(
                                  tabBox(title = "Número de Atendimentos Odontológicos na UPA sul por Mês", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "odontologia_sul_mes_plot")),
                                         tabPanel("Dados", dataTableOutput("odontologia_sul_mes_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("odontologia_sul_mes_info"))
                                  ),
                                  tabBox(title = "Média de Atendimentos Odontológicos na UPA sul por Dia de Semana", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "odontologia_sul_dia_plot")),
                                         tabPanel("Dados", dataTableOutput("odontologia_sul_dia_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("odontologia_sul_dia_info"))
                                  ),
                                  tabBox(title = "Média de Atendimentos Odontológicos na UPA sul por Hora do Dia", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "odontologia_sul_hora_plot")),
                                         tabPanel("Dados", dataTableOutput("odontologia_sul_hora_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("odontologia_sul_hora_info"))
                                  )
                                )
                        )
                        
                         ###########################################################################################  
    )  
  )
)

########################################################################################### 
server <- function(input, output, session) {
  ########################################################################################### 
  #Página inicial
  #Mapa das unidades
  
  ###########################################################################################
  #UPA Norte
  ###########################################################################################
  
  ###########################################################################################
  #Dashboard Médicos UPA Norte  
  ###########################################################################################
  
  #gráfico de atendimentos médicos no norte por mês
  output$medicos_norte_mes_plot <- renderPlotly({
    
    
    
    if(input$medicos_norte_especialidade == "Comparação"){
      
      medicos_norte_mes <- atendimentos_med_norte_mes
      
      a<-ggplot(medicos_norte_mes, aes(x = as.factor(DATA))) + 
        geom_col(aes(y = QUANTIDADE, fill = ESPECIALIDADE), position = "dodge")+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    }else{
      
      medicos_norte_mes <- atendimentos_med_norte_mes %>% filter(ESPECIALIDADE == input$medicos_norte_especialidade)  
      
      a<-ggplot(medicos_norte_mes, aes(x = as.factor(DATA))) + 
        geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))}
    
    
    ggplotly(a)
    
  })
  
  #tabela de atendimentos médicos no norte por mês
  output$medicos_norte_mes_tab <- renderDataTable({
    
    if(input$medicos_norte_especialidade == "Comparação"){
      
      medicos_norte_mes <- atendimentos_med_norte_mes
    }else{
      
      medicos_norte_mes <- atendimentos_med_norte_mes %>% filter(ESPECIALIDADE == input$medicos_norte_especialidade)}
    
    medicos_norte_mes
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações de atendimentos médicos no norte por mês
  output$medicos_norte_mes_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de medicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  #gráfico de atendimentos médicos no norte por dia da semana
  output$medicos_norte_dia_plot <- renderPlotly({
    
    
    
    if(input$medicos_norte_especialidade == "Comparação"){
      
      medicos_norte_dia <- atendimentos_med_norte_dia
      
      a<-ggplot(medicos_norte_dia, aes(x = as.factor(DIA))) + 
        geom_col(aes(y = QUANTIDADE, fill = ESPECIALIDADE), position = "dodge")+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    }else{
      
      medicos_norte_dia <- atendimentos_med_norte_dia %>% filter(ESPECIALIDADE == input$medicos_norte_especialidade)  
      
      a<-ggplot(medicos_norte_dia, aes(x = as.factor(DIA))) + 
        geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))}
    
    
    ggplotly(a)
    
  })
  
  #tabela de atendimentos médicos no norte por dia da semana
  output$medicos_norte_dia_tab <- renderDataTable({
    
    if(input$medicos_norte_especialidade == "Comparação"){
      
      medicos_norte_dia <- atendimentos_med_norte_dia
    }else{
      
      medicos_norte_dia <- atendimentos_med_norte_dia %>% filter(ESPECIALIDADE == input$medicos_norte_especialidade)}
    
    medicos_norte_dia
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações de atendimentos médicos no norte por mês
  output$medicos_norte_dia_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de medicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  
  #gráfico de atendimentos médicos no norte por hora do dia
  output$medicos_norte_hora_plot <- renderPlotly({
    
    
    
    if(input$medicos_norte_especialidade == "Comparação"){
      
      medicos_norte_hora <- atendimentos_med_norte_hora
      
      a<-ggplot(medicos_norte_hora, aes(x = as.factor(HORA))) + 
        geom_col(aes(y = QUANTIDADE, fill = ESPECIALIDADE), position = "dodge")+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    }else{
      
      medicos_norte_hora <- atendimentos_med_norte_hora %>% filter(ESPECIALIDADE == input$medicos_norte_especialidade)  
      
      a<-ggplot(medicos_norte_hora, aes(x = as.factor(HORA))) + 
        geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))}
    
    
    ggplotly(a)
    
  })
  
  #tabela de atendimentos médicos no norte por hora do dia
  output$medicos_norte_hora_tab <- renderDataTable({
    
    if(input$medicos_norte_especialidade == "Comparação"){
      
      medicos_norte_hora <- atendimentos_med_norte_hora
    }else{
      
      medicos_norte_hora <- atendimentos_med_norte_hora %>% filter(ESPECIALIDADE == input$medicos_norte_especialidade)}
    
    medicos_norte_hora
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações de atendimentos médicos no norte por hora do dia
  output$medicos_norte_hora_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de medicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  ###########################################################################################
  #Dashboard Classificação de Risco UPA Norte  
  ###########################################################################################
  
  #gráfico de classificação de risco no norte por mês
  output$classificacao_norte_mes_plot <- renderPlotly({
    
    
    
    if(input$classificacao_norte_tipo == "Comparação"){
      
      classificacao_norte_mes <- atendimentos_classificacao_norte_mes
      
      a<-ggplot(classificacao_norte_mes, aes(x = as.factor(DATA))) + 
        geom_col(aes(y = QUANTIDADE, fill = CLASSIFICACAO), position = "dodge")+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    }else{
      
      classificacao_norte_mes <- atendimentos_classificacao_norte_mes %>% filter(CLASSIFICACAO == input$classificacao_norte_tipo)  
      
      a<-ggplot(classificacao_norte_mes, aes(x = as.factor(DATA))) + 
        geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))}
    
    
    ggplotly(a)
    
  })
  
  #tabela de classificação de risco norte por mês
  output$classificacao_norte_mes_tab <- renderDataTable({
    
    if(input$classificacao_norte_tipo == "Comparação"){
      
      classificacao_norte_mes <- atendimentos_classificacao_norte_mes
    }else{
      
      classificacao_norte_mes <- atendimentos_classificacao_norte_mes %>% filter(CLASSIFICACAO == input$classificacao_norte_tipo)}
    
    classificacao_norte_mes
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações dde classificação de risco norte por mês
  output$classificacao_norte_mes_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de classificacaoicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  #gráfico de classificação de risco norte por dia da semana
  output$classificacao_norte_dia_plot <- renderPlotly({
    
    
    
    if(input$classificacao_norte_tipo == "Comparação"){
      
      classificacao_norte_dia <- atendimentos_classificacao_norte_dia
      
      a<-ggplot(classificacao_norte_dia, aes(x = as.factor(DIA))) + 
        geom_col(aes(y = QUANTIDADE, fill = CLASSIFICACAO), position = "dodge")+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    }else{
      
      classificacao_norte_dia <- atendimentos_classificacao_norte_dia %>% filter(CLASSIFICACAO == input$classificacao_norte_tipo)  
      
      a<-ggplot(classificacao_norte_dia, aes(x = as.factor(DIA))) + 
        geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))}
    
    
    ggplotly(a)
    
  })
  
  #tabela de classificação de risco norte por dia da semana
  output$classificacao_norte_dia_tab <- renderDataTable({
    
    if(input$classificacao_norte_tipo == "Comparação"){
      
      classificacao_norte_dia <- atendimentos_classificacao_norte_dia
    }else{
      
      classificacao_norte_dia <- atendimentos_classificacao_norte_dia %>% filter(CLASSIFICACAO == input$classificacao_norte_tipo)}
    
    classificacao_norte_dia
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações de classificação de risco norte por mês
  output$classificacao_norte_dia_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de classificacaoicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  
  #gráfico de classificação de risco norte por hora do dia
  output$classificacao_norte_hora_plot <- renderPlotly({
    
    
    
    if(input$classificacao_norte_tipo == "Comparação"){
      
      classificacao_norte_hora <- atendimentos_classificacao_norte_hora
      
      a<-ggplot(classificacao_norte_hora, aes(x = as.factor(HORA))) + 
        geom_col(aes(y = QUANTIDADE, fill = CLASSIFICACAO), position = "dodge")+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    }else{
      
      classificacao_norte_hora <- atendimentos_classificacao_norte_hora %>% filter(CLASSIFICACAO == input$classificacao_norte_tipo)  
      
      a<-ggplot(classificacao_norte_hora, aes(x = as.factor(HORA))) + 
        geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))}
    
    
    ggplotly(a)
    
  })
  
  #tabela de classificação de risco norte por hora do dia
  output$classificacao_norte_hora_tab <- renderDataTable({
    
    if(input$classificacao_norte_CLASSIFICACAO == "Comparação"){
      
      classificacao_norte_hora <- atendimentos_classificacao_norte_hora
    }else{
      
      classificacao_norte_hora <- atendimentos_classificacao_norte_hora %>% filter(CLASSIFICACAO == input$classificacao_norte_tipo)}
    
    classificacao_norte_hora
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações de classificação de risco norte por hora do dia
  output$classificacao_norte_hora_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de classificacaoicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  
  ###########################################################################################
  #Dashboard Odontologia UPA Norte  
  ###########################################################################################
  
  #gráfico de odontologia no norte por mês
  output$odontologia_norte_mes_plot <- renderPlotly({
    
     
      a<-ggplot(atendimentos_odontologia_norte_mes, aes(x = as.factor(DATA))) + 
        geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    ggplotly(a)
    
  })
  
  #tabela odontologia no norte por mês
  output$odontologia_norte_mes_tab <- renderDataTable({
    
    atendimentos_odontologia_norte_mes
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações odontologia no norte por mês
  output$odontologia_norte_mes_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de odontologiaicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  #gráfico odontologia no norte por dia da semana
  output$odontologia_norte_dia_plot <- renderPlotly({
    
      a<-ggplot(atendimentos_odontologia_norte_dia, aes(x = as.factor(DIA))) + 
        geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    ggplotly(a)
    
  })
  
  #tabela odontologia no norte por dia da semana
  output$odontologia_norte_dia_tab <- renderDataTable({
    
    atendimentos_odontologia_norte_dia
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações odontologia no norte por mês
  output$odontologia_norte_dia_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de odontologiaicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  
  #gráfico odontologia no norte por hora do dia
  output$odontologia_norte_hora_plot <- renderPlotly({
    
      a<-ggplot(atendimentos_odontologia_norte_hora, aes(x = as.factor(HORA))) + 
        geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    ggplotly(a)
    
  })
  
  #tabela odontologia norte por hora do dia
  output$odontologia_norte_hora_tab <- renderDataTable({
    
    atendimentos_odontologia_norte_hora
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações odontologia norte por hora do dia
  output$odontologia_norte_hora_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de odontologiaicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  ###########################################################################################
  #Dashboard Médicos UPA sul  
  ###########################################################################################
  
  #gráfico de atendimentos médicos no sul por mês
  output$medicos_sul_mes_plot <- renderPlotly({
    
    
    
    if(input$medicos_sul_especialidade == "Comparação"){
      
      medicos_sul_mes <- atendimentos_med_sul_mes
      
      a<-ggplot(medicos_sul_mes, aes(x = as.factor(DATA))) + 
        geom_col(aes(y = QUANTIDADE, fill = ESPECIALIDADE), position = "dodge")+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    }else{
      
      medicos_sul_mes <- atendimentos_med_sul_mes %>% filter(ESPECIALIDADE == input$medicos_sul_especialidade)  
      
      a<-ggplot(medicos_sul_mes, aes(x = as.factor(DATA))) + 
        geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))}
    
    
    ggplotly(a)
    
  })
  
  #tabela de atendimentos médicos no sul por mês
  output$medicos_sul_mes_tab <- renderDataTable({
    
    if(input$medicos_sul_especialidade == "Comparação"){
      
      medicos_sul_mes <- atendimentos_med_sul_mes
    }else{
      
      medicos_sul_mes <- atendimentos_med_sul_mes %>% filter(ESPECIALIDADE == input$medicos_sul_especialidade)}
    
    medicos_sul_mes
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações de atendimentos médicos no sul por mês
  output$medicos_sul_mes_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de medicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  #gráfico de atendimentos médicos no sul por dia da semana
  output$medicos_sul_dia_plot <- renderPlotly({
    
    
    
    if(input$medicos_sul_especialidade == "Comparação"){
      
      medicos_sul_dia <- atendimentos_med_sul_dia
      
      a<-ggplot(medicos_sul_dia, aes(x = as.factor(DIA))) + 
        geom_col(aes(y = QUANTIDADE, fill = ESPECIALIDADE), position = "dodge")+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    }else{
      
      medicos_sul_dia <- atendimentos_med_sul_dia %>% filter(ESPECIALIDADE == input$medicos_sul_especialidade)  
      
      a<-ggplot(medicos_sul_dia, aes(x = as.factor(DIA))) + 
        geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))}
    
    
    ggplotly(a)
    
  })
  
  #tabela de atendimentos médicos no sul por dia da semana
  output$medicos_sul_dia_tab <- renderDataTable({
    
    if(input$medicos_sul_especialidade == "Comparação"){
      
      medicos_sul_dia <- atendimentos_med_sul_dia
    }else{
      
      medicos_sul_dia <- atendimentos_med_sul_dia %>% filter(ESPECIALIDADE == input$medicos_sul_especialidade)}
    
    medicos_sul_dia
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações de atendimentos médicos no sul por mês
  output$medicos_sul_dia_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de medicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  
  #gráfico de atendimentos médicos no sul por hora do dia
  output$medicos_sul_hora_plot <- renderPlotly({
    
    
    
    if(input$medicos_sul_especialidade == "Comparação"){
      
      medicos_sul_hora <- atendimentos_med_sul_hora
      
      a<-ggplot(medicos_sul_hora, aes(x = as.factor(HORA))) + 
        geom_col(aes(y = QUANTIDADE, fill = ESPECIALIDADE), position = "dodge")+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    }else{
      
      medicos_sul_hora <- atendimentos_med_sul_hora %>% filter(ESPECIALIDADE == input$medicos_sul_especialidade)  
      
      a<-ggplot(medicos_sul_hora, aes(x = as.factor(HORA))) + 
        geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))}
    
    
    ggplotly(a)
    
  })
  
  #tabela de atendimentos médicos no sul por hora do dia
  output$medicos_sul_hora_tab <- renderDataTable({
    
    if(input$medicos_sul_especialidade == "Comparação"){
      
      medicos_sul_hora <- atendimentos_med_sul_hora
    }else{
      
      medicos_sul_hora <- atendimentos_med_sul_hora %>% filter(ESPECIALIDADE == input$medicos_sul_especialidade)}
    
    medicos_sul_hora
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações de atendimentos médicos no sul por hora do dia
  output$medicos_sul_hora_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de medicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  ###########################################################################################
  #Dashboard Classificação de Risco UPA sul  
  ###########################################################################################
  
  #gráfico de classificação de risco no sul por mês
  output$classificacao_sul_mes_plot <- renderPlotly({
    
    
    
    if(input$classificacao_sul_tipo == "Comparação"){
      
      classificacao_sul_mes <- atendimentos_classificacao_sul_mes
      
      a<-ggplot(classificacao_sul_mes, aes(x = as.factor(DATA))) + 
        geom_col(aes(y = QUANTIDADE, fill = CLASSIFICACAO), position = "dodge")+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    }else{
      
      classificacao_sul_mes <- atendimentos_classificacao_sul_mes %>% filter(CLASSIFICACAO == input$classificacao_sul_tipo)  
      
      a<-ggplot(classificacao_sul_mes, aes(x = as.factor(DATA))) + 
        geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))}
    
    
    ggplotly(a)
    
  })
  
  #tabela de classificação de risco sul por mês
  output$classificacao_sul_mes_tab <- renderDataTable({
    
    if(input$classificacao_sul_tipo == "Comparação"){
      
      classificacao_sul_mes <- atendimentos_classificacao_sul_mes
    }else{
      
      classificacao_sul_mes <- atendimentos_classificacao_sul_mes %>% filter(CLASSIFICACAO == input$classificacao_sul_tipo)}
    
    classificacao_sul_mes
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações dde classificação de risco sul por mês
  output$classificacao_sul_mes_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de classificacaoicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  #gráfico de classificação de risco sul por dia da semana
  output$classificacao_sul_dia_plot <- renderPlotly({
    
    
    
    if(input$classificacao_sul_tipo == "Comparação"){
      
      classificacao_sul_dia <- atendimentos_classificacao_sul_dia
      
      a<-ggplot(classificacao_sul_dia, aes(x = as.factor(DIA))) + 
        geom_col(aes(y = QUANTIDADE, fill = CLASSIFICACAO), position = "dodge")+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    }else{
      
      classificacao_sul_dia <- atendimentos_classificacao_sul_dia %>% filter(CLASSIFICACAO == input$classificacao_sul_tipo)  
      
      a<-ggplot(classificacao_sul_dia, aes(x = as.factor(DIA))) + 
        geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))}
    
    
    ggplotly(a)
    
  })
  
  #tabela de classificação de risco sul por dia da semana
  output$classificacao_sul_dia_tab <- renderDataTable({
    
    if(input$classificacao_sul_tipo == "Comparação"){
      
      classificacao_sul_dia <- atendimentos_classificacao_sul_dia
    }else{
      
      classificacao_sul_dia <- atendimentos_classificacao_sul_dia %>% filter(CLASSIFICACAO == input$classificacao_sul_tipo)}
    
    classificacao_sul_dia
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações de classificação de risco sul por mês
  output$classificacao_sul_dia_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de classificacaoicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  
  #gráfico de classificação de risco sul por hora do dia
  output$classificacao_sul_hora_plot <- renderPlotly({
    
    
    
    if(input$classificacao_sul_tipo == "Comparação"){
      
      classificacao_sul_hora <- atendimentos_classificacao_sul_hora
      
      a<-ggplot(classificacao_sul_hora, aes(x = as.factor(HORA))) + 
        geom_col(aes(y = QUANTIDADE, fill = CLASSIFICACAO), position = "dodge")+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    }else{
      
      classificacao_sul_hora <- atendimentos_classificacao_sul_hora %>% filter(CLASSIFICACAO == input$classificacao_sul_tipo)  
      
      a<-ggplot(classificacao_sul_hora, aes(x = as.factor(HORA))) + 
        geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
        ylab("  ")+
        xlab("  ")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))}
    
    
    ggplotly(a)
    
  })
  
  #tabela de classificação de risco sul por hora do dia
  output$classificacao_sul_hora_tab <- renderDataTable({
    
    if(input$classificacao_sul_CLASSIFICACAO == "Comparação"){
      
      classificacao_sul_hora <- atendimentos_classificacao_sul_hora
    }else{
      
      classificacao_sul_hora <- atendimentos_classificacao_sul_hora %>% filter(CLASSIFICACAO == input$classificacao_sul_tipo)}
    
    classificacao_sul_hora
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações de classificação de risco sul por hora do dia
  output$classificacao_sul_hora_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de classificacaoicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  
  ###########################################################################################
  #Dashboard Odontologia UPA sul  
  ###########################################################################################
  
  #gráfico de odontologia no sul por mês
  output$odontologia_sul_mes_plot <- renderPlotly({
    
    
    a<-ggplot(atendimentos_odontologia_sul_mes, aes(x = as.factor(DATA))) + 
      geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
      ylab("  ")+
      xlab("  ")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    ggplotly(a)
    
  })
  
  #tabela odontologia no sul por mês
  output$odontologia_sul_mes_tab <- renderDataTable({
    
    atendimentos_odontologia_sul_mes
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações odontologia no sul por mês
  output$odontologia_sul_mes_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de odontologiaicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  #gráfico odontologia no sul por dia da semana
  output$odontologia_sul_dia_plot <- renderPlotly({
    
    a<-ggplot(atendimentos_odontologia_sul_dia, aes(x = as.factor(DIA))) + 
      geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
      ylab("  ")+
      xlab("  ")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    ggplotly(a)
    
  })
  
  #tabela odontologia no sul por dia da semana
  output$odontologia_sul_dia_tab <- renderDataTable({
    
    atendimentos_odontologia_sul_dia
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações odontologia no sul por mês
  output$odontologia_sul_dia_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de odontologiaicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  
  #gráfico odontologia no sul por hora do dia
  output$odontologia_sul_hora_plot <- renderPlotly({
    
    a<-ggplot(atendimentos_odontologia_sul_hora, aes(x = as.factor(HORA))) + 
      geom_col(aes(y = QUANTIDADE, fill = QUANTIDADE))+ 
      ylab("  ")+
      xlab("  ")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    ggplotly(a)
    
  })
  
  #tabela odontologia sul por hora do dia
  output$odontologia_sul_hora_tab <- renderDataTable({
    
    atendimentos_odontologia_sul_hora
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações odontologia sul por hora do dia
  output$odontologia_sul_hora_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de odontologiaicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  
  
}    

###########################################################################################
shinyApp(ui, server)