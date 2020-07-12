############################################# Entrando com a Interface pessoal ############################################ 

#Entrando com os pacotes necessários para análise

#install.packages("shiny")
library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("DT")
library(DT)
#install.packages("knitr")
library(knitr)
#install.packages("plotly")
library(plotly)
#install.packages("datasets")
library(datasets)
#install.packages("leaflet")
library(leaflet)
#install.packages("colourpicker")
library(colourpicker)

#Criação da interface pessoal 

page <-  dashboardPage(skin = "blue",
                       
                       # Serve para modificar a formatação do texto no topo da página, adicionados links para sites externos e quadros de mensagens no topo da tela
                       
                       header <- dashboardHeader(title = span(
                         div(class="head",
                         img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/53/Google_%22G%22_Logo.svg/512px-Google_%22G%22_Logo.svg.png",width="40",height="32", align = "center"), # Colocando o slogan ao lado do nome
                         "Tópicos em Estatística 1", # Título do Dashboard
                         style = "font-family: Tahoma; font-weight: bold"
                       )),titleWidth = "400px",
                      
                       tags$li(a(href = 'https://matriculaweb.unb.br/graduacao/sec/login.aspx', # página externa
                                 icon("wifi"),
                                 title = "Matrícula Web"),
                               class = "dropdown"),
                       tags$li(a(href = 'https://aprender.unb.br/index.php', # página externa
                                 icon("link"),
                                 #img(src = 'https://s1.static.brasilescola.uol.com.br/be/vestibular/-5bfe74cfc335e.jpg', # opção de colocar uma foto no icone de acesso
                                 title = "Aprender (Moodle)", height = "30px"),
                               #style = "padding-top:10px; padding-bottom:10px;",
                               class = "dropdown"),
                       
                       dropdownMenu(type = "message", #Criando uma mensagem de notificação
                                    messageItem(from = "Texto 1", message = "Clique na mensagem", href="https://shiny.rstudio.com/"), 
                                    messageItem(from = "Texto 2", message = "Mensagem número 2", icon = icon("r-project")) 
                       )),
                       
                       sidebar <- dashboardSidebar( uiOutput("sidebarpanel")),
                       
                       body <- dashboardBody(tags$style(".topimg {
                            margin-left:-30px;
                            margin-right:-400px; 
                            margin-top:-15px;
                          }"), # Dimensões da imagem na tela de login
                                
                                             tags$style(".head {
                            margin-left:-50px;
                            margin-right:-50px; 
                            margin-top:-5px;
                          }"),
                                             tags$head(tags$style(HTML(' 
                           /* tabBox color text */                    
                           .nav-tabs-custom > .nav-tabs > li.header {
                           color: orange;
                           font: normal 12pt Arial;
                           text-align: center;
                          }'))), #Serve para trocar a cor do cabeçalho 
                                             
                           tags$head(tags$style('body {color:black;font: normal 10pt Arial;}')), # Trocar tamanho da fonte do texto
                          
                           tags$style(".nav-tabs {
                           background-color: #FFFFFF;height: 30px;}

                          .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
                          background-color: transparent;
                          border-color: transparent;}

                          .nav-tabs-custom .nav-tabs li.active {
                          border-top-color: #FFFFFF;
                          }"),   # Mudando a cor do cabeçalho das caixas de texto
                           
                         #   tags$head(tags$style(HTML('
                         #                 
                         #  /* Cor do sidebar menu*/
                         #  .skin-blue .main-sidebar {
                         #  background-color: rgb(255,125,125);
                         #  }
                         # 
                         #  /* Cor e fonte interna do sidebar menu */
                         #  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                         #  background-color: rgb(255,125,125);
                         #  color: rgb(255,255,255);font-weight: bold;font-size: 18px;
                         #  }
                         #                             
                         #  /* Cor da fonte externa */
                         #  .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                         #  background-color: rgb(255,125,125);
                         #  color: rgb(255,255,255);font-weight: bold;
                         #  }
                         # 
                         # /* toggle button when hovered  */
                         # .skin-blue .main-header .navbar .sidebar-toggle:hover{
                         #  background-color: rgb(255,125,125);color:rgb(255,255,255);
                         #  }'))), #Serve para manipular as cores do Menu lateral
                             
                           uiOutput("body"),
                                              
                                              tabItems(
                                                
                                                tabItem(tabName = "aba_2", # Adicionando as caixas de InfoBox na aba 2 
                                                        
                                                        fluidRow(
                                                          valueBoxOutput("Info1"),
                                                          valueBoxOutput("Info2"),
                                                          valueBoxOutput("Info3"),
                                                          infoBoxOutput("Info4"),
                                                          infoBoxOutput("Info5"),
                                                          infoBoxOutput("Info6")),
                                                        
                                                        fluidRow(
                                                          box(title = strong("Gráfico número 1"), status = "primary", solidHeader = TRUE, plotlyOutput("grafico_aba2_1")), #Gráfico 1
                                                          box(title = strong("Gráfico número 2"), status = "warning", solidHeader = TRUE, plotlyOutput("grafico_aba2_2")), #Gráfico 2
                                                          box(title = strong("Gráfico número 3"), status = "primary", solidHeader = TRUE, plotlyOutput("grafico_aba2_3")), #Gráfico 3
                                                          box(title = strong("Gráfico número 4"), status = "warning", solidHeader = TRUE, plotlyOutput("grafico_aba2_4")) #Gráfico 4
                                                          
                                                        )),
                                                
                                                
                                                tabItem(tabName = "aba_1", # Adicionando as caixas de texto na aba 1 
                                                        
                                                        fluidRow(
                                                          tabBox(
                                                            side = "right", height = "200px", width = '90%',
                                                            title = "Caixa de Texto",
                                                            # The id lets us use input$tabset1 on the server to find the current ta
                                                            tabPanel(" ",p(em("Coloque um "),em(strong("texto")),em("qualquer.", align = "center")),
                                                                     
                                                                     p(em("Pulando uma linha"),em(strong("(Parágrafo)")),em(".", align = "left"))
                                                                     , height = "350px", width = '100%')
                                                          ),
                                                          
                                                          tabBox(
                                                            side = "right", height = "390px", width = '90%',
                                                            title = "Colocando uma tabela qualquer",
                                                            # The id lets us use input$tabset1 on the server to find the current ta
                                                            tabPanel("","", height = "350px", width = '100%'),
                                                            fluidRow(align = "center",
                                                                     tableOutput("iris")
                                                            )
                                                          )),
                                                        
                                                        fluidRow(
                                                          box(title = strong("Gráfico número 1"), status = "primary", solidHeader = TRUE, plotlyOutput("grafico_aba1_1")),#Gráfico 1
                                                          box(title = strong("Gráfico número 2"), status = "warning", solidHeader = TRUE, plotlyOutput("grafico_aba1_2")), #Gráfico 2
                                                          box(title = strong("Gráfico número 3"), status = "primary", solidHeader = TRUE, plotlyOutput("grafico_aba1_3")), #Gráfico 3
                                                          box(title = strong("Gráfico número 4"),status = "warning", solidHeader = TRUE, plotlyOutput("grafico_aba1_4"))) #Gráfico 4
                                                ),
                                                
                                                tabItem(tabName = "aba_3", # Adicionando as caixas de texto na aba 3  
                                                        
                                                        fluidRow(
                                                          tabBox(
                                                            side = "right", height = "325px", width = '90%',
                                                            title = "Descrição sobre alguma coisa referente ao banco",
                                                            # The id lets us use input$tabset1 on the server to find the current ta
                                                            tabPanel(" ",p(em("O banco de dados analisado"),em(strong("(TEXTO EM NEGRITO)")),em("apresenta tais características ...", align = "center")), height = "350px", width = '100%'),
                                                            p(em("Nova Linha")),
                                                            tags$br(),
                                                            tags$div(tags$ul(
                                                              tags$li(a(span(icon("wolf-pack-battalion"), style = "color:red")),strong(span("Tópico 1: ", style = "color:red")),em("texto")),
                                                              tags$br(),
                                                              tags$li(a(span(icon("facebook"), style = "color:blue")),strong(span("Tópico 2: ", style = "color:blue")),em("texto")),
                                                              tags$br(),
                                                              tags$li(a(span(icon("broom"), style = "color:purple")),strong(span("Tópico 3: ", style = "color:purple")),em("texto")),
                                                              tags$br(),
                                                              tags$li(a(span(icon("feather-alt"), style = "color:green")),strong(span("Tópico 4: ", style = "color:green")),em("texto")),
                                                              tags$br(),
                                                              tags$li(a(span(icon("d-and-d"), style = "color:orange")),strong(span("Tópico 5: ", style = "color:orange")),em("texto"))),  style = "font-size: 15px")
                                                          )
                                                        ),
                                                        
                                                        fluidRow(
                                                          box(title = "Gráfico número 1", status = "primary", solidHeader = TRUE, plotlyOutput("grafico_aba3_1")), #Gráfico 1
                                                          box(title = "Gráfico número 2", status = "warning", solidHeader = TRUE,plotlyOutput("grafico_aba3_2")), #Gráfico 2
                                                          box(title = "Gráfico número 3", status = "primary", solidHeader = TRUE,plotlyOutput("grafico_aba3_3")), #Gráfico 3
                                                          box(title = "Gráfico número 4", status = "warning", solidHeader = TRUE,plotlyOutput("grafico_aba3_4")) #Gráfico 4
                                                          
                                                        )
                                                ),
                                                
                                                tabItem(tabName = "aba_5", # Adicionando o mapa na quinta aba
                                                        
                                                        fluidRow(
                                                          box(leafletOutput("mymap", width="100%", height = 940),
                                                              width = 12,
                                                              title = "Map",
                                                              status = "primary",
                                                              solidHeader = TRUE,
                                                              collapsible = FALSE,
                                                              height = "100%"
                                                          )) # Mapa
                                                ),
                                                
                                                
                                                tabItem(
                                                  tabName = "aba_4",
                                                  fluidRow(
                                                    dataTableOutput("visualizacao")
                                                  )
                                                )
                                              )
                       )
)


ui <- dashboardPage(header,sidebar, body)
