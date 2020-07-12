#############################################################################################################
#                                       Entrando com identificador de usuário
#############################################################################################################

login_details <- data.frame(user = c("sam", "pam", "ron"),     # Logins e Senhas para acesso
                            pswd = c("123", "123", "123"))
login <- box(
  title = "Login",height = 270,
  textInput("userName", "Username"),
  passwordInput("passwd", "Password"),    # Estruturando a caixa de login que fica na página principal
  br(),
  actionButton("Login", "Log in"),
  div(class="topimg",img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/53/Google_%22G%22_Logo.svg/512px-Google_%22G%22_Logo.svg.png", height = 180, width = 250, align = "right")) # Adicionando a foto da logo na pagina de login
)


# Entrando com o server

shinyServer(function(input, output, session){ 
  
  # Comando para retornar a página de login
  
  login.page = paste(
    isolate(session$clientData$url_protocol),
    "//",
    isolate(session$clientData$url_hostname),
    ":",
    isolate(session$clientData$url_port),
    sep = ""
  )
  
  USER <- reactiveValues(Logged = F)
  observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(login_details$user %in% Username)
          Id.password <- which(login_details$pswd %in% Password)
          if (length(Id.username) > 0 & length(Id.password) > 0){
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            }
          }
        }
      }
    }
  })
  output$sidebarpanel <- renderUI({
    if (USER$Logged == TRUE) {
      div(
        sidebarUserPanel(
          isolate(input$userName),
          subtitle = a("Logout", href = login.page),
          image = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/53/Google_%22G%22_Logo.svg/512px-Google_%22G%22_Logo.svg.png"), #Serve para adicionar a foto do usuário
        
        sidebarMenu(
          
          # É necessário que parte da interface pessoal (SidebarMenu) esteja no server já que ele vai ser ocultado enquanto
          # não entrarem com o login e a senha 
          
          menuItem("Aba número 1", icon = icon("user-tie"),  # Criando a primeira aba 
                   
                   menuSubItem(icon = NULL,
                               selectInput("aba1_1", "Selecione o mês de análise:", choices=c("Janeiro","Fevereiro","Março","Abril","Maio",
                                                                                              "Junho","Julho","Agosto","Setembro","Outubro","Novembro","Dezembro","Todos os meses"),multiple = TRUE, selected = TRUE), tabName = "aba_1"),
                   menuSubItem(icon = NULL,
                               selectInput("aba1_2", "Selecione o ano de análise:", choices=c("2017","2018","2019","2020","Todos os anos"),multiple = TRUE, selected = TRUE), tabName = "aba_1"),

                   menuSubItem(icon = NULL,
                               radioButtons("aba1_3", label = ("Selecione uma opção:"),
                                            choices = list("Opção 1" = 1, "Opção 2" = 2, "Opção 3" = 3),
                                            selected = 1), tabName = "aba_1"),
                   
                   menuSubItem(icon = NULL,
                               actionButton("aba1_4", "Botão"))), 
          
          
          menuItem("Aba número 2", tabName = "aba_2", icon = icon("tasks")), # Criando a segunda aba 
          
          menuItem("Aba número 3", tabName = "aba_3", icon = icon("chart-line")), # Criando a terceira aba 
          
          menuItem("Aba número 4", tabName = "aba_4", icon = icon("street-view")), # Criando a quarta aba
          
          menuItem("Aba número 5", icon = icon("calendar"), # Criando a quinta aba
                   
                   menuSubItem(icon = NULL,
                               textInput("aba_5_1", "Digite um texto", value = "Enter text..."), tabName = "aba_5"),
                   
                   
                   conditionalPanel(
                     condition = "input.aba_5_1 != 'Enter text...'",
                     radioButtons("aba_5_2", label = ("Selecione uma opção:"),
                                  choices = list("Opção 1" = 1, "Opção 2" = 2, "Opção 3" = 3), 
                                  selected = 0), tabName = "aba_5"),
                   
                   conditionalPanel(
                     condition = "input.aba_5_2 == 1",
                     sliderInput("aba_5_2_1", "Escolha um número", min = 0, max = 5, value = 1), tabName = "aba_5"),
                   
                   conditionalPanel(
                     condition = "input.aba_5_2 == 2",
                     dateInput('aba_5_2_2',
                               label = 'Selecione uma data',
                               value = Sys.Date(),
                               language = "pt-BR"
                     ), tabName = "aba_5"),
                   
                   conditionalPanel(
                     condition = "input.aba_5_2 == 3",
                     colourInput("aba_5_2_3"
                                 ,"Escolha uma cor:", value ="blue"), tabName = "aba_5"),
                   
                   menuSubItem(icon = NULL,
                               actionButton("aba_5_3", "Botão"))))
        
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$Logged == TRUE) {
      
    } else {   # Final do código para login 
      login
    }
  })
  
  observe({  # Serve para criar um sistema de atualização automático da página (temporizador)
    
    invalidateLater(120000, session) # Código programado para atualizar a cada 2 minutos
    
    ############################## Entrando com os bancos de dados #################################
    
    banco <- data(iris)
    
    #Caso tenha necessidade de atualizar os filtros na Ui enquanto o sistema funciona 
    
    updateSelectInput(session, "aba_5_2",
                      choices = list("Opção 1" = 1, "Opção 2" = 2, "Opção 3" = 3)
    )
    
    updateSelectInput(session, "aba_6_1",
                      choices = list("Opção 1" = 1, "Opção 2" = 2, "Opção 3" = 3)
    )
    
    
    ############################# Estruturando os Gráficos para a Aba número 1 ###########################
    
    # Plotando a tabela  inicial 
    
    output$iris <- renderTable(head(iris, n=10), rownames = TRUE)
    
    observeEvent(input$aba1_4, { 
      
      ############################################# Imagem número 1
      
      if(input$aba1_3 == 1){ # Mostrando que pode usar condicionais 
        
        output$grafico_aba1_1 <- renderPlotly({ 
          
          c1 <-  ggplot(mpg, aes(x=class , y=hwy))  + scale_x_discrete() +
            geom_jitter(aes(color = class, x = class), 
                        position = position_jitter(width = .05), alpha = 0.5) +
            geom_boxplot(aes(color = class), outlier.colour = NA, position = "dodge") +
            xlab("Class") + ylab("Highway miles per gallon")
          
          ggplotly(c1)
          
        })}
      
      ############################################# Imagem número 2
      
      if(input$aba1_3 == 1){# Mostrando que pode usar condicionais
        
        output$grafico_aba1_2 <- renderPlotly({ 
          
          c2 <- ggplot(data=iris, aes(x=Sepal.Width, fill=Species)) + geom_density(stat="density", alpha=I(0.2)) +
            xlab("Sepal Width") +  ylab("Density") 
          
          ggplotly(c2)
          
        })}
      
      ############################################# Imagem número 3
      
      if(input$aba1_3 == 1){# Mostrando que pode usar condicionais
        
        output$grafico_aba1_3 <- renderPlotly({ 
          
          c3 <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + 
            geom_point(aes(shape=Species), size=1.5) + xlab("Sepal Length") + ylab("Sepal Width") + geom_smooth(method="lm")
          
          ggplotly(c3)
          
        })}    
      
      ############################################# Imagem número 4
      
      if(input$aba1_3 == 1){# Mostrando que pode usar condicionais
        
        output$grafico_aba1_4 <- renderPlotly({
          
          c4 <- ggplot(data = diamonds) + 
            geom_bar(mapping = aes(x = cut, fill = clarity))
          
          ggplotly(c4)
          
        })}
      
    })
    
    ############################# Estruturando os Info gráficos para a Aba número 2 ###########################
    
    output$Info1 <- renderValueBox({ #Infobox de Processos Encerrados
      
      if(nrow(iris) > 10){ # Colocando condições para plotagem de diversos Info-Box
        valueBox(
          "Box 1", nrow(iris),icon = icon("address-card"),color = "purple")}
      
      else{
        valueBox(
          "Box 1", nrow(iris),icon = icon("accusoft"),color = "orange")}
    })
    
    output$Info2 <- renderValueBox({
      
      if(nrow(iris) > 10){ # Colocando condições para plotagem de diversos Info-Box
        valueBox(
          "Box 2", round(mean(iris$Sepal.Length),2),icon = icon("apple"),color = "red")}
      
      else{
        valueBox(
          "Box 2", round(mean(iris$Sepal.Length),2),icon = icon("android"),color = "green")}
    })
    
    output$Info3 <- renderValueBox({
      
      if(nrow(iris) > 10){ # Colocando condições para plotagem de diversos Info-Box
        valueBox(
          "Box 3", round(mean(iris$Sepal.Width),2),icon = icon("battery-full"),color = "blue")}
      
      else{
        valueBox(
          "Box 3", round(mean(iris$Sepal.Width),2),icon = icon("battery-quarter"),color = "red")}
    })
    
    output$Info4 <- renderInfoBox({
      
      if(nrow(iris) > 10){ # Colocando condições para plotagem de diversos Info-Box
        infoBox(
          "Box 4", round(var(iris$Sepal.Width),2),icon = icon("map-signs"),color = "green")}
      
      else{
        infoBox(
          "Box 4", round(var(iris$Sepal.Width),2),icon = icon("map-marked-alt"),color = "red")}
    })
    
    output$Info5 <- renderInfoBox({
      
      if(nrow(iris) > 10){ # Colocando condições para plotagem de diversos Info-Box
        infoBox(
          "Box 5", round(var(iris$Sepal.Length),2),icon = icon("thumbs-up"),color = "orange")}
      
      else{
        infoBox(
          "Box 5", round(var(iris$Sepal.Length),2),icon = icon("thumbs-down"),color = "purple")}
    })
    
    output$Info6 <- renderInfoBox({
      
      if(nrow(iris) > 10){ # Colocando condições para plotagem de diversos Info-Box
        infoBox(
          "Box 6", round(var(iris$Petal.Width),2),icon = icon("reddit-alien"),color = "purple")}
      
      else{
        infoBox(
          "Box 6", round(var(iris$Petal.Width),2),icon = icon("wolf-pack-battalion"),color = "blue")}
    })
    
    
    ############################# Estruturando os Gráficos para a Aba número 2 ###########################
    
    output$grafico_aba2_1 <- renderPlotly({ ########################## Imagem 1 
      
      b1 <- ggplot(mpg, aes(displ, hwy)) + 
        geom_point() + 
        geom_smooth()
      
      ggplotly(b1)
      
    })
    
    
    output$grafico_aba2_2 <- renderPlotly({ ########################## Imagem 2
      
      b2 <- ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
        geom_point()
      
      ggplotly(b2)
      
    })
    
    output$grafico_aba2_3 <- renderPlotly({ ########################## Imagem 3
      
      b3 <- ggplot(data = diamonds) + 
        geom_bar(mapping = aes(x = cut, fill = clarity))
      
      ggplotly(b3)
      
    })
    
    output$grafico_aba2_4 <- renderPlotly({ ########################## Imagem 4
      
      b4 <- ggplot(mpg, aes(x=class , y=hwy))  + scale_x_discrete() +
        geom_jitter(aes(color = class, x = class), 
                    position = position_jitter(width = .05), alpha = 0.5) +
        geom_boxplot(aes(color = class), outlier.colour = NA, position = "dodge") +
        xlab("Class") + ylab("Highway miles per gallon")
      
      ggplotly(b4)
    })
    
    
    
    ############################# Estruturando os Gráficos para a número 3 ###########################
    
    output$grafico_aba3_1 <- renderPlotly({ ########################## Imagem 1
      
      a1 <- ggplot(data=iris, aes(x=Sepal.Width)) + geom_histogram(binwidth=0.2, color="black", fill="steelblue", aes(y=..density..)) +
        geom_density(stat="density", alpha=I(0.2), fill="blue") +
        xlab("Sepal Width") +  ylab("Density")
      
      ggplotly(a1)
      
    })
    
    output$grafico_aba3_2 <- renderPlotly({ ########################## Imagem 2
      
      a2 <- ggplot(data=iris, aes(x=Sepal.Width, fill=Species)) + geom_density(stat="density", alpha=I(0.2)) +
        xlab("Sepal Width") +  ylab("Density") 
      
      ggplotly(a2)
      
    })
    
    output$grafico_aba3_3 <- renderPlotly({ ########################## Imagem 3
      
      a3 <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + 
        geom_point(aes(shape=Species), size=1.5) + xlab("Sepal Length") + ylab("Sepal Width") + 
        ggtitle("Scatterplot with smoothers")+ geom_smooth(method="lm")
      
      ggplotly(a3)
      
    })
    
    output$grafico_aba3_4 <- renderPlotly({ ########################## Imagem 4
      
      
      a4 <- ggplot(data=iris, aes(x = Sepal.Length)) + 
        stat_density(aes(ymax = ..density..,  ymin = -..density.., 
                         fill = Species, color = Species), 
                     geom = "ribbon", position = "identity") +
        facet_grid(. ~ Species) + coord_flip() + xlab("Sepal Length") 
      
      ggplotly(a4)
      
    })
    
    ######################################### Tabela da aba número 4 ########################################
    
    output$visualizacao <- DT::renderDataTable({
      
      datatable(mpg,colnames =c("Título A", "Título B","Título C","Título D", "Título E","Título F","Título G","Título H","Título I","Título J","Título K","Título L"),  extensions = c('FixedColumns',"FixedHeader","Buttons","ColReorder","Scroller"), 
                options = list(dom = 'Blfrtip', colReorder = TRUE, autoWidth=TRUE, lengthMenu = list(c(10, 25 ,50, 75, -1), c('10','25' ,'50', '75','All')),
                               buttons = list('copy', 'print', list(
                                 extend = 'collection',
                                 buttons = c('csv', 'excel', 'pdf'),
                                 text = 'Download'
                               )),scrollX = TRUE, scrollY  = 500, 
                               fixedHeader=TRUE, fixedColumns = list(leftColumns = 2, rightColumns = 0),
                               initComplete = JS("function(settings, json) {",
                                                 "$(this.api().table().header()).css({'background-color': '#f2f5f9', 'color': 'black'});",
                                                 "}")
                )) %>%
        formatStyle( 
          'manufacturer',
          target = 'row', fontWeight = styleEqual(c("audi"), c('bold'))
        )%>%
        formatStyle( 
          'manufacturer',
          target = 'row',
          backgroundColor = styleEqual(c("chevrolet","dodge"), c('orange', 'green'))
        )})
    
    
    ############################# Estruturando o mapa para a Aba número 5 ###########################
    
    points <- eventReactive(input$aba_5_3, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(data = points())
    })
    
  })
  
  
  
})


