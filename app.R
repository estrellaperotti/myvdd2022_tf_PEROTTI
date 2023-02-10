################################################################################

# TRABAJO PRÁCTICO FINAL - ESTRELLA PEROTTI
# Maestría en Estadística Aplicada
# Manejo y Visualización de datos
# Rosario 10/02/2023

################################################################################

#Paquetes instalados para poder trabajar con serie de tiempo

#install.packages("ggpmisc")
#install.packages("PerformanceAnalytics")
#install.packages("timeSeries")
#install.packages("plotly")
#install.packages("colorspace")


#Librerías utilizadas

library(shiny)
library(quantmod)
library(xts)
library(ggplot2)
library(tidyverse)
library(zoo)
library(lubridate)
library(knitr)
library("tseries")
library("forecast")
library(ggpmisc)




# Interfaz del usuario

ui <- (fluidPage(
  br(),
  titlePanel(strong("Análisis de tendencia de precios de la soja CME®
                     utilizando Modelos ARIMA")),
  br(),
  
  sidebarLayout(
    sidebarPanel("Seleccione la posición que desea analizar", 
                 selectInput('Posición', label = 'Posición', 
                             choices = c("Mayo 2023"="ZSK23.CBT", "Julio 2023"="ZSN23.CBT",
                                         "Noviembre 2023"="ZSX23.CBT","Mayo 2024"="ZSK24.CBT")),
                 
                 # En las opciones de menú, se le proporcionarán diferentes alternativas 
                 # al usuario. La primera será seleccionar entre una serie de vencimientos de futuros
                 # previamente seteados por el progrador. 
                 
                 dateInput(inputId="fechadesde", label="Desde fecha",
                           language= "es", width = "40%", value = "2022-01-01"),
                 
                 dateInput(inputId="fechahasta", label="Hasta fecha",
                           language= "es", width = "40%", value = today()),
                 
                 # Se le permite también al usuario elegir las fechas que quiere evaluar dentro de 
                 # las diferentes series de tiempo. Por default se utilizan datos desde comienzos de 
                 # 2022 y hasta el día actual; en este último caso se utiliza la función today()
                 # del paquete lubricate. 
                 
                 
                 # A continuación se permite seleccionar un modelo de estimación futura;en nuestro
                 # caso un modelo ARIMA, que por default muestra un modelo ajustado por la función
                 # auto.arima del paquete forecast. Adicionalmente, el usuario podrá elegir los
                 # los p,d,q que considere interesantes. 
                 
                 
                 radioButtons('Modelo', label = 'Elija el Modelo a utilizar', 
                              choices = c("AUTOARIMA (default)", "ARIMA(p,d,q)"),
                              selected = "AUTOARIMA (default)" ),
                 
                 
                 fluidRow(
                   column(3, 
                          numericInput("arima1", label = "p", value = "1", min = 0, max = 10)),
                   
                   column(3,
                          numericInput("arima2", label = "d", value = "1" , min = 0, max = 10)),
                   
                   column(3,        
                          numericInput("arima3", label = "q", value = "1" , min = 0, max = 10))),
                 
                 
                 # Se proporciona asimismo la posibilidad de elegir la cantidad de días en el futuro
                 # para la predicción (h) con una cantidad arbitraria de días. Es de hacer notar que, 
                 # a diferencia de las acciones o bonos u otros activos financieros, los contratos 
                 # de futuros tienen un vencimiento establecido de antemano por el mercado en donde
                 # operan, razón por la cual para la app solo se ponen a disposición del usuario 
                 # aquellas a las que les resta tiempo al vencimiento. 
                 
                 selectInput('Estimación', label = 'Proyección futura (en días)', 
                             choices = c(30,50,100,150,252)),
                 
                 
                 #Finalmente, se coloca dentro del sidebarPanel un disclaimer y una definición muy
                 #simplificada del modelo ARIMA utilizado.
                 
                 
                 helpText("Datos obtenidos en tiempo real de Yahoo Finance y utilizados sólo a 
             los fines educativos.", br(),br(),
             "El método de estimación ARIMA es un modelo dinámico de series 
             temporales, es decir, las estimaciones futuras vienen explicadas 
             por los datos del pasado y no por variables independientes. Para 
             realizar las estimaciones se utilizan los precios de cierre de 
             las posiciones de interés.", br(),br(),
             "La presente aplicación utiliza por default la función auto.arima 
             del lenguaje R, la cual 
             devuelve un ARIMA con AIC y MLE minimizados.", br(), br(), 
             strong("Para mayor información sobre cómo elegir 
             los componentes de un Modelos ARIMA diferente al default
             recomendamos consultar bibliografía especializada.",  br(), br(),
             strong(" MANEJO Y VISUALIZACIÓN DE DATOS"),br(), 
             strong("MAESTRÍA EN ESTADÍSTICA APLICADA, FCEyE - UNR"),br(),
             ("Desarrollado por: Estrella Perotti, Mg"), br(),
             ("INFO DE CONTACTO: estrella.perotti@gmail.com"))), 
             
    ),
    
    # Se requiere asimismo que dentro del panel principal se muestren dos tipos de gráficos, 
    # una serie de precios histórica del producto seleccionado y un gráfico de 
    # predicción bajo el modelo seleccionado.   
    
    
    mainPanel("Gráfico de evolución de precios futuros de soja en CME (en Dlr/bus)",
              plotOutput('grafico1'), br(),
              
              plotOutput('grafico2'))
    
    
  )))


# SERVER

#Para proporcionar las salidas sujeridas anteriormente se extraen datos de una
#fuente de información pública (YAHOO FINANCE) y en tiempo real mediante el uso 
#de la función Getsymbols del paquete quantmode. 




server<-function(input, output) {
  
  Fecha_desde <- reactive({input$fechadesde})
  Fecha_hasta <- reactive({input$fechahasta})
  
  Modelo <- reactive ({input$Modelo})
  
  
  p <- reactive({input$arima1})
  d <- reactive({input$arima2})
  q <- reactive({input$arima3})
  
  
  
  ################################################################################  
  
  # Primer output : Gráfico de evolución de precios ( mediante un candleChart)
  
  
  output$grafico1 <- renderPlot({
    
    
    futuredata <- getSymbols(input$Posición, src="yahoo", from = Fecha_desde(),
                             to = Fecha_hasta(), auto.assign = FALSE)
    
    candleChart(futuredata, name=input$Posición, theme ="white")
    
  }) 
  
  
  #NOTA ACLARATORIA SOBRE EL TIPO DE VARIABLE y GRÁFICO UTILIZADO
  
  # class (futuredata) = [1] "xts" "zoo". La construcción de objetos de la clase 
  # ts requieren que los datos de partida estén distribuidos regularmente en la 
  # escala temporal que vayamos a utilizar, por ejemplo, que tengamos un dato por 
  # cada día ó uno al mes, etc. En caso de que la serie temporal que usemos 
  # esté constituida por valores irregularmente distribuidos en el tiempo 
  # tendremos que usar objetos de la clase zoo. De allí los paquetes con los que
  # se trabajó en el presente TP. 
  
  
  # Un gráfico de tipo "vela" muestra en un solo punto todo lo que ha sucedido
  # durante el día en un activo en particular, el volumen operado, los precios de 
  # apertura y cierre, los minimos operados y los máximos del día, de allí que para
  # mostrar la evolución se haya optado por este tipo de gráficos y no por uno que 
  # solo muestre sólo elprecio de cierre. 
  
  
  ###############################################################################
  
  # Comentario al margen pero que puede resultar de interés    
  
  #Si como programadores qusieramos utilizar un data.frame, para, por ejemplo trabajar
  #directamente con ggplot2 y no con los paquetes específicos de series de tiempo, 
  #Recomendamos: 
  #Primeramente se extraen los datos con la función Getsymbols y luego pasar el 
  #formato ts a dataframe, utilizando, por ejemplo, la siguiente función:  
  
  
  #xts_to_datframe<-function(data_xts){
  #  df_t<-data.frame(Fecha=(index(data_xts)),
  #                   Cotización=coredata(data_xts))
  #  colnames(df_t)<-c("Fecha", "sk23", "sn23", "sx23", "sk24")
  #  df_t
  #}
  
  # index(data_xts): es una declaración que toma el tiempo de una serie xts diaria.
  #coredata(data_xts): es una declaración que toma los valores de la serie xts diaria.
  #colnames (df_t): para renombrar las columnas que se crearan en el dataframe
  
  ##################################################################################    
  
  
  # Segundo output: Gráfico de estimación de tendencia de precios por ARIMA
  
  
  output$grafico2 <- renderPlot({
    
    if (Modelo() == "AUTOARIMA (default)") {
      
      futuredata <- getSymbols(input$Posición, src="yahoo", from = Fecha_desde(),
                               to = Fecha_hasta(), auto.assign = FALSE)
      
      est_sk23 <- auto.arima(futuredata[,4], seasonal =TRUE) #La columna 4 se 
      
      #corresponde con los precios de cierre de la variable de intereés
      
      forsk23 <- forecast(est_sk23, input$Estimación)
      
      autoplot(forsk23)+ labs(x = 'Tiempo (transcurrido y proyectado)', y = "Cotizaciones") + 
        theme_minimal()+
        labs(title=("Estimación de tendencia con Modelo ARIMA"))
      
      
    } else {
      
      
      futuredata <- getSymbols(input$Posición, src="yahoo", from = Fecha_desde(),
                               to = Fecha_hasta(), auto.assign = FALSE)
      
      est_sk23 <- arima(futuredata[,4], c(p(),d(),q()))
      forsk23 <- forecast(est_sk23, input$Estimación)
      
      autoplot(forsk23)+ labs(x = 'Tiempo (transcurrido y proyectado)', y = "Cotizaciones") + theme_minimal()+
        labs(title="Estimación de tendencia con Modelo ARIMA")
    }
    
    
  })
  
  # NOTA DE INTERÉS
  
  # Por default se elije un modelo auto arima, evitando de esa manera sembrar  
  # cuestionamientos acerca de la visión del mercado del desarrollador. De todos 
  # modos, se ofrece la posibilidad de que el usuario pueda trabajar con los 
  # factores pdq que necesite; no obstante ello, se presentan por default los 
  #factores pdq (111). 
  # Forecasting: principle and practise, es un libro que puede consultarse https://otexts.com/fpp2/
  
  
}

shinyApp(ui=ui, server=server)
