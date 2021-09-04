library(shiny)
library(ggplot2)
library(cowplot)
library(dplyr)

#Donde se muestran los datos y las visualizaciones
ui <-  fluidPage(
    #Título de nuestra página
    h1("Propiedades de una distribución t de Student"),
    
    #Barra para seleccionar los df
    fluidRow(column(4, sliderInput("df",
                                   label = h3("Grados de Libertad"),
                                   min = 1,
                                   max = 25,
                                   value = 1))),
    
    #Texto que nos dice que df tenemos seleccionados
    textOutput("df_sel"),
    
    #Gráfico
    plotOutput("plot")
)

#Donde se manipula los datos y se generan los gráficos
server <-  function(input, output, session) {

    #Texto al seleccionar los df
    output$df_sel <- renderText({
        paste("Tienes seleccionados", input$df, "grados de libertad")
    })
    
    
    data <- data.frame(x = c(-4,4))
        
    #Gráfico de ggplot2
    output$plot <- renderPlot({
        g <- ggplot(data, aes(x = x))
        g + stat_function(fun = dt, args = list(df = input$df), size = 0.6, color = "red") +
            stat_function(fun =dnorm, size = 0.6, linetype = "dashed") +
            geom_text(aes(x = 2, label = c("df =          ", input$df), y = 0.3), color = "red") +
            scale_y_continuous(breaks = NULL) +
            ylab("") +
            xlab("") +
            theme_minimal_hgrid(color = "black")
    })
}

#Lanza el código
shinyApp(ui, server)