library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(janitor) 
library(lubridate) 
library(ggplot2)
library(gt)
library(leaflet)
library(writexl)
library(shiny)

datos_empleo <- read_csv("datos/datos_empleo_genero.csv")
View(datos_empleo)




ui <- dashboardPage(
  dashboardHeader(title = "Ánalisis sobre la Situación Laboral y Género en Latinoamérica y el Caribe", titleWidth = 800),
  dashboardSidebar(
    selectInput("pais", "Selecciona un país o región", choices = unique(datos_empleo$pais_region)),
    selectInput("genero", "Selecciona un género", choices = c("Hombres", "Mujeres")),
    sliderInput("ano_inicio", "Año de inicio", min = 2000, max = max(datos_empleo$anyo), value = 2000),
    actionButton("actualizar", "Actualizar")
  ),
  dashboardBody(
    skin = "black",  
    fluidRow(
      box(
        title = "Representación Gráfica de empleadores (a) según género",
        plotOutput("plot_empleadoras", height = "400px")
      ),
      box(
        title = "Representación Gráfica de Trabajadores Informales según género",
        plotOutput("plot_informal", height = "400px")
      ),
      box(
        title = "Representación Gráfica de Desempleo según género",
        plotOutput("plot_desempleo", height = "400px")
      )
    ),
    box(
      title = "Tabla de Datos empleo",
      DTOutput("tabla"),
      width = 30,  
      height = "810px",  
      DT::dataTableOutput("table")
    )
  )
)


server <- function(input, output) {
  datos_filtrados <- eventReactive(input$actualizar, {
    datos_empleo |> 
      filter(pais_region == input$pais, anyo >= input$ano_inicio)
  })
  
  output$plot_empleadoras <- renderPlot({
    genero_variable <- ifelse(input$genero == "Hombres", "empleadores_hombres", "empleadoras_mujeres")
    ggplot(datos_filtrados(), aes(x = anyo, y = !!sym(genero_variable))) +
      geom_line() +
      labs(title = paste("Porcentaje de", input$genero, "empleadores (as) en", input$pais),
           y = "Porcentaje",
           x = "Año")
  })
  
  output$plot_informal <- renderPlot({
    genero_variable <- ifelse(input$genero == "Hombres", "empleo_informal_hombres", "empleo_informal_mujeres")
    ggplot(datos_filtrados(), aes(x = anyo, y = !!sym(genero_variable))) +
      geom_line() +
      labs(title = paste("Porcentaje de", input$genero, "trabajadores (as) informales en", input$pais),
           y = "Porcentaje",
           x = "Año")
  })
  
  output$plot_desempleo <- renderPlot({
    genero_variable <- ifelse(input$genero == "Hombres", "desempleo_hombres", "desempleo_mujeres")
    ggplot(datos_filtrados(), aes(x = anyo, y = !!sym(genero_variable))) +
      geom_line() +
      labs(title = paste("Porcentaje de", input$genero, "desempleo en", input$pais),
           y = "Porcentaje",
           x = "Año")
  })
  
  output$tabla <- renderDT({
    datatable(datos_filtrados(), options = list(pageLength = 100, scrollX = TRUE))
  })
}

shinyApp(ui, server)