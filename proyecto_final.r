# Instalamos Las librerias Necesarias
install.packages("shinydashboard")
install.packages("data.table")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("plotly")
install.packages("dplyr")
install.packages("shiny")
install.packages("fmsb")

# Cargamos las librerias necesarias
library(shinydashboard)
library(data.table)
library(lubridate)
library(ggplot2)
library(plotly)
library(dplyr)
library(shiny)
library(fmsb)

# Cargar y preparar datos
dataset_path <- file.choose()
data <- fread(dataset_path)
data[, Date := ymd(Date)]
data[, Total_Calculated := Quantity * `Price per Unit`]

# UI
ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/4/42/Rica_logo.png/600px-Rica_logo.png", height = "30px"),
      "Dashboard Ejecutivo"
    ),
    dropdownMenu(
      type = "tasks",
      badgeStatus = "info",
      taskItem(
        value = 100,
        color = "aqua",
        "Samantha Rosario - Ejecutivo de Marketing"
      )
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen Ejecutivo", tabName = "resumen", icon = icon("chart-line")),
      menuItem("Ventas", tabName = "ventas", icon = icon("shopping-cart")),
      menuItem("Clientes", tabName = "clientes", icon = icon("users")),
      menuItem("Productos", tabName = "productos", icon = icon("box")),
      menuItem("Geografía", tabName = "mapas", icon = icon("map"))
    )
  ),
  
  dashboardBody(
    fluidRow(
      box(
        width = 12,
        dateRangeInput("date_range", "Rango de Fechas", start = Sys.Date() - 30, end = Sys.Date())
      )
    ),
    tabItems(
      tabItem(tabName = "resumen", h2("Resumen Ejecutivo"), fluidRow(
        valueBox(
          value = "89,935",
          subtitle = "Clientes Nuevos",
          icon = icon("user-plus"),
          color = "green",
          width = 4
        ),
        valueBox(
          value = "23,283",
          subtitle = "Total Conversiones",
          icon = icon("exchange-alt"),
          color = "aqua",
          width = 4
        ),
        valueBox(
          value = "124,854",
          subtitle = "Total Ventas",
          icon = icon("shopping-basket"),
          color = "green",
          width = 4
        ),
        
        valueBox(
          value = "$18.20",
          subtitle = "Ticket Promedio",
          icon = icon("receipt"),
          color = "light-blue",
          width = 4
        ),
        valueBox(
          value = "$124,854",
          subtitle = "CPC Total",
          icon = icon("money-bill-wave"),
          color = "red",
          width = 4
        ),
        valueBox(
          value = "46,827",
          subtitle = "Objetivos Alcanzados",
          icon = icon("bullseye"),
          color = "yellow",
          width = 4
        ),
        box(
          title = "Evolución de Ventas en el Tiempo",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("ventas_series_plot", height = "400px")
        )
      )),
      tabItem(tabName = "clientes", h2("Análisis de Clientes"),
              fluidRow(
                box(title = "Distribución de Clientes por Categoría y Género", width = 12, plotlyOutput("audiencia_barras")),
                box(title = "Pirámide de Audiencia por Edad y Género", width = 12, plotlyOutput("audiencia_piramide")),
                box(title = "Perfil Promedio por Género", width = 12, plotOutput("audiencia_radar"))
              )),
      tabItem(tabName = "productos", h2("Productos Destacados")),
      tabItem(tabName = "mapas", h2("Visualización Geográfica"))
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$audiencia_barras <- renderPlotly({
    resumen <- data %>%
      group_by(`Product Category`, Gender) %>%
      summarise(clientes = n(), .groups = "drop")
    
    p <- ggplot(resumen, aes(x = `Product Category`, y = clientes, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Distribución de Clientes por Categoría y Género",
        x = "Categoría de Producto",
        y = "Cantidad de Clientes"
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")
    
    ggplotly(p)
  })
  
  output$audiencia_piramide <- renderPlotly({
    data_piramide <- data %>%
      mutate(AgeGroup = cut(Age, breaks = seq(10, 70, by = 10), right = FALSE)) %>%
      group_by(AgeGroup, Gender) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(Count = ifelse(Gender == "Male", -Count, Count))  # Valores negativos para hombres

    p <- ggplot(data_piramide, aes(x = AgeGroup, y = Count, fill = Gender)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        title = "Pirámide de Audiencia por Edad y Género",
        x = "Grupo de Edad",
        y = "Cantidad de Clientes"
      ) +
      theme_minimal() +
      scale_fill_manual(values = c("Male" = "#3498DB", "Female" = "#E91E63"))
    
    ggplotly(p)
  })
  
  output$audiencia_radar <- renderPlot({
    radar_data <- data %>%
      group_by(Gender) %>%
      summarise(
        avg_age = mean(Age, na.rm = TRUE),
        avg_amount = mean(`Total Amount`, na.rm = TRUE),
        avg_quantity = mean(Quantity, na.rm = TRUE)
      )

    radar_df <- as.data.frame(radar_data[, -1])
    rownames(radar_df) <- radar_data$Gender

    # Añadir límites para normalización
    radar_df <- rbind(
      max = c(70, 500, 10),
      min = c(10, 0, 1),
      radar_df
    )

    radarchart(radar_df,
      axistype = 1,
      pcol = c("#3498DB", "#E91E63"),
      pfcol = c("#3498DB80", "#E91E6380"),
      plwd = 2,
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey",
      caxislabels = seq(0, 100, 20),
      title = "Perfil Promedio por Género"
    )

    legend("topright", legend = rownames(radar_df)[-c(1, 2)], col = c("#3498DB", "#E91E63"), lty = 1, bty = "n")
  })
}

# Run App
shinyApp(ui, server)
