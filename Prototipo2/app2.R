library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(reshape2)
library(dplyr)
library(tidyr)
library(readr)

# Cargar los resultados de las simulaciones
source("D:/UNIVERSIDAD/TFG/Codigo/Simmer2.R")

# Cargar datos
base_output_folder <- "D:/UNIVERSIDAD/TFG/Codigo/Prototipo2"
replicates <- list.files(base_output_folder, pattern = "Simulacion_*", full.names = TRUE)

arrivals <- bind_rows(lapply(replicates, function(rep) read_csv(file.path(rep, "arrivals.csv"), show_col_types = FALSE)), .id = "simulacion")
recursos <- bind_rows(lapply(replicates, function(rep) read_csv(file.path(rep, "recursos.csv"), show_col_types = FALSE)), .id = "simulacion")
atributos <- bind_rows(lapply(replicates, function(rep) read_csv(file.path(rep, "atributos.csv"), show_col_types = FALSE)), .id = "simulacion")
facturas <- bind_rows(lapply(replicates, function(rep) read_csv(file.path(rep, "facturas.csv"), show_col_types = FALSE)), .id = "simulacion")
info_adicional <- bind_rows(lapply(replicates, function(rep) read_csv(file.path(rep, "info_adicional.csv"), show_col_types = FALSE)), .id = "simulacion")
tiempos_espera <- bind_rows(lapply(replicates, function(rep) read_csv(file.path(rep, "tiempos_espera.csv"), show_col_types = FALSE)), .id = "simulacion")
costos_promedios <- bind_rows(lapply(replicates, function(rep) read_csv(file.path(rep, "costos_promedio.csv"), show_col_types = FALSE)), .id = "simulacion")
itv_clients <- bind_rows(lapply(replicates, function(rep) read_csv(file.path(rep, "itv_clients.csv"), show_col_types = FALSE)), .id = "simulacion")

data <- list(arrivals = arrivals, recursos = recursos, atributos = atributos, facturas = facturas, info_adicional = info_adicional,
             tiempos_espera = tiempos_espera, costos_promedios = costos_promedios, itv_clients = itv_clients)

# UI
ui <- fluidPage(
  titlePanel("Análisis del Taller de Vehículos"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("simulacion", "Seleccionar Simulación", choices = c("Global", unique(data$arrivals$simulacion))),
      selectInput("tipoGrafico", "Selecciona el Tipo de Gráfico Específico:", 
                  choices = c("Circular", "Llegadas", "Densidad de Servicio", "Utilización de Recursos", 
                              "Tiempo de Espera", "Tiempo en el Sistema", "Gasto por Cliente", 
                              "Utilización de Recursos (Global)", "Uso de Recursos (Global)", 
                              "Tiempo de Espera (Global)", "Tiempo en el Sistema (Global)",
                              "Activity Time Median (Global)", "Waiting Time Median (Global)",
                              "Activity Time by Client Type (Global)", "Waiting Time by Client Type (Global)",
                              "Fallos por Tipo de Vehículo y Antigüedad", "Tiempo de Espera2 (Global)")),
      actionButton("analizar", "Analizar")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen y Estadísticas",
                 tableOutput("summary_stats"),
                 tableOutput("resource_stats") 
        ),
        tabPanel("Gráficos", 
                 conditionalPanel(
                   condition = "input.tipoGrafico == 'Circular'",
                   plotOutput("piechart")
                 ),
                 conditionalPanel(
                   condition = "input.tipoGrafico != 'Circular'",
                   plotOutput("plot"),
                   uiOutput("global_plot_ui")
                 )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  get_filtered_data <- reactive({
    req(input$simulacion)
    if (input$simulacion == "Global") {
      list(
        arrivals = data$arrivals,
        recursos = data$recursos,
        atributos = data$atributos,
        facturas = data$facturas,
        info_adicional = data$info_adicional,
        tiempos_espera = data$tiempos_espera,
        costos_promedios = data$costos_promedios,
        itv_clients = data$itv_clients
      )
    } else {
      list(
        arrivals = data$arrivals %>% filter(simulacion == input$simulacion),
        recursos = data$recursos %>% filter(simulacion == input$simulacion),
        atributos = data$atributos %>% filter(simulacion == input$simulacion),
        facturas = data$facturas %>% filter(simulacion == input$simulacion),
        info_adicional = data$info_adicional %>% filter(simulacion == input$simulacion),
        tiempos_espera = data$tiempos_espera %>% filter(simulacion == input$simulacion),
        costos_promedios = data$costos_promedios %>% filter(simulacion == input$simulacion),
        itv_clients = data$itv_clients %>% filter(simulacion == input$simulacion)
      )
    }
  })
  
  output$summary_stats <- renderTable({
    req(input$analizar)
    filtered_data <- get_filtered_data()
    

    
    # Calcular métricas generales
    summary_data <- filtered_data$arrivals %>%
      mutate(tipo_cliente = case_when(
        str_detect(name, "cliente VIP") ~ "Cliente VIP",
        str_detect(name, "cliente Lavado") ~ "Cliente Lavado",
        str_detect(name, "cliente ITV") ~ "Cliente ITV",
        TRUE ~ "Cliente"
      )) %>%
      group_by(tipo_cliente) %>%
      summarise(
        `Total Llegadas` = n(),
        `Tiempo en el Sistema Mínimo (min)` = min(end_time - start_time, na.rm = TRUE),
        `Tiempo en el Sistema Máximo (min)` = max(end_time - start_time, na.rm = TRUE),
        `Tiempo en el Sistema Promedio (min)` = mean(end_time - start_time, na.rm = TRUE),
        `Mediana Tiempo en el Sistema (min)` = median(end_time - start_time, na.rm = TRUE),
        `Tiempo en el Sistema (min^2) Varianza` = var(end_time - start_time, na.rm = TRUE),
        
        `Tiempo de Servicio Mínimo (min)` = min(activity_time, na.rm = TRUE),
        `Tiempo de Servicio Máximo (min)` = max(activity_time, na.rm = TRUE),
        `Tiempo de Servicio Promedio (min)` = mean(activity_time, na.rm = TRUE),
        `Mediana Tiempo de Servicio (min)` = median(activity_time, na.rm = TRUE),
        `Tiempo de Servicio (min^2) Varianza` = var(activity_time, na.rm = TRUE),
        
        `Tiempo de Cola Mínimo (min)` =  abs(min(end_time - start_time - activity_time, na.rm = TRUE)),
        `Tiempo de Cola Máximo (min)` =  max(end_time - start_time - activity_time, na.rm = TRUE),
        `Tiempo de Cola Promedio (min)` = mean(end_time - start_time - activity_time, na.rm = TRUE),
        `Mediana Tiempo en Cola (min)` = median(end_time - start_time - activity_time, na.rm = TRUE),
        `Tiempo de Cola (min^2) Varianza` = var(end_time - start_time - activity_time, na.rm = TRUE),
      )
    
    costo_promedio <- filtered_data$costos_promedios %>%
      rename(`Costo Promedio (EUR)` = costo_promedio)
    
    varianza_costo <- filtered_data$facturas %>%
      mutate(tipo_cliente = case_when(
        str_detect(name, "cliente VIP") ~ "Cliente VIP",
        str_detect(name, "cliente Lavado") ~ "Cliente Lavado",
        str_detect(name, "cliente ITV") ~ "Cliente ITV",
        TRUE ~ "Cliente"
      )) %>%
      group_by(tipo_cliente) %>%
      summarise(
        `Costo Mínimo (EUR)` = min(costo_total, na.rm = TRUE),
        `Costo Máximo (EUR)` = max(costo_total, na.rm = TRUE),
        `Costo Medio (EUR)` = mean(costo_total, na.rm = TRUE),
        `Mediana Costo (EUR)` = median(costo_total, na.rm = TRUE),
        `Varianza Costo (EUR^2)` = var(costo_total, na.rm = TRUE),
      )
    
    summary_stats <- summary_data %>%
      left_join(costo_promedio, by = "tipo_cliente") %>%
      left_join(varianza_costo, by = "tipo_cliente") %>%
      distinct(tipo_cliente, .keep_all = TRUE)
    
    summary_stats
  })
  
  output$resource_stats <- renderTable({
    req(input$analizar)
    filtered_data <- get_filtered_data()
    
    resource_data <- filtered_data$info_adicional %>%
      group_by(recurso) %>%
      summarise(
        `Capacidad` = mean(capacidad, na.rm = TRUE),
        `Utilización` = mean(utilizacion, na.rm = TRUE),
        `Longitud Media Cola` = mean(longitud_media_cola, na.rm = TRUE),
        `Tiempo Medio Servicio (min)` = mean(tiempo_medio_servicio, na.rm = TRUE),
        `Tiempo Total Uso (min)` = sum(tiempo_total_uso, na.rm = TRUE),
        `Número de Servidos` = sum(num_served, na.rm = TRUE)
      )
    
    resource_data
  })
  
  output$piechart <- renderPlot({
    req(input$analizar)
    filtered_data <- get_filtered_data()
    
    resource_usage <- filtered_data$recursos %>%
      group_by(resource) %>%
      summarise(total_usage = sum(server))
    
    ggplot(resource_usage, aes(x = "", y = total_usage, fill = resource)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      geom_text(aes(label = scales::percent(total_usage / sum(total_usage), accuracy = 1)), position = position_stack(vjust = 0.5)) +
      labs(title = "Distribución del Uso de Recursos", fill = "Recurso", y = "Total Uso del Recurso")
  })
  
  output$plot <- renderPlot({
    req(input$analizar)
    filtered_data <- get_filtered_data()
    
    if (input$tipoGrafico == "Llegadas") {
      ggplot(filtered_data$arrivals, aes(x = start_time)) +
        geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
        labs(title = "Llegadas de Clientes", x = "Tiempo (minutos)", y = "Número de Llegadas") +
        theme(
          text = element_text(size = 20),           # Adjusts all text sizes
          plot.title = element_text(size = 22, face = "bold"),  # Title size
          axis.title = element_text(size = 18),     # Axis titles size
          axis.text = element_text(size = 16)       # Axis text size
        )
    } else if (input$tipoGrafico == "Densidad de Servicio") {
      ggplot(filtered_data$arrivals, aes(x = activity_time)) +
        geom_density(fill = "blue", alpha = 0.5) +
        labs(title = "Densidad de Tiempo de Servicio", x = "Tiempo de Servicio (minutos)", y = "Densidad")
    } else if (input$tipoGrafico == "Utilización de Recursos") {
      ggplot(filtered_data$recursos, aes(x = time, y = server, color = resource)) +
        geom_line() +
        labs(title = "Utilización de Recursos en el Tiempo", x = "Tiempo (minutos)", y = "Uso del Recurso")
    } else if (input$tipoGrafico == "Tiempo de Espera") {
      ggplot(filtered_data$arrivals %>% mutate(waiting_time = end_time - start_time - activity_time), aes(x = name, y = waiting_time)) +
        geom_boxplot() +
        labs(title = "Tiempo de Espera por Entidad", x = "Entidad", y = "Tiempo de Espera (minutos)")
    } else if (input$tipoGrafico == "Tiempo de Servicio") {
      ggplot(filtered_data$arrivals, aes(x = name, y = activity_time)) +
        geom_boxplot() +
        labs(title = "Tiempo en el Sistema por Entidad", x = "Entidad", y = "Tiempo en el Sistema (minutos)")
    } else if (input$tipoGrafico == "Gasto por Cliente") {
      ggplot(filtered_data$facturas %>% mutate(tipo_cliente = case_when(
        str_detect(name, "cliente VIP") ~ "Cliente VIP",
        str_detect(name, "cliente Lavado") ~ "Cliente Lavado",
        str_detect(name, "cliente ITV") ~ "Cliente ITV",
        TRUE ~ "Cliente"
      )), aes(x = tipo_cliente, y = costo_total)) +
        geom_boxplot() +
        labs(title = "Distribución del Gasto por Cliente", x = "Tipo de Cliente", y = "Gasto Total (EUR)")
    } else if (input$tipoGrafico == "Activity Time by Client Type (Global)") {
      ggplot(filtered_data$arrivals %>% mutate(tipo_cliente = case_when(
        str_detect(name, "cliente VIP") ~ "Cliente VIP",
        str_detect(name, "cliente Lavado") ~ "Cliente Lavado",
        str_detect(name, "cliente ITV") ~ "Cliente ITV",
        TRUE ~ "Cliente"
      )), aes(x = activity_time, fill = tipo_cliente)) +
        geom_histogram(position = "dodge", binwidth = 10, color = "black") +
        labs(title = "Activity Time by Client Type (Global)", x = "Activity Time (min)", y = "Count") +
        theme(legend.title = element_blank())
    } else if (input$tipoGrafico == "Waiting Time by Client Type (Global)") {
      ggplot(filtered_data$arrivals %>% mutate(waiting_time = end_time - start_time - activity_time) %>% mutate(tipo_cliente = case_when(
        str_detect(name, "cliente VIP") ~ "Cliente VIP",
        str_detect(name, "cliente Lavado") ~ "Cliente Lavado",
        str_detect(name, "cliente ITV") ~ "Cliente ITV",
        TRUE ~ "Cliente"
      )), aes(x = waiting_time, fill = tipo_cliente)) +
        geom_histogram(position = "dodge", binwidth = 10, color = "black") +
        labs(title = "Waiting Time by Client Type (Global)", x = "Waiting Time (min)", y = "Count") +
        theme(legend.title = element_blank())
    } else if (input$tipoGrafico == "Fallos por Tipo de Vehículo y Antigüedad") {
      ggplot(filtered_data$itv_clients, aes(x = vehiculo_antiguedad, fill = fallo_tipo)) +
        geom_bar(position = "fill") +
        facet_wrap(~ vehiculo_tipo) +
        scale_y_continuous(labels = scales::percent) +
        labs(title = "Porcentaje de Fallos por Tipo de Vehículo y Antigüedad", x = "Antigüedad del Vehículo", y = "Porcentaje de Fallos", fill = "Tipo de Fallo")
    }
    else if (input$tipoGrafico == "Tiempo de Espera2 (Global)") {
      ggplot(filtered_data$arrivals %>% mutate(waiting_time = end_time - start_time - activity_time), aes(x = activity_time, y = waiting_time)) +
        geom_point(alpha = 0.5) +
        labs(title = "Gráfico de Dispersión: Tiempo de Espera vs. Tiempo de Actividad (Global)", x = "Tiempo de Actividad (minutos)", y = "Tiempo de Espera (minutos)") +
        theme_minimal()
    }
  })
  
  output$global_plot_ui <- renderUI({
    if (input$simulacion == "Global" && grepl("Global", input$tipoGrafico)) {
      tagList(
        if (input$tipoGrafico == "Activity Time Median (Global)") {
          tagList(
            plotOutput("global_activity_time_above_median"),
            plotOutput("global_activity_time_below_median")
          )
        } else if (input$tipoGrafico == "Waiting Time Median (Global)") {
          tagList(
            plotOutput("global_waiting_time_above_median"),
            plotOutput("global_waiting_time_below_median")
          )
        } else if (input$tipoGrafico == "Tiempo de Espera2 (Global)") {
          plotOutput("global_scatterplot")
        }
          else {
          imageOutput("global_plot")
        }
      )
    }
  })
  
  output$global_plot <- renderImage({
    req(input$analizar)
    if (input$simulacion == "Global") {
      if (input$tipoGrafico == "Utilización de Recursos (Global)") {
        list(src = file.path(base_output_folder, "Global/utilization_plot_global.png"), 
             contentType = "image/png", 
             width = "100%")
      } else if (input$tipoGrafico == "Uso de Recursos (Global)") {
        list(
          src = file.path(base_output_folder, "Global/usage_plot_global.png"), 
          contentType = "image/png", 
          width = "100%"
        )
      } else if (input$tipoGrafico == "Tiempo de Espera (Global)") {
        list(src = file.path(base_output_folder, "Global/waiting_time_plot_global.png"), 
             contentType = "image/png", 
             width = "100%")
      } else if (input$tipoGrafico == "Tiempo en el Sistema (Global)") {
        list(src = file.path(base_output_folder, "Global/activity_time_plot_global.png"), 
             contentType = "image/png", 
             width = "100%")
      }
    }
  }, deleteFile = FALSE)
  
  output$global_activity_time_above_median <- renderPlot({
    filtered_data <- get_filtered_data()
    mediana_activity_time <- median(filtered_data$arrivals$activity_time, na.rm = TRUE)
    arrivals_above_median <- filtered_data$arrivals %>%
      filter(activity_time > mediana_activity_time)
    
    ggplot(arrivals_above_median, aes(x = activity_time)) +
      geom_histogram(binwidth = 10, fill = "blue", color = "black") +
      labs(title = "Activity Time Above Median (Global)", x = "Activity Time (min)", y = "Count") +
      theme(
        plot.title = element_text(size = 20, face = "bold"),  # Tamaño del título del gráfico
        axis.title.x = element_text(size = 16),  # Tamaño del título del eje x
        axis.title.y = element_text(size = 16),  # Tamaño del título del eje y
        axis.text.x = element_text(size = 14),   # Tamaño de los valores del eje x
        axis.text.y = element_text(size = 14)    # Tamaño de los valores del eje y
      )
  })
  
  output$global_activity_time_below_median <- renderPlot({
    filtered_data <- get_filtered_data()
    mediana_activity_time <- median(filtered_data$arrivals$activity_time, na.rm = TRUE)
    arrivals_below_median <- filtered_data$arrivals %>%
      filter(activity_time <= mediana_activity_time)
    
    ggplot(arrivals_below_median, aes(x = activity_time)) +
      geom_histogram(binwidth = 10, fill = "blue", color = "black") +
      labs(title = "Activity Time Below Median (Global)", x = "Activity Time (min)", y = "Count") +
      theme(
        plot.title = element_text(size = 20, face = "bold"),  # Tamaño del título del gráfico
        axis.title.x = element_text(size = 16),  # Tamaño del título del eje x
        axis.title.y = element_text(size = 16),  # Tamaño del título del eje y
        axis.text.x = element_text(size = 14),   # Tamaño de los valores del eje x
        axis.text.y = element_text(size = 14)    # Tamaño de los valores del eje y
      )
  })
  
  output$global_waiting_time_above_median <- renderPlot({
    filtered_data <- get_filtered_data()
    mediana_waiting_time <- median(filtered_data$arrivals$end_time - filtered_data$arrivals$start_time - filtered_data$arrivals$activity_time, na.rm = TRUE)
    arrivals_above_median <- filtered_data$arrivals %>%
      filter((end_time - start_time - activity_time) > mediana_waiting_time)
    
    ggplot(arrivals_above_median, aes(x = end_time - start_time - activity_time)) +
      geom_histogram(binwidth = 10, fill = "blue", color = "black") +
      labs(title = "Waiting Time Above Median (Global)", x = "Waiting Time (min)", y = "Count") +
      theme(
        plot.title = element_text(size = 20, face = "bold"),  # Tamaño del título del gráfico
        axis.title.x = element_text(size = 16),  # Tamaño del título del eje x
        axis.title.y = element_text(size = 16),  # Tamaño del título del eje y
        axis.text.x = element_text(size = 14),   # Tamaño de los valores del eje x
        axis.text.y = element_text(size = 14)    # Tamaño de los valores del eje y
      )
  })
  
  output$global_waiting_time_below_median <- renderPlot({
    filtered_data <- get_filtered_data()
    mediana_waiting_time <- median(filtered_data$arrivals$end_time - filtered_data$arrivals$start_time - filtered_data$arrivals$activity_time, na.rm = TRUE)
    arrivals_below_median <- filtered_data$arrivals %>%
      filter((end_time - start_time - activity_time) <= mediana_waiting_time)
    
    ggplot(arrivals_below_median, aes(x = end_time - start_time - activity_time)) +
      geom_histogram(binwidth = 10, fill = "blue", color = "black") +
      labs(title = "Waiting Time Below Median (Global)", x = "Waiting Time (min)", y = "Count") +
      theme(
        plot.title = element_text(size = 20, face = "bold"),  # Tamaño del título del gráfico
        axis.title.x = element_text(size = 16),  # Tamaño del título del eje x
        axis.title.y = element_text(size = 16),  # Tamaño del título del eje y
        axis.text.x = element_text(size = 14),   # Tamaño de los valores del eje x
        axis.text.y = element_text(size = 14)    # Tamaño de los valores del eje y
      )
  })
  
  output$global_scatterplot <- renderPlot({
    filtered_data <- get_filtered_data()
    
    ggplot(filtered_data$arrivals %>% mutate(waiting_time = end_time - start_time - activity_time), aes(x = activity_time, y = waiting_time)) +
      geom_point(alpha = 0.5) +
      labs(title = "Gráfico de Dispersión: Tiempo de Espera vs. Tiempo de Actividad (Global)", x = "Tiempo de Actividad (minutos)", y = "Tiempo de Espera (minutos)") +
      theme_minimal()
  })
  
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
