library("simmer")
library("simmer.plot")
library("gridExtra")
library("dplyr")
library("plotly")
library("stringr")
library("tidyr")

# Funciones auxiliares
necesita_mant <- function() rbinom(1, 1, 0.50) == 1 
necesita_chapa <- function() rbinom(1, 1, 0.35) == 1
necesita_pedir_piezas <- function() rbinom(1, 1, 0.35) == 1
pasa_inspeccion <- function() rbinom(1, 1, 0.75) == 1
necesita_mas_piezas <- function() rbinom(1, 1, 0.45) == 1
quiere_lavado <- function() rbinom(1, 1, 0.40) == 1
necesita_pulido <- function() rbinom(1, 1, 0.30) == 1
necesita_abolladura <- function() rbinom(1, 1, 0.35) == 1
necesita_pintura_parcial <- function() rbinom(1, 1, 0.40) == 1
necesita_pintura_completa <- function() rbinom(1, 1, 0.25) == 1
#quiere_mantenimiento<- function() rbinom(1, 1, 0.99) == 1
lavado_exterior <- function() rbinom(1, 1, 0.5) == 1
lavado_pistola <- function() rbinom(1, 1, 0.5) == 1

CHECK_VIP <- function() { 
  ifelse(get_attribute("tipo_cliente") == 2, 4,  # Cliente VIP
         ifelse(get_attribute("tipo_cliente") == 4, 3,  # Cliente ITV
                ifelse(get_attribute("tipo_cliente") == 1, 2,  # Cliente normal
                       ifelse(get_attribute("tipo_cliente") == 3, 1, 2))))  # Cliente lavado
}

CHECK_FALLO_GRAVE <- function() {
  if (get_attribute("fallo_tipo") == 2) {
    return(1)
  } else {
    return(0)
  }
}

# --------------------------
# -------VEHÍCULO-----------
# --------------------------
Vehicle <- setRefClass("Vehicle",
                       fields = list(
                         tipo = "character",
                         antiguedad = "numeric",
                         prob_fallo_grave = "numeric",
                         prob_fallo_leve = "numeric"
                       ),
                       methods = list(
                         initialize = function(tipo, antiguedad) {
                           .self$tipo <- tipo
                           .self$antiguedad <- antiguedad
                           .self$calculate_probabilities()
                         },
                         calculate_probabilities = function() {
                           stop("Debe ser implementado por subclases.")
                         }
                       )
)

# Definir subclase Coche
Coche <- Vehicle$copy()  # Utiliza copy para crear una subclase
Coche$methods(calculate_probabilities = function() {
  if (.self$antiguedad < 5) {
    .self$prob_fallo_grave <- rbinom(1, 1, 0.063)
    .self$prob_fallo_leve <- rbinom(1, 1, 0.030)
  } else if (.self$antiguedad <= 10) {
    .self$prob_fallo_grave <- rbinom(1, 1, 0.099)
    .self$prob_fallo_leve <- rbinom(1, 1, 0.050)
  } else if (.self$antiguedad <= 15) {
    .self$prob_fallo_grave <- rbinom(1, 1, 0.175)
    .self$prob_fallo_leve <- rbinom(1, 1, 0.088)
  } else {
    .self$prob_fallo_grave <- rbinom(1, 1, 0.235)
    .self$prob_fallo_leve <- rbinom(1, 1, 0.118)
  }
})

# Similarmente, definir Moto
Moto <- Vehicle$copy()
Moto$methods(calculate_probabilities = function() {
  if (.self$antiguedad < 5) {
    .self$prob_fallo_grave <- rbinom(1, 1, 0.245)
    .self$prob_fallo_leve <- rbinom(1, 1, 0.122)
  } else if (.self$antiguedad <= 10) {
    .self$prob_fallo_grave <- rbinom(1, 1, 0.272)
    .self$prob_fallo_leve <- rbinom(1, 1, 0.136)
  } else {
    .self$prob_fallo_grave <- rbinom(1, 1, 0.283)
    .self$prob_fallo_leve <- rbinom(1, 1, 0.141)
  }
})

# Definir subclase Camion
Camion <- Vehicle$copy()
Camion$methods(calculate_probabilities = function() {
  if (.self$antiguedad <= 10) {
    .self$prob_fallo_grave <- rbinom(1, 1, 0.234)
    .self$prob_fallo_leve <- rbinom(1, 1, 0.117)
  } else {
    .self$prob_fallo_grave <- rbinom(1, 1, 0.284)
    .self$prob_fallo_leve <- rbinom(1, 1, 0.142)
  }
})

# Asignar tipo de vehículo
asignar_tipo_vehiculo <- function(tipo) {
  if (tipo == "Coche") {
    return(1)
  } else if (tipo == "Camion") {
    return(2)
  } else {
    return(3)
  }
}

generate_vehicle_attributes <- function() {
  type <- sample(c("Coche", "Camion", "Moto"), 1, prob = c(0.5, 0.15, 0.35))
  age <- sample(1:20, 1)
  vehiculo <- switch(type,
                     "Coche" = Coche$new(tipo = type, antiguedad = age),
                     "Camion" = Camion$new(tipo = type, antiguedad = age),
                     "Moto" = Moto$new(tipo = type, antiguedad = age))
  vehiculo$calculate_probabilities()
  tipo <<- asignar_tipo_vehiculo(vehiculo$tipo)
  antiguedad <<- vehiculo$antiguedad
  fallo_tipo <<- ifelse(vehiculo$prob_fallo_grave == 1, 2, ifelse(vehiculo$prob_fallo_leve == 1, 1, 0))
}

# --------------------------
# -------ITV----------------
# --------------------------
trayectoria_itv <- function() {
  trajectory() %>%
    set_attribute("vehiculo_tipo", function() {
      generate_vehicle_attributes()
      return(tipo)
    }) %>%
    set_attribute("vehiculo_antiguedad", function() {
      return(antiguedad)
    }) %>%
    set_attribute("fallo_tipo", function() {
      return(fallo_tipo)
    }) %>%
    seize("ITV", 1, priority = CHECK_VIP) %>%
    log_("Inicio Revisión ITV") %>%
    set_attribute("EnITV", 1) %>%
    renege_in(
      function() { rexp(n = 1, rate = 1/40) },
      out = trajectory("Impaciente") %>%
        set_attribute("Impaciente", 1) %>%
        log_("Cliente impaciente, abandona el sistema")
    ) %>%
    timeout(function() rexp(1, rate = 1/20)) %>%
    renege_abort() %>%
    log_("Fin Revisión ITV") %>%
    set_attribute("EnITV", 0) %>%
    release("ITV") %>%
    set_attribute("costo", 75) %>%
    branch(
      function() {
        fallo_tipo == 2
      },
      continue = FALSE,
      trajectory() %>%
        log_("Fallo grave, uniendo a trayectoria principal") %>%
        join(trayectoria_principal())
    ) %>%
    log_("Fallo leve o sin fallos, cliente finaliza proceso ITV")
}

# --------------------------
# ------MANTENIMIENTO-------
# --------------------------
trayectoria_evaluacion <- function() {
  trajectory() %>%
    seize("Evaluacion", 1, priority = CHECK_VIP) %>%
    log_("INICIO Evaluacion") %>%
    set_attribute("EnEvaluacion", 1) %>%
    timeout(function() rexp(1, rate = 1/20)) %>%
    log_("FIN Evaluacion") %>%
    set_attribute("EnEvaluacion", 0) %>%
    release("Evaluacion") %>%
    set_attribute("costo", 10) %>%
    branch(
      necesita_mant,
      continue = FALSE,
      trajectory() %>%
        log_("Necesita Mantenimiento") %>%
        join(trayectoria_principal()) %>%
        branch(
          necesita_chapa,  # Si necesita chapa después del mantenimiento
          continue = FALSE,
          trajectory() %>%
            log_("Procede a Chapa y Pintura después del Mantenimiento") %>%
            join(trayectoria_chapa_y_pint()) %>%
            branch(
              quiere_lavado,  # Decide si quiere lavado después de chapa y pintura
              continue = TRUE,
              trajectory() %>%
                log_("Opta por Lavado después de Chapa y Pintura") %>%
                join(trayectoria_lavado())
            )
        ) %>%
        branch(
          quiere_lavado,  # Si quiere lavado directamente después del mantenimiento
          continue = TRUE,
          trajectory() %>%
            log_("Opta por Lavado después del Mantenimiento") %>%
            join(trayectoria_lavado())
        )
    ) %>%
    log_("No necesita Mantenimiento, procede a Chapa y Pintura") %>%
    join(trayectoria_chapa_y_pint()) %>%
    branch(
      quiere_lavado,
      continue = TRUE,
      trajectory() %>%
        log_("Opta por Lavado después de Chapa y Pintura") %>%
        join(trayectoria_lavado())
    ) %>%
    log_("SALE DEL SISTEMA")
}

trayectoria_espera_piezas <- function() {
  trajectory() %>%
    branch(
      necesita_pedir_piezas, 
      continue = FALSE,
      trajectory() %>%
        log_("INICIO EsperaPiezas") %>%
        log_("No está la pieza en el taller, necesitan ser pedidas") %>%
        seize("EsperaPiezas", 1, priority = CHECK_VIP) %>%
        set_attribute("EnEsperaPiezas", 1) %>%
        timeout(function() rexp(1, rate = 1/120)) %>%  # Mayor tiempo de espera por envío
        set_attribute("costo", 120) %>%  # Costo adicional por envío
        log_("FIN Espera Piezas con pedido") %>%
        set_attribute("EnEsperaPiezas", 0) %>%
        release("EsperaPiezas")
    ) %>%
    log_("INICIO EsperaPiezas") %>%
    log_("Piezas disponibles en el taller.") %>%
    seize("EsperaPiezas", 1, priority = CHECK_VIP) %>%
    set_attribute("EnEsperaPiezas", 1) %>%
    timeout(function() rexp(1, rate = 1/10)) %>%  # Menor tiempo de espera
    set_attribute("costo", 80) %>%  # Costo reducido sin envío
    log_("FIN Espera Piezas sin pedido") %>%
    set_attribute("EnEsperaPiezas", 0) %>%
    release("EsperaPiezas")
}


trayectoria_reparacion <- function() {
  trajectory() %>%
    seize("Reparacion", 1, priority = CHECK_VIP) %>%  ## Prioridad si espero por piezas
    log_("INICIO Reparacion") %>%
    set_attribute("EnReparacion", 1) %>%
    timeout(function() rexp(1, rate = 1/60)) %>%  ## 60 minutos
    log_("FIN Reparacion") %>%
    set_attribute("EnReparacion", 0) %>%
    release("Reparacion") %>%
    set_attribute("costo", 180)  ## 180 EUR
}

trayectoria_inspeccion <- function() {
  trajectory() %>%
    seize("Inspeccion", 1, priority = CHECK_VIP) %>%
    log_("INICIO Inspeccion") %>%
    set_attribute("EnInspeccion", 1) %>%
    timeout(function() rexp(1, rate = 1/15)) %>%  ## 15 minutos
    release("Inspeccion") %>%
    set_attribute("costo", 20) %>% ## 20 EUR
    
    branch( function() !pasa_inspeccion(),
            continue = TRUE,
            trajectory() %>%
              branch( necesita_mas_piezas,
                      continue = FALSE,
                      trajectory() %>%
                        log_("FIN Inspeccion --> necesita más piezas") %>%
                        set_attribute("EnInspeccion", 0) %>%
                        join(trayectoria_espera_piezas()) %>%
                        join(trayectoria_reparacion()) %>%
                        log_("FIN Inspeccion --> Inspección Aprobada") %>%
                        log_("FIN Mantenimiento") %>%
                        set_attribute("EnInspeccion", 0)
              ) %>%
              log_("FIN Inspeccion --> necesita reparación adicional") %>%
              set_attribute("EnInspeccion", 0) %>%
              join(trayectoria_reparacion())
    ) %>%
    log_("FIN Inspeccion --> Inspección Aprobada") %>%
    log_("FIN Mantenimiento") %>%
    set_attribute("EnInspeccion", 0)
}

# --------------------------
# -----CHAPA Y PINTURA -----
# --------------------------
# Trayectoria principal de chapa y pintura
trayectoria_chapa_y_pint <- function() {
  trajectory("Proceso Chapa y Pintura") %>%
    log_("INICIO Chapa y Pintura") %>%
    set_attribute("EnChapaPintura", 1) %>%
    branch(
      necesita_pintura_parcial,
      continue = TRUE,
      trajectory() %>%
        join(trayectoria_pintura_parcial())
    ) %>%
    branch(
      necesita_pintura_completa,
      continue = TRUE,
      trajectory() %>%
        join(trayectoria_pintura_completa())
    ) %>%
    branch(
      necesita_abolladura,
      continue = TRUE,
      trajectory() %>%
        join(trayectoria_abolladura())
    ) %>%
    branch(
      necesita_pulido,
      continue = TRUE,
      trajectory() %>%
        join(trayectoria_pulido())
    ) %>%
    log_("FIN Chapa y Pintura") %>%
    set_attribute("EnChapaPintura", 0)
}

# Trayectorias específicas para cada servicio dentro de chapa y pintura
trayectoria_pintura_parcial <- function() {
  trajectory("Pintura Parcial") %>%
    seize("PinturaPar", 1, priority = CHECK_VIP) %>%
    log_("INICIO Pintura Parcial") %>%
    set_attribute("EnPinturaPar", 1) %>%
    set_attribute("pintoParcialmente", 1) %>%
    timeout(function() rexp(1, rate = 1/30)) %>%
    log_("FIN Pintura Parcial") %>%
    set_attribute("EnPinturaPar", 0) %>%
    release("PinturaPar") %>%
    set_attribute("costo", 200)
}

trayectoria_pintura_completa <- function() {
  trajectory("Pintura Completa") %>%
    seize("PinturaComp", 1, priority = CHECK_VIP) %>%
    log_("INICIO Pintura Completa") %>%
    set_attribute("EnPinturaComp", 1) %>%
    timeout(function() rexp(1, rate = 1/90)) %>%
    log_("FIN Pintura Completa") %>%
    set_attribute("EnPinturaComp", 0) %>%
    release("PinturaComp") %>%
    set_attribute("costo", 400)
}

trayectoria_abolladura <- function() {
  trajectory("Reparación de Abolladuras") %>%
    seize("Abolladura", 1, priority = CHECK_VIP) %>%
    log_("INICIO Abolladura") %>%
    set_attribute("EnAbolladura", 1) %>%
    timeout(function() rexp(1, rate = 1/35)) %>%
    log_("FIN Abolladura") %>%
    set_attribute("EnAbolladura", 0) %>%
    release("Abolladura") %>%
    set_attribute("costo", 250)
}

trayectoria_pulido <- function() {
  trajectory("Pulido") %>%
    seize("Pulido", 1, priority = CHECK_VIP) %>%
    log_("INICIO Pulido") %>%
    set_attribute("EnPulido", 1) %>%
    timeout(function() rexp(1, rate = 1/25)) %>%
    log_("FIN Pulido") %>%
    set_attribute("EnPulido", 0) %>%
    release("Pulido") %>%
    set_attribute("costo", 110)
}


# --------------------------
# ---------LAVADO-----------
# --------------------------
trayectoria_lavado <- function() {
  trajectory() %>%
    seize("Lavado", 1, priority = CHECK_VIP) %>%
    log_("INICIO Lavado") %>%
    set_attribute("EnLavado", 1) %>%
    branch(
      lavado_exterior,
      continue = FALSE,
      trajectory() %>%
        log_("Lavado Exterior seleccionado") %>%
        branch(
          lavado_pistola,
          continue = FALSE,
          trajectory() %>%
            log_("Lavado con Pistola seleccionado") %>%
            timeout(function() rexp(1, rate = 1/20)) %>%  # Tiempo para lavado con pistola
            set_attribute("costo", 20) %>%
            log_("FIN Lavado") %>%
            set_attribute("EnLavado", 0) %>%
            release("Lavado") %>%
            log_("SALE DEL SISTEMA")
        ) %>%
            log_("Túnel de Lavado seleccionado") %>%
            timeout(function() rexp(1, rate = 1/10)) %>%  # Tiempo para túnel de lavado
            set_attribute("costo", 15) %>%
            log_("FIN Lavado") %>%
            set_attribute("EnLavado", 0) %>%
            release("Lavado") %>%
            log_("SALE DEL SISTEMA")
    ) %>%
        log_("Lavado Completo seleccionado") %>%
        timeout(function() rexp(1, rate = 1/60)) %>%  # Tiempo para lavado completo
        set_attribute("costo", 50) %>%
        log_("FIN Lavado") %>%
        set_attribute("EnLavado", 0) %>%
        release("Lavado") %>%
        log_("SALE DEL SISTEMA")
}

# ----------------------------------
# -----TRAYECTORIA PRINCIPAL--------
# ----------------------------------
# # Trayectoria principal que une todas las partes
trayectoria_principal <- function() {
  trajectory("Proceso Principal") %>%
    join(trayectoria_espera_piezas()) %>%
    join(trayectoria_reparacion()) %>%
    join(trayectoria_inspeccion())
}

trayectoria_vip <- function() {
  trajectory() %>%
    set_attribute("costo", 50) %>%  # Costo inicial para VIPs
    join(trayectoria_evaluacion())
}

# ----------------------------------
# ---------REPETICIONES-------------
# ----------------------------------
base_output_folder <- "D:/UNIVERSIDAD/TFG/Codigo/Prototipo2"

# Función para correr una simulación
correr_simulacion <- function(replica_id, tiempo) {
  sim_folder <- file.path(base_output_folder, sprintf("Simulacion_%03d", replica_id))
  
  if (!dir.exists(sim_folder)) {
    dir.create(sim_folder, recursive = TRUE)
  }
  
  # Establecer una semilla única para cada réplica
  set.seed(replica_id)
  
  # Crear el archivo de log
  log_file <- file.path(sim_folder, sprintf("TRAZAS_sim_%03d.txt", replica_id))
  sink(log_file)
  on.exit(sink(), add = TRUE)
  
  # Creación del entorno de simulación
  env <- simmer("Taller de Vehiculos")
  recursos_list <- c("Evaluacion", "Reparacion", "EsperaPiezas", "Inspeccion", "Lavado", "PinturaPar", "PinturaComp", "Abolladura", "Pulido", "ITV")
  
  capturar_datos <- function(env) {
    capacity <- sapply(recursos_list, function(res) get_capacity(env, res))
    utilization <- sapply(recursos_list, function(res) get_server_count(env, res) / get_capacity(env, res))
    queue_lengths <- sapply(recursos_list, function(res) mean(get_mon_resources(env) %>% filter(resource == res) %>% pull(queue)))
    service_times <- sapply(recursos_list, function(res) mean(get_mon_resources(env) %>% filter(resource == res) %>% pull(time)))
    total_usage_times <- sapply(recursos_list, function(res) sum(get_mon_resources(env) %>% filter(resource == res) %>% pull(time)))
    num_served <- sapply(recursos_list, function(res) nrow(get_mon_resources(env) %>% filter(resource == res, server > 0)))
    
    arrivals <- get_mon_arrivals(env)
    waiting_times <- arrivals %>%
      mutate(
        time_in_system = end_time - start_time,
        waiting_time = pmax(0, end_time - start_time - activity_time)
      ) %>%
      group_by(name) %>%
      summarise(
        mean_waiting_time = mean(waiting_time, na.rm = TRUE),
        mean_time_in_system = mean(time_in_system, na.rm = TRUE)
      )
    
    list(
      capacity = capacity,
      utilization = utilization,
      queue_lengths = queue_lengths,
      service_times = service_times,
      total_usage_times = total_usage_times,
      num_served = num_served,
      waiting_times = waiting_times
    )
  }
  
  for (recurso in recursos_list) {
    env <- env %>% add_resource(recurso, 2, queue_size = Inf)
  }
  
  env <- env %>%
    add_generator("cliente", 
                  trajectory() %>% 
                    set_attribute("tipo_cliente", 1) %>% 
                    join(trayectoria_evaluacion()), 
                  function() rexp(n = 1, rate = 1/25), priority = 2, mon = 2) %>%
    add_generator("cliente VIP", 
                  trajectory() %>% 
                    set_attribute("tipo_cliente", 2) %>% 
                    join(trayectoria_vip()), 
                  function() rexp(n = 1, rate = 1/40), priority = 4, mon = 2) %>%
    add_generator("cliente Lavado", 
                  trajectory() %>% 
                    set_attribute("tipo_cliente", 3) %>% 
                    join(trayectoria_lavado()), 
                  function() rexp(n = 1, rate = 1/60), priority = 1, mon = 2) %>%
    add_generator("cliente ITV", 
                  trajectory() %>% 
                    set_attribute("tipo_cliente", 4) %>% 
                    join(trayectoria_itv()), 
                  function() rexp(n = 1, rate = 1/60), priority = 3, mon = 2)
  
  # Correr la simulación
  env %>% run(until = tiempo)
  
  # Guardar resultados
  recursos <- get_mon_resources(env)
  arrivals <- get_mon_arrivals(env)
  atributos <- get_mon_attributes(env)
  
  costos_por_cliente <- atributos %>%
    filter(key == "costo") %>%
    group_by(name) %>%
    summarise(costo_total = sum(as.numeric(value)))
  
  calcular_costo_promedio <- function(tipo_cliente) {
    total_cost <- sum(costos_por_cliente %>% filter(grepl(tipo_cliente, name)) %>% pull(costo_total))
    total_arrivals <- nrow(arrivals %>% filter(grepl(tipo_cliente, name)))
    if (total_arrivals > 0) {
      total_cost / total_arrivals
    } else {
      NA
    }
  }
  
  costos_promedio <- data.frame(
    tipo_cliente = c("Cliente", "Cliente VIP", "Cliente Lavado", "Cliente ITV"),
    costo_promedio = c(
      calcular_costo_promedio("cliente"),
      calcular_costo_promedio("cliente VIP"),
      calcular_costo_promedio("cliente Lavado"),
      calcular_costo_promedio("cliente ITV")
    )
  )
  
  # Obtener información adicional
  datos_adicionales <- capturar_datos(env)
  
  # Crear un dataframe para la información adicional
  info_adicional <- data.frame(
    recurso = recursos_list,
    capacidad = datos_adicionales$capacity,
    utilizacion = datos_adicionales$utilization,
    longitud_media_cola = datos_adicionales$queue_lengths,
    tiempo_medio_servicio = datos_adicionales$service_times,
    tiempo_total_uso = datos_adicionales$total_usage_times,
    num_served = datos_adicionales$num_served
  )
  
  # Guardar en archivos CSV
  write.csv(costos_por_cliente, file.path(sim_folder, "facturas.csv"), row.names = FALSE)
  write.csv(arrivals, file.path(sim_folder, "arrivals.csv"), row.names = FALSE)
  write.csv(recursos, file.path(sim_folder, "recursos.csv"), row.names = FALSE)
  write.csv(atributos, file.path(sim_folder, "atributos.csv"), row.names = FALSE)
  write.csv(info_adicional, file.path(sim_folder, "info_adicional.csv"), row.names = FALSE)
  write.csv(datos_adicionales$waiting_times, file.path(sim_folder, "tiempos_espera.csv"), row.names = FALSE)
  write.csv(costos_promedio, file.path(sim_folder, "costos_promedio.csv"), row.names = FALSE)
  
  itv_clients <- atributos %>%
    filter(grepl("cliente ITV", name)) %>%
    filter(key %in% c("vehiculo_tipo", "vehiculo_antiguedad", "fallo_tipo")) %>%
    pivot_wider(names_from = key, values_from = value) %>%
    filter(!is.na(vehiculo_tipo) & !is.na(vehiculo_antiguedad) & !is.na(fallo_tipo)) %>%  # Filtra solo las filas con todos los atributos
    mutate(
      vehiculo_tipo = recode(vehiculo_tipo, `1` = "Coche", `2` = "Camion", `3` = "Moto"),
      fallo_tipo = recode(fallo_tipo, `2` = "grave", `1` = "leve", `0` = "sin fallos")
    )
  
  # Guardar en archivo CSV
  write.csv(itv_clients, file.path(sim_folder, "itv_clients.csv"), row.names = FALSE)
  
  return(env)
}

# Ejecutar las réplicas
numero_de_replicas <- 10
tiempo_de_simulacion <- 30 * 12 * 60  # 30 días en minutos

reps <- lapply(1:numero_de_replicas, function(i) {
  correr_simulacion(i, tiempo_de_simulacion)
})

all_recursos <- get_mon_resources(reps)
all_arrivals <- get_mon_arrivals(reps)
all_atributos <- get_mon_attributes(reps)

# Guardar gráficos y resultados globales
global_output_folder <- file.path(base_output_folder, "Global")
if (!dir.exists(global_output_folder)) {
  dir.create(global_output_folder, recursive = TRUE)
}

# Supongamos que 'p5' es un objeto ggplot
p5 <- plot(all_recursos, metric = "utilization", c("Evaluacion", "Reparacion", "EsperaPiezas", "Inspeccion", "Lavado", "PinturaPar", "PinturaComp", "Abolladura", "Pulido", "ITV"))

# Convertir el gráfico a ggplot y ajustar el ancho de las barras (si es necesario)
p5 <- p5 + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),  # Rotar las etiquetas del eje x
        axis.title.x = element_text(size = 14),  # Aumentar tamaño del título del eje x
        axis.title.y = element_text(size = 14),  # Aumentar tamaño del título del eje y
        plot.title = element_text(size = 16, hjust = 0.5))  # Aumentar tamaño del título del gráfico

# Ajustar el tamaño de la ventana del gráfico
file_path_p5 <- file.path(global_output_folder, "utilization_plot_global.png")
if (file.exists(file_path_p5)) file.remove(file_path_p5)

# Ajustar el tamaño de la imagen guardada
ggsave(file_path_p5, plot = p5) 

p6 <- plot(all_recursos, metric = "usage", c("Evaluacion", "Reparacion", "EsperaPiezas"), items = c("queue", "server"))
file_path_p6 <- file.path(global_output_folder, "usage_plot_global.png")
if (file.exists(file_path_p6)) file.remove(file_path_p6)
ggsave(file_path_p6, plot = p6)

p7 <- plot(all_recursos, metric = "usage", c("Inspeccion", "Lavado", "PinturaPar"), items = c("queue", "server"))
file_path_p7 <- file.path(global_output_folder, "usage_plot_global2.png")
if (file.exists(file_path_p7)) file.remove(file_path_p7)
ggsave(file_path_p7, plot = p7)

p8 <- plot(all_recursos, metric = "usage", c("PinturaComp", "Abolladura", "Pulido"), items = c("queue", "server"))
file_path_p8 <- file.path(global_output_folder, "usage_plot_global3.png")
if (file.exists(file_path_p8)) file.remove(file_path_p8)
ggsave(file_path_p8, plot = p8)

p9 <- plot(all_recursos, metric = "usage", c("ITV"), items = c("queue", "server"))
file_path_p8 <- file.path(global_output_folder, "usage_plot_global4.png")
if (file.exists(file_path_p8)) file.remove(file_path_p8)
ggsave(file_path_p8, plot = p8)

p10 <- plot(all_arrivals, metric = "waiting_time")
file_path_p9 <- file.path(global_output_folder, "waiting_time_plot_global.png")
if (file.exists(file_path_p9)) file.remove(file_path_p9)
ggsave(file_path_p9, plot = p9)

p11 <- plot(all_arrivals, metric = "activity_time")
file_path_p10 <- file.path(global_output_folder, "activity_time_plot_global.png")
if (file.exists(file_path_p10)) file.remove(file_path_p10)
ggsave(file_path_p10, plot = p10)

write.csv(all_recursos, file.path(global_output_folder, "recursos_global.csv"), row.names = FALSE)
write.csv(all_arrivals, file.path(global_output_folder, "arrivals_global.csv"), row.names = FALSE)
write.csv(all_atributos, file.path(global_output_folder, "atributos_global.csv"), row.names = FALSE)
