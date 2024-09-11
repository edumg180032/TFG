library("simmer")
library("simmer.plot")
library("gridExtra")
library("dplyr")
library("plotly")
library(stringr)

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

#CHECK_PRIORIDAD <- function() {   ifelse(get_attribute("esperoPorPiezas") == 1, 1, 2) } #CHECKEO si el cliente esperoporPiezas tiene prioridad
CHECK_VIP <- function() {
  tipo_cliente <- get_attribute("tipo_cliente")
  if (tipo_cliente == 3) {
    return(1)  # Prioridad 1 para Cliente Lavado
  } else if (tipo_cliente == 2) {
    return(3)  # Prioridad 3 para Cliente VIP
  } else {
    return(2)  # Prioridad 2 para Cliente Normal
  }
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
    timeout(function() rexp(1, rate = 1/15)) %>%  ## 10 minutos
    log_("FIN Lavado") %>%
    set_attribute("EnLavado", 0) %>%
    release("Lavado") %>%
    set_attribute("costo", 30)  %>% ## 30 EUR
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
base_output_folder <- "D:/UNIVERSIDAD/TFG/Codigo/Prototipo1"

# Función para correr una simulación
correr_simulacion <- function(replica_id, tiempo) {
  sim_folder <- file.path(base_output_folder, sprintf("Simulacion_%03d", replica_id))
  
  if (!dir.exists(sim_folder)) {
    dir.create(sim_folder)
  }
  
  # Establecer una semilla única para cada réplica
  set.seed(replica_id)
  
  # Crear el archivo de log
  log_file <- file.path(sim_folder, sprintf("TRAZAS_sim_%03d.txt", replica_id))
  sink(log_file)
  on.exit(sink(), add = TRUE)
  
  # Creación del entorno de simulación
  env <- simmer("Taller de Vehiculos")
  recursos_list <- c("Evaluacion", "Reparacion", "EsperaPiezas", "Inspeccion", "Lavado", "PinturaPar", "PinturaComp", "Abolladura", "Pulido")
  
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
                  function() rexp(n = 1, rate = 1/40), priority = 3, mon = 2) %>%
    add_generator("cliente Lavado", 
                  trajectory() %>% 
                    set_attribute("tipo_cliente", 3) %>% 
                    join(trayectoria_lavado()), 
                  function() rexp(n = 1, rate = 1/60), priority = 1, mon = 2)
  
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
    tipo_cliente = c("Cliente", "Cliente VIP", "Cliente Lavado"),
    costo_promedio = c(
      calcular_costo_promedio("cliente"),
      calcular_costo_promedio("cliente VIP"),
      calcular_costo_promedio("cliente Lavado")
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
  
  # p1 <- plot(recursos, metric = "utilization", c("Evaluacion", "Reparacion", "EsperaPiezas", "Inspeccion"))
  # ggsave(file.path(sim_folder, "utilization_plot.png"), plot = p1)
  # 
  # p2 <- plot(recursos, metric = "usage", c("Evaluacion", "Reparacion", "EsperaPiezas", "Inspeccion"), items = c("queue", "server"))
  # ggsave(file.path(sim_folder, "usage_plot.png"), plot = p2)
  # 
  # p3 <- plot(arrivals, metric = "waiting_time")
  # ggsave(file.path(sim_folder, "waiting_time_plot.png"), plot = p3)
  # 
  # p4 <- plot(arrivals, metric = "activity_time")
  # ggsave(file.path(sim_folder, "activity_time_plot.png"), plot = p4)
  
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


p5 <- plot(all_recursos, metric = "utilization", c("Evaluacion", "Reparacion", "EsperaPiezas", "Inspeccion", "Lavado", "PinturaPar", "PinturaComp", "Abolladura", "Pulido"))
file_path_p5 <- file.path(base_output_folder, "Global/utilization_plot_global.png")
if (file.exists(file_path_p5)) file.remove(file_path_p5)
ggsave(file_path_p5, plot = p5)

width <- 16  # ancho en pulgadas
height <- 6  # altura en pulgadas

p6 <- plot(all_recursos, metric = "usage", c("Evaluacion", "Reparacion", "EsperaPiezas"), items = c("queue", "server"))
file_path_p6 <- file.path(base_output_folder, "Global/usage_plot_global.png")
if (file.exists(file_path_p6)) file.remove(file_path_p6)
ggsave(file_path_p6, plot = p6, width = width, height = height)

p7 <- plot(all_recursos, metric = "usage", c("Inspeccion", "Lavado", "PinturaPar"), items = c("queue", "server"))
file_path_p7 <- file.path(base_output_folder, "Global/usage_plot_global2.png")
if (file.exists(file_path_p7)) file.remove(file_path_p7)
ggsave(file_path_p7, plot = p7,  width = width, height = height)

p8 <- plot(all_recursos, metric = "usage", c("PinturaComp", "Abolladura", "Pulido"), items = c("queue", "server"))
file_path_p8 <- file.path(base_output_folder, "Global/usage_plot_global3.png")
if (file.exists(file_path_p8)) file.remove(file_path_p8)
ggsave(file_path_p8, plot = p8,  width = width, height = height)

p9 <- plot(all_arrivals, metric = "waiting_time")
file_path_p9 <- file.path(base_output_folder, "Global/waiting_time_plot_global.png")
if (file.exists(file_path_p9)) file.remove(file_path_p9)
ggsave(file_path_p9, plot = p9)

p10 <- plot(all_arrivals, metric = "activity_time")
file_path_p10 <- file.path(base_output_folder, "Global/activity_time_plot_global.png")
if (file.exists(file_path_p10)) file.remove(file_path_p10)
ggsave(file_path_p10, plot = p10)

write.csv(all_recursos, file.path(base_output_folder, "Global/recursos_global.csv"), row.names = FALSE)
write.csv(all_arrivals, file.path(base_output_folder, "Global/arrivals_global.csv"), row.names = FALSE)
write.csv(all_atributos, file.path(base_output_folder, "Global/atributos_global.csv"), row.names = FALSE)