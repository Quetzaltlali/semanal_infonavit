# ======================================================
#  LIBRERÍAS
# ======================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr2,readxl,tidyverse,janitor,lubridate)

# ======================================================
# 1) URL Y RUTA DE GUARDADO (NOMBRE DINÁMICO)
# ======================================================
url <- "https://bcrdgdcprod.blob.core.windows.net/documents/entorno-internacional/documents/Serie_Historica_Spread_del_EMBI.xlsx"

fecha_descarga <- Sys.Date()

nombre_archivo <- paste0(
  "Serie_Historica_Spread_del_EMBI_",
  fecha_descarga,
  ".xlsx"
)

#ruta_carpeta <- "C:/Users/quetz/Documentos"
ruta_carpeta <- "C:/Users/Infonavit/Documents/semanal/embi"

if (!dir.exists(ruta_carpeta)) {
  dir.create(ruta_carpeta, recursive = TRUE)
}

ruta_archivo <- file.path(ruta_carpeta, nombre_archivo)

# ======================================================
# 2) DESCARGA DEL ARCHIVO
# ======================================================
request(url) |>
  req_perform(path = ruta_archivo)

message("Archivo descargado correctamente")

# ======================================================
# 3) LECTURA DEL EXCEL
# ======================================================
embi <- read_excel(ruta_archivo)

# ======================================================
# 4) LIMPIEZA Y ESCALADO (EMBI MÉXICO EN PB)
# ======================================================
embi <- embi %>%
  row_to_names(row_number = 1) %>%
  select(
    Fecha,
    México
  ) %>%
  mutate(
    Fecha = as.character(Fecha),
    
    # Serial Excel → Date
    Fecha = suppressWarnings(
      as.Date(as.numeric(Fecha), origin = "1899-12-30")
    ),
    
    # Texto → Date
    Fecha = suppressWarnings(
      if_else(
        is.na(Fecha),
        dmy(Fecha),
        Fecha
      )
    )
  ) %>%
  filter(!is.na(Fecha)) %>%
  rename(Mexico = México) %>%
  mutate(
    # CONVERSIÓN OFICIAL A PUNTOS BASE
    Mexico = round(as.numeric(Mexico) * 100, 1)
  ) %>%
  arrange(Fecha)

# ======================================================
# 5) FUNCIONES AUXILIARES DE FECHA
# ======================================================
# Viernes previo disponible
get_prev_friday <- function(fecha, fechas_disponibles) {
  fechas_disponibles %>%
    keep(~ wday(.x) == 6 & .x < fecha) %>%
    max()
}

# Mismo día hábil del año anterior
get_same_day_last_year <- function(fecha, fechas_disponibles) {
  target <- fecha %m-% years(1)
  fechas_disponibles[fechas_disponibles <= target] %>% max()
}

# Último jueves disponible (corte oficial riesgo país)
get_last_thursday <- function(fechas_disponibles) {
  fechas_disponibles %>%
    keep(~ wday(.x) == 5) %>%   # jueves = 5
    max()
}

# ======================================================
# 6) FUNCIÓN PRINCIPAL DE MÉTRICAS (RIESGO PAÍS)
# ======================================================
calcular_metricas_embi <- function(data, columna, frecuencia = "semanal", verbose = TRUE) {
  
  data <- data %>% arrange(Fecha)
  fechas_disponibles <- data$Fecha
  
  # ===============================
  # FECHA ACTUAL
  # ===============================
  fecha_actual <- max(fechas_disponibles)
  
  actual <- data %>%
    filter(Fecha == fecha_actual) %>%
    pull({{ columna }})
  
  # ===============================
  # REGISTRO ANTERIOR
  # ===============================
  fecha_anterior <- fechas_disponibles %>%
    sort() %>%
    tail(2) %>%
    .[1]
  
  anterior <- data %>%
    filter(Fecha == fecha_anterior) %>%
    pull({{ columna }})
  
  # ===============================
  # VARIACIÓN ANUAL
  # ===============================
  fecha_anual <- fechas_disponibles[
    fechas_disponibles <= (fecha_actual %m-% years(1))
  ] %>% max()
  
  valor_anual <- data %>%
    filter(Fecha == fecha_anual) %>%
    pull({{ columna }})
  
  # ===============================
  # DEBUG OPCIONAL
  # ===============================
  if (verbose) {
    message("Fecha actual: ", fecha_actual, " | Valor: ", actual)
    message("Fecha anual usada: ", fecha_anual, " | Valor anual: ", valor_anual)
  }
  
  # ===============================
  # RESULTADO
  # ===============================
  list(
    actual        = actual,
    anterior      = anterior,
    var_ultimo    = round(actual - anterior, 1),
    var_anual     = round(actual - valor_anual, 1),
    fecha_ult     = fecha_actual,
    fecha_anual   = fecha_anual,
    valor_anual   = valor_anual,
    frecuencia    = frecuencia
  )
}

# ======================================================
# 7) EJECUCIÓN – EMBI MÉXICO (RIESGO PAÍS)
# ======================================================
r_riesgo_pais <- calcular_metricas_embi(
  data = embi,
  columna = Mexico,
  frecuencia = "semanal"
)


