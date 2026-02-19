# ==================================================
# PAQUETES
# ==================================================
# PAQUETES
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, dplyr, janitor, httr)

options(timeout = 1500) #Timeout para las descargas completas :)
# ==================================================
# 1) DIRECTORIO BASE IMSS
# ==================================================
dir_base <- "C:/Users/Infonavit/Documents/semanal/imss"
if (!dir.exists(dir_base)) dir.create(dir_base, recursive = TRUE)

get_dir_anio <- function(fecha, dir_base) {
  dir_anio <- file.path(dir_base, format(as.Date(fecha), "%Y"))
  if (!dir.exists(dir_anio)) dir.create(dir_anio)
  dir_anio
}

base_url <- "http://datos.imss.gob.mx/sites/default/files/"
# ==================================================
# 2) FUNCIONES DE FECHAS
# ==================================================
ultimo_dia_mes <- function(fecha) ceiling_date(as.Date(fecha), "month") - days(1)

# ==================================================
# 3) DETECTAR ARCHIVO MÁS RECIENTE DISPONIBLE
# ==================================================
fecha_detectada <- NULL
for (i in 0:36) {
  fecha_fin <- ultimo_dia_mes(Sys.Date() %m-% months(i))
  url <- paste0(base_url, "asg-", format(fecha_fin, "%Y-%m-%d"), ".csv")
  
  if (tryCatch(httr::HEAD(url)$status_code == 200, error = function(e) FALSE)) {
    fecha_detectada <- fecha_fin
    break
  }
}

if (is.null(fecha_detectada)) stop("❌ No se encontró ningún archivo IMSS válido")

# Fechas clave
fechas <- list(
  mes_actual        = fecha_detectada,
  mes_anterior      = ultimo_dia_mes(fecha_detectada %m-% months(1)),
  anio_anterior     = ultimo_dia_mes(fecha_detectada %m-% years(1)),
  mes_anterior_anio = ultimo_dia_mes(fecha_detectada %m-% years(1) %m-% months(1))
)

# ==================================================
# 4) DESCARGA Y LECTURA DE ARCHIVOS
# ==================================================
descargar_y_leer <- function(fecha) {
  nombre <- paste0("asg-", format(fecha, "%Y-%m-%d"), ".csv")
  ruta <- file.path(get_dir_anio(fecha, dir_base), nombre)
  
  if (!file.exists(ruta)) {
    r <- httr::GET(paste0(base_url, nombre),
                   httr::write_disk(ruta, overwrite = TRUE),
                   httr::timeout(600))
    httr::stop_for_status(r)
    
    if ((file.info(ruta)$size / 1024^2) < 50) stop("❌ Descarga incompleta: ", nombre)
    
    message("⬇ Descargado: ", nombre)
  } else {
    message("✔ Archivo existente: ", nombre)
  }
  
  fread(ruta, sep = "|", encoding = "Latin-1") |> clean_names()
}

dfs <- lapply(fechas, descargar_y_leer)

# ==================================================
# 5) VALIDACIONES
# ==================================================
stopifnot(sapply(dfs, nrow) > 1e6)

# ==================================================
# 6) CALCULO SALARIO PROMEDIO
# ==================================================
calc_salario <- function(df) {
  df <- df %>% filter(ta_sal > 0, masa_sal_ta > 0)
  sum(df$masa_sal_ta, na.rm = TRUE) / sum(df$ta_sal, na.rm = TRUE)
}

sal_actual      <- calc_salario(dfs$mes_actual)
sal_mes_anterior <- calc_salario(dfs$mes_anterior)
sal_anio_pasado <- calc_salario(dfs$anio_anterior)

# ==================================================
# 7) VARIACIONES
# ==================================================
variacion_mensual <- (sal_actual / sal_mes_anterior - 1) * 100
variacion_anual   <- (sal_actual / sal_anio_pasado - 1) * 100

# ==================================================
# 8) RESULTADO SALARIOS
# ==================================================
r_salarios <- list(
  actual      = round(sal_actual, 1),
  anterior    = round(sal_mes_anterior, 1),
  var_ultimo  = round(variacion_mensual, 1),
#  anio_pasado = round(sal_anio_pasado, 1),
  var_anual   = round(variacion_anual, 1),
  fecha_ult   = fechas$mes_actual,
  frecuencia  = "Mensual"
)

r_salarios