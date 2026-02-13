# ======================================================
# PAQUETES
# ======================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  lubridate,
  zoo,
  Rblpapi,
  tibble,
  purrr
)
# ======================================================
# CONEXIÓN BLOOMBERG
# ======================================================
con <- blpConnect()
# ======================================================
# FECHAS DINÁMICAS (usar último dato REAL disponible)
# ======================================================
today <- Sys.Date()
end_date   <- today
start_date <- today %m-% months(18)
# ======================================================
# TICKERS
# ======================================================
tickers_bb <- c(
  # FX
  "USDMXN CURNCY","USDEUR CURNCY","USDCNY CURNCY","USDBRL CURNCY",
  
  # Commodities
  "CRAMMMIX Index","CL1 Comdty",
  
  # Equity
  "MEXBOL Index","INDU Index","SPX Index","CEMEXCPO MF Equity",
  
  # Tasas
  "GCETAA28 Index","GTMXN10Y Govt","MB30 Index",
  
  # Volatilidad
  "VIX Index",
  
  # Macro
  "BCOMIN Index","MXPRTLCR Index","MXSAASTL Index",
  "MXSATOTL Index","IMBWTOT Index")
# ======================================================
# DESCARGA HISTÓRICA ÚNICA
# ======================================================
data_raw <- bdh(
  securities = tickers_bb,
  fields     = "PX_LAST",
  start.date = start_date,
  end.date   = end_date
)
# ======================================================
# FORMATO PANEL
# ======================================================
data_long <- data_raw %>%
  imap(~ mutate(.x, ticker = .y)) %>%
  bind_rows() %>%
  transmute(
    date,
    ticker,
    valor = as.numeric(PX_LAST)
  )
# ======================================================
# FUNCIÓN MÉTRICAS POR TICKER
# ======================================================
resumen_por_ticker <- function(df) {
  
  df <- as_tibble(df) %>% arrange(date)
  
  # si no hay datos
  if (nrow(df) == 0) {
    return(tibble(
      actual = NA_real_,
      anterior = NA_real_,
      anual = NA_real_,
      var_ultimo = NA_real_,
      var_anual = NA_real_,
      fecha_actual = as.Date(NA),
      fecha_ant = as.Date(NA),
      fecha_anual = as.Date(NA),
      frecuencia = "diaria"
    ))
  }
  # ================= actual
  actual_row   <- slice_tail(df, n = 1)
  actual_val   <- pull(actual_row, valor)
  actual_fecha <- pull(actual_row, date)
  
  # ================= anterior
  if (nrow(df) >= 2) {
    anterior_row   <- slice_tail(df, n = 2) %>% slice_head(n = 1)
    anterior_val   <- pull(anterior_row, valor)
    anterior_fecha <- pull(anterior_row, date)
  } else {
    anterior_val   <- NA_real_
    anterior_fecha <- as.Date(NA)
  }
  
  # ================= anual
  fecha_ref <- actual_fecha %m-% years(1)
  
  anual_row <- df %>%
    filter(date <= fecha_ref) %>%
    slice_tail(n = 1)
  
  if (nrow(anual_row) == 1) {
    anual_val   <- pull(anual_row, valor)
    anual_fecha <- pull(anual_row, date)
  } else {
    anual_val   <- NA_real_
    anual_fecha <- as.Date(NA)
  }
  
  # ================= variaciones seguras
  var_ultimo <- ifelse(is.na(anterior_val), NA_real_,
                       actual_val / anterior_val - 1)
  
  var_anual  <- ifelse(is.na(anual_val), NA_real_,
                       actual_val / anual_val - 1)
  
  tibble(
    actual   = round(actual_val, 2),
    anterior = round(anterior_val, 2),
    anual    = round(anual_val, 2),
    var_ultimo = round(var_ultimo, 4),
    var_anual  = round(var_anual, 4),
    fecha_actual = actual_fecha,
    fecha_ant    = anterior_fecha,
    fecha_anual  = anual_fecha,
    frecuencia   = "diaria"
  )
}
# ======================================================
# TABLA MAESTRA
# ======================================================
resumen_final <- data_long %>%
  group_by(ticker) %>%
  group_modify(~ resumen_por_ticker(.x)) %>%
  ungroup()
# ======================================================
# CONSULTA DE MÉTRICAS
# ======================================================
get_bbg_metrics <- function(ticker,
                            tipo_var = "ratio",
                            fuente = resumen_final) {
  
  out <- fuente %>%
    filter(ticker == !!ticker) %>%
    select(-ticker)
  
  if (tipo_var == "pct") {
    out <- mutate(out,
                  var_ultimo = var_ultimo * 100,
                  var_anual  = var_anual  * 100)
  }
  
  out
}
# ======================================================
# HISTÓRICOS LARGOS (6 años rolling)
# ======================================================
end_date_6y   <- end_date
start_date_6y <- end_date_6y %m-% years(6)

get_bbg_df <- function(ticker,
                       start = start_date_6y,
                       end   = end_date_6y) {
  
  bdh(
    securities = ticker,
    fields     = "PX_LAST",
    start.date = start,
    end.date   = end
  ) |>
    transmute(
      fecha = as.Date(date),
      datos = as.numeric(PX_LAST)
    ) |>
    arrange(fecha)
}
# ======================================================
# MAPA AUTOMÁTICO r_*
# ======================================================
metricas_map <- list(
  
  ppemex   = "MXPRTLCR Index",
  antad_tt = "MXSAASTL Index",
  antad_ti = "MXSATOTL Index",
  imss     = "IMBWTOT Index",
  
  petroleo = "CL1 Comdty",
  petmm    = "CRAMMMIX Index",
  vix      = "VIX Index",
  
  usdmxn   = "USDMXN CURNCY",
  usdeur   = "USDEUR CURNCY",
  usdcny   = "USDCNY CURNCY",
  usdbrl   = "USDBRL CURNCY",
  
  bolmex   = "MEXBOL Index",
  djones   = "INDU Index",
  syp      = "SPX Index",
  cemexpo  = "CEMEXCPO MF Equity",
  
  cetes28  = "GCETAA28 Index",
  mx10y    = "GTMXN10Y Govt",
  mb30     = "MB30 Index",
  
  bcomin   = "BCOMIN Index"
)
metricas <- imap(metricas_map, ~ get_bbg_metrics(.x))

# ======================================================
# AJUSTE ESPECIAL IMSS
# ======================================================
metricas$imss <- metricas$imss %>%
  mutate(
    actual   = round(actual   / 1000),
    anterior = round(anterior / 1000),
    anual    = round(anual    / 1000),
    var_ultimo = var_ultimo * 100,
    var_anual  = var_anual  * 100
  )
# ======================================================
# CREAR OBJETOS r_*
# ======================================================
names(metricas) <- paste0("r_", names(metricas))
list2env(metricas, .GlobalEnv)

# ======================================================
# SERIES LARGAS EJEMPLO
# ======================================================
df_vix     <- get_bbg_df("VIX Index")
df_bcomin  <- get_bbg_df("BCOMIN Index")
df_cemexpo <- get_bbg_df("CEMEXCPO MF Equity")
