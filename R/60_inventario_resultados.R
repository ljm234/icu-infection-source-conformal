# Inventario del deposito de resultados. Antes de redactar documentacion
# publica procede establecer que cifras admiten respaldo en un archivo
# versionado y cuales no. Escribir un valor recordado, y no leido, reintroduce
# precisamente el riesgo que el procedimiento de verificacion pretende
# eliminar.

archivos <- list.files("outputs", pattern = "\\.csv$|\\.json$|\\.md$",
                       recursive = TRUE, full.names = TRUE)

cat("=== CONTENIDO DEL DEPOSITO DE RESULTADOS ===\n")
cat("Archivos encontrados:", length(archivos), "\n")
cat("Directorios de fase:", length(unique(dirname(archivos))), "\n\n")

tam <- sapply(archivos, file.size)
vacios <- archivos[tam == 0]
if (length(vacios) > 0) {
  cat("ARCHIVOS VACIOS DETECTADOS\n")
  for (f in vacios) cat("  ", f, "\n")
  cat("\n")
} else {
  cat("Ningun archivo vacio.\n\n")
}

cat("=== ARCHIVOS POR FASE ===\n")
for (dir in sort(unique(dirname(archivos)))) {
  fs <- archivos[dirname(archivos) == dir]
  cat(sprintf("%-22s %2d archivos  %8d bytes\n",
              dir, length(fs), sum(sapply(fs, file.size))))
}

# La lista replica la empleada por el procedimiento de verificacion, con la
# reposicion de la fuente relativa a la asociacion entre constantes y
# determinaciones bioquimicas, omitida de forma inadvertida al reescribirlo.
VERIFICADAS <- c(
  "outputs/fase7/comparacion_lambda.csv",
  "outputs/fase8/cobertura.csv",
  "outputs/fase8/umbrales.csv",
  "outputs/fase8/riesgo_cobertura.csv",
  "outputs/fase9/curva_alfa.csv",
  "outputs/fase9/cobertura_por_alfa.csv",
  "outputs/fase10/cobertura_louo.csv",
  "outputs/fase10/cobertura_clase_louo.csv",
  "outputs/fase10/auc_louo.csv",
  "outputs/fase11/cobertura_sellado.csv",
  "outputs/fase12/recalibracion.csv",
  "outputs/fase13/referencias.csv",
  "outputs/fase15/sensibilidad_lactato.csv",
  "outputs/fase15/comparacion_agregacion.csv",
  "outputs/fase16/curvas_decision.csv",
  "outputs/fase16/gbm_comparacion.csv",
  "outputs/fase17/cobertura_vitales.csv",
  "outputs/fase17/implausibles.csv",
  "outputs/fase17/intubacion_por_unidad.csv",
  "outputs/fase17/circularidad_glasgow.csv",
  "outputs/fase17/vitales_por_estrato.csv",
  "outputs/fase17/correlacion_vitales_labs.csv",
  "outputs/fase18/comparacion_ampliado.csv",
  "outputs/fase18/ganancia_splines.csv",
  "outputs/fase18/umbrales_ampliado.csv",
  "outputs/fase19/cobertura_louo_ampliado.csv",
  "outputs/fase19/cobertura_clase_louo_ampliado.csv",
  "outputs/fase19/auc_louo_ampliado.csv")

datos <- archivos[grepl("\\.csv$", archivos)]
huerfanos <- setdiff(datos, VERIFICADAS)
inexistentes <- setdiff(VERIFICADAS, archivos)

cat("\n=== COBERTURA DEL PROCEDIMIENTO DE VERIFICACION ===\n")
cat("Archivos de datos:", length(datos), "\n")
cat("Incluidos en la verificacion:", length(intersect(datos, VERIFICADAS)), "\n")
cat("Sin verificar:", length(huerfanos), "\n")

if (length(inexistentes) > 0) {
  cat("\nFUENTES DECLARADAS QUE NO EXISTEN\n")
  for (f in inexistentes) cat("  ", f, "\n")
}

if (length(huerfanos) > 0) {
  cat("\nArchivos presentes que ningun contraste examina\n")
  for (f in huerfanos) cat("  ", f, "\n")
}

# Las cifras del embudo de seleccion figuran en la documentacion publica. Se
# comprueba si constan en algun archivo de las fases iniciales o si proceden
# unicamente de la salida por pantalla de las consultas correspondientes.
cat("\n=== FUENTES DE LAS FASES INICIALES ===\n")
tempranos <- archivos[grepl("outputs/fase[0-5]/", archivos)]
if (length(tempranos) == 0) {
  cat("No consta ningun archivo en las fases cero a cinco.\n")
} else {
  for (f in tempranos) {
    x <- try(read.csv(f, stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(x, "try-error")) {
      cat(sprintf("%-46s no admite lectura tabular\n", substr(f, 1, 46)))
      next
    }
    cat(sprintf("%-46s %5d filas  %s\n", substr(f, 1, 46), nrow(x),
                substr(paste(head(names(x), 5), collapse = ", "), 1, 60)))
  }
}

# Relacion de afirmaciones que la documentacion publica sostiene y estado de
# su respaldo documental.
cat("\n=== AFIRMACIONES DE LA DOCUMENTACION Y SU RESPALDO ===\n")
AFIRMACIONES <- list(
  c("Embudo de seleccion de la cohorte", "fases iniciales"),
  c("Distribucion de categorias", "fases iniciales"),
  c("Discriminacion en el conjunto de prueba", "outputs/fase8/cobertura.csv"),
  c("Cobertura conforme condicional", "outputs/fase8/cobertura.csv"),
  c("Compromiso segun nivel de confianza", "outputs/fase9/curva_alfa.csv"),
  c("Transportabilidad entre unidades", "outputs/fase10/cobertura_louo.csv"),
  c("Validacion en la unidad reservada", "outputs/fase11/cobertura_sellado.csv"),
  c("Limite de la recalibracion local", "outputs/fase12/recalibracion.csv"),
  c("Comparacion con referencias simples", "outputs/fase13/referencias.csv"),
  c("Sensibilidad al lactato", "outputs/fase15/sensibilidad_lactato.csv"),
  c("Curvas de decision", "outputs/fase16/curvas_decision.csv"),
  c("Comparador con arboles potenciados", "outputs/fase16/gbm_comparacion.csv"),
  c("Cobertura de constantes vitales", "outputs/fase17/cobertura_vitales.csv"),
  c("Exclusion de la presion arterial", "sin archivo especifico"),
  c("Exclusion de la escala de conciencia", "outputs/fase17/circularidad_glasgow.csv"),
  c("Discriminacion del modelo ampliado", "outputs/fase18/comparacion_ampliado.csv"),
  c("Transportabilidad del modelo ampliado", "outputs/fase19/cobertura_louo_ampliado.csv"),
  c("Validacion de la imputacion", "sin archivo especifico"))

for (a in AFIRMACIONES) {
  estado <- if (a[2] %in% archivos) "respaldada"
            else if (grepl("^outputs/", a[2])) "FUENTE AUSENTE"
            else "SIN RESPALDO DOCUMENTAL"
  cat(sprintf("%-42s %s\n", substr(a[1], 1, 42), estado))
}

cat("\n=== PROCEDIMIENTOS DEL PROYECTO ===\n")
scripts <- list.files("R", pattern = "\\.R$", full.names = TRUE)
cat("Procedimientos en R:", length(scripts), "\n")
cat("Lineas totales:",
    sum(sapply(scripts, function(f) length(readLines(f, warn = FALSE)))), "\n")
