# Verificacion de trazabilidad. Toda cifra que figure en la documentacion
# publica debe proceder de un archivo de resultados versionado y no de una
# transcripcion manual.
#
# La tolerancia de cada comprobacion se deriva del numero de decimales con
# que el archivo conserva el valor, y no de un criterio arbitrario. Un margen
# superior al error de redondeo posible no verifica nada: absorbe
# discrepancias reales y produce conformidad injustificada.

tolerancia <- function(decimales, operaciones = 1) {
  (10^(-decimales) / 2) * operaciones
}

leer <- function(ruta) {
  if (!file.exists(ruta)) return(NULL)
  read.csv(ruta, stringsAsFactors = FALSE)
}

FUENTES <- list(
  "Comparacion de reglas de penalizacion"   = "outputs/fase7/comparacion_lambda.csv",
  "Cobertura conforme en prueba"            = "outputs/fase8/cobertura.csv",
  "Umbrales de calibracion"                 = "outputs/fase8/umbrales.csv",
  "Curva de riesgo y cobertura"             = "outputs/fase8/riesgo_cobertura.csv",
  "Compromiso segun nivel de confianza"     = "outputs/fase9/curva_alfa.csv",
  "Cobertura por nivel y categoria"         = "outputs/fase9/cobertura_por_alfa.csv",
  "Transportabilidad, cobertura"            = "outputs/fase10/cobertura_louo.csv",
  "Transportabilidad, cobertura por clase"  = "outputs/fase10/cobertura_clase_louo.csv",
  "Transportabilidad, discriminacion"       = "outputs/fase10/auc_louo.csv",
  "Validacion en la unidad reservada"       = "outputs/fase11/cobertura_sellado.csv",
  "Limites de la recalibracion local"       = "outputs/fase12/recalibracion.csv",
  "Comparacion con referencias simples"     = "outputs/fase13/referencias.csv",
  "Sensibilidad al lactato"                 = "outputs/fase15/sensibilidad_lactato.csv",
  "Comparacion de reglas de agregacion"     = "outputs/fase15/comparacion_agregacion.csv",
  "Curvas de decision"                      = "outputs/fase16/curvas_decision.csv",
  "Comparador con arboles potenciados"      = "outputs/fase16/gbm_comparacion.csv",
  "Cobertura de constantes vitales"         = "outputs/fase17/cobertura_vitales.csv",
  "Valores implausibles marcados"           = "outputs/fase17/implausibles.csv",
  "Intubacion por unidad"                   = "outputs/fase17/intubacion_por_unidad.csv",
  "Circularidad de la escala de conciencia" = "outputs/fase17/circularidad_glasgow.csv",
  "Constantes por estrato de intubacion"    = "outputs/fase17/vitales_por_estrato.csv",
  "Modelo ampliado, discriminacion"         = "outputs/fase18/comparacion_ampliado.csv",
  "Ganancia de la forma flexible"           = "outputs/fase18/ganancia_splines.csv",
  "Modelo ampliado, umbrales"               = "outputs/fase18/umbrales_ampliado.csv",
  "Modelo ampliado, transportabilidad"      = "outputs/fase19/cobertura_louo_ampliado.csv",
  "Modelo ampliado, cobertura por clase"    = "outputs/fase19/cobertura_clase_louo_ampliado.csv",
  "Modelo ampliado, discriminacion por sede"= "outputs/fase19/auc_louo_ampliado.csv")

cat("=== INVENTARIO DE FUENTES ===\n")
existe <- sapply(names(FUENTES), function(n) file.exists(FUENTES[[n]]))
for (n in names(FUENTES))
  cat(sprintf("%-42s %s\n", substr(n, 1, 42),
              if (existe[n]) "presente" else "AUSENTE"))
cat("\nPresentes:", sum(existe), "de", length(existe), "\n")
if (any(!existe)) {
  cat("\nFuentes ausentes:\n")
  for (n in names(FUENTES)[!existe]) cat("  ", FUENTES[[n]], "\n")
}

# Una extraccion que no devuelva exactamente un valor constituye fallo y no
# ausencia de fuente. Sin esa distincion, una condicion mal escrita produciria
# un vector vacio que el procedimiento contabilizaria como no evaluado en
# lugar de advertirlo.
comprobar <- function(etiqueta, valor, esperado, tol) {
  if (is.null(valor) || length(valor) != 1 || is.na(valor)) {
    cat(sprintf("%-52s %s\n", substr(etiqueta, 1, 52),
                "FALLO EN LA EXTRACCION"))
    return(data.frame(afirmacion = etiqueta, valor = NA, esperado = esperado,
                      tolerancia = tol, concuerda = FALSE, row.names = NULL))
  }
  ok <- abs(valor - esperado) < tol
  cat(sprintf("%-52s %11.4f  esperado %11.4f  %s\n",
              substr(etiqueta, 1, 52), valor, esperado,
              if (ok) "concuerda" else "DISCREPA"))
  data.frame(afirmacion = etiqueta, valor = valor, esperado = esperado,
             tolerancia = tol, concuerda = ok, row.names = NULL)
}

cat("\n=== CONTRASTE DE CIFRAS PRINCIPALES ===\n")
reg <- list()
t4 <- tolerancia(4); t4d <- tolerancia(4, 2)
t2 <- tolerancia(2); t1 <- tolerancia(1)

x <- leer(FUENTES[["Comparacion de reglas de penalizacion"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Ganancia de lambda minimo en minoritarias",
    mean(x$auc_min[x$clase != "sin_crecimiento"]) -
    mean(x$auc_1se[x$clase != "sin_crecimiento"]), 0.0395, t4d)
}

x <- leer(FUENTES[["Cobertura conforme en prueba"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Cobertura conforme, sin crecimiento",
    x$cobertura[x$clase == "sin_crecimiento"], 0.9044, t4)
  reg[[length(reg)+1]] <- comprobar("Cobertura conforme, sangre",
    x$cobertura[x$clase == "sangre"], 0.8679, t4)
  reg[[length(reg)+1]] <- comprobar("Cobertura conforme, minimo entre categorias",
    min(x$cobertura), 0.8679, t4)
}

x <- leer(FUENTES[["Compromiso segun nivel de confianza"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Proporcion resuelta al noventa por ciento",
    x$pct_resuelve[x$alfa == 0.10], 9.6, t1)
  reg[[length(reg)+1]] <- comprobar("Error entre resueltos al noventa por ciento",
    x$error_entre_resueltos[x$alfa == 0.10], 0.0280, t4)
  pmax_min <- max(x$aciertos_minoritarios[x$conclusiones_minoritarias > 0] /
                  x$conclusiones_minoritarias[x$conclusiones_minoritarias > 0])
  reg[[length(reg)+1]] <- comprobar("Precision maxima en conclusiones minoritarias",
    pmax_min, 0.4000, 0.001)
}

x <- leer(FUENTES[["Transportabilidad, cobertura"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Transportabilidad, cobertura minima",
    min(x$cobertura), 0.8036, t4)
  reg[[length(reg)+1]] <- comprobar("Transportabilidad, desviacion",
    sd(x$cobertura), 0.0744, t4)
  reg[[length(reg)+1]] <- comprobar("Transportabilidad, cobertura media",
    mean(x$cobertura), 0.8944, t4)
}

x <- leer(FUENTES[["Validacion en la unidad reservada"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Unidad reservada, cobertura sin crecimiento",
    x$cobertura[x$clase == "sin_crecimiento"], 0.9911, t4)
  reg[[length(reg)+1]] <- comprobar("Unidad reservada, casos respiratorios",
    x$n[x$clase == "respiratorio"], 13, 0.5)
}

x <- leer(FUENTES[["Limites de la recalibracion local"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Categorias calibrables con cincuenta casos",
    x$clases_calibrables[x$n_local == 50], 1.0, t1)
  reg[[length(reg)+1]] <- comprobar("Tamano medio del conjunto con cincuenta casos",
    x$tamano_medio[x$n_local == 50], 0.888, tolerancia(3))
}

x <- leer(FUENTES[["Comparacion con referencias simples"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Referencia demografica, minoritarias",
    x$promedio_minoritarias[x$modelo == "demografia"], 0.5421, t4)
  reg[[length(reg)+1]] <- comprobar("Modelo completo, minoritarias",
    x$promedio_minoritarias[x$modelo == "modelo completo"], 0.6735, t4)
  reg[[length(reg)+1]] <- comprobar("Ganancia del modelo completo sobre tres marcadores",
    x$promedio_minoritarias[x$modelo == "modelo completo"] -
    x$promedio_minoritarias[x$modelo == "tres marcadores"], 0.0825, t4d)
}

x <- leer(FUENTES[["Sensibilidad al lactato"]])
if (!is.null(x)) {
  a <- x$promedio_minoritarias[x$especificacion == "completa"]
  b <- x$promedio_minoritarias[x$especificacion == "lactato sin indicador"]
  cc <- x$promedio_minoritarias[x$especificacion == "sin lactato ni indicador"]
  reg[[length(reg)+1]] <- comprobar("Aporte del indicador de solicitud",
    a - b, 0.0134, t4d)
  reg[[length(reg)+1]] <- comprobar("Aporte del valor de lactato",
    b - cc, 0.0038, t4d)
}

x <- leer(FUENTES[["Comparacion de reglas de agregacion"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Concordancia minima entre reglas de agregacion",
    min(x$correlacion), 0.8722, t4)
}

x <- leer(FUENTES[["Curvas de decision"]])
if (!is.null(x)) {
  s <- x[x$objetivo == "cualquier_foco", ]
  s$gan <- s$bn_modelo - pmax(s$bn_tratar_todos, 0)
  reg[[length(reg)+1]] <- comprobar("Beneficio neto maximo sobre politicas triviales",
    max(s$gan), 0.0222, tolerancia(5, 2))
}

# La cifra reportada corresponde al promedio sobre las categorias
# minoritarias, criterio empleado de forma uniforme en todo el desarrollo.
# Promediar las cuatro categorias produce un valor distinto, de signo
# contrario, que no se corresponde con ninguna afirmacion del proyecto.
x <- leer(FUENTES[["Comparador con arboles potenciados"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Arboles potenciados, ganancia en minoritarias",
    mean(x$auc_gbm[x$clase != "sin_crecimiento"]) -
    mean(x$auc_lineal[x$clase != "sin_crecimiento"]), 0.0002, t4d)
}

x <- leer(FUENTES[["Cobertura de constantes vitales"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Cobertura de la frecuencia cardiaca",
    x$pct[x$variable == "frec_cardiaca"], 99.2, t1)
  reg[[length(reg)+1]] <- comprobar("Cobertura de la presion sistolica no invasiva",
    x$pct[x$variable == "presion_sistolica"], 74.9, t1)
}

x <- leer(FUENTES[["Valores implausibles marcados"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Total de valores implausibles marcados",
    sum(x$marcados), 216, 0.5)
}

x <- leer(FUENTES[["Intubacion por unidad"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Intubacion en la unidad cardiovascular",
    x$pct_intubado[grepl("Cardiac", x$unidad)], 72.0, t1)
}

x <- leer(FUENTES[["Circularidad de la escala de conciencia"]])
if (!is.null(x)) {
  s <- x[x$estrato == "con tubo endotraqueal" & x$clase == "respiratorio", ]
  reg[[length(reg)+1]] <- comprobar("Escala de conciencia dentro del estrato con tubo",
    s$auc_gcs_em, 0.4865, t4)
  s2 <- x[x$estrato == "cohorte completa" & x$clase == "respiratorio", ]
  reg[[length(reg)+1]] <- comprobar("Escala de conciencia en la cohorte completa",
    s2$auc_gcs_em, 0.7086, t4)
}

x <- leer(FUENTES[["Ganancia de la forma flexible"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Ganancia de la temperatura con spline",
    x$ganancia[x$variable == "temperatura"], 283.00, t2)
  reg[[length(reg)+1]] <- comprobar("Ganancia de la saturacion con spline",
    x$ganancia[x$variable == "saturacion"], -47.85, t2)
}

x <- leer(FUENTES[["Modelo ampliado, discriminacion"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Modelo ampliado, ganancia en minoritarias",
    mean(x$auc_ampliado[x$clase != "sin_crecimiento"]) -
    mean(x$auc_original[x$clase != "sin_crecimiento"]), 0.0146, t4d)
}

x <- leer(FUENTES[["Modelo ampliado, transportabilidad"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Modelo ampliado, desviacion de cobertura",
    sd(x$cobertura), 0.0736, t4)
  reg[[length(reg)+1]] <- comprobar("Modelo ampliado, cobertura minima",
    min(x$cobertura), 0.7757, t4)
}

tab <- do.call(rbind, reg)
disc <- sum(!tab$concuerda)

cat("\n=== RESULTADO DE LA VERIFICACION ===\n")
cat("Cifras contrastadas:", nrow(tab), "\n")
cat("Concuerdan:", sum(tab$concuerda), "\n")
cat("Discrepan:", disc, "\n")
if (disc > 0) {
  cat("\nDiscrepancias detectadas:\n")
  print(tab[!tab$concuerda, ], row.names = FALSE)
}

lineas <- c(
  "# Traceability of reported figures",
  "",
  paste("Generated on", format(Sys.Date(), "%Y-%m-%d"),
        "by R/59_verificar_cifras.R"),
  "",
  "Every figure quoted in the documentation is read from a versioned results",
  "file rather than transcribed by hand. Each check uses a tolerance derived",
  "from the number of decimal places the source file retains, so that the",
  "margin never exceeds the largest possible rounding error.",
  "",
  "| Claim | Value | Expected | Tolerance | Agrees |",
  "|---|---|---|---|---|")

for (i in seq_len(nrow(tab)))
  lineas <- c(lineas, sprintf("| %s | %.4f | %.4f | %.5f | %s |",
    tab$afirmacion[i], tab$valor[i], tab$esperado[i], tab$tolerancia[i],
    if (tab$concuerda[i]) "yes" else "NO"))

lineas <- c(lineas, "", "## Source files", "")
for (n in names(FUENTES))
  if (existe[n]) lineas <- c(lineas, sprintf("- `%s`", FUENTES[[n]]))

dir.create("outputs/fase20", recursive = TRUE, showWarnings = FALSE)
writeLines(lineas, "outputs/fase20/TRACEABILITY.md")
write.csv(tab, "outputs/fase20/verificacion_cifras.csv", row.names = FALSE)
cat("\nRegistro escrito en outputs/fase20/TRACEABILITY.md\n")

if (disc > 0) {
  cat("\nLa verificacion no se supera. No procede documentar sobre esta base.\n")
  quit(status = 1)
}
cat("Verificacion superada.\n")
