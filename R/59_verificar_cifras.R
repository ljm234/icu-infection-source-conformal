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
  "Embudo de seleccion"                      = "outputs/fase2/flujo.csv",
  "Distribucion de categorias"               = "outputs/fase2/clases.csv",
  "Imputacion frente a la mediana"           = "outputs/fase6/validacion_enmascaramiento.csv",
  "Comparacion de reglas de penalizacion"    = "outputs/fase7/comparacion_lambda.csv",
  "Cobertura conforme en prueba"             = "outputs/fase8/cobertura.csv",
  "Umbrales de calibracion"                  = "outputs/fase8/umbrales.csv",
  "Curva de riesgo y cobertura"              = "outputs/fase8/riesgo_cobertura.csv",
  "Compromiso segun nivel de confianza"      = "outputs/fase9/curva_alfa.csv",
  "Cobertura por nivel y categoria"          = "outputs/fase9/cobertura_por_alfa.csv",
  "Transportabilidad, cobertura"             = "outputs/fase10/cobertura_louo.csv",
  "Transportabilidad, cobertura por clase"   = "outputs/fase10/cobertura_clase_louo.csv",
  "Transportabilidad, discriminacion"        = "outputs/fase10/auc_louo.csv",
  "Validacion en la unidad reservada"        = "outputs/fase11/cobertura_sellado.csv",
  "Limites de la recalibracion local"        = "outputs/fase12/recalibracion.csv",
  "Comparacion con referencias simples"      = "outputs/fase13/referencias.csv",
  "Sensibilidad al lactato"                  = "outputs/fase15/sensibilidad_lactato.csv",
  "Comparacion de reglas de agregacion"      = "outputs/fase15/comparacion_agregacion.csv",
  "Curvas de decision"                       = "outputs/fase16/curvas_decision.csv",
  "Comparador con arboles potenciados"       = "outputs/fase16/gbm_comparacion.csv",
  "Cobertura de constantes vitales"          = "outputs/fase17/cobertura_vitales.csv",
  "Presion por cateter segun unidad"         = "outputs/fase17/cobertura_presion.csv",
  "Concordancia entre metodos de presion"    = "outputs/fase17/concordancia_presion.csv",
  "Valores implausibles marcados"            = "outputs/fase17/implausibles.csv",
  "Indicador de tubo endotraqueal"           = "outputs/fase17/indicador_tubo.csv",
  "Intubacion por unidad"                    = "outputs/fase17/intubacion_por_unidad.csv",
  "Circularidad de la escala de conciencia"  = "outputs/fase17/circularidad_glasgow.csv",
  "Recorrido de constantes entre unidades"   = "outputs/fase17/recorrido_por_unidad.csv",
  "Constantes por estrato de intubacion"     = "outputs/fase17/vitales_por_estrato.csv",
  "Modelo ampliado, discriminacion"          = "outputs/fase18/comparacion_ampliado.csv",
  "Ganancia de la forma flexible"            = "outputs/fase18/ganancia_splines.csv",
  "Modelo ampliado, umbrales"                = "outputs/fase18/umbrales_ampliado.csv",
  "Modelo ampliado, transportabilidad"       = "outputs/fase19/cobertura_louo_ampliado.csv",
  "Modelo ampliado, cobertura por clase"     = "outputs/fase19/cobertura_clase_louo_ampliado.csv",
  "Modelo ampliado, discriminacion por sede" = "outputs/fase19/auc_louo_ampliado.csv",
  "Coincidencias en la hora de registro"     = "outputs/fase20/empates_constantes.csv",
  "Coincidencias en bioquimica"              = "outputs/fase20/empates_laboratorio.csv",
  "Divergencia de la extraccion"             = "outputs/fase20/divergencia_extraccion.csv",
  "Determinismo de la extraccion"             = "outputs/fase20/determinismo_extraccion.csv",
  "Cobertura antes y despues del desempate"   = "outputs/fase20/cobertura_extraccion.csv",
  "Fraccion de informacion faltante"         = "outputs/fase20/fraccion_informacion_faltante.csv",
  "Validacion interna de la penalizacion"    = "outputs/fase22/decision_lambda.csv",
  "Calibracion en la unidad reservada"       = "outputs/fase23/calibracion_sellado.csv",
  "Regla del maximo"                         = "outputs/fase24/regla_maximo.csv",
  "Divergencia de etiquetado"                = "outputs/fase25/divergencia_etiquetado.csv",
  "Intervalos de cobertura por celda"        = "outputs/fase26/intervalos_cobertura.csv",
  "Recuentos bajo cada correccion"           = "outputs/fase26/recuentos_multiplicidad.csv",
  "Causa del fallo por sede"                 = "outputs/fase26/causas_fallo.csv",
  "Cobertura de constantes por etapa"        = "outputs/fase28/cobertura_vitales_por_etapa.csv")

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

# El nivel nominal y la relacion de clases modeladas se leen de la fuente. Los
# recuentos que dependen de ellos quedan asi anclados al archivo y no a un
# valor escrito en este procedimiento.
CLASES  <- c("sin_crecimiento", "urinario", "respiratorio", "sangre")
cob_ref <- leer(FUENTES[["Cobertura conforme en prueba"]])
NOMINAL <- if (is.null(cob_ref)) NA_real_ else cob_ref$nominal[1]

x <- leer(FUENTES[["Comparacion de reglas de penalizacion"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Ganancia de lambda minimo en minoritarias",
    mean(x$auc_min[x$clase != "sin_crecimiento"]) -
    mean(x$auc_1se[x$clase != "sin_crecimiento"]), 0.0395, t4d)
}

x <- leer(FUENTES[["Cobertura conforme en prueba"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Cobertura conforme, minimo entre categorias",
    min(x$cobertura), 0.8679, t4)
  reg[[length(reg)+1]] <- comprobar("Cobertura conforme, maximo entre categorias",
    max(x$cobertura), 0.9044, t4)
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
    pmax_min, 0.4000, tolerancia(4))
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

# La cobertura publicada es la de la tabla limpia, que es la que el modelo
# ajusta. La cruda se contrasta tambien: el documento publica ambas columnas y
# la diferencia entre ellas es lo que los limites de plausibilidad descartan.
x <- leer(FUENTES[["Cobertura de constantes por etapa"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Cobertura limpia de la frecuencia cardiaca",
    x$pct_limpio[x$variable == "frec_cardiaca"], 99.2, t1)
  reg[[length(reg)+1]] <- comprobar("Cobertura limpia de la presion sistolica",
    x$pct_limpio[x$variable == "presion_sistolica"], 74.9, t1)
  reg[[length(reg)+1]] <- comprobar("Valores anulados por los limites",
    sum(x$marcados), 216, 0.5)
  reg[[length(reg)+1]] <- comprobar("Denominador de la cobertura",
    x$estancias[1], 23213, 0.5)
}

x <- leer(FUENTES[["Cobertura de constantes vitales"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Cobertura cruda de la frecuencia cardiaca",
    x$pct[x$variable == "frec_cardiaca"], 99.2, t1)
  reg[[length(reg)+1]] <- comprobar("Disponible crudo de temperatura",
    x$disponible[x$variable == "temperatura"], 21587, 0.5)
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

x <- leer(FUENTES[["Cobertura de constantes por etapa"]])
if (!is.null(x)) {
  lim <- setNames(c(92.7, 98.3, 98.8),
                  c("temperatura","frec_respiratoria","saturacion"))
  cru <- setNames(c(93.0, 98.7, 98.9),
                  c("temperatura","frec_respiratoria","saturacion"))
  for (v in names(lim)) {
    reg[[length(reg)+1]] <- comprobar(paste("Cobertura limpia de", v),
      x$pct_limpio[x$variable == v], lim[[v]], t1)
    reg[[length(reg)+1]] <- comprobar(paste("Cobertura cruda de", v),
      x$pct_crudo[x$variable == v], cru[[v]], t1)
  }
}

x <- leer(FUENTES[["Compromiso segun nivel de confianza"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Conclusiones minoritarias, total",
    sum(x$conclusiones_minoritarias), 1390, 0.5)
  reg[[length(reg)+1]] <- comprobar("Resolucion al nivel mas laxo",
    max(x$pct_resuelve), 69.1, t1)
  reg[[length(reg)+1]] <- comprobar("Error al nivel mas laxo",
    max(x$error_entre_resueltos), 0.3753, t4)
}

x <- leer(FUENTES[["Modelo ampliado, transportabilidad"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Modelo ampliado, cobertura media",
    mean(x$cobertura), 0.8838, t4)
}

# ---------------------------------------------------------------------------
# Cobertura de la documentacion. Las comprobaciones anteriores dejaban sin
# contrastar la mayor parte de las cifras que los documentos publican: el
# embudo, la tabla de categorias, las coberturas por clase de la unidad
# reservada y los recuentos derivados, entre otras. Cada valor esperado que
# sigue se leyo de su archivo en el momento de escribirlo.
# ---------------------------------------------------------------------------

x <- leer(FUENTES[["Embudo de seleccion"]])
if (!is.null(x)) {
  esp <- setNames(c(65366, 65366, 33045, 23213, 23213),
                  c("p1_estancias_unicas", "p2_adultos", "p3_con_cultivo",
                    "p4_sospecha_infeccion", "p5_cohorte_final"))
  for (p in names(esp))
    reg[[length(reg)+1]] <- comprobar(paste("Embudo,", p),
      x$n[x$paso == p], esp[[p]], 0.5)
}

x <- leer(FUENTES[["Distribucion de categorias"]])
if (!is.null(x)) {
  cl  <- c("sin_crecimiento", "otro_sitio", "urinario", "respiratorio",
           "sangre", "herida", "intraabdominal")
  esn <- c(19688, 1070, 805, 671, 573, 340, 66)
  esp <- c(84.81, 4.61, 3.47, 2.89, 2.47, 1.46, 0.28)
  for (i in seq_along(cl)) {
    reg[[length(reg)+1]] <- comprobar(paste("Categoria,", cl[i], "casos"),
      x$n[x$clase == cl[i]], esn[i], 0.5)
    reg[[length(reg)+1]] <- comprobar(paste("Categoria,", cl[i], "porcentaje"),
      x$pct[x$clase == cl[i]], esp[i], tolerancia(2))
  }
}

x <- leer(FUENTES[["Cobertura conforme en prueba"]])
if (!is.null(x)) {
  esp <- setNames(c(0.9044, 0.8881, 0.8984, 0.8679), CLASES)
  for (k in CLASES)
    reg[[length(reg)+1]] <- comprobar(paste("Cobertura conforme por clase,", k),
      x$cobertura[x$clase == k], esp[[k]], t4)
  reg[[length(reg)+1]] <- comprobar("Nivel nominal declarado en la fuente",
    x$nominal[1], 0.9, t1)
  reg[[length(reg)+1]] <- comprobar("Clases modeladas en la cobertura conforme",
    nrow(x), 4, 0.5)
}

x <- leer(FUENTES[["Modelo ampliado, discriminacion"]])
if (!is.null(x)) {
  esp <- setNames(c(0.6521, 0.6518, 0.6597, 0.709), CLASES)
  for (k in CLASES)
    reg[[length(reg)+1]] <- comprobar(paste("Discriminacion del modelo original,", k),
      x$auc_original[x$clase == k], esp[[k]], t4)
}

x <- leer(FUENTES[["Compromiso segun nivel de confianza"]])
if (!is.null(x)) {
  s <- x[x$conclusiones_minoritarias > 0, ]
  i <- which.max(s$aciertos_minoritarios / s$conclusiones_minoritarias)
  reg[[length(reg)+1]] <- comprobar("Conclusiones en el nivel de precision maxima",
    s$conclusiones_minoritarias[i], 5, 0.5)
}

x <- leer(FUENTES[["Transportabilidad, cobertura"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Transportabilidad, cobertura maxima",
    max(x$cobertura), 0.9612, t4)
  reg[[length(reg)+1]] <- comprobar("Sedes bajo el nominal en cobertura marginal",
    sum(x$cobertura < NOMINAL), 2, 0.5)
  reg[[length(reg)+1]] <- comprobar("Sedes evaluadas dejando una fuera",
    nrow(x), 5, 0.5)
}

x <- leer(FUENTES[["Transportabilidad, cobertura por clase"]])
if (!is.null(x)) {
  # Criterio puntual sobre la fuente original, sin intervalo ni correccion:
  # no es el criterio que el documento adopta, que vive en las celdas de la
  # fase veintiseis, comprobadas mas abajo. El nombre lo dice.
  reg[[length(reg)+1]] <- comprobar("Sedes con alguna clase puntualmente bajo el nominal",
    sum(apply(x[, CLASES], 1, function(r) any(r < NOMINAL, na.rm = TRUE))),
    5, 0.5)
  # Diagnostico del mismo criterio puntual: conjuntos distintos de categorias
  # por debajo del nominal. Ningun documento publica ya este recuento; se
  # conserva como vigilancia de la fuente, con el criterio explicito en el
  # nombre.
  reg[[length(reg)+1]] <- comprobar("Conjuntos distintos de clases puntualmente bajo el nominal",
    length(unique(apply(x[, CLASES], 1, function(r)
      paste(CLASES[!is.na(r) & r < NOMINAL], collapse = "|")))), 3, 0.5)
}

x <- leer(FUENTES[["Validacion en la unidad reservada"]])
if (!is.null(x)) {
  esp <- setNames(c(0.9911, 0.8654, 0.8462, 0.9615), CLASES)
  for (k in CLASES)
    reg[[length(reg)+1]] <- comprobar(paste("Unidad reservada, cobertura de", k),
      x$cobertura[x$clase == k], esp[[k]], t4)
  reg[[length(reg)+1]] <- comprobar("Unidad reservada, clases bajo el nominal",
    sum(x$cobertura < NOMINAL), 2, 0.5)
  reg[[length(reg)+1]] <- comprobar("Unidad reservada, clases evaluadas",
    nrow(x), 4, 0.5)
}

x <- leer(FUENTES[["Limites de la recalibracion local"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Recalibracion, recuento local minimo",
    min(x$n_local), 50, 0.5)
}

x <- leer(FUENTES[["Comparacion con referencias simples"]])
if (!is.null(x)) {
  mo  <- c("demografia", "tres marcadores", "lineal sin unidad",
           "modelo completo")
  # El recuento de la segunda referencia se esperaba en dieciseis. Esa cifra
  # contaba los interceptos y las otras tres no, de modo que la columna
  # mezclaba dos definiciones. R/39 las unifico en la que ya empleaban las dos
  # ultimas, coeficientes no nulos sin interceptos, y el recuento paso a doce.
  esn <- c(8, 12, 62, 148)
  esa <- c(0.5421, 0.591, 0.6343, 0.6735)
  for (i in seq_along(mo)) {
    reg[[length(reg)+1]] <- comprobar(paste("Referencia,", mo[i], "parametros"),
      x$parametros[x$modelo == mo[i]], esn[i], 0.5)
    reg[[length(reg)+1]] <- comprobar(paste("Referencia,", mo[i], "minoritarias"),
      x$promedio_minoritarias[x$modelo == mo[i]], esa[i], t4)
  }
}

x <- leer(FUENTES[["Concordancia entre metodos de presion"]])
if (!is.null(x)) {
  s <- x[x$tramo == "menos de 15 min", ]
  reg[[length(reg)+1]] <- comprobar("Concordancia de presion a menos de quince minutos",
    s$correlacion, 0.4969, t4)
  reg[[length(reg)+1]] <- comprobar("Discrepancia absoluta mediana de presion",
    s$dif_absoluta_mediana, 12, 0.5)
}

x <- leer(FUENTES[["Presion por cateter segun unidad"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Presion por cateter, proporcion minima",
    min(x$pct_por_cateter), 2.5, t1)
  reg[[length(reg)+1]] <- comprobar("Presion por cateter, proporcion maxima",
    max(x$pct_por_cateter), 80.4, t1)
}

x <- leer(FUENTES[["Indicador de tubo endotraqueal"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Indicador de tubo, categoria respiratoria",
    x$auc_solo_tubo[x$clase == "respiratorio"], 0.7111, t4)
}

x <- leer(FUENTES[["Circularidad de la escala de conciencia"]])
if (!is.null(x)) {
  s <- x[x$estrato == "cohorte completa" & x$clase == "respiratorio", ]
  reg[[length(reg)+1]] <- comprobar("Escala completa en la cohorte, tres componentes",
    s$auc_gcs_total, 0.7313, t4)
  s2 <- x[x$estrato == "con tubo endotraqueal" & x$clase == "respiratorio", ]
  reg[[length(reg)+1]] <- comprobar("Escala completa dentro del estrato con tubo",
    s2$auc_gcs_total, 0.4865, t4)
}

x <- leer(FUENTES[["Recorrido de constantes entre unidades"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Recorrido de la mediana, frecuencia cardiaca",
    x$recorrido_mediana[x$variable == "frec_cardiaca"], 16, tolerancia(2))
  reg[[length(reg)+1]] <- comprobar("Recorrido de la mediana, frecuencia respiratoria",
    x$recorrido_mediana[x$variable == "frec_respiratoria"], 7, tolerancia(2))
  s <- x[x$variable == "temperatura", ]
  reg[[length(reg)+1]] <- comprobar("Temperatura, disponibilidad minima por unidad",
    s$cobertura_minima, 76.5, t1)
  reg[[length(reg)+1]] <- comprobar("Temperatura, disponibilidad maxima por unidad",
    s$cobertura_maxima, 97.9, t1)
  reg[[length(reg)+1]] <- comprobar("Temperatura, recorrido de la disponibilidad",
    s$recorrido_cobertura, 21.4, t1)
}

x <- leer(FUENTES[["Fraccion de informacion faltante"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Informacion faltante, maximo en laboratorio",
    max(x$fmi[x$pct_ausente > 20]), 0.6214, t4)
  reg[[length(reg)+1]] <- comprobar("Informacion faltante, maximo en constantes",
    max(x$fmi[x$pct_ausente < 10]), 0.0648, t4)
}

x <- leer(FUENTES[["Imputacion frente a la mediana"]])
if (!is.null(x)) {
  mi <- x[x$metodo == "mice con unidad", ]
  md <- x[x$metodo == "mediana", ]
  o  <- match(mi$variable, md$variable)
  reg[[length(reg)+1]] <- comprobar("Variables donde la mediana obtiene menor error",
    sum(mi$error_absoluto_mediano > md$error_absoluto_mediano[o]), 9, 0.5)
  reg[[length(reg)+1]] <- comprobar("Variables donde la imputacion obtiene menor error",
    sum(mi$error_absoluto_mediano < md$error_absoluto_mediano[o]), 7, 0.5)
}

x <- leer(FUENTES[["Coincidencias en la hora de registro"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Coincidencias de registro, proporcion minima",
    min(x$pct), 25.46, tolerancia(2))
  reg[[length(reg)+1]] <- comprobar("Coincidencias de registro, proporcion maxima",
    max(x$pct), 40.05, tolerancia(2))
  reg[[length(reg)+1]] <- comprobar("Determinaciones simultaneas, maximo",
    max(x$maximo_coincidentes), 38, 0.5)
}

x <- leer(FUENTES[["Coincidencias en bioquimica"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Coincidencias en laboratorio, proporcion",
    x$pct, 0.1, t1)
}

x <- leer(FUENTES[["Divergencia de la extraccion"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Divergencia material maxima por variable",
    max(x$pct_material), 0.11, tolerancia(3))
}

x <- leer(FUENTES[["Determinismo de la extraccion"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Ejecuciones de la consulta corregida",
    x$ejecuciones, 3, 0.5)
  reg[[length(reg)+1]] <- comprobar("Ejecuciones identicas a la primera",
    x$identicas_a_la_primera, 3, 0.5)
  reg[[length(reg)+1]] <- comprobar("Grupos que empatan en las dos primeras claves",
    x$grupos_ambiguos, 82, 0.5)
}

# La cobertura antes y despues procede de dos tablas distintas. Se comprueba
# que el recuento de estancias coincida: sin esa igualdad los dos porcentajes
# no serian comparables y la afirmacion sobre el desplazamiento no se
# sostendria.
x <- leer(FUENTES[["Cobertura antes y despues del desempate"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Cobertura, denominadores que no coinciden",
    sum(x$n_anterior != x$n_determinista), 0, 0.5)
  reg[[length(reg)+1]] <- comprobar("Desplazamiento maximo de cobertura",
    max(abs(x$diferencia)), 0.1, t1)
}

# ---------------------------------------------------------------------------
# Cobertura condicional bajo el criterio unico
#
# Se contrastan los recuentos y las comparaciones, que es donde un error de
# codigo produciria una cifra equivocada. Las celdas de la tabla de intervalos
# que el documento reproduce no se contrastan una a una: se componen leyendo
# directamente de este mismo archivo, de modo que compararlas contra el seria
# compararlo consigo mismo.
# ---------------------------------------------------------------------------

x <- leer(FUENTES[["Intervalos de cobertura por celda"]])
if (!is.null(x)) {
  fam <- x$seccion == "dejando una sede fuera"
  sel <- x$seccion == "unidad reservada"
  reg[[length(reg)+1]] <- comprobar("Celdas contrastadas en la familia",
    sum(fam), 20, 0.5)
  reg[[length(reg)+1]] <- comprobar("Celdas con intervalo bajo el nominal",
    sum(fam & x$beta_por_debajo), 5, 0.5)
  reg[[length(reg)+1]] <- comprobar("Celdas que resisten la correccion",
    sum(fam & x$resiste_beta_holm_0025), 3, 0.5)
  reg[[length(reg)+1]] <- comprobar("Sedes que fallan bajo la correccion",
    length(unique(x$sede[fam & x$resiste_beta_holm_0025])), 3, 0.5)
  reg[[length(reg)+1]] <- comprobar("Sedes evaluadas por celda",
    length(unique(x$sede[fam])), 5, 0.5)
  sd_ <- x[fam & x$beta_por_debajo & !x$resiste_beta_holm_0025, ]
  sd_ <- sd_[order(-sd_$cobertura), ]
  reg[[length(reg)+1]] <- comprobar("Cobertura sin demostracion, la mayor",
    sd_$cobertura[1], 0.7818, t4)
  reg[[length(reg)+1]] <- comprobar("Cobertura sin demostracion, la menor",
    sd_$cobertura[2], 0.7586, t4)
  reg[[length(reg)+1]] <- comprobar("Casos de la celda sin demostracion mayor",
    sd_$n[1], 55, 0.5)
  reg[[length(reg)+1]] <- comprobar("Casos de la celda sin demostracion menor",
    sd_$n[2], 58, 0.5)
  reg[[length(reg)+1]] <- comprobar("Unidad reservada, celdas bajo el nominal",
    sum(sel & x$beta_por_debajo), 0, 0.5)
  an <- (x$ic_superior[fam & x$beta_por_debajo] -
         x$ic_inferior[fam & x$beta_por_debajo]) /
        (x$ic_beta_superior[fam & x$beta_por_debajo] -
         x$ic_beta_inferior[fam & x$beta_por_debajo])
  reg[[length(reg)+1]] <- comprobar("Estrechamiento minimo al fijar el umbral",
    100 * (1 - max(an)), 10, 0.5)
  reg[[length(reg)+1]] <- comprobar("Estrechamiento maximo al fijar el umbral",
    100 * (1 - min(an)), 34, 0.5)
}

x <- leer(FUENTES[["Recuentos bajo cada correccion"]])
if (!is.null(x)) {
  b <- x[grepl("^beta_", x$correccion), ]
  reg[[length(reg)+1]] <- comprobar("Recuentos distintos bajo la correccion",
    length(unique(b$celdas_resisten)), 1, 0.5)
  reg[[length(reg)+1]] <- comprobar("Celdas que resistirian sin calibracion, Holm al nivel convencional",
    x$celdas_resisten[x$correccion == "holm" & x$nivel == 0.05], 4, 0.5)
  reg[[length(reg)+1]] <- comprobar("Celdas que resistirian sin calibracion, Holm al nivel adoptado",
    x$celdas_resisten[x$correccion == "holm" & x$nivel == 0.025], 4, 0.5)
  reg[[length(reg)+1]] <- comprobar("Falsas positivas esperadas por azar",
    20 * max(x$nivel), 1, 0.05)
}

x <- leer(FUENTES[["Causa del fallo por sede"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Sedes que fallan en la mayoritaria",
    x$sedes[x$clases == "sin_crecimiento"], 2, 0.5)
  reg[[length(reg)+1]] <- comprobar("Sedes que fallan en respiratorio",
    x$sedes[x$clases == "respiratorio"], 1, 0.5)
  reg[[length(reg)+1]] <- comprobar("Sedes sin fallo demostrable",
    x$sin_fallo_demostrable[1], 2, 0.5)
}

x <- leer(FUENTES[["Calibracion en la unidad reservada"]])
if (!is.null(x)) {
  s <- x[which.max(x$n), ]
  reg[[length(reg)+1]] <- comprobar("Sellada, probabilidad media mayoritaria",
    s$probabilidad_media, 0.9054, t4)
  reg[[length(reg)+1]] <- comprobar("Sellada, frecuencia observada mayoritaria",
    s$frecuencia_observada, 0.9802, t4)
}

x <- leer(FUENTES[["Validacion interna de la penalizacion"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Validacion interna, mejora",
    x$mejora_interna, 0.0375, t4)
  reg[[length(reg)+1]] <- comprobar("Validacion interna, mejora original",
    x$mejora_original, 0.0395, t4)
  reg[[length(reg)+1]] <- comprobar("Validacion interna, estancias de ajuste",
    x$pacientes_ajuste, 5578, 0.5)
  reg[[length(reg)+1]] <- comprobar("Validacion interna, estancias apartadas",
    x$pacientes_validacion, 2791, 0.5)
}

x <- leer(FUENTES[["Indicador de tubo endotraqueal"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Intubacion entre los respiratorios",
    x$pct_intubado[x$clase == "respiratorio"], 72.2, t1)
  reg[[length(reg)+1]] <- comprobar("Intubacion entre los sin crecimiento",
    x$pct_intubado[x$clase == "sin_crecimiento"], 39.3, t1)
}

x <- leer(FUENTES[["Limites de la recalibracion local"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Tamanos locales del ejercicio",
    nrow(x), 7, 0.5)
}

# La afirmacion publicada es que la regla del maximo no produjo ninguna
# conclusion minoritaria con independencia del metodo. Sostenerla exige que
# el recuento sea cero y que los cuatro metodos figuren en la tabla.
x <- leer(FUENTES[["Regla del maximo"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Regla del maximo, conclusiones minoritarias",
    sum(x$conclusiones_minoritarias), 0, 0.5)
  reg[[length(reg)+1]] <- comprobar("Regla del maximo, metodos contrastados",
    nrow(x), 4, 0.5)
}

# La afirmacion publicada es que ninguna estancia cambia de etiqueta entre
# las dos escalas de desempate. El cruce solo la sostiene si cubre la
# cohorte completa, de modo que el total tambien se contrasta.
x <- leer(FUENTES[["Divergencia de etiquetado"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Estancias que cambian de etiqueta",
    sum(x$estancias[!x$esperada]), 0, 0.5)
  reg[[length(reg)+1]] <- comprobar("Estancias cubiertas por el cruce de etiquetas",
    sum(x$estancias), 23213, 0.5)
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
