# Auditoria previa a la publicacion. Reune tres comprobaciones cuyo resultado
# debe constar en el propio deposito y no unicamente en la salida por
# pantalla, de modo que cualquiera pueda repetirlas.
#
# La primera examina si algun archivo bajo control de versiones contiene
# identificadores por paciente. El acuerdo de uso de PhysioNet prohibe la
# redistribucion de datos derivados, y su publicacion podria acarrear la
# perdida del acceso. La comprobacion inspecciona el encabezado de cada
# archivo en lugar de confiar en las reglas de exclusion, dado que estas
# expresan una intencion y no un hecho verificado.

cat("=== ARCHIVOS BAJO CONTROL DE VERSIONES CON IDENTIFICADORES ===\n")

versionados <- system("git ls-files '*.csv'", intern = TRUE)
cat("Archivos de datos versionados:", length(versionados), "\n")

peligrosos <- sapply(versionados, function(f) {
  if (!file.exists(f)) return(FALSE)
  h <- try(readLines(f, n = 1, warn = FALSE), silent = TRUE)
  if (inherits(h, "try-error") || length(h) == 0) return(FALSE)
  grepl("stay_id|subject_id|hadm_id", h)
})

if (any(peligrosos)) {
  cat("\nARCHIVOS QUE CONTIENEN IDENTIFICADORES POR PACIENTE\n")
  for (f in versionados[peligrosos]) cat("  ", f, "\n")
} else {
  cat("Ninguno contiene identificadores por paciente.\n")
}

binarios <- system("git ls-files '*.rds'", intern = TRUE)
cat("\nObjetos binarios versionados:", length(binarios), "\n")
for (f in binarios) {
  tam <- if (file.exists(f)) file.size(f) else NA
  cat(sprintf("  %-44s %8s bytes\n", substr(f, 1, 44),
              if (is.na(tam)) "?" else format(tam)))
}
cat("Los objetos de reducido tamano contienen coeficientes y nudos, no datos.\n")

derivados <- system("git ls-files 'data/'", intern = TRUE)
cat("\nContenido del directorio de derivados bajo control de versiones:",
    length(derivados), "\n")
if (length(derivados) > 0) for (f in derivados) cat("  ", f, "\n")

PROTEGIDAS <- c("data",
  "outputs/fase4/matriz.csv", "outputs/fase4/matriz_limpia.csv",
  "outputs/fase5/matriz_particionada.csv", "outputs/fase5/SELLADO_NO_ABRIR.csv",
  "outputs/fase6/imputaciones.rds", "outputs/fase6/objeto_mice.rds",
  "outputs/fase6/objeto_mice_ampliado.rds",
  "outputs/fase7/prob_calibracion.csv", "outputs/fase7/prob_prueba.csv",
  "outputs/fase8/prob_calibracion.csv", "outputs/fase8/prueba_con_conjuntos.csv",
  "outputs/fase11/sellado_evaluado.csv", "outputs/fase15/matriz_peor_valor.csv")

historial <- system(paste("git log --all --oneline --",
  paste(PROTEGIDAS, collapse = " "), "2>/dev/null"), intern = TRUE)
cat("\nRegistros del historial que alcanzan datos por paciente:",
    length(historial), "\n")
if (length(historial) > 0) for (l in historial) cat("  ", l, "\n")

seguro <- !any(peligrosos) && length(derivados) == 0 && length(historial) == 0

# La relacion de afirmaciones consigna la ruta efectiva de cada fuente. En la
# version anterior figuraban expresiones descriptivas en lugar de rutas, de
# modo que el procedimiento no las hallaba entre los archivos y notificaba
# ausencia de respaldo alli donde este existia desde la fase segunda.
cat("\n=== RESPALDO DOCUMENTAL DE LAS AFIRMACIONES ===\n")

AFIRMACIONES <- list(
  c("Embudo de seleccion de la cohorte", "outputs/fase2/flujo.csv"),
  c("Distribucion de categorias", "outputs/fase2/clases.csv"),
  c("Unidades elegibles", "outputs/fase2/unidades_elegibles.csv"),
  c("Cobertura de las determinaciones", "outputs/fase3/cobertura_labs.csv"),
  c("Repeticion en la ventana", "outputs/fase3/repeticion.csv"),
  c("Distancia entre unidades", "outputs/fase5/distancia_unidades.csv"),
  c("Comparacion de reglas de penalizacion", "outputs/fase7/comparacion_lambda.csv"),
  c("Cobertura conforme condicional", "outputs/fase8/cobertura.csv"),
  c("Compromiso segun nivel de confianza", "outputs/fase9/curva_alfa.csv"),
  c("Transportabilidad entre unidades", "outputs/fase10/cobertura_louo.csv"),
  c("Validacion en la unidad reservada", "outputs/fase11/cobertura_sellado.csv"),
  c("Limite de la recalibracion local", "outputs/fase12/recalibracion.csv"),
  c("Comparacion con referencias simples", "outputs/fase13/referencias.csv"),
  c("Sensibilidad al lactato", "outputs/fase15/sensibilidad_lactato.csv"),
  c("Reglas de agregacion", "outputs/fase15/comparacion_agregacion.csv"),
  c("Curvas de decision", "outputs/fase16/curvas_decision.csv"),
  c("Comparador con arboles potenciados", "outputs/fase16/gbm_comparacion.csv"),
  c("Cobertura de constantes vitales", "outputs/fase17/cobertura_vitales.csv"),
  c("Exclusion de la presion arterial", "outputs/fase17/concordancia_presion.csv"),
  c("Exclusion de la escala de conciencia", "outputs/fase17/circularidad_glasgow.csv"),
  c("Discriminacion del modelo ampliado", "outputs/fase18/comparacion_ampliado.csv"),
  c("Forma funcional de las constantes", "outputs/fase18/ganancia_splines.csv"),
  c("Transportabilidad del modelo ampliado", "outputs/fase19/cobertura_louo_ampliado.csv"),
  c("Validacion de la imputacion", "outputs/fase20/fraccion_informacion_faltante.csv"),
  c("Dispersion entre imputaciones", "outputs/fase20/dispersion_imputaciones.csv"),
  c("Coincidencias en la hora de registro", "outputs/fase20/empates_constantes.csv"),
  c("Divergencia de la extraccion", "outputs/fase20/divergencia_extraccion.csv"),
  c("Determinismo de la extraccion", "outputs/fase20/determinismo_extraccion.csv"),
  c("Cobertura antes y despues", "outputs/fase20/cobertura_extraccion.csv"),
  c("Validacion interna de lambda", "outputs/fase22/decision_lambda.csv"),
  c("Comparacion interna por categoria", "outputs/fase22/comparacion_lambda_interna.csv"),
  c("Particion de la validacion interna", "outputs/fase22/particion_interna.csv"),
  c("Calibracion en la unidad reservada", "outputs/fase23/calibracion_sellado.csv"),
  c("Regla del maximo por metodo", "outputs/fase24/regla_maximo.csv"),
  c("Divergencia de etiquetado", "outputs/fase25/divergencia_etiquetado.csv"),
  c("Sitios positivos en la cohorte", "outputs/fase25/multisitio_cohorte.csv"),
  c("Intervalos de cobertura por celda", "outputs/fase26/intervalos_cobertura.csv"),
  c("Recuentos bajo cada correccion", "outputs/fase26/recuentos_multiplicidad.csv"),
  c("Causa del fallo por sede", "outputs/fase26/causas_fallo.csv"),
  c("Criterios de cobertura", "outputs/fase26/criterios_cobertura.csv"),
  c("Procedencia de los manifiestos", "outputs/fase27/procedencia_manifiestos.csv"),
  c("Cobertura de constantes por etapa", "outputs/fase28/cobertura_vitales_por_etapa.csv"))

faltan <- 0
for (a in AFIRMACIONES) {
  ok <- file.exists(a[2])
  if (!ok) faltan <- faltan + 1
  cat(sprintf("%-42s %s\n", substr(a[1], 1, 42),
              if (ok) "respaldada" else "FUENTE AUSENTE"))
}
cat("\nAfirmaciones:", length(AFIRMACIONES),
    " Respaldadas:", length(AFIRMACIONES) - faltan,
    " Sin respaldo:", faltan, "\n")

# Registro de los hallazgos relativos a la reproducibilidad de la extraccion.
# Las cifras se leen de los archivos depositados.
cat("\n=== HALLAZGOS SOBRE LA REPRODUCIBILIDAD DE LA EXTRACCION ===\n")

ec <- read.csv("outputs/fase20/empates_constantes.csv", stringsAsFactors = FALSE)
el <- read.csv("outputs/fase20/empates_laboratorio.csv", stringsAsFactors = FALSE)
dv <- read.csv("outputs/fase20/divergencia_extraccion.csv", stringsAsFactors = FALSE)
det <- read.csv("outputs/fase20/determinismo_extraccion.csv", stringsAsFactors = FALSE)
cb <- read.csv("outputs/fase20/cobertura_extraccion.csv", stringsAsFactors = FALSE)

cat("Coincidencias en la hora de registro, constantes vitales\n")
cat("  minimo:", round(min(ec$pct), 2), "por ciento\n")
cat("  maximo:", round(max(ec$pct), 2), "por ciento\n")
cat("  determinaciones simultaneas maximas:", max(ec$maximo_coincidentes), "\n")
cat("Coincidencias en determinaciones bioquimicas:",
    round(el$pct, 2), "por ciento\n")
cat("\nDivergencia material tras el desempate explicito\n")
print(dv[, c("variable","distintos_numericamente",
             "distintos_materialmente","pct_material")], row.names = FALSE)
cat("\nMaxima divergencia material por constante:",
    round(max(dv$pct_material), 3), "por ciento\n")
cat("\nDeterminismo de la consulta corregida\n")
cat("  ejecuciones identicas a la primera:", det$identicas_a_la_primera,
    "de", det$ejecuciones, "\n")
cat("  grupos que empatan en las dos primeras claves con valor distinto:",
    det$grupos_ambiguos, "\n")
cat("  denominadores de cobertura coincidentes:",
    all(cb$n_anterior == cb$n_determinista), "\n")
cat("  desplazamiento maximo de cobertura:",
    max(abs(cb$diferencia)), "puntos\n")

# ---------------------------------------------------------------------------
# Guardias de composicion. Este procedimiento escribe un documento publicado y
# era el unico de los tres generadores que componia sus lineas sin comprobacion
# alguna. Se traslada el mecanismo ya probado en los otros dos: una puerta
# admite unicamente prosa y se detiene ante cualquier digito; la otra admite un
# patron de formato acompanado de valores leidos de archivo, y se detiene si el
# patron carece de codigo de sustitucion, si contiene digitos ajenos a el, o si
# el resultado no constituye exactamente una linea.
# ---------------------------------------------------------------------------

EXENTAS <- c("R/64_auditoria_publicacion.R", "R/19_matriz.R")

despojar <- function(s) {
  for (e in EXENTAS) s <- gsub(e, "", s, fixed = TRUE)
  s
}

tiene_digito <- function(s) grepl("[0-9]", despojar(s))

patron_con_digito <- function(fmt)
  grepl("[0-9]", despojar(gsub("%[-+0-9.]*[sdfe]", "", fmt)))

compone_una_linea <- function(fmt, ...) length(sprintf(fmt, ...)) == 1

cat("\n=== PRUEBAS DE LAS GUARDIAS ===\n")

p1 <- tiene_digito("texto con el numero 7 escrito a mano")
cat("Detecta cifra en la prosa:", if (p1) "si" else "NO", "\n")

p2 <- !tiene_digito("texto sin cifra alguna")
cat("Admite prosa limpia:", if (p2) "si" else "NO", "\n")

p3 <- patron_con_digito("valor fijo de 86 unidades")
cat("Detecta cifra en un patron:", if (p3) "si" else "NO", "\n")

p4 <- !patron_con_digito("valor de %.4f unidades")
cat("Admite patron con codigo de sustitucion:", if (p4) "si" else "NO", "\n")

p5 <- !compone_una_linea("texto sin sustitucion", NULL)
cat("Detecta composicion con argumento nulo:", if (p5) "si" else "NO", "\n")

p6 <- !compone_una_linea("valor %d", c(1, 2, 3))
cat("Detecta composicion multiple:", if (p6) "si" else "NO", "\n")

p7 <- compone_una_linea("valor %.2f", 3.14)
cat("Admite composicion valida:", if (p7) "si" else "NO", "\n")

if (!all(c(p1, p2, p3, p4, p5, p6, p7))) {
  cat("\nLas guardias no superan sus pruebas. No procede componer.\n")
  quit(status = 1)
}
cat("Las siete pruebas se superan.\n")

prosa <- function(...) {
  x <- c(...)
  for (s in x)
    if (tiene_digito(s)) {
      cat("Cifra literal en la prosa:\n  ", s, "\n"); quit(status = 1)
    }
  x
}

cifra <- function(fmt, ...) {
  if (!grepl("%[-+0-9.]*[sdfe]", fmt)) {
    cat("Patron sin codigo de sustitucion. Corresponde a prosa:\n  ", fmt, "\n")
    quit(status = 1)
  }
  if (patron_con_digito(fmt)) {
    cat("Cifra literal en un patron:\n  ", fmt, "\n"); quit(status = 1)
  }
  if (!compone_una_linea(fmt, ...)) {
    cat("La composicion no produjo una linea:\n  ", fmt, "\n"); quit(status = 1)
  }
  sprintf(fmt, ...)
}

L <- character(0)
add <- function(...) L <<- c(L, ...)

add(prosa(
"# Reproducibility of the extraction step",
""))

add(cifra("Generated on %s by R/64_auditoria_publicacion.R",
          format(Sys.Date(), "%Y-%m-%d")))

add(prosa(
"",
"## Finding",
"",
"Nursing observations in MIMIC-IV are validated in batches, so several",
"measurements of the same variable share an identical storetime. Selecting",
"the first measurement by ordering on storetime alone leaves ties",
"unresolved, and the row retained can differ between runs of the same",
"query.",
""))

add(cifra("Ties affect between %.2f and %.2f percent of stays depending on the",
          min(ec$pct), max(ec$pct)))
add(cifra("variable, with up to %d simultaneous measurements of a single",
          as.integer(max(ec$maximo_coincidentes))))

add(prosa(
"variable in one stay.",
""))

add(cifra(
  "Laboratory results are affected in %.2f percent of stays. Analysers",
  el$pct))

add(prosa(
"timestamp each result individually, which makes ties far rarer there, but",
"not absent.",
"",
"## Correction",
"",
"Ordering now uses four keys: storetime, then charttime, then the value",
"after unit conversion, then itemid. Ordering on the converted value",
"matters because temperature is recorded under two itemids in different",
"scales, so the raw figure places readings of very different temperatures",
"side by side.",
"",
"Ties on the first two keys are real:"))

add(cifra("%d groups of measurements agree on stay_id, storetime and charttime while",
          as.integer(det$grupos_ambiguos)))

add(prosa(
"disagreeing on the converted value, which is why that value enters the",
"ordering rather than closing it."))

add(cifra("The query was run %d times and all %d results are identical.",
          as.integer(det$ejecuciones), as.integer(det$identicas_a_la_primera)))

add(prosa(
"",
"## Impact",
""))

add(cifra("Material divergence from the earlier extraction reaches %.3f percent",
          max(dv$pct_material)))
add(cifra("of stays per variable, and no coverage figure moves by more than %.1f",
          max(abs(cb$diferencia))))

add(prosa(
"points. Coverage before and after is measured on the same stays.",
"The published figures come from the earlier extraction; the deterministic",
"version is provided alongside and the divergence is quantified above.",
"",
"The core phases, up to and including the sealed model, draw only on",
"laboratory results. Their extraction in `R/19_matriz.R` orders by storetime",
"alone, which is the pattern corrected here, and this correction re-extracted",
"the vital signs only. No versioned file bounds the divergence the core",
"phases could carry, so that limitation stands unquantified. They are not",
"re-extracted, since rebuilding the cohort after the sealed set has been",
"opened would void the external validation."))

writeLines(L, "outputs/fase20/REPRODUCIBILITY.md")
cat("\nRegistro escrito en outputs/fase20/REPRODUCIBILITY.md\n")

cat("\n=== RESULTADO DE LA AUDITORIA ===\n")
cat("Deposito libre de datos por paciente:", if (seguro) "si" else "NO", "\n")
cat("Afirmaciones sin respaldo:", faltan, "\n")

if (!seguro || faltan > 0) {
  cat("\nLa auditoria no se supera. No procede publicar.\n")
  quit(status = 1)
}
cat("\nAuditoria superada. El deposito admite publicacion.\n")
