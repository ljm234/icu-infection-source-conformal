library(jsonlite)

options(scipen = 999)

# Estado de la guardia de nombres en cada deposito versionado.
#
# Los manifiestos escritos desde que la guardia existe lo declaran. Los
# anteriores no llevan el campo, y no se les inventa. Pero su estado no es
# desconocido: la guardia entro en el repositorio en una fecha que el
# historial fija, y un deposito escrito antes de esa fecha se escribio sin
# ella por construccion. Eso es un hecho derivable, no una suposicion, y
# decirlo es mejor que dejar cinco fases centrales sin declarar.
#
# La derivacion no toca los procedimientos que escribieron esos depositos.
# Compara la marca de ejecucion que cada manifiesto ya consigna contra la
# fecha del commit que introdujo la guardia, que se lee del historial y no se
# escribe aqui.
#
# Lo que esto NO acredita: que aquellos depositos esten libres de
# emparejamiento parcial. Acredita que se escribieron sin vigilancia. Cerrar
# esa duda exige reejecutarlos con la guardia puesta y cotejar, y eso es otra
# tarea que este procedimiento no hace ni sustituye.

OUT <- "outputs/fase37"
PERFIL <- ".Rprofile"
OPCION <- "warnPartialMatchDollar"
CAMPO <- "guarda_de_emparejamiento_parcial"

detener <- function(...) {
  cat("\n", ..., "\n", sep = "")
  cat("El procedimiento se detiene. No procede concluir sobre esta base.\n")
  quit(status = 1)
}

if (!dir.exists(".git")) detener("No hay historial que consultar.")
if (!file.exists(PERFIL)) detener("Fuente ausente: ", PERFIL)

ejecutar <- function(cmd) {
  r <- suppressWarnings(system(cmd, intern = TRUE))
  if (!is.null(attr(r, "status")) && attr(r, "status") != 0)
    detener("El comando fallo: ", cmd)
  r
}

# El commit que introdujo la guardia. La busqueda por contenido devuelve los
# commits en que la opcion aparece o desaparece del perfil; el mas antiguo es
# el que la introdujo. Aqui la busqueda por token si es la adecuada: lo que
# interesa es cuando aparecio el nombre de la opcion, no como cambio despues.
CMD <- sprintf("git log --format='%%at %%H %%ad' --date=short -S'%s' -- %s",
               OPCION, PERFIL)
cat("=== COMANDO ACREDITADO ===\n  ", CMD, "\n", sep = "")
hist <- ejecutar(CMD)
if (length(hist) == 0)
  detener("El historial no registra la introduccion de la guardia.")
alta <- strsplit(hist[length(hist)], " ")[[1]]
T_GUARDIA <- as.numeric(alta[1])
SHA_GUARDIA <- alta[2]
F_GUARDIA <- alta[3]

cat("\n=== LA GUARDIA ===\n")
cat("Introducida en", SHA_GUARDIA, "el", F_GUARDIA, "\n")
cat("Commits que tocan la opcion en el perfil:", length(hist), "\n")

mfs <- ejecutar("git ls-files 'outputs/*/manifiesto.json'")
if (length(mfs) == 0) detener("No hay manifiestos versionados.")

filas <- do.call(rbind, lapply(mfs, function(m) {
  d <- fromJSON(m)
  if (is.null(d$ejecutado_en))
    detener("El manifiesto ", m, " no consigna marca de ejecucion, de modo ",
            "que su estado no puede derivarse.")
  # La marca lleva el desfase horario pegado. Se normaliza para que el
  # analisis no dependa de la zona en que se ejecute esta comprobacion.
  t <- as.numeric(as.POSIXct(sub("([+-][0-9]{2}):?([0-9]{2})$", "\\1\\2",
                                 d$ejecutado_en),
                             format = "%Y-%m-%dT%H:%M:%S%z"))
  if (is.na(t)) detener("La marca de ", m, " no tiene forma de instante.")
  decl <- d[[CAMPO]]
  anterior <- t < T_GUARDIA
  data.frame(
    manifiesto = m,
    fase = if (is.null(d$fase)) NA_character_ else as.character(d$fase),
    ejecutado_en = d$ejecutado_en,
    declara_la_guardia = if (is.null(decl)) NA else isTRUE(decl),
    anterior_a_la_guardia = anterior,
    guardia_activa = if (!is.null(decl)) isTRUE(decl)
                     else if (anterior) FALSE else NA,
    origen = if (!is.null(decl)) "declarado en el manifiesto"
             else if (anterior) "derivado del historial"
             else "indeterminado",
    row.names = NULL)
}))
filas <- filas[order(filas$origen, filas$manifiesto), ]

cat("\n=== ESTADO POR DEPOSITO ===\n")
print(filas[, c("fase", "ejecutado_en", "guardia_activa", "origen")],
      row.names = FALSE)

# Un manifiesto sin campo y posterior a la guardia no admite derivacion: seria
# un deposito nuevo escrito sin declararlo, que es justo la via por la que la
# guardia se eludiria sin dejar rastro.
ind <- filas$origen == "indeterminado"
if (any(ind)) {
  cat("\nManifiestos posteriores a la guardia que no la declaran:\n")
  for (m in filas$manifiesto[ind]) cat("  ", m, "\n")
  detener("Un deposito posterior a la guardia no declara su estado.")
}

n_decl <- sum(filas$origen == "declarado en el manifiesto")
n_der  <- sum(filas$origen == "derivado del historial")
n_sin  <- sum(!filas$guardia_activa)

cat("\n=== RECUENTO ===\n")
cat("Manifiestos versionados:", nrow(filas), "\n")
cat("Con la guardia declarada:", n_decl, "\n")
cat("Con la guardia derivada del historial:", n_der, "\n")
cat("Escritos sin la guardia:", n_sin, "\n")
cat("\nLo que esto no acredita: que los depositos anteriores esten libres de\n")
cat("emparejamiento parcial. Acredita que se escribieron sin vigilancia.\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(filas, file.path(OUT, "guarda_por_deposito.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "37",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  guarda_de_emparejamiento_parcial = isTRUE(getOption("warnPartialMatchDollar")),
  proposito = paste("declarar, para cada deposito versionado, si la guardia",
                    "de nombres estaba activa cuando se escribio"),
  guardia_introducida_en = SHA_GUARDIA,
  guardia_introducida_el = F_GUARDIA,
  comando = CMD,
  derivacion = paste("un manifiesto sin el campo cuya marca de ejecucion sea",
                     "anterior al commit que introdujo la guardia se escribio",
                     "sin ella por construccion; no es una suposicion sino un",
                     "hecho del historial"),
  lo_que_no_acredita = paste("que los depositos anteriores esten libres de",
                             "emparejamiento parcial; acredita que se",
                             "escribieron sin vigilancia. Cerrar esa duda",
                             "exige reejecutarlos con la guardia y cotejar"),
  manifiestos = nrow(filas),
  declarados = n_decl,
  derivados = n_der,
  sin_guardia = n_sin), auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
