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

# Se enumeran las FASES con deposito versionado, no los manifiestos. Enumerar
# manifiestos dejaba fuera a toda fase que no lo tuviera, que son quince de
# cuarenta y dos, y entre ellas la validacion por sedes y la evaluacion de la
# unidad reservada. Una guardia que no puede dispararse donde no hay
# manifiesto no cubre lo que su proposito decia cubrir.
versionados <- ejecutar("git ls-files 'outputs/*/*'")
fases <- sort(unique(dirname(versionados)))
if (length(fases) == 0) detener("No hay fases con deposito versionado.")

fecha_a_instante <- function(x)
  as.numeric(as.POSIXct(paste(x, "23:59:59"), tz = "UTC"))

filas <- do.call(rbind, lapply(fases, function(f) {
  m <- file.path(f, "manifiesto.json")
  # Primer y ultimo commit de cualquiera de sus archivos. El primero acota
  # cuando el deposito entro; el ultimo, cuando cambio por ultima vez. Se
  # consignan los dos porque decir solo el primero ocultaria que cinco fases
  # se han reescrito despues de existir la guardia.
  ca <- ejecutar(sprintf("git log --format=%%ad --date=short --reverse -- %s",
                         shQuote(f)))
  cu <- ejecutar(sprintf("git log -1 --format=%%ad --date=short -- %s",
                         shQuote(f)))
  # Una fase que el historial no registra todavia es la que se esta
  # introduciendo. Si trae manifiesto, su marca de ejecucion resuelve la
  # pregunta y las fechas del historial son informativas; si no lo trae, no
  # hay de donde derivar y el procedimiento se detiene.
  primero <- if (length(ca) > 0) ca[1] else ""
  ultimo  <- if (length(cu) > 0) cu[1] else ""
  if (!file.exists(m) && primero == "")
    detener("La fase ", f, " no tiene manifiesto y el historial no la ",
            "registra. Su estado no puede derivarse.")
  if (!file.exists(m)) {
    # Sin manifiesto no hay marca de ejecucion, de modo que se deriva del
    # historial: un deposito cuyo primer commit es anterior a la guardia se
    # escribio sin ella por construccion. Es conservador para las fases que
    # despues se rehicieron, y esa es la direccion segura.
    ant <- fecha_a_instante(primero) < T_GUARDIA
    return(data.frame(
      fase = basename(f), manifiesto = "", ejecutado_en = "",
      primer_commit = primero, ultimo_commit = ultimo,
      rehecha_tras_la_guardia = fecha_a_instante(ultimo) >= T_GUARDIA,
      declara_la_guardia = NA,
      anterior_a_la_guardia = ant,
      guardia_activa = if (ant) FALSE else NA,
      origen = if (ant) "derivado del historial, sin manifiesto"
               else "indeterminado",
      row.names = NULL))
  }
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
    fase = basename(f),
    manifiesto = m,
    ejecutado_en = d$ejecutado_en,
    primer_commit = primero, ultimo_commit = ultimo,
    rehecha_tras_la_guardia = ultimo != "" &&
                              fecha_a_instante(ultimo) >= T_GUARDIA,
    declara_la_guardia = if (is.null(decl)) NA else isTRUE(decl),
    anterior_a_la_guardia = anterior,
    guardia_activa = if (!is.null(decl)) isTRUE(decl)
                     else if (anterior) FALSE else NA,
    origen = if (!is.null(decl)) "declarado en el manifiesto"
             else if (anterior) "derivado de la marca de ejecucion"
             else "indeterminado",
    row.names = NULL)
}))
filas <- filas[order(filas$origen, filas$fase), ]

cat("\n=== ESTADO POR FASE ===\n")
print(filas[, c("fase", "primer_commit", "ultimo_commit", "guardia_activa",
                "origen")], row.names = FALSE)

# Queda indeterminada una fase posterior a la guardia que no la declara: con
# manifiesto, seria un deposito nuevo escrito sin declararlo; sin manifiesto,
# una fase entera nacida despues de la guardia y sin nada que la atestigue.
# Las dos son la via por la que la guardia se eludiria sin dejar rastro.
ind <- filas$origen == "indeterminado"
if (any(ind)) {
  cat("\nFases posteriores a la guardia cuyo estado no consta:\n")
  for (f in filas$fase[ind]) cat("  ", f, "\n")
  detener("Una fase posterior a la guardia no declara su estado.")
}

n_decl <- sum(filas$origen == "declarado en el manifiesto")
n_marca <- sum(filas$origen == "derivado de la marca de ejecucion")
n_hist <- sum(filas$origen == "derivado del historial, sin manifiesto")
n_sin  <- sum(!filas$guardia_activa)
n_sinm <- sum(filas$manifiesto == "")
n_rehe <- sum(filas$manifiesto == "" & filas$rehecha_tras_la_guardia)

cat("\n=== RECUENTO ===\n")
cat("Fases con deposito versionado:", nrow(filas), "\n")
cat("Con la guardia declarada en su manifiesto:", n_decl, "\n")
cat("Derivadas de la marca de ejecucion:", n_marca, "\n")
cat("Sin manifiesto, derivadas del historial:", n_hist, "\n")
cat("Escritas sin la guardia:", n_sin, "\n")
cat("Sin manifiesto y rehechas despues de la guardia:", n_rehe, "\n")
cat("\nLo que esto no acredita: que los depositos anteriores esten libres de\n")
cat("emparejamiento parcial. Acredita que se escribieron sin vigilancia.\n")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
write.csv(filas, file.path(OUT, "guarda_por_deposito.csv"), row.names = FALSE)

writeLines(toJSON(list(
  fase = "37",
  ejecutado_en = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  guarda_de_emparejamiento_parcial = isTRUE(getOption("warnPartialMatchDollar")),
  proposito = paste("declarar, para cada fase con deposito versionado, si la",
                    "guardia de nombres estaba activa cuando se escribio. Se",
                    "enumeran fases y no manifiestos: quince de las cuarenta y",
                    "dos no tienen manifiesto, y enumerar manifiestos las",
                    "dejaba fuera del alcance que este campo declaraba"),
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
  sin_manifiesto = paste("la fase sin manifiesto no tiene marca de ejecucion,",
                         "de modo que se deriva del primer commit de",
                         "cualquiera de sus archivos: si es anterior a la",
                         "guardia, el deposito se escribio sin ella por",
                         "construccion. Es conservador para las que despues",
                         "se rehicieron, y se consigna cuales son"),
  fases = nrow(filas),
  declarados = n_decl,
  derivados_de_la_marca = n_marca,
  derivados_del_historial = n_hist,
  sin_manifiesto_recuento = n_sinm,
  sin_manifiesto_rehechas = n_rehe,
  sin_guardia = n_sin), auto_unbox = TRUE, pretty = TRUE),
  file.path(OUT, "manifiesto.json"))

cat("\nDepositado en", OUT, "\n")
