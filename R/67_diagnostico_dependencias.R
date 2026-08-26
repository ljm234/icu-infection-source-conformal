# Comprobacion del alcance del archivo de dependencias.
#
# La version anterior notificaba dos ausencias, ambas espurias. La primera
# correspondia a una linea del propio procedimiento, que contiene la
# expresion buscada dentro de una cadena de texto y por tanto se detectaba a
# si mismo al carecer el patron de anclaje al inicio de linea. La segunda
# correspondia a un paquete de la distribucion basica del lenguaje, que
# acompana a toda instalacion y que el gestor de dependencias omite del
# archivo de bloqueo por ese motivo.
#
# Se corrige anclando el patron y excluyendo la distribucion basica,
# determinada por consulta al propio lenguaje y no mediante una relacion
# escrita a mano, que envejeceria en silencio.
#
# El procedimiento se somete ademas a dos pruebas antes de emitir su
# resultado, dado que en el curso del desarrollo varios procedimientos de
# verificacion han presentado defectos advertidos al examinar su salida y no
# por diseno.

com <- system("git show HEAD:renv.lock", intern = TRUE)
a <- names(jsonlite::fromJSON(paste(com, collapse = "\n"))$Packages)
basicos <- rownames(installed.packages(priority = "base"))

cat("=== PRUEBAS DEL PROPIO PROCEDIMIENTO ===\n")

p1 <- "splines" %in% basicos
cat("Reconoce splines como parte de la distribucion basica:",
    if (p1) "si" else "NO", "\n")

propio <- grep("^[[:space:]]*library\\(",
               readLines("R/67_diagnostico_dependencias.R", warn = FALSE),
               value = TRUE)
p2 <- length(propio) == 0
cat("No detecta cargas dentro de su propio texto:",
    if (p2) "si" else "NO", "\n")

if (!p1 || !p2) {
  cat("\nEl procedimiento no supera sus propias pruebas. No procede confiar\n")
  cat("en su resultado.\n")
  quit(status = 1)
}

cat("\n=== ARCHIVO DE DEPENDENCIAS ===\n")
cat("Paquetes distintos en la version comprometida:", length(a), "\n")
cat("Paquetes de la distribucion basica del lenguaje:", length(basicos), "\n")

todos <- list.files("R", full.names = TRUE)
scripts <- todos[endsWith(todos, ".R")]
cat("Procedimientos examinados:", length(scripts), "\n")

cargas <- lapply(scripts, function(f) {
  x <- grep("^[[:space:]]*library\\(", readLines(f, warn = FALSE), value = TRUE)
  if (length(x) == 0) return(character(0))
  unique(sub("^[[:space:]]*library\\(([A-Za-z0-9._]+)\\).*", "\\1", x))
})
usados <- sort(unique(unlist(cargas)))

cat("\n=== BIBLIOTECAS QUE LOS PROCEDIMIENTOS CARGAN ===\n")
for (p in usados) {
  estado <- if (p %in% basicos) "distribucion basica"
            else if (p %in% a) "presente en el archivo"
            else "AUSENTE"
  cat(sprintf("  %-14s %s\n", p, estado))
}

falta <- setdiff(setdiff(usados, a), basicos)
cat("\nCargadas, ausentes del archivo y ajenas a la distribucion basica:",
    if (length(falta)) paste(falta, collapse = ", ") else "ninguna", "\n")

rotos <- unlist(lapply(seq_along(scripts), function(i) {
  p <- cargas[[i]]
  if (length(p) == 0) return(NULL)
  if (length(setdiff(setdiff(p, a), basicos)) > 0) basename(scripts[i]) else NULL
}))

cat("Procedimientos que no arrancarian tras restaurar:", length(rotos), "\n")
if (length(rotos) > 0) for (r in rotos) cat("  ", r, "\n")

cat("\n=== RESULTADO ===\n")
if (length(falta) == 0) {
  cat("El archivo comprometido cubre la totalidad de las bibliotecas que los\n")
  cat("procedimientos requieren y que no acompanan al lenguaje. El entorno\n")
  cat("resulta restaurable por un tercero.\n")
} else {
  cat("El archivo comprometido esta incompleto. Procede una nueva instantanea\n")
  cat("antes de publicar.\n")
  quit(status = 1)
}
