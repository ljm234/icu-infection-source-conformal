# Registro de decisiones vigentes del proyecto.
#
# Recoge lo que ya esta decidido, con que fundamento, y en que archivo
# consta. Se consulta antes de escribir sobre cualquiera de estos puntos,
# de modo que lo que se publique se apoye en los archivos y no en el
# recuerdo de quien redacta. Esa es toda su funcion: el recuerdo se
# degrada y los archivos no.
#
# El documento se compone con las mismas guardias que los demas. Ninguna
# cifra se escribe a mano. A ellas se anaden dos propias de este registro.
#
# La primera somete cada ruta citada a una comprobacion doble: ha de
# existir en el disco y ha de estar bajo control de versiones. Lo segundo
# no se deduce de lo primero, y un registro que remita a un archivo sin
# versionar remite a algo que quien clone el repositorio no encontrara.
#
# La segunda recorre el documento terminado, extrae de el toda expresion
# con forma de ruta y verifica que cada una paso por la comprobacion
# anterior. Sin ella bastaria escribir una ruta dentro de la prosa para
# eludir el control, que es precisamente el descuido que este registro
# existe para prevenir.
#
# El caracter posterior de los analisis no se declara: se comprueba. La
# fecha en que cada procedimiento entro en el repositorio se lee del
# historial, y ha de ser posterior a aquella en que quedo fijada la
# seleccion de determinaciones, que consta en la fase trigesimo tercera.

EXENTAS <- c("R/82_decisiones_vivas.R")

despojar <- function(s) {
  for (e in EXENTAS) s <- gsub(e, "", s, fixed = TRUE)
  s
}

tiene_digito <- function(s) grepl("[0-9]", despojar(s))

patron_con_digito <- function(fmt)
  grepl("[0-9]", despojar(gsub("%[-+0-9.]*[sdfe]", "", fmt)))

compone_una_linea <- function(fmt, ...) length(sprintf(fmt, ...)) == 1

cat("=== PRUEBAS DE LAS GUARDIAS ===\n")

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

p8 <- !grepl("%[-+0-9.]*[sdfe]", "texto sin codigo de sustitucion")
cat("Detecta patron sin codigo de sustitucion:", if (p8) "si" else "NO", "\n")

VERSIONADOS <- system("git ls-files", intern = TRUE)
if (length(VERSIONADOS) == 0) {
  cat("El historial no responde. No procede componer.\n"); quit(status = 1)
}

p9 <- "README.md" %in% VERSIONADOS
cat("Reconoce un archivo versionado:", if (p9) "si" else "NO", "\n")

p10 <- !("outputs/fase4/matriz.csv" %in% VERSIONADOS)
cat("Reconoce un archivo excluido:", if (p10) "si" else "NO", "\n")

ext <- "[A-Za-z0-9_.~-]+/[A-Za-z0-9_./-]+[.](R|csv|json|md)"
p11 <- length(unlist(regmatches(
  "cita outputs/fase2/flujo.csv en la prosa",
  gregexpr(ext, "cita outputs/fase2/flujo.csv en la prosa")))) == 1
cat("Extrae una ruta escrita en la prosa:", if (p11) "si" else "NO", "\n")

p12 <- length(unlist(regmatches(
  "texto sin ruta alguna", gregexpr(ext, "texto sin ruta alguna")))) == 0
cat("No inventa rutas donde no las hay:", if (p12) "si" else "NO", "\n")

if (!all(c(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12))) {
  cat("\nLas guardias no superan sus pruebas. No procede componer.\n")
  quit(status = 1)
}
cat("Las doce pruebas se superan.\n\n")

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

CITADAS <- character(0)

ruta <- function(p) {
  if (!file.exists(p)) {
    cat("Ruta citada que no existe:", p, "\n"); quit(status = 1)
  }
  if (!(p %in% VERSIONADOS)) {
    cat("Ruta citada que no esta versionada:", p, "\n"); quit(status = 1)
  }
  CITADAS <<- union(CITADAS, p)
  p
}

leer <- function(p) read.csv(ruta(p), stringsAsFactors = FALSE)

fecha_alta <- function(p) {
  ruta(p)
  h <- system(paste("git log --diff-filter=A --format=%ad --date=short --",
                    shQuote(p)), intern = TRUE)
  if (length(h) == 0) {
    cat("El historial no registra el alta de:", p, "\n"); quit(status = 1)
  }
  h[length(h)]
}

# ---------------------------------------------------------------------------
# Fuentes y comprobacion de posterioridad.
# ---------------------------------------------------------------------------

proc  <- leer("outputs/fase33/procedencia_seleccion.csv")
comp  <- leer("outputs/fase31/casos_completos.csv")
cmp29 <- leer("outputs/fase32/comparacion_resumen.csv")
mlt   <- leer("outputs/fase32/multiplicidad_diferencias.csv")

FIJADA <- proc$fecha[1]
if (!grepl("^20[0-9]{2}-[0-9]{2}-[0-9]{2}$", FIJADA)) {
  cat("La fecha de fijacion no tiene forma de fecha.\n"); quit(status = 1)
}

POSTERIORES <- c(
  "R/70_lambda_validacion_interna.R",
  "R/74_intervalos_cobertura.R",
  "R/77_cohorte_analizada.R",
  "R/78_determinaciones_candidatas.R",
  "R/79_casos_completos_candidatas.R",
  "R/80_comparacion_ampliada_labs.R",
  "R/81_procedencia_seleccion.R")

cat("=== POSTERIORIDAD DE LOS ANALISIS ===\n")
ALTAS <- setNames(sapply(POSTERIORES, fecha_alta), POSTERIORES)
for (p in POSTERIORES) {
  ok <- ALTAS[[p]] > FIJADA
  cat(sprintf("  %-42s %s  %s\n", p, ALTAS[[p]],
              if (ok) "posterior" else "NO POSTERIOR"))
  if (!ok) {
    cat("\nUn analisis declarado posterior no lo es. No procede componer.\n")
    quit(status = 1)
  }
}
cat("Los", length(POSTERIORES), "son posteriores a la fijacion.\n\n")

# El recuento de rutas reservadas se lee del archivo de exclusiones, que es
# donde la decision surte efecto. Copiarlas aqui crearia una segunda lista
# que podria divergir de la primera sin que nada lo advirtiera.
gi <- readLines(ruta(".gitignore"))
i <- grep("Patient level derived data", gi)
if (length(i) != 1) {
  cat("El archivo de exclusiones no delimita las rutas reservadas.\n")
  quit(status = 1)
}
reservadas <- gi[(i + 1):length(gi)]
reservadas <- reservadas[nzchar(trimws(reservadas)) &
                         !grepl("^\\s*#", reservadas)]
if (length(reservadas) == 0) {
  cat("El archivo de exclusiones no relaciona ruta reservada alguna.\n")
  quit(status = 1)
}

# ---------------------------------------------------------------------------
# Composicion.
# ---------------------------------------------------------------------------

r19  <- ruta("R/19_matriz.R")
r32  <- ruta("R/32_comparar_lambda.R")
r59  <- ruta("R/59_verificar_cifras.R")
r64  <- ruta("R/64_auditoria_publicacion.R")
r65  <- ruta("R/65_generar_readme.R")
r67  <- ruta("R/67_diagnostico_dependencias.R")
r70  <- ruta("R/70_lambda_validacion_interna.R")
r74  <- ruta("R/74_intervalos_cobertura.R")
r77  <- ruta("R/77_cohorte_analizada.R")
r78  <- ruta("R/78_determinaciones_candidatas.R")
r79  <- ruta("R/79_casos_completos_candidatas.R")
r80  <- ruta("R/80_comparacion_ampliada_labs.R")
r81  <- ruta("R/81_procedencia_seleccion.R")
r82  <- ruta("R/82_decisiones_vivas.R")
lee  <- ruta("README.md")
trz  <- ruta("outputs/fase20/TRACEABILITY.md")
rep  <- ruta("outputs/fase20/REPRODUCIBILITY.md")
d22  <- ruta("outputs/fase22/decision_lambda.csv")
d25  <- ruta("outputs/fase25/divergencia_etiquetado.csv")
d26c <- ruta("outputs/fase26/criterios_cobertura.csv")
d26m <- ruta("outputs/fase26/recuentos_multiplicidad.csv")
d26i <- ruta("outputs/fase26/intervalos_cobertura.csv")
d28  <- ruta("outputs/fase28/cobertura_vitales_por_etapa.csv")
d29  <- ruta("outputs/fase29/cohorte_analizada.csv")
d30s <- ruta("outputs/fase30/separacion_candidatas.csv")
d30v <- ruta("outputs/fase30/viabilidad_comparacion.csv")
d31  <- ruta("outputs/fase31/casos_completos.csv")
d32c <- ruta("outputs/fase32/comparacion_por_clase.csv")
d32i <- ruta("outputs/fase32/intervalo_diferencia.csv")
d32m <- ruta("outputs/fase32/multiplicidad_diferencias.csv")
d32r <- ruta("outputs/fase32/comparacion_resumen.csv")
d33  <- ruta("outputs/fase33/procedencia_seleccion.csv")
d33v <- ruta("outputs/fase33/versiones_del_bloque.csv")
d15  <- ruta("outputs/fase15/sensibilidad_lactato.csv")
exc  <- ruta(".gitignore")

# El documento se acumula por parrafos y no por lineas. Cada pieza se anade
# al parrafo en curso y `cerrar` lo envuelve al ancho de la pagina. Componer
# linea a linea obligaba a que una ruta larga cupiese en el hueco que dejara
# la frase, lo que no depende de la frase sino de la longitud de la ruta.
#
# El envoltorio de la distribucion basica separa con dos espacios el punto
# que cierra oracion. El resto de los documentos del proyecto emplea uno, de
# modo que se normaliza despues de envolver, lo que solo acorta lineas.
doc <- character(0)
buf <- character(0)
add <- function(x) buf <<- c(buf, x)
cerrar <- function() {
  if (length(buf) == 0) return(invisible(NULL))
  parr <- strwrap(paste(buf, collapse = " "), width = 80)
  doc <<- c(doc, gsub("  +", " ", parr), "")
  buf <<- character(0)
}

add(prosa("# Decisiones vigentes"))
cerrar()

add(prosa(
"Registro de trabajo. Recoge lo que ya esta decidido, con que fundamento y",
"en que archivo consta. Se consulta antes de escribir sobre cualquiera de",
"estos puntos, para que lo que se publique se apoye en los archivos y no en",
"el recuerdo de quien redacta."))
cerrar()

add(cifra("Generado el %s por `%s`. Cada ruta que aqui figura se comprueba",
          format(Sys.Date()), r82))
add(prosa(
"al componer el documento: ha de existir y ha de estar bajo control de",
"versiones. El procedimiento se detiene si alguna falta, de modo que este",
"registro no puede sobrevivir a los archivos que lo sostienen."))
cerrar()

add(prosa("## Analisis posteriores a la seleccion"))
cerrar()

add(cifra("La seleccion de determinaciones quedo fijada el %s en `%s`, y el",
          FIJADA, r19))
add(cifra("registro del historial que lo acredita esta en `%s`, con el", d33))
add(cifra("detalle por version en `%s`. La acreditacion", d33v))
add(prosa(
"compara el contenido del bloque en cada version que el historial conserva y",
"exige que todos los resumenes coincidan entre si y con el que hay en disco.",
"Una busqueda que solo contara apariciones de la cadena que abre la lista no",
"habria visto una edicion dentro de ella, y por eso se sustituyo. Lo que",
"acredita es cuando se fijo, no que estuviera razonada: el criterio con que",
"se eligieron no consta en ninguna parte, y ningun procedimiento posterior",
"lo reconstruye ni debe presentarse como si lo hiciera."))
cerrar()

add(prosa(
"Los procedimientos que siguen se escribieron despues de esa fecha. El",
"documento lo comprueba contra el historial en lugar de fiarlo a lo que",
"cada cabecera declare."))
cerrar()

add(cifra("`%s`, alta el %s. Pregunta que distingue a las", r78, ALTAS[[r78]]))
add(cifra("determinaciones retenidas de las descartadas. La respuesta, en `%s`,",
          d30s))
add(prosa(
"es que ningun valor de los tres atributos registrados es exclusivo de las",
"retenidas. El tipo de muestra si informa en un sentido, y el documento lo",
"dice: todas las retenidas son de sangre y todas las de orina se",
"descartaron, pero ninguna de esas alcanza la cobertura de la retenida menos",
"frecuente, de modo que no anade nada al orden por cobertura. Ese resultado",
"negativo es el hallazgo, y se publica como tal."))
cerrar()

add(cifra("`%s`, alta el %s. Cuenta cuantas", r79, ALTAS[[r79]]))
add(cifra("estancias reunen todas las candidatas. Consta en `%s`.", d31))
cerrar()

add(cifra("`%s`, alta el %s. Compara la", r80, ALTAS[[r80]]))
add(cifra("especificacion retenida contra una ampliada. Deposita en `%s`,",
          d32r))
add(cifra("`%s`, `%s` y", d32c, d32i))
add(cifra("`%s`.", d32m))
cerrar()

add(cifra("`%s`, alta el %s. Recoge la procedencia de la", r81, ALTAS[[r81]]))
add(cifra("seleccion en `%s`.", d33))
cerrar()

add(cifra("`%s`, alta el %s. Comprueba si la", r70, ALTAS[[r70]]))
add(prosa(
"penalizacion se habria elegido igual sin mirar el conjunto de evaluacion."))
add(cifra("Consta en `%s`.", d22))
cerrar()

add(cifra("`%s`, alta el %s. Calcula los intervalos", r74, ALTAS[[r74]]))
add(cifra("de cobertura y corrige por multiplicidad. Deposita en `%s`,",
          d26i))
add(cifra("`%s` y `%s`.", d26c, d26m))
cerrar()

add(cifra("`%s`, alta el %s. Deriva la composicion de la", r77, ALTAS[[r77]]))
add(cifra("cohorte analizada en `%s`.", d29))
cerrar()

add(prosa("## Sesgos que acompanan a cada cifra"))
cerrar()

add(prosa(
"Cada uno se declara donde la cifra se publica, no en un apartado aparte de",
"limitaciones que el lector alcanza cuando ya ha leido el resultado."))
cerrar()

add(cifra("Casos completos. Las cifras de `%s` se calculan sobre las", d32r))
add(prosa(
"estancias que tienen completas todas las determinaciones comparadas. Esas",
"estancias no son una muestra aleatoria: son las mas monitorizadas, y la",
"intensidad de monitorizacion se asocia a la gravedad y a la unidad. La",
"comparacion responde entre pacientes con analitica completa y no en la",
"cohorte. Se declara en la cabecera del procedimiento y en el apartado de",
"laboratorio del documento principal."))
cerrar()

add(prosa(
"Sesgo de verificacion. La probabilidad de confirmar una etiologia depende",
"de que alguien la sospeche y solicite la prueba. El indicador de que una",
"determinacion se solicito resulta mas predictivo que su valor medido, lo"))
add(cifra("que consta en `%s`. Toda lectura del modelo como", d15))
add(prosa(
"descripcion fisiologica ha de acompanarse de esa advertencia."))
cerrar()

add(cifra("Extraccion anterior a la correccion de empates. `%s`", rep))
add(prosa(
"acota la divergencia para las constantes vitales y hace constar que para",
"las determinaciones de laboratorio no la acota ningun archivo. Las fases",
"centrales no se reextrajeron, porque rehacer la cohorte despues de abierto",
"el conjunto sellado invalidaria la validacion externa. La limitacion queda",
"sin cuantificar, y asi ha de escribirse."))
cerrar()

add(cifra("Penalizacion elegida sobre el conjunto de evaluacion. `%s`", d22))
add(prosa(
"recoge que al repetir la comparacion dentro del entrenamiento la decision",
"resulto la misma. Eso mitiga el defecto y no lo suprime: se comprobo",
"despues y pudo haber salido de otro modo."))
cerrar()

add(cifra("Divergencia de etiquetado. `%s` recoge el cotejo", d25))
add(prosa(
"completo entre las dos escaleras de desempate. Ninguna estancia cambia de",
"categoria, y el archivo se cita siempre que se afirme."))
cerrar()

add(prosa(
"Centro unico. Todas las unidades pertenecen a un mismo hospital terciario,",
"de modo que la heterogeneidad observada es una cota inferior de la que",
"mostrarian instituciones distintas."))
cerrar()

add(prosa("## Afirmaciones que no se sostienen"))
cerrar()

add(prosa(
"Ninguna de estas puede escribirse, ni en el articulo ni en el repositorio,",
"por mucho que la intuicion las respalde."))
cerrar()

add(cifra("Que las candidatas rindan mas o menos que las retenidas. `%s`", d31))
add(prosa(
"recoge que ninguna estancia de la cohorte las reune todas dentro de la",
"ventana. La comparacion directa no admite respuesta por casos completos, y",
"lo que se compara es otra cosa, mas estrecha, que ha de nombrarse."))
cerrar()

add(cifra("Que algun atributo registrado reproduzca la particion. `%s`", d30s))
add(prosa(
"recoge que ninguno lo hace. Escribir un criterio ahora seria presentar una",
"reconstruccion posterior como decision original."))
cerrar()

add(cifra("Que la especificacion ampliada pierda. `%s` recoge que el", d32r))
add(prosa(
"intervalo de la diferencia contiene el cero. La direccion no queda",
"establecida, y no basta con que el signo apunte a un lado."))
cerrar()

add(cifra("Que una categoria concreta empeore. `%s` recoge que", d32m))
add(prosa(
"ninguna resiste la correccion por multiplicidad. La que excluye el cero sin",
"corregir se reporta con esa salvedad, nunca sin ella y nunca omitida."))
cerrar()

add(cifra("Que la unidad reservada cumpla la garantia condicional. `%s`",
          d26i))
add(prosa(
"recoge sus celdas. Ninguna categoria minoritaria presenta alli un intervalo",
"por debajo del nivel, y eso no",
"acredita cumplimiento: los casos son demasiado pocos para establecer un",
"deficit en cualquiera de los dos sentidos. La ausencia de demostracion no",
"es demostracion de ausencia."))
cerrar()

add(cifra("Que algo sea inviable porque una cota lo sugiera. `%s`", d30v))
add(prosa(
"recoge la cota superior de casos completos con que se dio por imposible la",
"comparacion directa antes de contarlos. El recuento posterior resulto ser",
"cero, de modo que la conclusion era cierta; pero no lo era en el momento en",
"que se afirmo, porque una cota acota y no cuenta. Lo que se publique ha de",
"venir del recuento, tambien cuando la cota parezca concluyente."))
cerrar()

add(prosa(
"Que la seleccion de determinaciones estuviera razonada. El historial",
"acredita cuando quedo fija. Sobre el porque no hay archivo, y por tanto no",
"hay afirmacion posible."))
cerrar()

add(cifra("Que el criterio de la penalizacion se fijara de antemano. `%s`", r32))
add(prosa(
"lo declara, y el procedimiento y el deposito que lo aplica entraron en el",
"mismo commit, de modo que el historial no separa el criterio de su",
"resultado. El asunto de aquel commit emplea la palabra que lo afirma, y",
"tampoco eso es artefacto. Lo que sostiene la decision es la comprobacion",
"posterior dentro del entrenamiento, no su anterioridad."))
cerrar()

add(prosa("## Cifras que no son comparables entre si"))
cerrar()

add(cifra("Las areas de `%s` no son comparables con las del", d32c))
add(prosa(
"modelo publicado. Difieren en cinco cosas, todas comunes a las dos ramas y",
"por tanto inocuas para la comparacion entre ellas, pero decisivas para",
"quien intente cotejar cifras con el resto del trabajo:"))
add(prosa(
"no hay imputacion, porque se trabaja sobre casos completos; no hay",
"splines, porque las determinaciones anadidas no tienen nudos definidos y",
"dar forma flexible a unas y no a otras confundiria el conjunto de",
"variables con la forma funcional; no entra el indicador de solicitud de",
"lactato; el conjunto de ajuste es mucho menor, al quedar restringido a esos",
"casos completos; y la penalizacion se valida dentro de cada rama en vez de",
"heredarse. La cifra de una rama solo significa algo frente a la de la otra."))
cerrar()

add(prosa(
"La cobertura de constantes vitales antes y despues de la limpieza tampoco",
"es la misma cantidad. Se publica por etapas separadas, y ambas se leen de"))
add(cifra("`%s`.", d28))
cerrar()

add(prosa("## Decisiones metodologicas y su fundamento"))
cerrar()

add(cifra("Beta-Binomial para la cobertura. `%s` recoge el", d26c))
add(prosa(
"criterio. El umbral conforme se reestima en cada pliegue y no es una",
"cantidad conocida, de modo que un intervalo binomial exacto trata como fijo",
"algo que varia y declara deficits que el procedimiento no acredita. La",
"cobertura condicional de un conformal por particion sigue una Beta, y el",
"recuento marginal que de ella resulta una Beta-Binomial. El contraste se",
"invierte dejando libre la posicion y fijando la sobredispersion en el",
"tamano de calibracion."))
cerrar()

add(cifra("Holm y no Bonferroni. `%s` y", d26m))
add(cifra("`%s` recogen la correccion. Holm domina a", d32m))
add(prosa(
"Bonferroni: controla la misma tasa de error por familia y rechaza al menos",
"tanto. No hay razon para preferir el mas conservador."))
cerrar()

add(cifra("Los dos niveles, %s y %s. El segundo es el que",
          format(mlt$nivel[1]), format(mlt$nivel[2])))
add(prosa(
"gobierna la lectura, y corresponde a repartir un contraste bilateral entre",
"sus dos colas. Se informan ambos para que el lector vea que la conclusion",
"no depende de esa eleccion."))
cerrar()

add(prosa(
"Un unico criterio de lectura. La cobertura se juzga por intervalo en todas",
"partes: en el conjunto de prueba, en la validacion dejando una sede fuera y",
"en la unidad reservada. Antes hubo dos, uno por intervalo y otro por",
"comparacion puntual con el nivel nominal, y de ahi salio una conclusion que",
"hubo que retirar."))
cerrar()

add(cifra("La familia son las cuatro diferencias por categoria de `%s`.",
          d32i))
add(prosa(
"El promedio sobre las minoritarias queda fuera: es un unico resumen",
"declarado de antemano, no una de cuatro comparaciones exploradas, y se",
"informa con su propio intervalo."))
cerrar()

add(cifra("El remuestreo emplea %s replicas. El valor mas pequeno que un",
          format(cmp29$replicas, big.mark = ",", scientific = FALSE)))
add(prosa(
"remuestreo puede expresar lo fija su numero de replicas, y el umbral",
"escalonado de Holm desciende por debajo de esa resolucion cuando las",
"replicas son pocas: la decision sobre una categoria situada junto al",
"umbral dependeria entonces del sorteo y no de los datos."))
cerrar()

add(prosa(
"El remuestreo es emparejado. Los modelos se ajustan una sola vez y se",
"remuestrea el conjunto de evaluacion, de modo que la incertidumbre estimada",
"es la de la comparacion y no la del procedimiento entero. Ambas ramas se",
"evaluan sobre las mismas estancias, y la diferencia emparejada elimina la",
"variacion comun."))
cerrar()

add(prosa("## Datos que no se abren ni se publican"))
cerrar()

add(prosa(
"El acuerdo de uso de PhysioNet prohibe redistribuir datos derivados a nivel",
"de paciente, y su publicacion podria costar el acceso. La lista de rutas",
"reservadas vive en un solo lugar, que es donde la exclusion surte efecto."))
cerrar()

add(cifra("`%s` relaciona %d rutas bajo ese encabezado, ademas del",
          exc, length(reservadas)))
add(prosa(
"directorio de derivados y de la copia local de la base, que quedan fuera",
"del repositorio por completo. Ninguna de esas rutas se abre para redactar,",
"y ninguna cifra de este trabajo procede de leerlas a mano: los",
"procedimientos las leen y depositan agregados, que es lo que se publica."))
cerrar()

add(cifra("`%s` comprueba lo anterior sobre los hechos y no sobre la", r64))
add(prosa(
"intencion: inspecciona el encabezado de cada archivo versionado en busca de",
"identificadores, revisa los objetos binarios y recorre el historial por si",
"alguna de esas rutas fue alcanzada en algun momento."))
cerrar()

add(prosa("## Defectos recurrentes y como se evitan"))
cerrar()

add(prosa(
"Los cuatro se repitieron en este desarrollo. Cada uno tiene ahora un",
"procedimiento que lo impide, y no una intencion de no volver a cometerlo."))
cerrar()

add(prosa(
"Cifras escritas de memoria. Los generadores de documentos rechazan toda",
"cifra literal en la prosa y exigen que se componga desde un archivo."))
add(cifra("`%s` contrasta ademas cada cifra publicada contra su fuente.", r59))
cerrar()

add(prosa(
"Ordenes no deterministas. Una consulta que ordena por menos claves de las",
"que hacen falta deja empates sin resolver y devuelve filas distintas entre",
"ejecuciones. Ocurrio con las constantes vitales, por el instante de",
"registro, y volvio a ocurrir en otra capa: el orden en que las columnas",
"salian de la reestructuracion se propagaba al descenso por coordenadas del",
"ajuste penalizado, y las areas se movian en la cuarta cifra. La regla que",
"queda es fijar el orden en ambos sitios, en la consulta y en la matriz, y",
"comprobar la reproduccion ejecutando dos veces y cotejando los depositos."))
cerrar()

add(prosa(
"Texto que afirma mas que el archivo. Toda afirmacion publicada se asocia a",
"la ruta que la sostiene, y esa asociacion se comprueba."))
add(cifra("`%s` recorre la relacion y se detiene si alguna fuente", r64))
add(cifra("falta. `%s` recoge cada cifra contra su archivo.", trz))
cerrar()

add(prosa(
"Dos varas de medir. El mismo trabajo llego a corregir por multiplicidad en",
"un apartado y no en otro, y a juzgar la cobertura por intervalo en un sitio",
"y por comparacion puntual en otro. La regla que queda es que un criterio",
"adoptado en cualquier parte rige en todas, y que introducir uno nuevo",
"obliga a revisar los apartados anteriores."))
cerrar()

add(prosa("## Como se comprueba este registro"))
cerrar()

add(cifra("`%s` compone este documento y se detiene ante una cifra", r82))
add(prosa(
"escrita a mano, ante una ruta que no exista o no este versionada, ante una",
"ruta escrita en la prosa que haya eludido esa comprobacion, y ante un",
"analisis declarado posterior cuya alta en el historial no lo sea."))
cerrar()

add(cifra("Los demas procedimientos de comprobacion son `%s`,", r59))
add(cifra("`%s`, `%s` y", r64, r65))
add(cifra("`%s`. El documento principal es `%s`.", r67, lee))
cerrar()
cerrar()

# ---------------------------------------------------------------------------
# Segunda guardia: ninguna ruta puede haber entrado sin comprobarse.
# ---------------------------------------------------------------------------

halladas <- unique(unlist(regmatches(doc, gregexpr(ext, doc))))
huerfanas <- setdiff(halladas, CITADAS)

cat("=== RUTAS DEL DOCUMENTO ===\n")
cat("Citadas y comprobadas:", length(CITADAS), "\n")
cat("Halladas en el texto compuesto:", length(halladas), "\n")
if (length(huerfanas) > 0) {
  cat("\nRUTAS SIN COMPROBAR EN EL TEXTO\n")
  for (h in huerfanas) cat("  ", h, "\n")
  cat("\nEl registro no se escribe.\n")
  quit(status = 1)
}
cat("Ninguna ruta eludio la comprobacion.\n")

largas <- which(nchar(doc) > 79)
if (length(largas) > 0) {
  cat("\nLineas que exceden el ancho:", length(largas), "\n")
  for (i in largas) cat("  ", doc[i], "\n")
  quit(status = 1)
}

if (any(grepl("[^ -~]", doc))) {
  cat("\nEl documento contiene caracteres fuera del repertorio basico.\n")
  quit(status = 1)
}

writeLines(doc, "DECISIONES.md")
cat("\n=== REGISTRO ESCRITO ===\n")
cat("Lineas:", length(doc), "\n")
