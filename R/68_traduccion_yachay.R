# Traduccion de los hallazgos del banco de pruebas a decisiones del protocolo
# de meningitis oportunista.
#
# El documento se compone acumulando secciones breves en lugar de anidar una
# expresion extensa. El procedimiento anterior fallo al analizarse por un
# parentesis sin cerrar en una expresion de decenas de lineas, defecto que
# resulta invisible a esa escala y evidente en llamadas de tres.
#
# Las comprobaciones se separan de las decisiones que provocan. Un predicado
# devuelve verdadero o falso y admite prueba; una guardia decide y detiene la
# ejecucion, lo que impide probarla sin interrumpir la sesion. Las guardias se
# someten a casos de resultado conocido antes de componer cuanto sea.
#
# Ninguna cifra se refiere al protocolo. La totalidad procede de este banco de
# pruebas y se lee de archivos versionados.

EXENTAS <- c("R/68_traduccion_yachay.R")

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

if (!all(c(p1, p2, p3, p4, p5, p6, p7, p8))) {
  cat("\nLas guardias no superan sus pruebas. No procede componer.\n")
  quit(status = 1)
}
cat("Las ocho pruebas se superan.\n\n")

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

leer <- function(ruta) {
  if (!file.exists(ruta)) {
    cat("Fuente ausente:", ruta, "\n"); quit(status = 1)
  }
  read.csv(ruta, stringsAsFactors = FALSE)
}

val <- function(tabla, columna, condicion) {
  x <- tabla[condicion, columna]
  if (length(x) != 1 || is.na(x)) {
    cat("Extraccion fallida:", columna, "\n"); quit(status = 1)
  }
  x
}

alfa  <- leer("outputs/fase9/curva_alfa.csv")
louo  <- leer("outputs/fase10/cobertura_louo.csv")
louoc <- leer("outputs/fase10/cobertura_clase_louo.csv")
recal <- leer("outputs/fase12/recalibracion.csv")
lac   <- leer("outputs/fase15/sensibilidad_lactato.csv")
gbm   <- leer("outputs/fase16/gbm_comparacion.csv")
cob8  <- leer("outputs/fase8/cobertura.csv")
gcs   <- leer("outputs/fase17/circularidad_glasgow.csv")
tubo  <- leer("outputs/fase17/indicador_tubo.csv")
amp   <- leer("outputs/fase18/comparacion_ampliado.csv")
louoa <- leer("outputs/fase19/cobertura_louo_ampliado.csv")
empc  <- leer("outputs/fase20/empates_constantes.csv")
empl  <- leer("outputs/fase20/empates_laboratorio.csv")
iv    <- leer("outputs/fase26/intervalos_cobertura.csv")
causa <- leer("outputs/fase26/causas_fallo.csv")

MIN <- c("urinario","respiratorio","sangre")

# Numero de hallazgos que el documento expone. Se declara aqui, se inserta
# mediante codigo de sustitucion y se contrasta al final contra el numero de
# bloques de decision efectivamente compuestos. El guardian de prosa no
# alcanza a detectar un recuento erroneo escrito con letras, de modo que la
# comprobacion se establece sobre la estructura del documento.
N_HALLAZGOS <- 7L
d10 <- alfa$alfa == 0.10
cm  <- alfa$conclusiones_minoritarias > 0
pmin_alfa <- max(alfa$aciertos_minoritarios[cm] /
                 alfa$conclusiones_minoritarias[cm])
nmin <- min(recal$n_local)

# La garantia que el trabajo declara es la condicional por clase. Se cuenta
# por separado de la marginal: una sede puede alcanzar el nivel nominal en el
# promedio de sus casos y no alcanzarlo en alguna categoria.
NOMINAL <- cob8$nominal[1]
CLC <- c("sin_crecimiento","urinario","respiratorio","sangre")
bajo_marg <- sum(louo$cobertura < NOMINAL)
falla_cond <- sum(apply(louoc[, CLC], 1, function(r) any(r < NOMINAL, na.rm = TRUE)))
n_sedes <- nrow(louo)

# Las categorias que fallan no son las mismas en todas las sedes, pero tampoco
# difieren una a una: dos pares de sedes comparten conjunto. Se cuentan los
# conjuntos distintos en lugar de calificar el patron con una palabra.
# La garantia condicional se lee ahora por intervalo y con correccion por
# multiplicidad, el mismo criterio en las dos secciones del informe.
fam <- iv$seccion == "dejando una sede fuera"
ADOPT <- "resiste_beta_holm_0025"
sedes_iv  <- length(unique(iv$sede[fam]))
sedes_dem <- length(unique(iv$sede[fam & iv[[ADOPT]]]))
sin_dem <- iv[fam & iv$beta_por_debajo & !iv[[ADOPT]], ]
sin_dem <- sin_dem[order(-sin_dem$cobertura), ]
if (nrow(causa) != 2 || nrow(sin_dem) != causa$sin_fallo_demostrable[1] ||
    causa$clases[2] != "respiratorio") {
  cat("La estructura de las causas no admite la redaccion prevista.\n")
  quit(status = 1)
}

L <- character(0)
add <- function(...) L <<- c(L, ...)

add(prosa(
"# Traduccion de los hallazgos al protocolo de meningitis oportunista",
"",
"Documento de trabajo dirigido al equipo investigador.",
""))

add(cifra("Generado el %s por `R/68_traduccion_yachay.R`.",
          format(Sys.Date(), "%Y-%m-%d")))

add(prosa(
"",
"Las cifras que siguen proceden en su totalidad del banco de pruebas sobre",
"MIMIC-IV y se leen de archivos versionados. Cuanto se afirma sobre el",
"protocolo es cualitativo: no se dispone de datos de la red de sedes y no se",
"formula estimacion alguna sobre ella.",
"",
"## Proposito",
"",
"El banco de pruebas replico sobre una cohorte de cuidados intensivos la",
"estructura metodologica prevista para el protocolo: clasificacion multiclase",
"con desbalance acusado, conjuntos de prediccion con garantia de cobertura,",
"mecanismo de abstencion y validacion en una sede no observada durante el",
"desarrollo. El proposito era detectar los modos de fallo antes de llevar el",
"diseno a datos peruanos.",
""))

add(cifra(
  "Se detectaron %d modos de fallo, y cada uno deriva en una decision concreta.",
  N_HALLAZGOS))

add(prosa(
"",
"## Primer hallazgo. La evaluacion exige cuatro dominios",
"",
"La capacidad de ordenar casos y la garantia de cobertura no se comportan",
"igual al cambiar de sede. Bajo validacion dejando una sede fuera, la primera",
"se conserva mientras la segunda se degrada de forma desigual."))

add(cifra("La cobertura marginal oscila entre %.4f y %.4f con media de %.4f,",
          min(louo$cobertura), max(louo$cobertura), mean(louo$cobertura)))
add(cifra("por debajo del nivel nominal. De las %d sedes, %d quedan por debajo",
          as.integer(n_sedes), as.integer(bajo_marg)))

add(prosa("en esa medida.", ""))

add(prosa(
"La garantia condicional por categoria, que es la que el trabajo declara, se",
"evalua celda a celda: cada categoria dentro de cada sede, con el mismo",
"criterio de lectura por intervalo que emplea el conjunto de prueba. Las",
"celdas se corrigen conjuntamente por multiplicidad, y el contraste reconoce",
"que el umbral conforme se reestima en cada pliegue y no es una cantidad",
"conocida."))

add(cifra("Bajo esa correccion fallan %d de %d sedes: %d en la categoria",
          as.integer(sedes_dem), as.integer(sedes_iv),
          as.integer(causa$sedes[1])))
add(cifra("%s y %d en la respiratoria. Las %d restantes presentan",
          causa$clases[1], as.integer(causa$sedes[2]),
          as.integer(causa$sin_fallo_demostrable[1])))
add(cifra("cobertura puntual de %.4f y %.4f sobre %d y %d casos, demasiado",
          sin_dem$cobertura[1], sin_dem$cobertura[2],
          as.integer(sin_dem$n[1]), as.integer(sin_dem$n[2])))

add(prosa(
"pocos para establecer el deficit. La ausencia de demostracion no acredita",
"cumplimiento, y un resumen marginal oculta cual categoria queda descubierta",
"en cada sede.",
"",
"**Decision.** El informe de resultados no puede limitarse a la",
"discriminacion. Debe presentar por separado, y desagregados por sede, la",
"capacidad de ordenar casos, la concordancia entre probabilidad predicha y",
"frecuencia observada, la cobertura de los conjuntos y la proporcion de casos",
"que el sistema resuelve. Comunicar unicamente el primero describiria un",
"comportamiento que los demas desmienten.",
"",
"## Segundo hallazgo. La cobertura promediada oculta ceros",
"",
"Al recalibrar con un umbral unico sobre una sede, la cobertura promediada se",
"aproxima al nivel nominal mientras las categorias poco frecuentes quedan",
"enteramente fuera de los conjuntos. El promedio esta dominado por la",
"categoria mayoritaria.",
"",
"**Decision.** No se reportara cobertura marginal como medida principal. La",
"garantia se calculara y comunicara por separado dentro de cada etiologia.",
"Toda etiologia que no alcance el minimo necesario se declarara expresamente",
"como no calibrada, en lugar de excluirse en silencio. Un sistema que omite",
"una etiologia sin advertirlo esta afirmando que el paciente no la padece,",
"sin fundamento para ello.",
"",
"## Tercer hallazgo. La recalibracion local tiene un limite duro",
"",
"La garantia condicional exige un numero minimo de casos por categoria en la",
"sede donde se recalibra. Por debajo de ese minimo el cuantil no existe."))

add(cifra("Con %d casos locales, una media de %.1f de las %d categorias",
          as.integer(nmin),
          val(recal, "clases_calibrables", recal$n_local == nmin),
          nrow(cob8)))
add(cifra("alcanzaba el minimo, y el tamano medio del conjunto descendia a %.3f:",
          val(recal, "tamano_medio", recal$n_local == nmin)))

add(prosa(
"los conjuntos quedaban a menudo vacios y el sistema se reducia a un",
"clasificador binario degenerado, con cobertura aparente proxima a la nominal.",
"",
"**Decision.** Antes de recalibrar en cada sede se verificara el recuento",
"disponible por etiologia. Las etiologias infrecuentes pueden exigir periodos",
"prolongados para reunir los casos necesarios en una sede concreta. El",
"protocolo debe",
"prever esa circunstancia y establecer de antemano el procedimiento:",
"declarar la etiologia no calibrada, agrupar sedes, o renunciar a la garantia",
"condicional para esa categoria haciendolo constar.",
"",
"## Cuarto hallazgo. El modelo lee juicio clinico",
"",
"La variable que indica si una determinacion se solicito resulto mas",
"predictiva que su valor medido."))

add(cifra("El indicador de solicitud aporta %.4f a la discriminacion; el valor",
          val(lac, "promedio_minoritarias", lac$especificacion == "completa") -
          val(lac, "promedio_minoritarias",
              lac$especificacion == "lactato sin indicador")))
add(cifra("determinado aporta %.4f. La decision de solicitar la prueba",
          val(lac, "promedio_minoritarias",
              lac$especificacion == "lactato sin indicador") -
          val(lac, "promedio_minoritarias",
              lac$especificacion == "sin lactato ni indicador")))

add(prosa(
"transmite mas informacion que la fisiologia que la prueba mide.",
"",
"**Decision.** El protocolo registra si cada prueba especifica se realizo.",
"Ese registro sera predictivo, pero por una razon ajena a la fisiologia:",
"codifica la sospecha del clinico que atendio al paciente. Debe analizarse",
"por separado del resultado, con la especificacion completa y con la que",
"prescinde del indicador, comunicando ambas. Un modelo que dependa del patron",
"de solicitud no transportara a una sede con otro protocolo de peticion.",
"",
"## Quinto hallazgo. La escala de conciencia puede medir el procedimiento",
"",
"Un indicador binario de intubacion, desprovisto de contenido fisiologico,",
"discrimino la categoria respiratoria casi tan bien como la escala de",
"conciencia."))

add(cifra("El indicador alcanza %.4f. La escala obtiene %.4f en su forma completa",
          val(tubo, "auc_solo_tubo", tubo$clase == "respiratorio"),
          val(gcs, "auc_gcs_total", gcs$estrato == "entrenamiento y calibracion" &
                                    gcs$clase == "respiratorio")))
add(cifra("de tres componentes y %.4f en la reducida a apertura ocular y respuesta",
          val(gcs, "auc_gcs_em", gcs$estrato == "entrenamiento y calibracion" &
                                 gcs$clase == "respiratorio")))

add(prosa(
"motora. Se reporta la reducida porque el componente verbal asigna la",
"puntuacion minima al paciente intubado y confunde la ausencia de respuesta",
"con la imposibilidad de hablar. Dentro del estrato intubado la escala"))

add(cifra("reducida desciende a %.4f, y la completa obtiene ese mismo %.4f. Ambas",
          val(gcs, "auc_gcs_em", gcs$estrato == "con tubo endotraqueal" &
                                 gcs$clase == "respiratorio"),
          val(gcs, "auc_gcs_total", gcs$estrato == "con tubo endotraqueal" &
                                    gcs$clase == "respiratorio")))

add(prosa(
"formas coinciden alli porque el componente verbal es constante, de modo que",
"la completa es la reducida mas un desplazamiento fijo, que deja inalterado",
"el orden y por tanto el area. La escala actuaba como indicador indirecto",
"del tubo, y el tubo se asocia con fuerza a que el sitio respiratorio se",
"cultive. Es asociacion, no determinacion: los datos no acreditan que lo",
"uno cause lo otro. Lo que si consta es que la escala no supera al indicador",
"de intubacion en esta cohorte, y eso basta para no tratarla como medida",
"fisiologica independiente aqui. Que quede invalidada como predictor",
"fisiologico es una lectura que ningun archivo de este trabajo sostiene, y",
"por eso no se hace.",
"",
"**Decision.** La escala figura entre las variables obligatorias del",
"protocolo, y con fundamento: la meningitis altera la conciencia de forma",
"directa, de modo que la relacion causal apunta en el sentido correcto. Pero",
"la trampa subsiste. El cuaderno de recogida debe consignar la puntuacion con",
"anterioridad a la sedacion o la intubacion siempre que resulte posible, si",
"el paciente se hallaba sedado o intubado en el momento de la valoracion, y",
"que componentes fueron evaluables. Sin ese registro, las sedes que dispongan",
"de ventilacion mecanica presentaran puntuaciones sistematicamente",
"inferiores, y la diferencia reflejara la infraestructura disponible y no el",
"estado del paciente.",
"",
"## Sexto hallazgo. El limite pertenece a la informacion",
"",
"Un metodo no parametrico capaz de aprender interacciones sin especificacion",
"previa alcanzo practicamente el mismo techo que la regresion penalizada."))

add(cifra("La diferencia es de %.4f sobre las categorias poco frecuentes.",
          mean(gbm$auc_gbm[gbm$clase != "sin_crecimiento"]) -
          mean(gbm$auc_lineal[gbm$clase != "sin_crecimiento"])))

add(prosa(
"",
"La incorporacion de un dominio nuevo de medicion produjo mejora, aunque"))

add(cifra("modesta: %.4f al anadir constantes vitales, muy por encima de lo que",
          mean(amp$auc_ampliado[amp$clase %in% MIN]) -
          mean(amp$auc_original[amp$clase %in% MIN])))

add(prosa(
"aporto refinar el algoritmo.",
"",
"**Decision.** No cabe esperar que un metodo mas flexible compense una",
"informacion insuficiente. La discusion sobre que variables recoger precede",
"en importancia a la discusion sobre que modelo emplear. El protocolo dispone",
"de ventajas que el banco de pruebas no tenia: exploracion neurologica,",
"tiempo de enfermedad y pruebas dirigidas al desenlace. Esas ventajas pesan",
"mas que cualquier eleccion algoritmica.",
"",
"## Septimo hallazgo. La extraccion puede no ser determinista",
"",
"El registro de observaciones de enfermeria se valida por lotes, de modo que",
"varias determinaciones comparten instante de registro."))

add(cifra("La coincidencia afecta a entre %.2f y %.2f por ciento de las",
          min(empc$pct), max(empc$pct)))
add(cifra("estancias segun la variable, con hasta %d determinaciones",
          as.integer(max(empc$maximo_coincidentes))))
add(cifra("simultaneas. Las de laboratorio resultan afectadas en un %.2f por ciento de",
          empl$pct))

add(prosa(
"las estancias. Los equipos consignan cada resultado por separado, de modo",
"que la coincidencia resulta alli mucho menos frecuente, pero no nula, y la",
"consulta que las extrae ordena tambien por el solo instante de registro. La",
"reextraccion corregida se limito a las constantes vitales, de modo que la",
"divergencia que esa ordenacion pueda inducir sobre las bioquimicas no esta",
"acotada.",
"",
"Seleccionar la primera determinacion ordenando unicamente por el instante de",
"registro deja las coincidencias sin resolver, y la fila retenida puede",
"variar entre ejecuciones de una misma consulta.",
"",
"**Decision.** El procedimiento de extraccion fijara desde el inicio un",
"criterio de desempate explicito. Sin el, dos analistas que ejecuten el mismo",
"codigo sobre los mismos datos pueden obtener resultados distintos, y la",
"discrepancia resultaria dificil de atribuir.",
"",
"## Riesgos adicionales que el protocolo ya contempla",
"",
"**La exactitud no puede ser la medida principal.** Con la distribucion",
"etiologica prevista, un sistema que responda siempre ausencia de",
"confirmacion acertara la mayoria de las veces sin identificar una sola",
"etiologia. En el banco de pruebas, la regla del maximo no asigno jamas una",
"categoria poco frecuente, con independencia del metodo empleado."))

add(cifra("Al nivel de confianza del noventa por ciento el sistema resolvia %.1f",
          val(alfa, "pct_resuelve", d10)))

add(prosa("por ciento de los casos, y la precision sobre las conclusiones de"))
add(cifra("categoria poco frecuente no supero en ningun nivel el valor de %.4f.",
          pmin_alfa))

add(prosa(
"",
"**La glucosa del liquido cefalorraquideo debe analizarse como indice.** El",
"valor absoluto depende de la glucemia simultanea. El protocolo recoge ambas",
"determinaciones; el analisis debe emplear el cociente y no la cifra aislada.",
"",
"**La eleccion de hiperparametros no puede hacerse sobre el conjunto de",
"evaluacion.** El banco de pruebas fijo la penalizacion comparando dos reglas",
"sobre el mismo conjunto en el que despues reporto discriminacion y",
"cobertura. Al repetir la comparacion dentro del entrenamiento, apartando una",
"porcion que no intervino en el ajuste, la decision resulto ser la misma; pero",
"eso se comprobo despues y pudo haber salido de otro modo. El protocolo",
"establecera de antemano que toda eleccion de esta clase se resuelva dentro",
"del entrenamiento, con una porcion apartada del ajuste, y nunca con los",
"datos que sostienen el resultado publicado.",
"",
"**El sesgo de verificacion es cuantificable y debe cuantificarse.** La",
"probabilidad de confirmar una etiologia depende de que alguien la sospeche",
"y solicite la prueba correspondiente. El protocolo registra la realizacion",
"de cada prueba, lo que permite estimar la magnitud del sesgo en lugar de",
"declararlo en abstracto.",
"",
"## Lo que este banco de pruebas no puede responder",
"",
"No compara el desempeno del sistema con el de un clinico que disponga de la",
"misma informacion. Sin esa comparacion se desconoce si el sistema aporta",
"algo sobre el juicio que ya existe. El protocolo, en cambio, puede",
"establecerla, dado que el medico tratante forma parte del circuito.",
"",
"No informa sobre la magnitud del desplazamiento de prevalencia entre sedes",
"peruanas. Ese desplazamiento es compatible con el fallo de transportabilidad",
"observado, pero el banco de pruebas no lo acredita: la incorporacion de las",
"constantes vitales no reemplaza a las determinaciones bioquimicas sino que",
"se suma a ellas, de modo que los predictores presuntamente contaminados",
"permanecen en el modelo y la comparacion no distingue entre ambas",
"hipotesis. El modelo ampliado, ademas, no transporta mejor. La desviacion",
"de la cobertura queda practicamente igual, de modo que la afirmacion se",
"apoya en las otras dos:"))

add(cifra("pasa de %.4f a %.4f, mientras que la media desciende de %.4f a",
          sd(louo$cobertura), sd(louoa$cobertura), mean(louo$cobertura)))
add(cifra("%.4f y el minimo de %.4f a %.4f.",
          mean(louoa$cobertura), min(louo$cobertura), min(louoa$cobertura)))

add(prosa(
"",
"No permite anticipar cuantos casos reunira cada sede ni con que distribucion",
"etiologica. Esa informacion condiciona la viabilidad de la recalibracion",
"local y debe estimarse con datos propios antes de comprometer el diseno.",
"",
"## Origen de las cifras",
"",
"Toda cifra de este documento se lee de un archivo de resultados versionado",
"del banco de pruebas, con una excepcion: el recuento de hallazgos, que es",
"autorreferencial y se contrasta contra el numero de encabezados y de bloques",
"de decision del propio documento. El procedimiento que lo compone se detiene",
"ante cualquier cifra escrita a mano, ante cualquier fuente ausente, ante",
"cualquier linea que no componga y ante una discrepancia en ese recuento."))

# Comprobacion estructural. El recuento declarado debe coincidir con el numero
# de bloques de decision compuestos. De no hacerlo, el documento afirmaria
# sobre si mismo algo que su propia estructura desmiente.
decisiones <- sum(grepl("^\\*\\*Decision\\.\\*\\*", L))
encabezados <- sum(grepl("^## .*hallazgo", L))
cat("=== COMPROBACION ESTRUCTURAL ===\n")
cat("Hallazgos declarados:", N_HALLAZGOS, "\n")
cat("Encabezados de hallazgo:", encabezados, "\n")
cat("Bloques de decision compuestos:", decisiones, "\n")
if (decisiones != N_HALLAZGOS || encabezados != N_HALLAZGOS) {
  cat("El recuento declarado no coincide con la estructura del documento.\n")
  quit(status = 1)
}
cat("Coinciden.\n\n")

dir.create("outputs/fase21", recursive = TRUE, showWarnings = FALSE)
# Envoltura de los parrafos de prosa.
#
# La composicion emite una linea por cadena, de modo que un parrafo que
# termina en dos palabras deja un renglon de dos palabras, y otro tanto cada
# vez que una cifra corta la frase. El generador del registro de decisiones no
# tiene ese defecto porque acumula y envuelve al cerrar; aqui se consigue lo
# mismo con una pasada al final, que ademas alcanza a los parrafos que nadie
# ha tocado en esta revision.
#
# No se envuelve lo que no es prosa: las tablas, cuya sangria y alineamiento
# son informacion; los titulos; y las relaciones.
envolver <- function(x) {
  es_prosa <- function(l) nzchar(trimws(l)) && !grepl("^ ", l) &&
                          !grepl("^#", l) && !grepl("^[-*] ", l)
  fuera <- character(0); buf <- character(0)
  volcar <- function() {
    if (length(buf) == 0) return(invisible(NULL))
    fuera <<- c(fuera, strwrap(paste(buf, collapse = " "), width = 79))
    buf <<- character(0)
  }
  for (l in x) {
    if (es_prosa(l)) buf <- c(buf, l)
    # Asignacion simple y no al entorno superior: dentro del cuerpo de la
    # funcion, el operador de superasignacion salta la variable local y
    # escribe en el global. Con el, las lineas que no son prosa, tablas y
    # renglones en blanco incluidos, se perdian del documento.
    else { volcar(); fuera <- c(fuera, l) }
  }
  volcar()
  fuera
}
L <- envolver(L)

# Ancho y repertorio. El documento se lee tambien en una terminal y en un
# visor sin fuentes anchas, y un caracter fuera del repertorio basico se
# convierte en un signo de interrogacion o en una caja. Las dos guardias
# existian en un solo generador de los cuatro.
largas <- which(nchar(L) > 79)
if (length(largas) > 0) {
  cat("\nLineas que exceden el ancho:", length(largas), "\n")
  for (i in largas) cat("  ", L[i], "\n")
  cat("El documento no se escribe.\n")
  quit(status = 1)
}
if (any(grepl("[^ -~]", L))) {
  cat("\nEl documento contiene caracteres fuera del repertorio basico.\n")
  for (i in which(grepl("[^ -~]", L))) cat("  ", L[i], "\n")
  cat("El documento no se escribe.\n")
  quit(status = 1)
}

writeLines(L, "outputs/fase21/PROTOCOLO.md")

cat("=== DOCUMENTO GENERADO ===\n")
cat("Lineas escritas:", length(L), "\n")
cat("\nToda cifra procede de una lectura del banco de pruebas.\n")
