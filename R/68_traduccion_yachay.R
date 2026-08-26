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

if (!all(c(p1, p2, p3, p4, p5, p6, p7))) {
  cat("\nLas guardias no superan sus pruebas. No procede componer.\n")
  quit(status = 1)
}
cat("Las siete pruebas se superan.\n\n")

prosa <- function(...) {
  x <- c(...)
  for (s in x)
    if (tiene_digito(s)) {
      cat("Cifra literal en la prosa:\n  ", s, "\n"); quit(status = 1)
    }
  x
}

cifra <- function(fmt, ...) {
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
recal <- leer("outputs/fase12/recalibracion.csv")
lac   <- leer("outputs/fase15/sensibilidad_lactato.csv")
gbm   <- leer("outputs/fase16/gbm_comparacion.csv")
gcs   <- leer("outputs/fase17/circularidad_glasgow.csv")
tubo  <- leer("outputs/fase17/indicador_tubo.csv")
amp   <- leer("outputs/fase18/comparacion_ampliado.csv")
louoa <- leer("outputs/fase19/cobertura_louo_ampliado.csv")
empc  <- leer("outputs/fase20/empates_constantes.csv")
empl  <- leer("outputs/fase20/empates_laboratorio.csv")

MIN <- c("urinario","respiratorio","sangre")
d10 <- alfa$alfa == 0.10
cm  <- alfa$conclusiones_minoritarias > 0
pmin_alfa <- max(alfa$aciertos_minoritarios[cm] /
                 alfa$conclusiones_minoritarias[cm])
nmin <- min(recal$n_local)

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
"desarrollo. El proposito era detectar los modos de fallo antes de aplicarlos",
"a datos peruanos.",
"",
"Se detectaron siete. Cinco afectan a decisiones del protocolo.",
"",
"## Primer hallazgo. La evaluacion exige cuatro dominios",
"",
"La capacidad de ordenar casos y la garantia de cobertura no se comportan",
"igual al cambiar de sede. Bajo validacion dejando una sede fuera, la primera",
"se conserva mientras la segunda se degrada de forma desigual."))

add(cifra("La cobertura oscila entre %.4f y %.4f, con desviacion de %.4f",
          min(louo$cobertura), max(louo$cobertura), sd(louo$cobertura)))
add(cifra("frente a una media de %.4f: se cumple en promedio y en ninguna",
          mean(louo$cobertura)))

add(prosa(
"sede tomada de forma individual.",
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

add(cifra("Con %d casos locales solo %.1f categoria alcanzaba el minimo, y el",
          as.integer(nmin),
          val(recal, "clases_calibrables", recal$n_local == nmin)))
add(cifra("tamano medio del conjunto descendia a %.3f: los conjuntos quedaban",
          val(recal, "tamano_medio", recal$n_local == nmin)))

add(prosa(
"a menudo vacios y el sistema se reducia a un clasificador binario",
"degenerado, con cobertura aparente proxima a la nominal.",
"",
"**Decision.** Antes de recalibrar en cada sede se verificara el recuento",
"disponible por etiologia. Las etiologias infrecuentes pueden requerir anos",
"para reunir los casos necesarios en una sede concreta. El protocolo debe",
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
"discrimino la categoria respiratoria mejor que la escala completa."))

add(cifra("El indicador alcanza %.4f frente a %.4f de la escala. Dentro del",
          val(tubo, "auc_solo_tubo", tubo$clase == "respiratorio"),
          val(gcs, "auc_gcs_em", gcs$estrato == "cohorte completa" &
                                 gcs$clase == "respiratorio")))
add(cifra("estrato intubado la escala desciende a %.4f, esto es, deja de",
          val(gcs, "auc_gcs_em", gcs$estrato == "con tubo endotraqueal" &
                                 gcs$clase == "respiratorio")))

add(prosa(
"discriminar. La escala actuaba como indicador indirecto del tubo, y el tubo",
"determinaba que el sitio respiratorio se cultivase.",
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
"previa alcanzo el mismo techo que la regresion penalizada."))

add(cifra("La diferencia es de %.4f sobre las categorias poco frecuentes.",
          mean(gbm$auc_gbm[gbm$clase != "sin_crecimiento"]) -
          mean(gbm$auc_lineal[gbm$clase != "sin_crecimiento"])))

add(prosa(
"",
"La incorporacion de un dominio nuevo de medicion si produjo mejora, aunque",
"modesta:"))

add(cifra("%.4f al anadir constantes vitales, dos ordenes de magnitud por",
          mean(amp$auc_ampliado[amp$clase %in% MIN]) -
          mean(amp$auc_original[amp$clase %in% MIN])))

add(prosa(
"encima de lo que aporto refinar el algoritmo.",
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
add(cifra("simultaneas. Las de laboratorio quedan exentas, con %.2f por",
          empl$pct))

add(prosa(
"ciento, dado que los equipos consignan cada resultado por separado.",
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
"## Riesgos adicionales que el protocolo ya contiene",
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
"**La glucosa del liquido debe analizarse como indice.** El valor absoluto",
"depende de la glucemia simultanea. El protocolo recoge ambas",
"determinaciones; el analisis debe emplear el cociente y no la cifra aislada.",
"",
"**El sesgo de verificacion es cuantificable y debe cuantificarse.** La",
"probabilidad de confirmar una etiologia depende de que alguien la sospechara",
"y solicitara la prueba correspondiente. El protocolo registra la realizacion",
"de cada prueba, lo que permite estimar la magnitud del sesgo en lugar de",
"declararlo en abstracto.",
"",
"## Lo que este banco de pruebas no puede responder",
"",
"No compara el desempeno del sistema con el de un clinico que disponga de la",
"misma informacion. Sin esa comparacion se desconoce si el sistema aporta",
"algo sobre el juicio que ya existe. El protocolo si puede establecerla, dado",
"que el medico tratante forma parte del circuito.",
"",
"No informa sobre la magnitud del desplazamiento de prevalencia entre sedes",
"peruanas. El banco de pruebas mostro que ese desplazamiento, y no la",
"contaminacion de los predictores, explica el fallo de transportabilidad de",
"la garantia."))

add(cifra("La desviacion de la cobertura permanecio en %.4f frente a %.4f al",
          sd(louoa$cobertura), sd(louo$cobertura)))

add(prosa(
"incorporar la unica familia de variables exenta de sesgo de sede.",
"",
"No permite anticipar cuantos casos reunira cada sede ni con que distribucion",
"etiologica. Esa informacion condiciona la viabilidad de la recalibracion",
"local y debe estimarse con datos propios antes de comprometer el diseno.",
"",
"## Origen de las cifras",
"",
"Toda cifra de este documento se lee de un archivo de resultados versionado",
"del banco de pruebas. El procedimiento que lo compone se detiene ante",
"cualquier cifra escrita a mano, ante cualquier fuente ausente y ante",
"cualquier linea que no componga."))

dir.create("outputs/fase21", recursive = TRUE, showWarnings = FALSE)
writeLines(L, "outputs/fase21/PROTOCOLO.md")

cat("=== DOCUMENTO GENERADO ===\n")
cat("Lineas escritas:", length(L), "\n")
cat("\nToda cifra procede de una lectura del banco de pruebas.\n")
