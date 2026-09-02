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

# ---------------------------------------------------------------------------
# La guardia contra el emparejamiento parcial de nombres.
#
# El operador de acceso admite prefijos y devuelve una columna cuyo nombre no
# es el pedido, sin error. Una cifra leida asi no es la que su nombre dice, y
# toda la trazabilidad que este procedimiento comprueba descansa en que lo
# sea. La guardia vive en el perfil del proyecto, de modo que alcanza a todo
# procedimiento sin que ninguno tenga que acordarse de ella.
#
# Se comprueba aqui que este activa y que ademas detenga, no solo avise. Lo
# segundo se prueba en un proceso aparte, porque probarlo en este lo mataria.
# Una guardia que no se comprueba es una intencion.
# ---------------------------------------------------------------------------

if (!isTRUE(getOption("warnPartialMatchDollar"))) {
  cat("La guardia de emparejamiento parcial no esta activa.\n")
  cat("No procede verificar cifras sin ella.\n")
  quit(status = 1)
}
prueba <- suppressWarnings(system(
  paste("Rscript -e 'd <- data.frame(alfa_larga = 1);",
        "invisible(d$alfa)' >/dev/null 2>&1"), intern = FALSE))
if (prueba == 0) {
  cat("La guardia avisa pero no detiene ante un emparejamiento parcial.\n")
  cat("Un aviso en una salida larga se pierde. No procede continuar.\n")
  quit(status = 1)
}
cat("Guardia de emparejamiento parcial: activa y detiene.\n")

# La comprobacion anterior acredita la guardia para este procedimiento, no
# para quien escribio cada deposito. Un procedimiento ejecutado con --vanilla
# no lee el perfil del proyecto y por tanto no tiene guardia; su deposito
# podria llevar una cifra que no es la que su nombre dice, y este verificador
# contrastaria el documento contra ella y concordaria. Por eso cada manifiesto
# declara si la guardia estaba puesta al escribirse, y aqui se exige que
# ninguno la declare ausente.
#
# Los manifiestos anteriores a la guardia no la declaran, y no se les inventa
# el campo. Su estado no queda sin embargo en blanco: la fase trigesimo
# septima lo deriva del historial, porque un deposito escrito antes de que la
# guardia existiera se escribio sin ella por construccion. Aqui se cuentan, y
# ese recuento solo puede bajar; si subiera, seria un deposito nuevo escrito
# sin declararlo.
mfs <- system("git ls-files 'outputs/*/manifiesto.json'", intern = TRUE)
decl <- sapply(mfs, function(m) {
  d <- jsonlite::fromJSON(m)
  v <- d[["guarda_de_emparejamiento_parcial"]]
  if (is.null(v)) NA else isTRUE(v)
})
falsos <- sum(!is.na(decl) & !decl)
N_MF_DECLARAN <- sum(!is.na(decl) & decl)
N_MF_SIN      <- sum(is.na(decl))
cat("Manifiestos versionados:", length(mfs),
    " declaran la guardia:", sum(!is.na(decl) & decl),
    " no la declaran:", sum(is.na(decl)),
    " la declaran ausente:", falsos, "\n")
if (falsos > 0) {
  cat("\nUn deposito se escribio sin la guardia de nombres:\n")
  for (m in mfs[!is.na(decl) & !decl]) cat("  ", m, "\n")
  cat("No procede verificar cifras contra el.\n")
  quit(status = 1)
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
  "Calibracion en el conjunto de prueba"     = "outputs/fase34/calibracion_prueba.csv",
  "Curva de calibracion"                     = "outputs/fase35/curva_calibracion.csv",
  "Recorrido de la probabilidad predicha"    = "outputs/fase35/rango_probabilidad.csv",
  "Cota sobre la regla del maximo"           = "outputs/fase35/separacion_argmax.csv",
  "Pendiente de calibracion"                 = "outputs/fase35/pendiente_calibracion.csv",
  "Regla del maximo"                         = "outputs/fase24/regla_maximo.csv",
  "Divergencia de etiquetado"                = "outputs/fase25/divergencia_etiquetado.csv",
  "Intervalos de cobertura por celda"        = "outputs/fase26/intervalos_cobertura.csv",
  "Recuentos bajo cada correccion"           = "outputs/fase26/recuentos_multiplicidad.csv",
  "Causa del fallo por sede"                 = "outputs/fase26/causas_fallo.csv",
  "Cobertura de constantes por etapa"        = "outputs/fase28/cobertura_vitales_por_etapa.csv",
  "Composicion de la cohorte analizada"      = "outputs/fase29/cohorte_analizada.csv",
  "Unidades de la cohorte"                   = "outputs/fase29/unidades_cohorte.csv",
  "Determinaciones candidatas"               = "outputs/fase30/determinaciones_candidatas.csv",
  "Separacion de las candidatas"             = "outputs/fase30/separacion_candidatas.csv",
  "Viabilidad de la comparacion"             = "outputs/fase30/viabilidad_comparacion.csv",
  "Casos completos en las candidatas"        = "outputs/fase31/casos_completos.csv",
  "Comparacion ampliada, por clase"          = "outputs/fase32/comparacion_por_clase.csv",
  "Comparacion ampliada, resumen"            = "outputs/fase32/comparacion_resumen.csv",
  "Cascada de casos completos"               = "outputs/fase32/cascada_casos_completos.csv",
  "Completos por conjunto de la particion"   = "outputs/fase32/completos_por_grupo.csv",
  "Conjunto ajustado por grupo y clase"      = "outputs/fase32/conjunto_por_grupo_y_clase.csv",
  "Completitud por conjunto"                 = "outputs/fase36/completitud_por_conjunto.csv",
  "Completitud por grupo"                    = "outputs/fase36/completitud_por_grupo.csv",
  "Completitud por determinacion"            = "outputs/fase36/completitud_por_determinacion.csv",
  "Guarda por deposito"                      = "outputs/fase37/guarda_por_deposito.csv",
  "Orden total de los depositos"             = "outputs/fase38/orden_total.csv",
  "Orden de la matriz"                       = "outputs/fase39/orden_de_la_matriz.csv",
  "Contactos con el sellado"                 = "outputs/fase40/contactos_con_el_sellado.csv",
  "Posterioridad de los analisis"            = "outputs/fase41/posterioridad.csv",
  "Distancia de la reservada"                = "outputs/fase42/distancia_de_la_reservada.csv",
  "Extremos por categoria"                   = "outputs/fase42/extremos_por_categoria.csv",
  "Fechas de la posterioridad"               = "outputs/fase41/fechas.csv",
  "Conjunto apartado"                        = "outputs/fase40/conjunto_apartado.csv",
  "Procedencia de la seleccion"              = "outputs/fase33/procedencia_seleccion.csv",
  "Versiones del bloque"                     = "outputs/fase33/versiones_del_bloque.csv",
  "Potencia no usada"                        = "outputs/fase32/potencia_no_usada.csv",
  "Intervalo de la diferencia"                = "outputs/fase32/intervalo_diferencia.csv",
  "Multiplicidad de las diferencias"          = "outputs/fase32/multiplicidad_diferencias.csv")

# ---------------------------------------------------------------------------
# La lista de rutas reservadas se obtiene en un solo sitio.
# ---------------------------------------------------------------------------
# Tres procedimientos la necesitan y hasta hace poco cada uno la obtenia por
# su cuenta: dos analizaban el archivo de exclusiones y el tercero lo
# transcribia. Los tres discrepaban en su tamano. Un lector unico no basta si
# nada impide que aparezca un cuarto analisis, de modo que se comprueba que
# solo un archivo lea el de exclusiones, que sea el mismo que define la
# funcion, y que quien la llame lo cargue en lugar de reescribirlo.
#
# Lo que se busca no es quien nombra el archivo, porque la documentacion lo
# cita sin leerlo, sino quien delimita la seccion: el encabezado es la firma
# inconfundible de extraer la lista. Los dos patrones llevan un signo de
# repeticion donde el texto buscado lleva un espacio, de modo que esta
# comprobacion no se cuenta a si misma. No hay excepcion escrita a mano: la
# distincion es estructural, quien delimita escribe el encabezado y quien
# busca delimitadores escribe su patron.
fuentes_r <- sort(list.files("R", pattern = "[.]R$", full.names = TRUE))
txt_r <- lapply(fuentes_r, function(f)
  paste(readLines(f, warn = FALSE), collapse = "\n"))
names(txt_r) <- basename(fuentes_r)
busca <- function(pat) names(txt_r)[vapply(txt_r, grepl, logical(1),
                                           pattern = pat)]

lee_exc <- busca("Patient level +derived data")
define  <- busca("rutas_reservadas <- +function")
llaman  <- busca("rutas_reservadas\\(\\)")

cat("=== EL LECTOR DE RUTAS RESERVADAS ===\n")
cat("Archivos que delimitan la seccion:  ", length(lee_exc), "\n")
cat("Archivos que definen la funcion:    ", length(define), "\n")
cat("Archivos que la llaman:             ", length(llaman), "\n")

if (length(define) != 1) {
  cat("La funcion no se define en un solo archivo.\n"); quit(status = 1)
}
if (length(lee_exc) != 1 || !identical(lee_exc, define)) {
  cat("La seccion la delimita alguien que no es el lector:\n")
  for (f in setdiff(lee_exc, define)) cat("  ", f, "\n")
  cat("La lista volveria a obtenerse en mas de un sitio.\n"); quit(status = 1)
}
sin_cargar <- setdiff(llaman, c(define,
  busca(sprintf("source\\(\"R/%s\"", gsub("[.]", "[.]", define)))))
if (length(sin_cargar) > 0) {
  cat("Llaman a la funcion sin cargar al lector:\n")
  for (f in sin_cargar) cat("  ", f, "\n"); quit(status = 1)
}

# Y que llamarlo desde otro directorio del deposito devuelva la misma lista,
# porque de eso depende que los tres consumidores reciban lo mismo.
# Se carga por su nombre literal, que es lo que la comprobacion de arriba
# exige de todo el que la llame, esta incluida. Derivar el nombre aqui la
# eximiria de su propia regla.
source("R/00_rutas_reservadas.R")
rr <- rutas_reservadas()
otro <- paste0("cd outputs/fase20 && Rscript -e 'source(\"",
               normalizePath(file.path("R", define)),
               "\"); cat(rutas_reservadas()$ruta, sep = \"\\n\")' 2>/dev/null")
desde_otro <- suppressWarnings(system(otro, intern = TRUE))
if (!identical(rr$ruta, desde_otro)) {
  cat("Llamado desde otro directorio devuelve otra lista.\n"); quit(status = 1)
}
cat("Rutas reservadas:", nrow(rr),
    " identica desde otro directorio: si\n\n")

# ---------------------------------------------------------------------------
# El manifiesto de la multiplicidad declara que analisis se adopta.
# ---------------------------------------------------------------------------
# Ese deposito ofrece varias columnas de resistencia y solo una gobierna lo
# publicado. El manifiesto describia la maquinaria que quedo atras y no
# nombraba la que se usa: quien lo abriera para saber que se publico obtenia
# la respuesta equivocada. Ahora lo declara, y aqui se comprueba que lo
# declarado sea una columna real del deposito y ademas la que el documento usa
# de verdad. Sin la segunda mitad, el campo podria declarar cualquier cosa.

MF26 <- "outputs/fase26/manifiesto.json"
IV26 <- "outputs/fase26/intervalos_cobertura.csv"
if (file.exists(MF26) && file.exists(IV26)) {
  m26 <- jsonlite::fromJSON(MF26)
  iv26 <- read.csv(IV26, stringsAsFactors = FALSE)
  cat("=== ANALISIS ADOPTADO ===\n")
  if (is.null(m26$analisis_adoptado)) {
    cat("El manifiesto no declara que analisis gobierna lo publicado.\n")
    quit(status = 1)
  }
  ADOPT26 <- m26$analisis_adoptado
  cat("Declarado:", ADOPT26, "\n")
  if (!ADOPT26 %in% names(iv26)) {
    cat("El analisis declarado no es una columna del deposito.\n")
    quit(status = 1)
  }
  gen65 <- paste(readLines("R/65_generar_readme.R", warn = FALSE),
                 collapse = "\n")
  if (!grepl(ADOPT26, gen65, fixed = TRUE)) {
    cat("El documento no compone desde el analisis declarado.\n")
    quit(status = 1)
  }
  otros <- setdiff(grep("^resiste_", names(iv26), value = TRUE), ADOPT26)
  if (!setequal(m26$analisis_depositados_no_adoptados, otros)) {
    cat("Los analisis declarados como no adoptados no son los del deposito.\n")
    quit(status = 1)
  }
  cat("Es columna del deposito, es la que el documento usa, y los",
      length(otros), "restantes\nfiguran como depositados y no adoptados.\n\n")
}

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

# Los manifiestos que declaran la guardia solo pueden aumentar, y los que no
# la declaran solo pueden disminuir: son anteriores a ella y se rehacen al
# reejecutar su fase. Si el segundo recuento subiera, un deposito nuevo se
# habria escrito sin declararlo, que es la via por la que la guardia se
# eludiria sin dejar rastro.
reg[[length(reg)+1]] <- comprobar("Manifiestos que declaran la guardia",
  N_MF_DECLARAN, 21, 0.5)
reg[[length(reg)+1]] <- comprobar("Manifiestos anteriores a la guardia",
  N_MF_SIN, 4, 0.5)
reg[[length(reg)+1]] <- comprobar("Rutas reservadas que el lector establece",
  nrow(rr), 14, 0.5)
reg[[length(reg)+1]] <- comprobar("Rutas reservadas que son un directorio",
  sum(rr$directorio_entero), 1, 0.5)
reg[[length(reg)+1]] <- comprobar("Consumidores del lector unico",
  length(llaman), 5, 0.5)

# El estado derivado. Ningun deposito puede quedar indeterminado: un
# manifiesto posterior a la guardia que no la declarara seria justo la via de
# elusion sin rastro, y la derivacion no alcanza a cubrirlo.
# El orden de cada deposito que sale de una consulta ha de quedar determinado
# por su contenido. Una clave de orden que repita valores deja el resto a la
# implementacion de la base, y dos ejecuciones devuelven ordenes distintos.
# El orden de la matriz, antes y despues del arreglo. Lo que el documento
# publica es que el contenido no cambia y el orden si, y que el arreglo lo
# fija: las tres cosas se contrastan.
ordm <- leer(FUENTES[["Orden de la matriz"]])
if (!is.null(ordm)) {
  reg[[length(reg)+1]] <- comprobar("Matriz, celdas que difieren entre versiones",
    sum(ordm$celdas_que_difieren), 0, 0.5)
  reg[[length(reg)+1]] <- comprobar("Matriz, celdas comparadas",
    ordm$celdas_comparadas[1], 580325, 0.5)
  reg[[length(reg)+1]] <- comprobar("Matriz, posiciones que cambian antes del arreglo",
    ordm$pct_posiciones[ordm$comparacion == "antes_a frente a antes_b"],
    98.77, tolerancia(2))
  reg[[length(reg)+1]] <- comprobar("Matriz, posiciones que cambian despues",
    ordm$pct_posiciones[ordm$comparacion == "despues_a frente a despues_b"],
    0, tolerancia(2))
  reg[[length(reg)+1]] <- comprobar("Matriz, comparaciones depositadas",
    nrow(ordm), 5, 0.5)
}

cont <- leer(FUENTES[["Contactos con el sellado"]])
if (!is.null(cont)) {
  usa <- cont[cont$clase != "define la etiqueta", ]
  reg[[length(reg)+1]] <- comprobar("Contactos con la unidad reservada",
    nrow(usa), 5, 0.5)
  reg[[length(reg)+1]] <- comprobar("Clases de contacto",
    length(unique(usa$clase)), 3, 0.5)
  reg[[length(reg)+1]] <- comprobar("Procedimientos que definen la etiqueta",
    sum(cont$clase == "define la etiqueta"), 1, 0.5)
  reg[[length(reg)+1]] <- comprobar("Procedimientos que predicen sobre ella",
    sum(cont$clase == "predice sobre la unidad"), 1, 0.5)
  reg[[length(reg)+1]] <- comprobar("Procedimientos que reutilizan lo almacenado",
    sum(cont$clase == "reutiliza lo almacenado"), 3, 0.5)
  reg[[length(reg)+1]] <- comprobar("Procedimientos que la describen sin predecir",
    sum(cont$clase == "la describe sin predecir"), 1, 0.5)
}

pos <- leer(FUENTES[["Posterioridad de los analisis"]])
if (!is.null(pos)) {
  TER <- "posterior a la apertura y lee filas por paciente"
  reg[[length(reg)+1]] <- comprobar("Procedimientos contrastados",
    nrow(pos), 89, 0.5)
  reg[[length(reg)+1]] <- comprobar("Posteriores a la fijacion",
    sum(pos$posterior_a_la_fijacion), 67, 0.5)
  reg[[length(reg)+1]] <- comprobar("Posteriores a la apertura",
    sum(pos$posterior_a_la_apertura), 50, 0.5)
  reg[[length(reg)+1]] <- comprobar("Ademas leen filas por paciente",
    sum(pos$categoria == TER), 29, 0.5)
  reg[[length(reg)+1]] <- comprobar("Ajustan tras la apertura",
    sum(pos$posterior_a_la_apertura & pos$ajusta_un_modelo), 6, 0.5)
  # La cifra que decide si esa contabilidad es tranquilizadora o grave.
  reg[[length(reg)+1]] <- comprobar("Escriben artefacto del modelo despues",
    sum(pos$posterior_a_la_apertura & pos$escribe_artefacto_del_modelo), 0, 0.5)
  # Y que el reparto se compusiera con el trabajo ya comprometido: un
  # procedimiento sin alta en el historial es el que se esta introduciendo, y
  # su fecha aun no existe.
  reg[[length(reg)+1]] <- comprobar("Procedimientos sin alta registrada",
    sum(!pos$registrado), 0, 0.5)
  reg[[length(reg)+1]] <- comprobar("Producen una cifra publicada",
    sum(pos$produce_cifra_publicada), 37, 0.5)
  reg[[length(reg)+1]] <- comprobar("Posteriores que producen cifra publicada",
    sum(pos$posterior_a_la_apertura & pos$produce_cifra_publicada), 27, 0.5)
}

reg[[length(reg)+1]] <- comprobar("Celdas que resisten el analisis adoptado",
  sum(iv26$en_familia & iv26[[ADOPT26]]), 3, 0.5)
reg[[length(reg)+1]] <- comprobar("Analisis depositados y no adoptados",
  length(setdiff(grep("^resiste_", names(iv26), value = TRUE), ADOPT26)), 9, 0.5)

dsel <- leer(FUENTES[["Distancia de la reservada"]])
xsel <- leer(FUENTES[["Extremos por categoria"]])
if (!is.null(dsel) && !is.null(xsel)) {
  reg[[length(reg)+1]] <- comprobar("Distancia de la unidad reservada",
    dsel$distancia_de_la_reservada[1], 0.402, tolerancia(3))
  reg[[length(reg)+1]] <- comprobar("Mayor distancia de las demas",
    dsel$mayor_de_las_demas[1], 0.220, tolerancia(3))
  reg[[length(reg)+1]] <- comprobar("Veces la siguiente",
    dsel$veces_la_siguiente[1], 1.83, tolerancia(2))
  reg[[length(reg)+1]] <- comprobar("Unidades del perfil",
    dsel$unidades[1], 6, 0.5)
  # Que sea la mas distante y el extremo en todo es lo que el documento
  # afirma; si dejara de serlo, el generador ya se detiene, y aqui se cuenta.
  reg[[length(reg)+1]] <- comprobar("Categorias en que es extremo",
    sum(xsel$es_extremo), nrow(xsel), 0.5)
  reg[[length(reg)+1]] <- comprobar("Columnas del perfil de categorias",
    nrow(xsel), 5, 0.5)
}

pot <- leer(FUENTES[["Potencia no usada"]])
if (!is.null(pot)) {
  reg[[length(reg)+1]] <- comprobar("Ajuste efectivo de la comparacion",
    pot$ajuste_efectivo[1], 2487, 0.5)
  reg[[length(reg)+1]] <- comprobar("Ajuste posible con la calibracion",
    pot$ajuste_posible[1], 4010, 0.5)
  reg[[length(reg)+1]] <- comprobar("Estancias que quedaron sin usar",
    pot$estancias_no_usadas[1], 1523, 0.5)
  reg[[length(reg)+1]] <- comprobar("Aumento porcentual desaprovechado",
    pot$aumento_pct[1], 61.24, tolerancia(2))
  # El ajuste efectivo ha de seguir siendo el que la comparacion declara: si
  # las dos cifras se separaran, una de las dos describiria otro conjunto.
  rsm0 <- leer(FUENTES[["Comparacion ampliada, resumen"]])
  if (!is.null(rsm0))
    reg[[length(reg)+1]] <- comprobar("Ajuste efectivo frente al resumen",
      pot$ajuste_efectivo[1], rsm0$estancias_ajuste[1], 0.5)
}

apar <- leer(FUENTES[["Conjunto apartado"]])
if (!is.null(apar)) {
  reg[[length(reg)+1]] <- comprobar("Conjunto apartado, lo nombran",
    apar$procedimientos_que_lo_nombran[1], 2, 0.5)
  reg[[length(reg)+1]] <- comprobar("Conjunto apartado, lo leen",
    apar$procedimientos_que_lo_leen[1], 0, 0.5)
}

ordt <- leer(FUENTES[["Orden total de los depositos"]])
if (!is.null(ordt)) {
  reg[[length(reg)+1]] <- comprobar("Depositos de consulta sin orden total",
    sum(!ordt$orden_total), 0, 0.5)
  reg[[length(reg)+1]] <- comprobar("Depositos de consulta contrastados",
    nrow(ordt), 13, 0.5)
}

gpd <- leer(FUENTES[["Guarda por deposito"]])
if (!is.null(gpd)) {
  reg[[length(reg)+1]] <- comprobar("Depositos con estado de guardia establecido",
    sum(gpd$origen != "indeterminado"), nrow(gpd), 0.5)
  reg[[length(reg)+1]] <- comprobar("Depositos con estado establecido, recuento",
    nrow(gpd), 25, 0.5)
  reg[[length(reg)+1]] <- comprobar("Depositos escritos sin la guardia",
    sum(!gpd$guardia_activa), 4, 0.5)
  reg[[length(reg)+1]] <- comprobar("Depositos con la guardia declarada",
    sum(gpd$origen == "declarado en el manifiesto"), N_MF_DECLARAN, 0.5)
  reg[[length(reg)+1]] <- comprobar("Depositos con la guardia derivada",
    sum(gpd$origen == "derivado del historial"), N_MF_SIN, 0.5)
}
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
  # El documento afirma que el beneficio cruza el cero y no que converja a el.
  # Se contrasta el recuento de umbrales con beneficio negativo, el primero de
  # ellos y el minimo, que es lo que la frase publica.
  reg[[length(reg)+1]] <- comprobar("Umbrales con beneficio neto negativo",
    sum(s$bn_modelo < 0), 11, 0.5)
  reg[[length(reg)+1]] <- comprobar("Umbrales examinados",
    nrow(s), 30, 0.5)
  reg[[length(reg)+1]] <- comprobar("Primer umbral con beneficio negativo",
    min(s$umbral[s$bn_modelo < 0]), 0.20, tolerancia(2))
  reg[[length(reg)+1]] <- comprobar("Beneficio neto minimo",
    min(s$bn_modelo), -0.00231, tolerancia(5))
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
  # El documento dice que la dispersion queda practicamente igual y que la
  # afirmacion se apoya en la media y el minimo. Lo primero exige que la
  # diferencia sea pequena; lo segundo, que las otras dos vayan en su contra.
  o <- leer(FUENTES[["Transportabilidad, cobertura"]])
  if (!is.null(o)) {
    reg[[length(reg)+1]] <- comprobar("Ampliado, diferencia de dispersion",
      abs(sd(x$cobertura) - sd(o$cobertura)), 0.0008, tolerancia(4, 2))
    reg[[length(reg)+1]] <- comprobar("Ampliado, la media empeora",
      as.integer(mean(x$cobertura) < mean(o$cobertura)), 1, 0.5)
    reg[[length(reg)+1]] <- comprobar("Ampliado, el minimo empeora",
      as.integer(min(x$cobertura) < min(o$cobertura)), 1, 0.5)
  }
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

# La calibracion del conjunto de prueba, que es el cuarto dominio sobre el
# conjunto en el que se reportan los otros tres. Los recuentos por categoria
# se contrastan ademas contra la cobertura conforme, que es fuente
# independiente: si difirieran, las dos tablas describirian evaluaciones
# distintas y la comparacion entre las dos sedes no significaria nada.
x <- leer(FUENTES[["Calibracion en el conjunto de prueba"]])
c8 <- leer(FUENTES[["Cobertura conforme en prueba"]])
if (!is.null(x)) {
  s <- x[which.max(x$n), ]
  reg[[length(reg)+1]] <- comprobar("Prueba, probabilidad media mayoritaria",
    s$probabilidad_media, 0.8884, t4)
  reg[[length(reg)+1]] <- comprobar("Prueba, frecuencia observada mayoritaria",
    s$frecuencia_observada, 0.8874, t4)
  reg[[length(reg)+1]] <- comprobar("Prueba, desajuste de la mayoritaria",
    s$diferencia, 0.0010, t4)
  reg[[length(reg)+1]] <- comprobar("Prueba, categorias calibradas",
    nrow(x), 4, 0.5)
  if (!is.null(c8)) {
    reg[[length(reg)+1]] <- comprobar("Calibracion y cobertura, misma mayoritaria",
      x$n[which.max(x$n)], c8$n[which.max(c8$n)], 0.5)
    reg[[length(reg)+1]] <- comprobar("Calibracion y cobertura, mismas estancias",
      sum(x$n), sum(c8$n), 0.5)
  }
}

# La curva por tramos. Se contrasta el numero de tramos que el documento
# publica y que el agrupamiento cubra cada conjunto entero: una curva que
# describiera un subconjunto sin declararlo seria peor que no tenerla.
x <- leer(FUENTES[["Curva de calibracion"]])
if (!is.null(x) && !is.null(c8)) {
  reg[[length(reg)+1]] <- comprobar("Curva, tramos por categoria",
    max(x$grupo), 5, 0.5)
  reg[[length(reg)+1]] <- comprobar("Curva, categorias con tramos",
    length(unique(x$clase)), 4, 0.5)
  cl_may <- c8$clase[which.max(c8$n)]
  reg[[length(reg)+1]] <- comprobar("Curva, estancias cubiertas por los tramos",
    sum(x$n[x$clase == cl_may]), sum(c8$n), 0.5)
  reg[[length(reg)+1]] <- comprobar("Curva, casos cubiertos por los tramos",
    sum(x$observados[x$clase == cl_may]), max(c8$n), 0.5)
}

# La cota sobre la regla del maximo. Ademas de los extremos publicados se
# comprueba la derivacion: que la cota sea el complemento del minimo y la
# separacion su diferencia, calculadas desde el propio archivo. Una cota que
# no se dedujera de su minimo no seria una cota.
x <- leer(FUENTES[["Cota sobre la regla del maximo"]])
if (!is.null(x)) {
  p <- x[x$conjunto == "prueba", ]
  s <- x[x$conjunto == "unidad reservada", ]
  reg[[length(reg)+1]] <- comprobar("Cota, minimo de la mayoritaria en prueba",
    p$prob_minima_mayoritaria, 0.5007, t4)
  reg[[length(reg)+1]] <- comprobar("Cota, tope de las minoritarias en prueba",
    p$cota_de_cada_minoritaria, 1 - p$prob_minima_mayoritaria, t4)
  reg[[length(reg)+1]] <- comprobar("Cota, margen en prueba",
    p$separacion,
    p$prob_minima_mayoritaria - p$cota_de_cada_minoritaria, t4)
  reg[[length(reg)+1]] <- comprobar("Cota, maximo observado en prueba",
    p$maximo_observado_minoritarias, 0.3331, t4)
  reg[[length(reg)+1]] <- comprobar("Cota, minimo de la mayoritaria en la sellada",
    s$prob_minima_mayoritaria, 0.5698, t4)
  reg[[length(reg)+1]] <- comprobar("Cota, tope de las minoritarias en la sellada",
    s$cota_de_cada_minoritaria, 1 - s$prob_minima_mayoritaria, t4)
  reg[[length(reg)+1]] <- comprobar("Cota, margen en la sellada",
    s$separacion,
    s$prob_minima_mayoritaria - s$cota_de_cada_minoritaria, t4)
  reg[[length(reg)+1]] <- comprobar("Cota, maximo observado en la sellada",
    s$maximo_observado_minoritarias, 0.3804, t4)
  # El documento enuncia la imposibilidad. Solo es licita si el minimo supera
  # la mitad en los dos conjuntos, de modo que se cuenta cuantos la sostienen.
  reg[[length(reg)+1]] <- comprobar("Cota, conjuntos que sostienen la imposibilidad",
    sum(x$sostiene_la_imposibilidad), 2, 0.5)
  reg[[length(reg)+1]] <- comprobar("Cota, conjuntos evaluados",
    nrow(x), 2, 0.5)
}

x <- leer(FUENTES[["Pendiente de calibracion"]])
if (!is.null(x)) {
  iw <- which.max(x$pendiente_ic_superior - x$pendiente_ic_inferior)
  reg[[length(reg)+1]] <- comprobar("Pendiente, la menor de las cuatro",
    min(x$pendiente), 0.8169, t4)
  reg[[length(reg)+1]] <- comprobar("Pendiente, la mayor de las cuatro",
    max(x$pendiente), 1.0381, t4)
  reg[[length(reg)+1]] <- comprobar("Calibracion en conjunto, la menor",
    min(x$calibracion_en_conjunto), -0.0103, t4)
  reg[[length(reg)+1]] <- comprobar("Calibracion en conjunto, la mayor",
    max(x$calibracion_en_conjunto), 0.0208, t4)
  reg[[length(reg)+1]] <- comprobar("Pendiente, extremo inferior del intervalo mas ancho",
    x$pendiente_ic_inferior[iw], 0.5666, t4)
  reg[[length(reg)+1]] <- comprobar("Pendiente, extremo superior del intervalo mas ancho",
    x$pendiente_ic_superior[iw], 1.1197, t4)
  reg[[length(reg)+1]] <- comprobar("Pendiente, intervalos que cubren la unidad",
    sum(x$contiene_la_unidad), 4, 0.5)
}

# La probabilidad que recibe un caso real de la categoria urinaria, que es la
# cifra con lectura clinica. Se contrasta ademas contra la prevalencia de esa
# categoria en la fase anterior, que es de donde el documento la toma.
x <- leer(FUENTES[["Recorrido de la probabilidad predicha"]])
cp <- leer(FUENTES[["Calibracion en el conjunto de prueba"]])
if (!is.null(x) && !is.null(cp)) {
  u <- x[x$conjunto == "prueba" & x$estrato == "de la categoria" &
         x$clase == "urinario", ]
  reg[[length(reg)+1]] <- comprobar("Urinario real, probabilidad mediana",
    u$mediana, 0.0456, t4)
  reg[[length(reg)+1]] <- comprobar("Urinario real, probabilidad maxima",
    u$maximo, 0.1141, t4)
  reg[[length(reg)+1]] <- comprobar("Urinario real, casos del estrato",
    u$n, cp$n[cp$clase == "urinario"], 0.5)
  reg[[length(reg)+1]] <- comprobar("Recorrido, conjuntos por categoria y estrato",
    nrow(x), 16, 0.5)
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

# ---------------------------------------------------------------------------
# Composicion de la cohorte y descripcion de las candidatas de laboratorio
#
# Ambos depositos derivan de agregados anteriores, de modo que aqui se
# contrasta la derivacion contra su fuente y no el archivo contra si mismo.
# ---------------------------------------------------------------------------

x <- leer(FUENTES[["Composicion de la cohorte analizada"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Umbral de retencion de unidades",
    x$umbral, 500, 0.5)
  reg[[length(reg)+1]] <- comprobar("Unidades retenidas",
    x$unidades_retenidas, 6, 0.5)
  reg[[length(reg)+1]] <- comprobar("Estancias retenidas",
    x$estancias_retenidas, 22778, 0.5)
  reg[[length(reg)+1]] <- comprobar("Unidades descartadas",
    x$unidades_descartadas, 10, 0.5)
  reg[[length(reg)+1]] <- comprobar("Estancias descartadas",
    x$estancias_descartadas, 435, 0.5)
  reg[[length(reg)+1]] <- comprobar("Estancias de las unidades de desarrollo",
    x$estancias_de_desarrollo, 18054, 0.5)
  reg[[length(reg)+1]] <- comprobar("Estancias de la unidad reservada",
    x$estancias_reservadas, 4724, 0.5)
  reg[[length(reg)+1]] <- comprobar("Unidades de desarrollo",
    x$unidades_de_desarrollo, 5, 0.5)
  # La suma ha de reproducir el embudo, que es la fuente independiente.
  f <- leer(FUENTES[["Embudo de seleccion"]])
  if (!is.null(f))
    reg[[length(reg)+1]] <- comprobar("Cohorte, retenidas mas descartadas",
      x$estancias_retenidas + x$estancias_descartadas,
      f$n[f$paso == "p5_cohorte_final"], 0.5)
}

x <- leer(FUENTES[["Unidades de la cohorte"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Unidades en la cohorte final",
    nrow(x), 16, 0.5)
}

x <- leer(FUENTES[["Determinaciones candidatas"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Determinaciones candidatas",
    nrow(x), 73, 0.5)
  reg[[length(reg)+1]] <- comprobar("Determinaciones retenidas",
    sum(x$retenida), 17, 0.5)
  reg[[length(reg)+1]] <- comprobar("Determinaciones descartadas",
    sum(!x$retenida), 56, 0.5)
  reg[[length(reg)+1]] <- comprobar("Cobertura minima de las retenidas",
    min(x$cobertura_pct[x$retenida]), 45.2, t1)
  reg[[length(reg)+1]] <- comprobar("Cobertura maxima de las descartadas",
    max(x$cobertura_pct[!x$retenida]), 68.5, t1)
  # Lo que el apartado de laboratorio publica sobre las candidatas. La
  # primera es la que refuta el ataque de que las diecisiete sean las de
  # mayor cobertura, y por eso se contrasta que la mayor este descartada.
  im <- which.max(x$cobertura_pct)
  reg[[length(reg)+1]] <- comprobar("La de mayor cobertura quedo retenida",
    as.integer(x$retenida[im]), 0, 0.5)
  reg[[length(reg)+1]] <- comprobar("Cobertura de la mayor de todas",
    x$cobertura_pct[im], 68.5, t1)
  reg[[length(reg)+1]] <- comprobar("Candidatas de orina",
    sum(x$fluido == "Urine"), 6, 0.5)
  reg[[length(reg)+1]] <- comprobar("Retenidas de orina",
    sum(x$retenida & x$fluido == "Urine"), 0, 0.5)
  reg[[length(reg)+1]] <- comprobar("Cobertura maxima entre las de orina",
    max(x$cobertura_pct[x$fluido == "Urine"]), 9.9, t1)
  reg[[length(reg)+1]] <- comprobar("Descartadas sobre la retenida minima",
    sum(x$cobertura_pct[!x$retenida] >
        min(x$cobertura_pct[x$retenida])), 12, 0.5)
}

x <- leer(FUENTES[["Separacion de las candidatas"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Atributos que separan los dos grupos",
    sum(x$separa_los_grupos), 0, 0.5)
}

# La cota de casos completos y el recuento de candidatas poco frecuentes se
# contrastan contra la tabla de la que se derivan, no contra si mismos.
x <- leer(FUENTES[["Viabilidad de la comparacion"]])
d <- leer(FUENTES[["Determinaciones candidatas"]])
if (!is.null(x) && !is.null(d)) {
  reg[[length(reg)+1]] <- comprobar("Cota de casos completos en las candidatas",
    x$cota_casos_completos, min(d$estancias), 0.5)
  reg[[length(reg)+1]] <- comprobar("Candidatas de cobertura baja",
    x$candidatas_por_debajo,
    sum(d$cobertura_pct < x$umbral_de_cobertura_baja), 0.5)
  reg[[length(reg)+1]] <- comprobar("Denominador de la cota",
    x$estancias_del_denominador, 65366, 0.5)
  reg[[length(reg)+1]] <- comprobar("Umbral de estancias de las candidatas",
    x$umbral_de_estancias, 3000, 0.5)
}

# El recuento efectivo de casos completos, que es lo que establece si la
# comparacion sobre las setenta y tres podia hacerse.
x <- leer(FUENTES[["Casos completos en las candidatas"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Casos completos en las candidatas",
    x$estancias_completas[x$conjunto == "candidatas"], 0, 0.5)
  reg[[length(reg)+1]] <- comprobar("Casos completos en el subconjunto",
    x$estancias_completas[x$conjunto == "subconjunto ampliado"], 5867, 0.5)
  reg[[length(reg)+1]] <- comprobar("Denominador del recuento",
    x$denominador[1], 23213, 0.5)
  reg[[length(reg)+1]] <- comprobar("Maximo de candidatas en una estancia",
    x$maximo_presentes[x$conjunto == "candidatas"], 69, 0.5)
}

x <- leer(FUENTES[["Comparacion ampliada, resumen"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Comparacion, determinaciones anadidas",
    x$determinaciones_anadidas, 12, 0.5)
  reg[[length(reg)+1]] <- comprobar("Comparacion, estancias completas",
    x$estancias_completas, 3498, 0.5)
  reg[[length(reg)+1]] <- comprobar("Comparacion, estancias analizables",
    x$estancias_analizables, 11718, 0.5)
  reg[[length(reg)+1]] <- comprobar("Comparacion, porcentaje de las analizables",
    x$pct_de_las_analizables, 29.85, tolerancia(2))
  reg[[length(reg)+1]] <- comprobar("Comparacion, ajuste mas prueba",
    x$estancias_ajuste + x$estancias_prueba, x$estancias_completas, 0.5)
  reg[[length(reg)+1]] <- comprobar("Comparacion, minoritarias retenidas",
    x$promedio_minoritarias_retenidas, 0.6635, t4)
  reg[[length(reg)+1]] <- comprobar("Comparacion, minoritarias ampliada",
    x$promedio_minoritarias_ampliada, 0.6523, t4)
  reg[[length(reg)+1]] <- comprobar("Comparacion, diferencia",
    x$diferencia_minoritarias, -0.0113, t4d)
  reg[[length(reg)+1]] <- comprobar("Comparacion, extremo inferior",
    x$ic_inferior_minoritarias, -0.0249, t4)
  reg[[length(reg)+1]] <- comprobar("Comparacion, extremo superior",
    x$ic_superior_minoritarias, 0.0027, t4)
  reg[[length(reg)+1]] <- comprobar("Comparacion, clases que excluyen el cero",
    x$clases_que_excluyen_el_cero, 1, 0.5)
  reg[[length(reg)+1]] <- comprobar("Comparacion, clases que resisten Holm",
    x$clases_que_resisten_holm, 0, 0.5)
  reg[[length(reg)+1]] <- comprobar("Comparacion, replicas del remuestreo",
    x$replicas, 10000, 0.5)
}

# La cascada de filtros. Cada peldano ha de cuadrar con el siguiente y con
# los desgloses, de modo que la comprobacion cruzada es una identidad y no
# una desigualdad: si algun dia dejara de cerrar, la cifra publicada
# describiria un conjunto distinto del ajustado y el verificador lo diria.
cas <- leer(FUENTES[["Cascada de casos completos"]])
grp <- leer(FUENTES[["Completos por conjunto de la particion"]])
gcl <- leer(FUENTES[["Conjunto ajustado por grupo y clase"]])
rsm <- leer(FUENTES[["Comparacion ampliada, resumen"]])
if (!is.null(cas)) {
  reg[[length(reg)+1]] <- comprobar("Cascada, peldanos depositados",
    nrow(cas), 4, 0.5)
  reg[[length(reg)+1]] <- comprobar("Cascada, cohorte de partida",
    cas$estancias[1], 23213, 0.5)
  reg[[length(reg)+1]] <- comprobar("Cascada, completas en las comparadas",
    cas$estancias[2], 5866, 0.5)
  reg[[length(reg)+1]] <- comprobar("Cascada, y ademas en entrenamiento o prueba",
    cas$estancias[3], 3774, 0.5)
  reg[[length(reg)+1]] <- comprobar("Cascada, y ademas en categoria modelada",
    cas$estancias[4], 3498, 0.5)
  if (!is.null(rsm))
    reg[[length(reg)+1]] <- comprobar("Cascada, cierra en el conjunto ajustado",
      cas$estancias[4], rsm$estancias_completas, 0.5)
  if (!is.null(grp)) {
    reg[[length(reg)+1]] <- comprobar("Cascada, el primer peldano suma los grupos",
      cas$estancias[1], sum(grp$estancias), 0.5)
    reg[[length(reg)+1]] <- comprobar("Cascada, el segundo suma las completas",
      cas$estancias[2], sum(grp$completas), 0.5)
    reg[[length(reg)+1]] <- comprobar("Cascada, el tercero suma los dos conjuntos",
      cas$estancias[3],
      sum(grp$completas[grp$grupo %in% c("entrenamiento", "prueba")]), 0.5)
    reg[[length(reg)+1]] <- comprobar("Completitud del conjunto sellado",
      grp$pct_del_grupo[grp$grupo == "sellado"], 7.87, tolerancia(2))
    reg[[length(reg)+1]] <- comprobar("Completitud del entrenamiento",
      grp$pct_del_grupo[grp$grupo == "entrenamiento"], 29.78, tolerancia(2))
  }
  if (!is.null(gcl))
    reg[[length(reg)+1]] <- comprobar("Cascada, el cuarto suma el desglose",
      cas$estancias[4], sum(gcl$estancias), 0.5)
}

# La completitud analitica sobre las determinaciones del modelo publicado.
# La asimetria se publica en dos secciones y en direccion contraria a la que
# la fase trigesimo segunda mide sobre otras determinaciones, de modo que la
# comprobacion fija las dos y el sentido de la diferencia.
cpc <- leer(FUENTES[["Completitud por conjunto"]])
cpg <- leer(FUENTES[["Completitud por grupo"]])
cpd <- leer(FUENTES[["Completitud por determinacion"]])
if (!is.null(cpc)) {
  de <- cpc[cpc$conjunto == "desarrollo", ]
  ur <- cpc[cpc$conjunto == "unidad reservada", ]
  reg[[length(reg)+1]] <- comprobar("Completitud, determinaciones del modelo",
    cpc[["determinaciones"]][1], 17, 0.5)
  reg[[length(reg)+1]] <- comprobar("Completitud, completas en desarrollo",
    de$pct_completas, 30.34, tolerancia(2))
  reg[[length(reg)+1]] <- comprobar("Completitud, completas en la reservada",
    ur$pct_completas, 60.69, tolerancia(2))
  reg[[length(reg)+1]] <- comprobar("Completitud, celdas observadas en desarrollo",
    de$pct_valores_presentes, 66, tolerancia(2))
  reg[[length(reg)+1]] <- comprobar("Completitud, celdas observadas en la reservada",
    ur$pct_valores_presentes, 88.08, tolerancia(2))
  # El sentido de la diferencia es lo que el texto afirma. Si se invirtiera,
  # la frase publicada diria lo contrario de lo que el archivo sostiene.
  reg[[length(reg)+1]] <- comprobar("Completitud, la reservada supera al desarrollo",
    as.integer(ur$pct_valores_presentes > de$pct_valores_presentes), 1, 0.5)
  reg[[length(reg)+1]] <- comprobar("Completitud, bloques que suman la cohorte",
    sum(cpc$estancias), 23213, 0.5)
  if (!is.null(cpg))
    reg[[length(reg)+1]] <- comprobar("Completitud, grupos y bloques coinciden",
      sum(cpg$valores_presentes), sum(cpc$valores_presentes), 0.5)
  if (!is.null(cpd)) {
    d1 <- cpd[cpd$conjunto == "desarrollo", ]
    d2 <- cpd[cpd$conjunto == "unidad reservada", ]
    o <- match(d1$determinacion, d2$determinacion)
    reg[[length(reg)+1]] <- comprobar("Completitud, determinaciones contrastadas",
      nrow(d1), 17, 0.5)
    reg[[length(reg)+1]] <- comprobar("Completitud, determinaciones mejor medidas alli",
      sum(d2$pct_presentes[o] > d1$pct_presentes), 15, 0.5)
  }
}

x <- leer(FUENTES[["Multiplicidad de las diferencias"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Diferencias corregidas conjuntamente",
    x$comparaciones[1], 4, 0.5)
  reg[[length(reg)+1]] <- comprobar("Diferencias que resisten en algun nivel",
    max(x$resisten), 0, 0.5)
  reg[[length(reg)+1]] <- comprobar("Niveles contrastados en la comparacion",
    nrow(x), 2, 0.5)
}

# El remuestreo es aleatorio: los extremos se contrastan con la tolerancia de
# cuatro decimales, que es la que el archivo conserva, y el recuento de
# intervalos que excluyen el cero con la suya. Si el remuestreo dejara de
# reproducirse, la semilla habria cambiado y procede saberlo.
x <- leer(FUENTES[["Intervalo de la diferencia"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Cantidades con intervalo depositado",
    nrow(x), 5, 0.5)
  reg[[length(reg)+1]] <- comprobar("Intervalos que excluyen el cero",
    sum(x$excluye_cero), 1, 0.5)
  reg[[length(reg)+1]] <- comprobar("Promedio minoritarias, excluye el cero",
    as.integer(x$excluye_cero[x$cantidad == "promedio_minoritarias"]), 0, 0.5)
}

x <- leer(FUENTES[["Comparacion ampliada, por clase"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Comparacion, clases evaluadas",
    nrow(x), 4, 0.5)
  reg[[length(reg)+1]] <- comprobar("Comparacion, clases donde la ampliada gana",
    sum(x$diferencia > 0), 1, 0.5)
}

# La procedencia de la seleccion. El acta resume, y el detalle por version
# esta al lado: se contrasta que el detalle tenga tantas filas como commits
# declara el acta y que todos sus resumenes coincidan con el que el acta
# publica. Un acta que afirmara invariancia sobre un detalle que no la
# mostrara seria peor que no tenerla.
x <- leer(FUENTES[["Procedencia de la seleccion"]])
v <- leer(FUENTES[["Versiones del bloque"]])
if (!is.null(x)) {
  reg[[length(reg)+1]] <- comprobar("Commits que cambian la seleccion",
    x$commits_que_tocan_la_lista, 1, 0.5)
  reg[[length(reg)+1]] <- comprobar("Commits que tocan el archivo",
    x$commits_que_tocan_el_archivo, 2, 0.5)
  reg[[length(reg)+1]] <- comprobar("Identificadores que la lista declara",
    x$identificadores, 17, 0.5)
  if (!is.null(v)) {
    reg[[length(reg)+1]] <- comprobar("Versiones del bloque contrastadas",
      nrow(v), x$commits_que_tocan_el_archivo, 0.5)
    reg[[length(reg)+1]] <- comprobar("Resumenes distintos entre versiones",
      length(unique(v$resumen_del_bloque)), x$commits_que_tocan_la_lista, 0.5)
    reg[[length(reg)+1]] <- comprobar("Versiones cuyo resumen es el del acta",
      sum(v$resumen_del_bloque == x$resumen_del_bloque), nrow(v), 0.5)
    reg[[length(reg)+1]] <- comprobar("Versiones que declaran los mismos identificadores",
      sum(v$identificadores == x$identificadores), nrow(v), 0.5)
  }
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
