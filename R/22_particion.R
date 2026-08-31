set.seed(20260818)

m <- read.csv("outputs/fase4/matriz_limpia.csv")

# La particion asigna por posicion de fila, y la semilla fija la permutacion
# de esas posiciones y no la de los pacientes. Si el archivo llega en otro
# orden, la misma semilla reparte a estancias distintas. La consulta que
# construye la matriz no fijaba su orden, de modo que dos ejecuciones del
# mismo codigo con la misma semilla producian particiones distintas: se midio
# el 2026-08-31 y el noventa y nueve por ciento de las posiciones cambiaban,
# con el contenido identico celda a celda.
#
# Se ordena por estancia antes de repartir, y las categorias se recorren en
# orden alfabetico en lugar de en orden de aparicion, que tambien dependia del
# orden de fila. Con las dos cosas, la semilla fija pacientes.
#
# LA PARTICION PUBLICADA ES ANTERIOR A ESTE ARREGLO. Vive en
# outputs/fase5/matriz_particionada.csv y no se regenera: rehacerla exigiria
# reejecutar el modelo entero, y sus decisiones de diseno se tomaron habiendo
# visto ya la unidad reservada. Este procedimiento queda reproducible hacia
# adelante y no reproduce aquel archivo, y asi ha de declararse.
m <- m[order(m$stay_id), ]
row.names(m) <- NULL

SELLADA <- "Cardiac Vascular Intensive Care Unit (CVICU)"

m$grupo <- NA_character_
m$grupo[m$unidad == SELLADA] <- "sellado"

# Solo se conservan las unidades con volumen suficiente. Las unidades
# residuales, con menos de 500 estancias, no permiten estimacion estable y
# se excluyen del analisis en lugar de agruparse de forma arbitraria.
grandes <- names(which(table(m$unidad) >= 500))
elegibles <- which(m$unidad %in% grandes & m$unidad != SELLADA)

# Particion estratificada por clase. Dentro de cada clase se reparte al azar
# en las proporciones fijadas, de modo que la distribucion del desenlace sea
# comparable entre los tres conjuntos. Sin estratificar, las clases pequenas
# podrian quedar concentradas en uno solo por azar.
for (cl in sort(unique(m$clase[elegibles]))) {
  idx <- elegibles[m$clase[elegibles] == cl]
  idx <- sample(idx)
  n <- length(idx)
  corte1 <- floor(0.50 * n)
  corte2 <- floor(0.80 * n)
  m$grupo[idx[1:corte1]]              <- "entrenamiento"
  m$grupo[idx[(corte1 + 1):corte2]]   <- "calibracion"
  m$grupo[idx[(corte2 + 1):n]]        <- "prueba"
}

m$grupo[is.na(m$grupo)] <- "excluido"

cat("\n=== TAMANO DE CADA GRUPO ===\n")
print(table(m$grupo))

cat("\n=== CLASES POR GRUPO ===\n")
print(table(m$grupo, m$clase))

cat("\n=== PROPORCIONES POR GRUPO (%) ===\n")
print(round(100 * prop.table(table(m$grupo, m$clase), 1), 1))

cat("\n=== CASOS DISPONIBLES PARA CALIBRACION CONFORME ===\n")
cal <- table(m$clase[m$grupo == "calibracion"])
cal <- cal[names(cal) != "abstencion"]
sd_cob <- round(sqrt(0.1 * 0.9 / as.integer(cal)), 3)
print(data.frame(clase = names(cal),
                 n = as.integer(cal),
                 sd_cobertura = sd_cob), row.names = FALSE)

write.csv(m, "outputs/fase5/matriz_particionada.csv", row.names = FALSE)

sellado <- m[m$grupo == "sellado", ]
write.csv(sellado, "outputs/fase5/SELLADO_NO_ABRIR.csv", row.names = FALSE)

cat("\nParticion escrita. El conjunto sellado queda en",
    "outputs/fase5/SELLADO_NO_ABRIR.csv\n")
cat("Semilla registrada: 20260818\n")
