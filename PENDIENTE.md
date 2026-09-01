# Pendiente

Estado del encargo, no documentacion del trabajo. Recoge lo que falta de la
revision previa al manuscrito, para que una sesion nueva sepa donde retomar.
Se borra cuando quede vacio.

Van once bloqueantes de catorce, mas el hallazgo de reproducibilidad que no
estaba en la lista y salio por el camino. Quedan tres y la parte B.

## Como se trabaja

Por lotes, un commit de una linea en ingles por lote y sin coletillas; cada
cifra publicada leida de un archivo versionado y ninguna escrita a mano;
`R/59_verificar_cifras.R` contrastando lo que se publique, y `R/64`, `R/67`,
`R/86` y `R/87` antes de cerrar. Sin subagentes. Cuando algo se puede
comprobar, se comprueba en vez de declararse.

La cadena de la fase cuarta a la quinta esta congelada y no se reejecuta:
rehacerla produciria un modelo cuyas decisiones de diseno se tomaron habiendo
visto ya la unidad reservada.

## Lote 7

### A10. Los analisis posteriores, enumerados a mano

`R/82_decisiones_vivas.R:157`, la constante `POSTERIORES`, declara siete y el
documento titula el apartado como si fuera un censo. Hay al menos nueve mas
que depositan cifras publicadas: R/62, R/63, R/66, R/69, R/71, R/72, R/73,
R/75, R/76.

Correccion acordada: derivar del historial todo procedimiento cuya alta sea
posterior a la fijacion, y separar en dos apartados, porque la categoria que
importa para el manuscrito no es la misma: posteriores a la seleccion, y
posteriores a la apertura del conjunto sellado. `R/72` y `R/73` son de los
segundos y leen matrices a nivel de fila.

### A11. Las cinco diferencias son seis

`R/82_decisiones_vivas.R:465` y el apartado de laboratorio del README. El
conjunto de ajuste de la comparacion de especificaciones es menor por dos
razones y solo se declara una: ademas de la restriccion a casos completos,
`R/80_comparacion_ampliada_labs.R:199` excluye el grupo de calibracion
entero, que es el treinta por ciento del desarrollo y aqui no hace falta.

Correccion acordada: declarar la sexta diferencia, y en `R/80` o bien
incorporar la calibracion al ajuste o justificar en la cabecera por que no.

### A12. El manifiesto de la fase 26 describe la maquinaria superseded

`R/74_intervalos_cobertura.R:633` y siguientes. Declara intervalos de Clopper
y Pearson, valor p binomial y correcciones de Bonferroni y Holm. El documento
publica los intervalos Beta-Binomial, el valor p beta y beta-Holm; el analisis
adoptado es `resiste_beta_holm_0025`. Quien abra el manifiesto para saber que
se publico obtiene la respuesta equivocada.

Correccion acordada: nombrar por separado lo publicado y lo condicionado al
umbral, y consignar el analisis adoptado.

## Lote 8. Parte B

Sin acuerdo cerrado todavia. Por orden de lo que un revisor puede comprobar
contra los archivos:

    B.4  texto que afirma mas que el archivo: el estrechamiento "entre 10 y
         34" cuyo minimo real es 9.69; la concordancia entre intervalo y
         contraste que ningun archivo deposita; la contencion que R/74
         comprueba solo por anchura; la ganancia del comparador de arboles
         sin nombrar sobre que se promedia; el orden de magnitud de la
         positividad, que ninguna comprobacion contrasta
    B.5  denominadores no declarados: el umbral de tres mil sobre primeras
         estancias y no sobre la cohorte; los empates por variable con su
         propio denominador cada uno; la frecuencia observada de la sellada
         sobre las cuatro categorias modeladas
    B.3  referentes ambiguos y precision semantica, nueve casos, entre ellos
         la aposicion colgante de las tres celdas que resisten y las "tres
         cosas" que acotan la comparacion siendo una de ellas una ventaja
    B.6  herramienta: R/59 y R/64 escriben su documento antes de la puerta de
         aprobado; R/65, R/68, R/64 y R/59 sin las guardas de ancho y de
         repertorio que R/82 si tiene; el denominador 65366 escrito a mano en
         R/16:33; el umbral 3000 duplicado en R/78:37; la auditoria que
         inspecciona solo la primera linea de cada archivo
    B.7  comprobaciones huerfanas y cifras sin comprobacion, entre ellas
         outputs/tipos_muestra_ventana6h.csv, que sostiene una afirmacion
         publicada y ninguna relacion lo nombra
    B.1  composicion: renglones huerfanos de dos y tres palabras en README y
         PROTOCOLO
    B.2  oraciones que empiezan por cifra, cinco casos
    B.8  nomenclatura entre archivos, cuatro casos
    B.9  un hecho favorable que el README no aprovecha: la unidad sellada es
         la de mayor distancia de las seis, de modo que la validacion externa
         es el caso peor

## Lo que no va al manuscrito

La seccion de asistencia por modelos de lenguaje del README se mantiene. Es
una declaracion exigida, no un rastro, y retirarla expondria al trabajo a la
unica acusacion de la que hoy esta a salvo.
