# Decisiones vigentes

Registro de trabajo. Recoge lo que ya esta decidido, con que fundamento y en
que archivo consta. Se consulta antes de escribir sobre cualquiera de estos
puntos, para que lo que se publique se apoye en los archivos y no en el
recuerdo de quien redacta.

Generado el 2026-09-01 por `R/82_decisiones_vivas.R`. Cada ruta que aqui figura
se comprueba al componer el documento: ha de existir y ha de estar bajo control
de versiones. El procedimiento se detiene si alguna falta, de modo que este
registro no puede sobrevivir a los archivos que lo sostienen.

## Analisis posteriores a la seleccion

La seleccion de determinaciones quedo fijada el 2026-08-18 en `R/19_matriz.R`,
y el registro del historial que lo acredita esta en
`outputs/fase33/procedencia_seleccion.csv`, con el detalle por version en
`outputs/fase33/versiones_del_bloque.csv`. La acreditacion compara el contenido
del bloque en cada version que el historial conserva y exige que todos los
resumenes coincidan entre si y con el que hay en disco. Una busqueda que solo
contara apariciones de la cadena que abre la lista no habria visto una edicion
dentro de ella, y por eso se sustituyo. Lo que acredita es cuando se fijo, no
que estuviera razonada: el criterio con que se eligieron no consta en ninguna
parte, y ningun procedimiento posterior lo reconstruye ni debe presentarse como
si lo hiciera.

Cuantos analisis son posteriores no se enumera. `R/90_posterioridad.R` lo
deriva del historial, y con tres categorias, porque no comprometen lo mismo.

Posteriores a la fijacion hay 66. De ellos, 49 son ademas posteriores al
2026-08-20, fecha en que la unidad reservada se abrio, que sale del alta de
`outputs/fase11/cobertura_sellado.csv` y no de una constante escrita. Y 29 de
esos leen ademas alguna tabla a nivel de paciente. El reparto completo,
procedimiento a procedimiento, queda en `outputs/fase41/posterioridad.csv`.

Ese ultimo numero solo alarma leido sin lo que lo acompana. Casi todo el
trabajo posterior a la apertura son rondas de verificacion y de documentacion,
y lo que importaria es que alguna reajustase o reescribiera el modelo
publicado.

Posteriores a la apertura que invocan un ajuste hay 6, y los que escriben en
las fases donde vive el modelo publicado, 0: ajustan el suyo leyendo la
especificacion congelada, y depositan fuera de ellas. Esas fases tampoco se
nombran en la comprobacion, se derivan de donde hay un objeto ajustado bajo
control de versiones, y el procedimiento se detiene si alguna vez alguno
escribiera en ellas.

Y de esos 49, 26 depositan algo de lo que el documento toma una cifra. Los
otros 23 son comprobaciones: se ejecutan, se detienen si algo no cuadra, y no
publican nada por su cuenta. Que un archivo sea publicado tampoco se enumera:
lo es si lo lee un generador de prosa, y un generador de prosa es el que
escribe un documento y lo compone con la guardia que rechaza cifras literales.
Esa guardia es lo que separa un documento de un libro de asientos, y se lee del
codigo y no de los nombres.

Los que siguen se describen uno a uno por lo que dicen de la seleccion, no
porque sean los unicos posteriores a ella. De cada uno se comprueba contra el
historial que su alta lo sea, en lugar de fiarlo a lo que su cabecera declare.

`R/78_determinaciones_candidatas.R`, alta el 2026-08-30. Pregunta que distingue
a las determinaciones retenidas de las descartadas. La respuesta, en
`outputs/fase30/separacion_candidatas.csv`, es que ningun valor de los tres
atributos registrados es exclusivo de las retenidas. El tipo de muestra si
informa en un sentido, y el documento lo dice: todas las retenidas son de
sangre y todas las de orina se descartaron, pero ninguna de esas alcanza la
cobertura de la retenida menos frecuente, de modo que no anade nada al orden
por cobertura. Ese resultado negativo es el hallazgo, y se publica como tal.

`R/79_casos_completos_candidatas.R`, alta el 2026-08-31. Cuenta cuantas
estancias reunen todas las candidatas. Consta en
`outputs/fase31/casos_completos.csv`.

`R/80_comparacion_ampliada_labs.R`, alta el 2026-08-31. Compara la
especificacion retenida contra una ampliada. Deposita en
`outputs/fase32/comparacion_resumen.csv`,
`outputs/fase32/comparacion_por_clase.csv`,
`outputs/fase32/intervalo_diferencia.csv` y
`outputs/fase32/multiplicidad_diferencias.csv`.

`R/81_procedencia_seleccion.R`, alta el 2026-08-31. Recoge la procedencia de la
seleccion en `outputs/fase33/procedencia_seleccion.csv`.

`R/70_lambda_validacion_interna.R`, alta el 2026-08-26. Comprueba si la
penalizacion se habria elegido igual sin mirar el conjunto de evaluacion.
Consta en `outputs/fase22/decision_lambda.csv`.

`R/74_intervalos_cobertura.R`, alta el 2026-08-27. Calcula los intervalos de
cobertura y corrige por multiplicidad. Deposita en
`outputs/fase26/intervalos_cobertura.csv`,
`outputs/fase26/criterios_cobertura.csv` y
`outputs/fase26/recuentos_multiplicidad.csv`.

`R/77_cohorte_analizada.R`, alta el 2026-08-30. Deriva la composicion de la
cohorte analizada en `outputs/fase29/cohorte_analizada.csv`.

## Sesgos que acompanan a cada cifra

Cada uno se declara donde la cifra se publica, no en un apartado aparte de
limitaciones que el lector alcanza cuando ya ha leido el resultado.

Casos completos. Las cifras de `outputs/fase32/comparacion_resumen.csv` se
calculan sobre las estancias que tienen completas todas las determinaciones
comparadas. Esas estancias no son una muestra aleatoria: son las mas
monitorizadas, y la intensidad de monitorizacion se asocia a la gravedad y a la
unidad. La comparacion responde entre pacientes con analitica completa y no en
la cohorte. Se declara en la cabecera del procedimiento y en el apartado de
laboratorio del documento principal.

Sesgo de verificacion. La probabilidad de confirmar una etiologia depende de
que alguien la sospeche y solicite la prueba. El indicador de que una
determinacion se solicito resulta mas predictivo que su valor medido, lo que
consta en `outputs/fase15/sensibilidad_lactato.csv`. Toda lectura del modelo
como descripcion fisiologica ha de acompanarse de esa advertencia.

Extraccion anterior a la correccion de empates.
`outputs/fase20/REPRODUCIBILITY.md` acota la divergencia para las constantes
vitales y hace constar que para las determinaciones de laboratorio no la acota
ningun archivo. Las fases centrales no se reextrajeron, porque rehacer la
cohorte despues de abierto el conjunto sellado invalidaria la validacion
externa. La limitacion queda sin cuantificar, y asi ha de escribirse.

Penalizacion elegida sobre el conjunto de evaluacion.
`outputs/fase22/decision_lambda.csv` recoge que al repetir la comparacion
dentro del entrenamiento la decision resulto la misma. Eso mitiga el defecto y
no lo suprime: se comprobo despues y pudo haber salido de otro modo.

Divergencia de etiquetado. `outputs/fase25/divergencia_etiquetado.csv` recoge
el cotejo completo entre las dos escaleras de desempate. Ninguna estancia
cambia de categoria, y el archivo se cita siempre que se afirme.

Centro unico. Todas las unidades pertenecen a un mismo hospital terciario, de
modo que la heterogeneidad observada es una cota inferior de la que mostrarian
instituciones distintas.

## Afirmaciones que no se sostienen

Ninguna de estas puede escribirse, ni en el articulo ni en el repositorio, por
mucho que la intuicion las respalde.

Que las candidatas rindan mas o menos que las retenidas.
`outputs/fase31/casos_completos.csv` recoge que ninguna estancia de la cohorte
las reune todas dentro de la ventana. La comparacion directa no admite
respuesta por casos completos, y lo que se compara es otra cosa, mas estrecha,
que ha de nombrarse.

Que algun atributo registrado reproduzca la particion.
`outputs/fase30/separacion_candidatas.csv` recoge que ninguno lo hace. Escribir
un criterio ahora seria presentar una reconstruccion posterior como decision
original.

Que la especificacion ampliada pierda. `outputs/fase32/comparacion_resumen.csv`
recoge que el intervalo de la diferencia contiene el cero. La direccion no
queda establecida, y no basta con que el signo apunte a un lado.

Que una categoria concreta empeore.
`outputs/fase32/multiplicidad_diferencias.csv` recoge que ninguna resiste la
correccion por multiplicidad. La que excluye el cero sin corregir se reporta
con esa salvedad, nunca sin ella y nunca omitida.

Que la unidad reservada cumpla la garantia condicional.
`outputs/fase26/intervalos_cobertura.csv` recoge sus celdas. Ninguna categoria
minoritaria presenta alli un intervalo por debajo del nivel, y eso no acredita
cumplimiento: los casos son demasiado pocos para establecer un deficit en
cualquiera de los dos sentidos. La ausencia de demostracion no es demostracion
de ausencia.

Que algo sea inviable porque una cota lo sugiera.
`outputs/fase30/viabilidad_comparacion.csv` recoge la cota superior de casos
completos con que se dio por imposible la comparacion directa antes de
contarlos. El recuento posterior resulto ser cero, de modo que la conclusion
era cierta; pero no lo era en el momento en que se afirmo, porque una cota
acota y no cuenta. Lo que se publique ha de venir del recuento, tambien cuando
la cota parezca concluyente.

Que la seleccion de determinaciones estuviera razonada. El historial acredita
cuando quedo fija. Sobre el porque no hay archivo, y por tanto no hay
afirmacion posible.

Que el criterio de la penalizacion se fijara de antemano.
`R/32_comparar_lambda.R` lo declara, y el procedimiento y el deposito que lo
aplica entraron en el mismo commit, de modo que el historial no separa el
criterio de su resultado. El asunto de aquel commit emplea la palabra que lo
afirma, y tampoco eso es artefacto. Lo que sostiene la decision es la
comprobacion posterior dentro del entrenamiento, no su anterioridad.

## Cifras que no son comparables entre si

Las areas de `outputs/fase32/comparacion_por_clase.csv` no son comparables con
las del modelo publicado. Difieren en 6 cosas, todas comunes a las dos ramas y
por tanto inocuas para la comparacion entre ellas, pero decisivas para quien
intente cotejar cifras con el resto del trabajo: no hay imputacion, porque se
trabaja sobre casos completos; no hay splines, porque las determinaciones
anadidas no tienen nudos definidos y dar forma flexible a unas y no a otras
confundiria el conjunto de variables con la forma funcional; no entra el
indicador de solicitud de lactato; el conjunto de ajuste es mucho menor, al
quedar restringido a esos casos completos; la penalizacion se valida dentro de
cada rama en vez de heredarse; y el ajuste prescinde ademas del grupo de
calibracion entero. La cifra de una rama solo significa algo frente a la de la
otra.

La ultima merece parrafo aparte, porque no es como las demas. Las otras 5
vienen impuestas por la comparacion; esta no. El grupo de calibracion existe
para fijar los umbrales conformes, y en esta comparacion no hay calibracion
conforme alguna, de modo que nada obligaba a dejarlo fuera. El ajuste pudo
usarlo y no lo uso.

Cuanto costo esta contado y no estimado, en
`outputs/fase32/potencia_no_usada.csv`: el ajuste se hizo con 2487 estancias
donde podia haberse hecho con 4010, de modo que quedaron sin usar 1523, un
61.24 por ciento mas de las que entraron. Eso no sesga la comparacion, porque
la restriccion afecta por igual a las dos ramas, pero le resta potencia. Y como
su conclusion es negativa, la consecuencia va en la direccion incomoda: la
ausencia de mejora esta peor establecida de lo que podria haberlo estado.

No se corrige, y la razon es la misma que este trabajo invoca en otro sitio.
Rehacer el ajuste con mas estancias despues de conocer el resultado, y en la
direccion que podria darle la vuelta, es el patron que aqui se reprocha al
describir como se fijo la penalizacion. Vale para lo que incomoda y para lo que
conviene, o no vale. De modo que la comparacion queda como se hizo y lo que se
anade es la medida de lo que dejo sin usar.

La cobertura de constantes vitales antes y despues de la limpieza tampoco es la
misma cantidad. Se publica por etapas separadas, y ambas se leen de
`outputs/fase28/cobertura_vitales_por_etapa.csv`.

## Decisiones metodologicas y su fundamento

Beta-Binomial para la cobertura. `outputs/fase26/criterios_cobertura.csv`
recoge el criterio. El umbral conforme se reestima en cada pliegue y no es una
cantidad conocida, de modo que un intervalo binomial exacto trata como fijo
algo que varia y declara deficits que el procedimiento no acredita. La
cobertura condicional de un conformal por particion sigue una Beta, y el
recuento marginal que de ella resulta una Beta-Binomial. El contraste se
invierte dejando libre la posicion y fijando la sobredispersion en el tamano de
calibracion.

Holm y no Bonferroni. `outputs/fase26/recuentos_multiplicidad.csv` y
`outputs/fase32/multiplicidad_diferencias.csv` recogen la correccion. Holm
domina a Bonferroni: controla la misma tasa de error por familia y rechaza al
menos tanto. No hay razon para preferir el mas conservador.

Los dos niveles, 0.05 y 0.025. El segundo es el que gobierna la lectura, y
corresponde a repartir un contraste bilateral entre sus dos colas. Se informan
ambos para que el lector vea que la conclusion no depende de esa eleccion.

Un unico criterio de lectura. La cobertura se juzga por intervalo en todas
partes: en el conjunto de prueba, en la validacion dejando una sede fuera y en
la unidad reservada. Antes hubo dos, uno por intervalo y otro por comparacion
puntual con el nivel nominal, y de ahi salio una conclusion que hubo que
retirar.

La familia son las cuatro diferencias por categoria de
`outputs/fase32/intervalo_diferencia.csv`. El promedio sobre las minoritarias
queda fuera: es un unico resumen declarado de antemano, no una de cuatro
comparaciones exploradas, y se informa con su propio intervalo.

El remuestreo emplea 10,000 replicas. El valor mas pequeno que un remuestreo
puede expresar lo fija su numero de replicas, y el umbral escalonado de Holm
desciende por debajo de esa resolucion cuando las replicas son pocas: la
decision sobre una categoria situada junto al umbral dependeria entonces del
sorteo y no de los datos.

El remuestreo es emparejado. Los modelos se ajustan una sola vez y se
remuestrea el conjunto de evaluacion, de modo que la incertidumbre estimada es
la de la comparacion y no la del procedimiento entero. Ambas ramas se evaluan
sobre las mismas estancias, y la diferencia emparejada elimina la variacion
comun.

## Datos que no se abren ni se publican

El acuerdo de uso de PhysioNet prohibe redistribuir datos derivados a nivel de
paciente, y su publicacion podria costar el acceso. La lista de rutas
reservadas vive en un solo lugar, que es donde la exclusion surte efecto, y un
solo lector la obtiene de ahi.

`.gitignore` relaciona 14 rutas bajo ese encabezado, y 1 de ellas excluye un
directorio entero. Queda fuera ademas la copia local de la base, que no forma
parte del deposito. Ninguna de esas rutas se abre para redactar, y ninguna
cifra de este trabajo procede de leerlas a mano: los procedimientos las leen y
depositan agregados, que es lo que se publica.

El lector es `R/00_rutas_reservadas.R`, y no hay otro. Se detiene si la seccion
no esta delimitada una sola vez, si alguna ruta declarada no la excluye git, o
si la seccion queda vacia, y devuelve la misma lista se le llame desde donde se
le llame. Antes cada procedimiento obtenia la lista por su cuenta y los tres
discrepaban en su tamano.

`R/64_auditoria_publicacion.R` comprueba lo anterior sobre los hechos y no
sobre la intencion: inspecciona el encabezado de cada archivo versionado en
busca de identificadores, revisa los objetos binarios y recorre el historial
por si alguna de esas rutas fue alcanzada en algun momento.

## Defectos recurrentes y como se evitan

Los cuatro se repitieron en este desarrollo. Cada uno tiene ahora un
procedimiento que lo impide, y no una intencion de no volver a cometerlo.

Cifras escritas de memoria. Los generadores de documentos rechazan toda cifra
literal en la prosa y exigen que se componga desde un archivo.
`R/59_verificar_cifras.R` contrasta ademas cada cifra publicada contra su
fuente.

Ordenes no deterministas. Una consulta que ordena por menos claves de las que
hacen falta deja empates sin resolver y devuelve filas distintas entre
ejecuciones. Ocurrio con las constantes vitales, por el instante de registro, y
volvio a ocurrir en otra capa: el orden en que las columnas salian de la
reestructuracion se propagaba al descenso por coordenadas del ajuste
penalizado, y las areas se movian en la cuarta cifra. La regla que queda es
fijar el orden en ambos sitios, en la consulta y en la matriz, y comprobar la
reproduccion ejecutando dos veces y cotejando los depositos.

Texto que afirma mas que el archivo. Toda afirmacion publicada se asocia a la
ruta que la sostiene, y esa asociacion se comprueba.
`R/64_auditoria_publicacion.R` recorre la relacion y se detiene si alguna
fuente falta. `outputs/fase20/TRACEABILITY.md` recoge cada cifra contra su
archivo.

Confundir nombrar una ruta con leerla. Una relacion que enumera archivos
reservados para comprobar que no esten publicados los nombra sin abrirlos, y
contar eso como lectura invierte el sentido de lo que hace. Ocurrio al
contabilizar los contactos con la unidad reservada, se corrigio, y volvio a
ocurrir en la misma sesion al derivar la posterioridad de los analisis: un
barrido por mencion metio en la categoria mas grave a cuatro procedimientos que
nombran el directorio de derivados justamente para comprobar que no esta
versionado. La correccion reciente no evito que la clase reapareciera. La regla
que queda es que una lectura se acredita por la llamada que la hace y por el
argumento que recibe, nunca porque la ruta figure en el texto del
procedimiento.

Dos varas de medir. El mismo trabajo llego a corregir por multiplicidad en un
apartado y no en otro, y a juzgar la cobertura por intervalo en un sitio y por
comparacion puntual en otro. La regla que queda es que un criterio adoptado en
cualquier parte rige en todas, y que introducir uno nuevo obliga a revisar los
apartados anteriores.

## Como se comprueba este registro

`R/82_decisiones_vivas.R` compone este documento y se detiene ante una cifra
escrita a mano, ante una ruta que no exista o no este versionada, ante una ruta
escrita en la prosa que haya eludido esa comprobacion, y ante un analisis
declarado posterior cuya alta en el historial no lo sea.

Los demas procedimientos de comprobacion son `R/59_verificar_cifras.R`,
`R/64_auditoria_publicacion.R`, `R/65_generar_readme.R` y
`R/67_diagnostico_dependencias.R`. El documento principal es `README.md`.

