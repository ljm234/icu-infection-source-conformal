# Traduccion de los hallazgos al protocolo de meningitis oportunista

Documento de trabajo dirigido al equipo investigador.

Generado el 2026-09-01 por `R/68_traduccion_yachay.R`.

Las cifras que siguen proceden en su totalidad del banco de pruebas sobre
MIMIC-IV y se leen de archivos versionados. Cuanto se afirma sobre el
protocolo es cualitativo: no se dispone de datos de la red de sedes y no se
formula estimacion alguna sobre ella.

## Proposito

El banco de pruebas replico sobre una cohorte de cuidados intensivos la
estructura metodologica prevista para el protocolo: clasificacion multiclase
con desbalance acusado, conjuntos de prediccion con garantia de cobertura,
mecanismo de abstencion y validacion en una sede no observada durante el
desarrollo. El proposito era detectar los modos de fallo antes de llevar el
diseno a datos peruanos.

Se detectaron 7 modos de fallo, y cada uno deriva en una decision concreta.

## Primer hallazgo. La evaluacion exige cuatro dominios

La capacidad de ordenar casos y la garantia de cobertura no se comportan igual
al cambiar de sede. Bajo validacion dejando una sede fuera, la primera se
conserva mientras la segunda se degrada de forma desigual. La cobertura
marginal oscila entre 0.8036 y 0.9612 con media de 0.8944, por debajo del
nivel nominal. De las 5 sedes, 2 quedan por debajo en esa medida.

La garantia condicional por categoria, que es la que el trabajo declara, se
evalua celda a celda: cada categoria dentro de cada sede, con el mismo
criterio de lectura por intervalo que emplea el conjunto de prueba. Las celdas
se corrigen conjuntamente por multiplicidad, y el contraste reconoce que el
umbral conforme se reestima en cada pliegue y no es una cantidad conocida.
Bajo esa correccion fallan 3 de 5 sedes: 2 en la categoria sin_crecimiento y 1
en la respiratoria. Las 2 restantes presentan cobertura puntual de 0.7818 y
0.7586 sobre 55 y 58 casos, demasiado pocos para establecer el deficit. La
ausencia de demostracion no acredita cumplimiento, y un resumen marginal
oculta cual categoria queda descubierta en cada sede.

**Decision.** El informe de resultados no puede limitarse a la discriminacion.
Debe presentar por separado, y desagregados por sede, la capacidad de ordenar
casos, la concordancia entre probabilidad predicha y frecuencia observada, la
cobertura de los conjuntos y la proporcion de casos que el sistema resuelve.
Comunicar unicamente el primero describiria un comportamiento que los demas
desmienten.

## Segundo hallazgo. La cobertura promediada oculta ceros

Al recalibrar con un umbral unico sobre una sede, la cobertura promediada se
aproxima al nivel nominal mientras las categorias poco frecuentes quedan
enteramente fuera de los conjuntos. El promedio esta dominado por la categoria
mayoritaria.

**Decision.** No se reportara cobertura marginal como medida principal. La
garantia se calculara y comunicara por separado dentro de cada etiologia. Toda
etiologia que no alcance el minimo necesario se declarara expresamente como no
calibrada, en lugar de excluirse en silencio. Un sistema que omite una
etiologia sin advertirlo esta afirmando que el paciente no la padece, sin
fundamento para ello.

## Tercer hallazgo. La recalibracion local tiene un limite duro

La garantia condicional exige un numero minimo de casos por categoria en la
sede donde se recalibra. Por debajo de ese minimo el cuantil no existe. Con 50
casos locales, una media de 1.0 de las 4 categorias alcanzaba el minimo, y el
tamano medio del conjunto descendia a 0.888: los conjuntos quedaban a menudo
vacios y el sistema se reducia a un clasificador binario degenerado, con
cobertura aparente proxima a la nominal.

**Decision.** Antes de recalibrar en cada sede se verificara el recuento
disponible por etiologia. Las etiologias infrecuentes pueden exigir periodos
prolongados para reunir los casos necesarios en una sede concreta. El
protocolo debe prever esa circunstancia y establecer de antemano el
procedimiento: declarar la etiologia no calibrada, agrupar sedes, o renunciar
a la garantia condicional para esa categoria haciendolo constar.

## Cuarto hallazgo. El modelo lee juicio clinico

La variable que indica si una determinacion se solicito resulto mas predictiva
que su valor medido. El indicador de solicitud aporta 0.0134 a la
discriminacion; el valor determinado aporta 0.0038. La decision de solicitar
la prueba transmite mas informacion que la fisiologia que la prueba mide.

**Decision.** El protocolo registra si cada prueba especifica se realizo. Ese
registro sera predictivo, pero por una razon ajena a la fisiologia: codifica
la sospecha del clinico que atendio al paciente. Debe analizarse por separado
del resultado, con la especificacion completa y con la que prescinde del
indicador, comunicando ambas. Un modelo que dependa del patron de solicitud no
transportara a una sede con otro protocolo de peticion.

## Quinto hallazgo. La escala de conciencia puede medir el procedimiento

Un indicador binario de intubacion, desprovisto de contenido fisiologico,
discrimino la categoria respiratoria casi tan bien como la escala de
conciencia. El indicador alcanza 0.7111. La escala obtiene 0.7313 en su forma
completa de tres componentes y 0.7086 en la reducida a apertura ocular y
respuesta motora. Se reporta la reducida porque el componente verbal asigna la
puntuacion minima al paciente intubado y confunde la ausencia de respuesta con
la imposibilidad de hablar. Dentro del estrato intubado la escala reducida
desciende a 0.4865, y la completa obtiene ese mismo 0.4865. Ambas formas
coinciden alli porque el componente verbal es constante, de modo que la
completa es la reducida mas un desplazamiento fijo, que deja inalterado el
orden y por tanto el area. La escala actuaba como indicador indirecto del
tubo, y el tubo se asocia con fuerza a que el sitio respiratorio se cultive.
Es asociacion, no determinacion: los datos no acreditan que lo uno cause lo
otro, pero la magnitud basta para invalidar la escala como predictor
fisiologico en esta cohorte.

**Decision.** La escala figura entre las variables obligatorias del protocolo,
y con fundamento: la meningitis altera la conciencia de forma directa, de modo
que la relacion causal apunta en el sentido correcto. Pero la trampa subsiste.
El cuaderno de recogida debe consignar la puntuacion con anterioridad a la
sedacion o la intubacion siempre que resulte posible, si el paciente se
hallaba sedado o intubado en el momento de la valoracion, y que componentes
fueron evaluables. Sin ese registro, las sedes que dispongan de ventilacion
mecanica presentaran puntuaciones sistematicamente inferiores, y la diferencia
reflejara la infraestructura disponible y no el estado del paciente.

## Sexto hallazgo. El limite pertenece a la informacion

Un metodo no parametrico capaz de aprender interacciones sin especificacion
previa alcanzo practicamente el mismo techo que la regresion penalizada. La
diferencia es de 0.0002 sobre las categorias poco frecuentes.

La incorporacion de un dominio nuevo de medicion produjo mejora, aunque
modesta: 0.0146 al anadir constantes vitales, muy por encima de lo que aporto
refinar el algoritmo.

**Decision.** No cabe esperar que un metodo mas flexible compense una
informacion insuficiente. La discusion sobre que variables recoger precede en
importancia a la discusion sobre que modelo emplear. El protocolo dispone de
ventajas que el banco de pruebas no tenia: exploracion neurologica, tiempo de
enfermedad y pruebas dirigidas al desenlace. Esas ventajas pesan mas que
cualquier eleccion algoritmica.

## Septimo hallazgo. La extraccion puede no ser determinista

El registro de observaciones de enfermeria se valida por lotes, de modo que
varias determinaciones comparten instante de registro. La coincidencia afecta
a entre 25.46 y 40.05 por ciento de las estancias segun la variable, con hasta
38 determinaciones simultaneas. Las de laboratorio resultan afectadas en un
0.10 por ciento de las estancias. Los equipos consignan cada resultado por
separado, de modo que la coincidencia resulta alli mucho menos frecuente, pero
no nula, y la consulta que las extrae ordena tambien por el solo instante de
registro. La reextraccion corregida se limito a las constantes vitales, de
modo que la divergencia que esa ordenacion pueda inducir sobre las bioquimicas
no esta acotada.

Seleccionar la primera determinacion ordenando unicamente por el instante de
registro deja las coincidencias sin resolver, y la fila retenida puede variar
entre ejecuciones de una misma consulta.

**Decision.** El procedimiento de extraccion fijara desde el inicio un
criterio de desempate explicito. Sin el, dos analistas que ejecuten el mismo
codigo sobre los mismos datos pueden obtener resultados distintos, y la
discrepancia resultaria dificil de atribuir.

## Riesgos adicionales que el protocolo ya contempla

**La exactitud no puede ser la medida principal.** Con la distribucion
etiologica prevista, un sistema que responda siempre ausencia de confirmacion
acertara la mayoria de las veces sin identificar una sola etiologia. En el
banco de pruebas, la regla del maximo no asigno jamas una categoria poco
frecuente, con independencia del metodo empleado. Al nivel de confianza del
noventa por ciento el sistema resolvia 9.6 por ciento de los casos, y la
precision sobre las conclusiones de categoria poco frecuente no supero en
ningun nivel el valor de 0.4000.

**La glucosa del liquido cefalorraquideo debe analizarse como indice.** El
valor absoluto depende de la glucemia simultanea. El protocolo recoge ambas
determinaciones; el analisis debe emplear el cociente y no la cifra aislada.

**La eleccion de hiperparametros no puede hacerse sobre el conjunto de
evaluacion.** El banco de pruebas fijo la penalizacion comparando dos reglas
sobre el mismo conjunto en el que despues reporto discriminacion y cobertura.
Al repetir la comparacion dentro del entrenamiento, apartando una porcion que
no intervino en el ajuste, la decision resulto ser la misma; pero eso se
comprobo despues y pudo haber salido de otro modo. El protocolo establecera de
antemano que toda eleccion de esta clase se resuelva dentro del entrenamiento,
con una porcion apartada del ajuste, y nunca con los datos que sostienen el
resultado publicado.

**El sesgo de verificacion es cuantificable y debe cuantificarse.** La
probabilidad de confirmar una etiologia depende de que alguien la sospeche y
solicite la prueba correspondiente. El protocolo registra la realizacion de
cada prueba, lo que permite estimar la magnitud del sesgo en lugar de
declararlo en abstracto.

## Lo que este banco de pruebas no puede responder

No compara el desempeno del sistema con el de un clinico que disponga de la
misma informacion. Sin esa comparacion se desconoce si el sistema aporta algo
sobre el juicio que ya existe. El protocolo, en cambio, puede establecerla,
dado que el medico tratante forma parte del circuito.

No informa sobre la magnitud del desplazamiento de prevalencia entre sedes
peruanas. Ese desplazamiento es compatible con el fallo de transportabilidad
observado, pero el banco de pruebas no lo acredita: la incorporacion de las
constantes vitales no reemplaza a las determinaciones bioquimicas sino que se
suma a ellas, de modo que los predictores presuntamente contaminados
permanecen en el modelo y la comparacion no distingue entre ambas hipotesis.
El modelo ampliado, ademas, no transporta mejor. La desviacion de la cobertura
queda practicamente igual, de modo que la afirmacion se apoya en las otras
dos: pasa de 0.0744 a 0.0736, mientras que la media desciende de 0.8944 a
0.8838 y el minimo de 0.8036 a 0.7757.

No permite anticipar cuantos casos reunira cada sede ni con que distribucion
etiologica. Esa informacion condiciona la viabilidad de la recalibracion local
y debe estimarse con datos propios antes de comprometer el diseno.

## Origen de las cifras

Toda cifra de este documento se lee de un archivo de resultados versionado del
banco de pruebas, con una excepcion: el recuento de hallazgos, que es
autorreferencial y se contrasta contra el numero de encabezados y de bloques
de decision del propio documento. El procedimiento que lo compone se detiene
ante cualquier cifra escrita a mano, ante cualquier fuente ausente, ante
cualquier linea que no componga y ante una discrepancia en ese recuento.
