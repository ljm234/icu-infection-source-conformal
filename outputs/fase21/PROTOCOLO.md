# Traduccion de los hallazgos al protocolo de meningitis oportunista

Documento de trabajo dirigido al equipo investigador.

Generado el 2026-08-26 por `R/68_traduccion_yachay.R`.

Las cifras que siguen proceden en su totalidad del banco de pruebas sobre
MIMIC-IV y se leen de archivos versionados. Cuanto se afirma sobre el
protocolo es cualitativo: no se dispone de datos de la red de sedes y no se
formula estimacion alguna sobre ella.

## Proposito

El banco de pruebas replico sobre una cohorte de cuidados intensivos la
estructura metodologica prevista para el protocolo: clasificacion multiclase
con desbalance acusado, conjuntos de prediccion con garantia de cobertura,
mecanismo de abstencion y validacion en una sede no observada durante el
desarrollo. El proposito era detectar los modos de fallo antes de aplicarlos
a datos peruanos.

Se detectaron siete. Cinco afectan a decisiones del protocolo.

## Primer hallazgo. La evaluacion exige cuatro dominios

La capacidad de ordenar casos y la garantia de cobertura no se comportan
igual al cambiar de sede. Bajo validacion dejando una sede fuera, la primera
se conserva mientras la segunda se degrada de forma desigual.
La cobertura oscila entre 0.8036 y 0.9612, con desviacion de 0.0744
frente a una media de 0.8944: se cumple en promedio y en ninguna
sede tomada de forma individual.

**Decision.** El informe de resultados no puede limitarse a la
discriminacion. Debe presentar por separado, y desagregados por sede, la
capacidad de ordenar casos, la concordancia entre probabilidad predicha y
frecuencia observada, la cobertura de los conjuntos y la proporcion de casos
que el sistema resuelve. Comunicar unicamente el primero describiria un
comportamiento que los demas desmienten.

## Segundo hallazgo. La cobertura promediada oculta ceros

Al recalibrar con un umbral unico sobre una sede, la cobertura promediada se
aproxima al nivel nominal mientras las categorias poco frecuentes quedan
enteramente fuera de los conjuntos. El promedio esta dominado por la
categoria mayoritaria.

**Decision.** No se reportara cobertura marginal como medida principal. La
garantia se calculara y comunicara por separado dentro de cada etiologia.
Toda etiologia que no alcance el minimo necesario se declarara expresamente
como no calibrada, en lugar de excluirse en silencio. Un sistema que omite
una etiologia sin advertirlo esta afirmando que el paciente no la padece,
sin fundamento para ello.

## Tercer hallazgo. La recalibracion local tiene un limite duro

La garantia condicional exige un numero minimo de casos por categoria en la
sede donde se recalibra. Por debajo de ese minimo el cuantil no existe.
Con 50 casos locales solo 1.0 categoria alcanzaba el minimo, y el
tamano medio del conjunto descendia a 0.888: los conjuntos quedaban
a menudo vacios y el sistema se reducia a un clasificador binario
degenerado, con cobertura aparente proxima a la nominal.

**Decision.** Antes de recalibrar en cada sede se verificara el recuento
disponible por etiologia. Las etiologias infrecuentes pueden requerir anos
para reunir los casos necesarios en una sede concreta. El protocolo debe
prever esa circunstancia y establecer de antemano el procedimiento:
declarar la etiologia no calibrada, agrupar sedes, o renunciar a la garantia
condicional para esa categoria haciendolo constar.

## Cuarto hallazgo. El modelo lee juicio clinico

La variable que indica si una determinacion se solicito resulto mas
predictiva que su valor medido.
El indicador de solicitud aporta 0.0134 a la discriminacion; el valor
determinado aporta 0.0038. La decision de solicitar la prueba
transmite mas informacion que la fisiologia que la prueba mide.

**Decision.** El protocolo registra si cada prueba especifica se realizo.
Ese registro sera predictivo, pero por una razon ajena a la fisiologia:
codifica la sospecha del clinico que atendio al paciente. Debe analizarse
por separado del resultado, con la especificacion completa y con la que
prescinde del indicador, comunicando ambas. Un modelo que dependa del patron
de solicitud no transportara a una sede con otro protocolo de peticion.

## Quinto hallazgo. La escala de conciencia puede medir el procedimiento

Un indicador binario de intubacion, desprovisto de contenido fisiologico,
discrimino la categoria respiratoria mejor que la escala completa.
El indicador alcanza 0.7111 frente a 0.7086 de la escala. Dentro del
estrato intubado la escala desciende a 0.4865, esto es, deja de
discriminar. La escala actuaba como indicador indirecto del tubo, y el tubo
determinaba que el sitio respiratorio se cultivase.

**Decision.** La escala figura entre las variables obligatorias del
protocolo, y con fundamento: la meningitis altera la conciencia de forma
directa, de modo que la relacion causal apunta en el sentido correcto. Pero
la trampa subsiste. El cuaderno de recogida debe consignar la puntuacion con
anterioridad a la sedacion o la intubacion siempre que resulte posible, si
el paciente se hallaba sedado o intubado en el momento de la valoracion, y
que componentes fueron evaluables. Sin ese registro, las sedes que dispongan
de ventilacion mecanica presentaran puntuaciones sistematicamente
inferiores, y la diferencia reflejara la infraestructura disponible y no el
estado del paciente.

## Sexto hallazgo. El limite pertenece a la informacion

Un metodo no parametrico capaz de aprender interacciones sin especificacion
previa alcanzo el mismo techo que la regresion penalizada.
La diferencia es de 0.0002 sobre las categorias poco frecuentes.

La incorporacion de un dominio nuevo de medicion si produjo mejora, aunque
modesta:
0.0146 al anadir constantes vitales, dos ordenes de magnitud por
encima de lo que aporto refinar el algoritmo.

**Decision.** No cabe esperar que un metodo mas flexible compense una
informacion insuficiente. La discusion sobre que variables recoger precede
en importancia a la discusion sobre que modelo emplear. El protocolo dispone
de ventajas que el banco de pruebas no tenia: exploracion neurologica,
tiempo de enfermedad y pruebas dirigidas al desenlace. Esas ventajas pesan
mas que cualquier eleccion algoritmica.

## Septimo hallazgo. La extraccion puede no ser determinista

El registro de observaciones de enfermeria se valida por lotes, de modo que
varias determinaciones comparten instante de registro.
La coincidencia afecta a entre 25.46 y 40.05 por ciento de las
estancias segun la variable, con hasta 38 determinaciones
simultaneas. Las de laboratorio quedan exentas, con 0.10 por
ciento, dado que los equipos consignan cada resultado por separado.

Seleccionar la primera determinacion ordenando unicamente por el instante de
registro deja las coincidencias sin resolver, y la fila retenida puede
variar entre ejecuciones de una misma consulta.

**Decision.** El procedimiento de extraccion fijara desde el inicio un
criterio de desempate explicito. Sin el, dos analistas que ejecuten el mismo
codigo sobre los mismos datos pueden obtener resultados distintos, y la
discrepancia resultaria dificil de atribuir.

## Riesgos adicionales que el protocolo ya contiene

**La exactitud no puede ser la medida principal.** Con la distribucion
etiologica prevista, un sistema que responda siempre ausencia de
confirmacion acertara la mayoria de las veces sin identificar una sola
etiologia. En el banco de pruebas, la regla del maximo no asigno jamas una
categoria poco frecuente, con independencia del metodo empleado.
Al nivel de confianza del noventa por ciento el sistema resolvia 9.6
por ciento de los casos, y la precision sobre las conclusiones de
categoria poco frecuente no supero en ningun nivel el valor de 0.4000.

**La glucosa del liquido debe analizarse como indice.** El valor absoluto
depende de la glucemia simultanea. El protocolo recoge ambas
determinaciones; el analisis debe emplear el cociente y no la cifra aislada.

**El sesgo de verificacion es cuantificable y debe cuantificarse.** La
probabilidad de confirmar una etiologia depende de que alguien la sospechara
y solicitara la prueba correspondiente. El protocolo registra la realizacion
de cada prueba, lo que permite estimar la magnitud del sesgo en lugar de
declararlo en abstracto.

## Lo que este banco de pruebas no puede responder

No compara el desempeno del sistema con el de un clinico que disponga de la
misma informacion. Sin esa comparacion se desconoce si el sistema aporta
algo sobre el juicio que ya existe. El protocolo si puede establecerla, dado
que el medico tratante forma parte del circuito.

No informa sobre la magnitud del desplazamiento de prevalencia entre sedes
peruanas. El banco de pruebas mostro que ese desplazamiento, y no la
contaminacion de los predictores, explica el fallo de transportabilidad de
la garantia.
La desviacion de la cobertura permanecio en 0.0736 frente a 0.0744 al
incorporar la unica familia de variables exenta de sesgo de sede.

No permite anticipar cuantos casos reunira cada sede ni con que distribucion
etiologica. Esa informacion condiciona la viabilidad de la recalibracion
local y debe estimarse con datos propios antes de comprometer el diseno.

## Origen de las cifras

Toda cifra de este documento se lee de un archivo de resultados versionado
del banco de pruebas. El procedimiento que lo compone se detiene ante
cualquier cifra escrita a mano, ante cualquier fuente ausente y ante
cualquier linea que no componga.
