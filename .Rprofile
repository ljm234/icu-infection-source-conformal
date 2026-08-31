source("renv/activate.R")

# Emparejamiento parcial de nombres.
#
# El operador de acceso admite prefijos: `d$determinaciones` devuelve
# `determinaciones_medias` cuando la columna pedida no existe, sin error y
# sin aviso bajo la configuracion por defecto. La cifra que sale de ahi no es
# la que su nombre dice, y este trabajo descansa entero en que toda cifra
# publicada proceda de un archivo y sea la que dice ser.
#
# No es hipotetico. Ocurrio una vez y llego hasta el texto compuesto, donde
# el numero de determinaciones del modelo aparecio como once en lugar de
# diecisiete; se detecto al leer la salida y no por comprobacion alguna.
#
# Se activa el aviso y se convierte en parada. Un aviso en una salida larga
# se pierde, y la garantia no puede depender de que alguien lo lea. En sesion
# interactiva se deja en aviso, para no derribar la consola de quien explora.
#
# Las tres formas de emparejamiento parcial se tratan igual. La de argumentos
# es la que mas usan los paquetes, y por eso hacerla fatal era el riesgo: si
# glmnet, mice o duckdb la emplearan por dentro, la guardia mataria el
# ajuste. Se comprobo sobre los veintidos procedimientos ejecutables, incluido
# el que consulta la base con duckdb y los dos que reajustan modelos, y
# ninguno la dispara.
options(warnPartialMatchDollar = TRUE,
        warnPartialMatchArgs   = TRUE,
        warnPartialMatchAttr   = TRUE)

# Cada deposito declara en su manifiesto si la guardia estaba puesta al
# escribirse. Comprobar la guardia dentro del verificador la acredita para el
# verificador y no para quien escribio el deposito: un procedimiento
# ejecutado sin este perfil no tiene guardia, y la verificacion posterior
# contrastaria el documento contra un archivo cuyo calculo nadie vigilo.
#
# El campo se compone leyendo la opcion directamente y no llamando a una
# funcion definida aqui. Una funcion definida aqui no existe cuando el perfil
# no se lee, de modo que el procedimiento moriria al escribir el manifiesto,
# despues de haber escrito los datos: quedarian depositos sin vigilar junto a
# un manifiesto anterior que sigue declarando que si lo estuvieron. Leyendo la
# opcion, el manifiesto se escribe siempre y declara lo que hubo.

if (!interactive()) {
  globalCallingHandlers(warning = function(w) {
    if (grepl("^partial match of", conditionMessage(w))) {
      cat("\nEMPAREJAMIENTO PARCIAL DE NOMBRE\n")
      cat("  ", conditionMessage(w), "\n", sep = "")
      cat("  en: ", paste(deparse(conditionCall(w)), collapse = " "), "\n",
          sep = "")
      cat("La columna leida no es la pedida. El procedimiento se detiene.\n")
      quit(status = 1)
    }
  })
}
