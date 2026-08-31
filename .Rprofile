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
options(warnPartialMatchDollar = TRUE)

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
