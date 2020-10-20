################################
#Validaciones Estrategia Unidos#
################################

Fecha = "20200821"

Carpeta = dirname(rstudioapi::getSourceEditorContext()$path)#El ultimo slash o backslash no se debe incluir

slash = "/"

Entradas=paste(Carpeta,"Entradas", sep = slash)#Defina el escritorio de entrada donde están los archivos requeridos.
Salidas =paste(paste(Carpeta,"Salidas","Validacion_", sep = slash), Fecha, sep = "")#Defina el escritorio de salida donde serán enviado los archivos generados.
General =paste(Carpeta,"General", sep = slash)#Defina el escritorio donde se encuentra el script con el nombre de "General.R"
Version =paste(Carpeta,"Version", sep = slash)#Defina la versión del diccionario de datos que usará para la validación.

setwd(General)
source("Librerias.R")# Las librerias que se usaran
################
#CARGA DE DATOS#
################
setwd(Entradas)# Se difine el directorio donde se encuentra el archivo que se va a validar.

#UNIDOS = read_delim(paste("SabanaPiloto_",Fecha,".txt", sep = ""),
#                    "|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),
#                    trim_ws = TRUE)

UNIDOS = read_delim(paste("SabanaPiloto_",Fecha,"_1",".csv", sep = ""), "|", escape_double = FALSE, trim_ws = TRUE)

############
#VALIDACION#
############

#Versión de diccionario de datos: "DiccionarioDatos_FormularioCaracterizacion_2019_V6"
setwd(Version)
source("Diccionario_2019_V2.R")# Contiene las variables definidas en el diccionario de datos correspondiente

setwd(General)
source("Nombres de campos.R")# Contiene las variables definidas en el diccionario de datos correspondiente

setwd(paste(Carpeta,"Salidas", sep = slash))
dir.create(paste0("Validacion_",Fecha),showWarnings = FALSE)

setwd(General)
source("Generacion de salidas.R")#Se utiliza para dirigir las salidas a las carpetas definidas.

rm(list = ls()[!ls() %in% grep("^UNIDOS|Error|Resumen|Version|Salidas|Entradas",ls(),value = TRUE)])
