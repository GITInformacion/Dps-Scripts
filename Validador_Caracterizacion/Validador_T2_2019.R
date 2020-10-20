################################
#Validaciones Estrategia Unidos#
################################

Fecha = "20200821"

slash="/"

Carpeta = dirname(rstudioapi::getSourceEditorContext()$path)#El ultimo slash o backslash no se debe incluir

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

UNIDOS = read.csv(paste("SabanaPiloto_",Fecha,"_1.csv", sep = ""), sep="|")

UNIDOS_HOG=UNIDOS[!duplicated(UNIDOS$A01),]#Genera base de datos a nivel de hogar

############
#VALIDACION#
############
setwd(General)
#source("Descriptiva.R")# Estadisticas descriptivas de los datos

############
#Duplicados#
############

source("Duplicados.R")

##########################
#Deteccion de vacios y NA#
##########################

source("NA&Vacios.R")# Genera conteos de NA, Sin dato y vacios

##########
#Formatos#
##########
setwd(Version)
source("Formatos_2019.R")# Contiene las variables definidas en el diccionario de datos correspondiente

#########################
#Generacion de variables#
#########################

#Las variables generadas se requieren para calculos posteriores.
setwd(General)
source("Generacion de campos.R")

#rm(list = ls()[!ls() %in% grep("^UNIDOS|Error|General|Version|Salidas|Entradas|Carpeta",ls(),value = TRUE)])

###############
# Validaciones#
###############

source("Validacion por capitulo.R")

####################
#Resumen de errores#
####################
setwd(paste(Carpeta,"Salidas", sep = slash))
dir.create(paste0("Validacion_",Fecha),showWarnings = FALSE)

setwd(General)
source("Generacion de salidas.R")#Se utiliza para dirigir las salidas a las carpetas definidas.

#Se exporta tabla de frecuencia de los errores.
setwd(paste(Carpeta,"Salidas",paste0("Validacion_",Fecha), sep = slash))
write.csv(Total_Errores, file = paste("Total_Errores","_",format(Sys.time(), "%d%m%Y"), ".csv", sep=""), row.names = FALSE, fileEncoding = "UTF-8")

rm(list = ls()[!ls() %in% grep("^UNIDOS|Error|Resumen|Version|Salidas|Entradas",ls(),value = TRUE)])
