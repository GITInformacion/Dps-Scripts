##########
#Formatos#
##########

variables=sort(c("EdadCaracterizacion","D03","G06","J04","J05_A","K01_1","K02","K12","K13","K13A","C04_3A","C04_2A","L05_A","L06_A","L07_B",paste0("L08F",1:3,"_A"),grep("C0[1|2]|C0[1|2]_1|[A|B|C|D|E|F|]_[A]$|K[0][3|4|5|6|7]_[A]$", names(UNIDOS), value = TRUE)))#Se definen variables a las que se van a convertir en formato numerico.

UNIDOS[variables] =lapply(UNIDOS[variables], as.character)#Convierte a formato caracter las variables
UNIDOS[variables] =lapply(UNIDOS[variables], as.numeric)#Convierte a formato numérico las variables
UNIDOS[c("E02","E07","fechaInicio")]=lapply(UNIDOS[c("E02","E07","fechaInicio")], as.Date)#Convierte a formato fecha la svariables
UNIDOS$A05 = as.character(UNIDOS$A05)
  