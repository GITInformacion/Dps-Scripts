library(dplyr)
library(knitr)
library(kableExtra)
library(data.table)

#Identifica los NA en la base de datos
Error_NA=as.data.frame(sapply(UNIDOS, function(x) sum(is.na(x))))
Error_NA=tibble::rownames_to_column(Error_NA, "Variables")
colnames(Error_NA)[2]="Frecuencias"
Error_NA=Error_NA[Error_NA$Frecuencias>0,]

Error_Vacio=as.data.frame(sapply(UNIDOS, function(x) sum(x %in% "")))
Error_Vacio=tibble::rownames_to_column(Error_Vacio, "Variables")
colnames(Error_Vacio)[2]="Frecuencias"
Error_Vacio=Error_Vacio[Error_Vacio$Frecuencias>0,]

Error_Sindato=as.data.frame(sapply(UNIDOS, function(x) sum(x %in% "Sin dato")))
Error_Sindato=tibble::rownames_to_column(Error_Sindato, "Variables")
colnames(Error_Sindato)[2]="Frecuencias"
Error_Sindato=Error_Sindato[Error_Sindato$Frecuencias>0,]
