#Nombres de campos

#Esta en UNIDOS pero no se encuentra en el diccionario.
Error_Nombres_variables_1=setdiff(names(UNIDOS),c("idEncuesta","fechaInicio",variables))#Identifica las variables que están en la base de datos pero no en el diccionario.

#Esta en el diccionario pero no se encuentra UNIDOS.
Error_Nombres_variables_2=setdiff(c("idEncuesta","fechaInicio",variables),names(UNIDOS))#Identifica las variables que están en el diccionario pero no están en la base de datos.

rm(list = ls()[ls() %in% grep("^capitulo|^[V|v]ar",ls(),value = TRUE)])
