####################
#Resumen de errores#
####################


#Elimina registros con na en toda la fila

lista = unlist(list(ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']))

res <- lapply(mget(lista),
              function(DF)
                DF[!apply(DF,1, function(x) all(is.na(x))),]
)

invisible(list2env(res, globalenv())) # overwrites original DFs


#Luego de obtener los errores se eliminan los dataframe sin errores.

to.rm <- unlist(eapply(.GlobalEnv, function(x) is.data.frame(x) && (nrow(x)  %in%  0 | all(is.na(x)))))
rm(list = names(to.rm)[to.rm], envir = .GlobalEnv)

#Se define directorio de almacenamiento de los archivos de errores.
setwd(paste(Carpeta,"Salidas",paste0("Validacion_",Fecha), sep = slash))

#Se exportan los dataframe con la expresión regular "Error" al directorio definido.
m = length(ls()[ls() %in% grep("Error",ls(),value = TRUE)])
n = ls()[ls() %in% grep("Error",ls(),value = TRUE)]

for(i in 1:m) {
  write.csv2(
    get(n[[i]]),
    file = paste0(n[[i]],"_",format(Sys.time(), "%d%m%Y"),".csv"),
    sep = "|",
    row.names = FALSE, fileEncoding = "UTF-8")
}

#Se genera dataframe que compila los dataframe con la expresión regular "Error".

Total_Errores=data.frame(do.call("rbind", lapply(ls(pattern = "^Error_"), function(x) {
  obj = get(x)
  if (class(obj)  %in%  c("data.frame","tbl_df"))
    c(name = x, rows = nrow(obj))
})))

colnames(Total_Errores)[c(1,2)]=c("Nombre del error","Frecuencias")

setwd(Entradas)#Se difine el directorio donde se encuentra el archivo que se va a validar.
Descripcion_errores <- read_excel("Descripcion errores.xlsx")
colnames(Descripcion_errores)[1]="Nombre del error"

Total_Errores =merge(Total_Errores,Descripcion_errores, by="Nombre del error")
