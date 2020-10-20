#######################
#Corrección de nombres#
#######################
#UNIDOS = subset(UNIDOS, select = -c(E031) )

vars_old = as.character(Error_Nombres_variables_1)
vars_new = sort(c(paste("B05",LETTERS[1:6],sep = "_"), paste("B05_",LETTERS[1:6],1,sep = ""), paste("C03_4",LETTERS[1:6],sep = ""), paste("C05",LETTERS[1:4],sep = "_"), paste0("C11_1",LETTERS[1:5])))

setnames(UNIDOS, old = vars_old[-28], new = vars_new)

#UNIDOS[c(paste0("K26",LETTERS[c(1:5,7)],"_1"),paste0("K38",LETTERS[c(1:6)],"_1"))] = NA
