# Duplicados

Error_duplicidad_IDHogar=UNIDOS_HOG[duplicated(UNIDOS_HOG$A01),c("idEncuesta","A01")]#Detecta IDHogar duplicados a nivel de hogar.
Error_duplicidad_IDIntegrante=UNIDOS[duplicated(UNIDOS$IdIntegranteHogar) & !is.na(UNIDOS$IdIntegranteHogar),c("A01","IdIntegranteHogar")]#Detecta IdIntegranteHogar a nivel de hogar.

source("Fonetico.R")#Se utiliza para dirigir las salidas a las carpetas definidas.
Error_duplicidad_Integrante_Fonetico=UNIDOS[duplicated(paste(fonetico(UNIDOS$E01_A),
                                                                fonetico(UNIDOS$E01_B),
                                                                fonetico(UNIDOS$E01_C),
                                                                fonetico(UNIDOS$E01_D),
                                                              UNIDOS$E02))|
                                                duplicated(paste(fonetico(UNIDOS$E01_A),
                                                                  fonetico(UNIDOS$E01_B),
                                                                  fonetico(UNIDOS$E01_C),
                                                                  fonetico(UNIDOS$E01_D),
                                                                  UNIDOS$E02),fromLast=TRUE),c("A01","IdIntegranteHogar",grep("E01",names(UNIDOS),value = T),"E02")]
