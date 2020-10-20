#Validación por capitulo

############
#Capitulo A#
############
Error_IdEncuesta = UNIDOS[is.na(as.numeric(UNIDOS$idEncuesta)),c("A01","IdIntegranteHogar","idEncuesta")]
Error_A01 =
Error_A04 = UNIDOS_HOG[!(UNIDOS_HOG$A04 %in% c("1","2","3")),c("A01","A04")]
Error_A04_1 = UNIDOS_HOG[((UNIDOS_HOG$A04 %in% "1") & (UNIDOS_HOG$A04_1 %in% 999))|
                         ((!UNIDOS_HOG$A04 %in% "1") & (!UNIDOS_HOG$A04_1 %in% 999))|
                         ((UNIDOS_HOG$A04 %in% "1") & (nchar(as.character(UNIDOS_HOG$A04_1))>42)), c("A01","A04","A04_1")]
Error_A04_2 = UNIDOS_HOG[(UNIDOS_HOG$A04 %in% "2") & (UNIDOS_HOG$A04_2 %in% 999),c("A01","A04","A04_2")]
Error_A05 = UNIDOS_HOG[UNIDOS_HOG$A04 %in% 1 & (!str_detect(UNIDOS_HOG$A05, "[:digit:]") | !str_detect(UNIDOS_HOG$A05, "[^0-9]")),c("A01","A04","A05")]
Error_A06_A = UNIDOS_HOG[((UNIDOS_HOG$A04 %in% "1") & (UNIDOS_HOG$A06_A %in% 999))|
                           (UNIDOS_HOG$A06_A %in% 99),c("A01","A04","A06_A")]
Error_A06_A1 = UNIDOS_HOG[(!UNIDOS_HOG$A06_A %in% c("999","")) & (!UNIDOS_HOG$A06_A1 %in% 999),c("A01","A04","A06_A","A06_A1")]
Error_A06_B = UNIDOS_HOG[(UNIDOS_HOG$A04 %in% c("2","3")) & (UNIDOS_HOG$A06_B %in% 999),c("A01","A04","A06_B")]
Error_A06_B1 = UNIDOS_HOG[(UNIDOS_HOG$A04 %in% "1") & (UNIDOS_HOG$A06_A %in% 999),c("A01","A04","A06_A")]
Error_A06_C = UNIDOS_HOG[(UNIDOS_HOG$A04 %in% c("2","3")) & (UNIDOS_HOG$A06_C %in% 999),c("A01","A04","A06_C")]
Error_A06_C1 = UNIDOS_HOG[(UNIDOS_HOG$A04 %in% c("2","3")) & (UNIDOS_HOG$A06_C %in% 999),c("A01","A04","A06_C")]

Error_A10 = UNIDOS_HOG[nchar(UNIDOS_HOG$A10)!=7,c("A01","A10")]
Error_A10 = Error_A10[!is.na(Error_A10$A01),]
Error_A10_1 = UNIDOS_HOG[nchar(UNIDOS_HOG$A10_1)!=10 |
                         !grepl("^3", UNIDOS_HOG$A10_1), c("A01","A10_1")]
Error_A10_1=Error_A10_1[!is.na(Error_A10_1$A10_1),]

Error_A11 = UNIDOS_HOG[!grepl("@[gmail|hotmail|yahoo|outlook]",tolower(UNIDOS_HOG$A11)), c("A01","A11")]

id=intersect(intersect(Error_A10$A01, Error_A10_1$A01), Error_A11$A01)
Error_A10_A11 = UNIDOS_HOG[(is.na(UNIDOS_HOG$A10) & is.na(UNIDOS_HOG$A10_1) & is.na(UNIDOS_HOG$A11))|
                             (UNIDOS_HOG$A01 %in% id),c("A01","A10","A10_1","A11")]

Coordenadas = UNIDOS_HOG[!UNIDOS_HOG$Latitud %in% 'Sin dato', c("A01","Latitud","Longitud","A03")]
Error_Latitud = Coordenadas[!between(as.numeric(as.character(Coordenadas$Latitud)),-4.226922,13.394123), c("A01","Latitud","Longitud","A03")]
Error_Longitud = Coordenadas[!between(as.numeric(as.character(Coordenadas$Longitud)),-81.370210,-66.845070), c("A01","Latitud","Longitud","A03")]
#Error_Altitud = UNIDOS_HOG[!is.na(UNIDOS_HOG$A01) & !is.na(UNIDOS_HOG$Altitud),c("A01","Altitud")]

############
#Capitulo B#
############
Error_B01=UNIDOS_HOG[!(UNIDOS_HOG$B01 %in% c("1","2","3","4","5")),c("A01","B01")]
Error_B02=UNIDOS_HOG[!(UNIDOS_HOG$B02 %in% c("1","2","3","4","5","6","7","0")) |
                      (UNIDOS_HOG$B01 %in% c("1","2","3") & UNIDOS_HOG$B02 %in% "0")|
                      (UNIDOS_HOG$B01 %in% c("3","5") & UNIDOS_HOG$B02 %in% "7")|
                      (UNIDOS_HOG$B01 %in% "4" & UNIDOS_HOG$B02 %in% c("1","2","3")),c("A01","B01","B02")]

Error_B03=UNIDOS_HOG[(!UNIDOS_HOG$B03 %in% c("1","2","3","4","5","6"))|
                     (UNIDOS_HOG$B01 %in% c("1","2","4") & UNIDOS_HOG$B02 %in% "7" & UNIDOS_HOG$B03 %in% c("1","2"))|
                     (UNIDOS_HOG$B01 %in% 4 & UNIDOS_HOG$B03 %in% c("1","2"))|
                     (UNIDOS_HOG$B02 %in% c("2","3","5") & UNIDOS_HOG$B03 %in% "1")|
                     (UNIDOS_HOG$B02 %in% c("6","7","0") & UNIDOS_HOG$B03 %in% c("1","2")), c("A01","B01","B02","B03")]

Error_B04A = UNIDOS_HOG[!UNIDOS_HOG$B04A %in% c("1","2"), c("A01","B04A")]
Error_B04B = UNIDOS_HOG[(UNIDOS_HOG$B01 %in% 4 & UNIDOS_HOG$B04B %in% 1)|
                          (UNIDOS_HOG$B01 %in% c("1","2") & UNIDOS_HOG$B02 %in% 7 & UNIDOS_HOG$B04B %in% 1), c("A01","B04B")]
Error_B04C = UNIDOS_HOG[(UNIDOS_HOG$B01 %in% 4 & UNIDOS_HOG$B04C %in% 1)|
                          (UNIDOS_HOG$B01 %in% c("1","2") & UNIDOS_HOG$B02 %in% 7 & UNIDOS_HOG$B04C %in% 1), c("A01","B04C")]
Error_B04E = UNIDOS_HOG[!UNIDOS_HOG$B04E %in% c("1","2"), c("A01","B04E")]
Error_B04F = UNIDOS_HOG[!UNIDOS_HOG$B04F %in% c("1","2"), c("A01","B04F")]

Error_B04_Energia=UNIDOS_HOG[(((UNIDOS_HOG$B04A %in% "1")) & !(UNIDOS_HOG$B04Energia %in% c("0","1","2","3","4","5","6","9","Sin dato"))) |
                               (((UNIDOS_HOG$B04A %in% "2")) &  (UNIDOS_HOG$B04Energia %in% c("0","1","2","3","4","5","6","9","Sin dato"))), c("A01","B04A","B04Energia")]

Error_B04_Acueducto=UNIDOS_HOG[(((UNIDOS_HOG$B04F %in% "1")) & !(UNIDOS_HOG$B04Acueducto %in% c("0","1","2","3","4","5","6","9","Sin dato"))) |
                                 (((UNIDOS_HOG$B04F %in% "2")) &  (UNIDOS_HOG$B04Acueducto %in% c("0","1","2","3","4","5","6","9","Sin dato"))), c("A01","B04F","B04Acueducto")]


Error_B04_1=UNIDOS_HOG[!(UNIDOS_HOG$B04_1 %in% c("1","2","3","4")),c("A01","B04_1")]

for (i in 1:6) {
  x = paste0("B05_",LETTERS[i])
  assign(paste0("Error_B05_",LETTERS[i]), UNIDOS_HOG[!UNIDOS_HOG[[x]] %in% c("1","2"), c("A01",x)])
}

for (i in 1:6) {
  x = paste0("B05_",LETTERS[i])
  y = paste0("B05_",LETTERS[i],"1")
  assign(paste0("Error_B05_",LETTERS[i],"1"), UNIDOS_HOG[UNIDOS_HOG[[x]] %in% 2 & between(UNIDOS_HOG[[y]],1,99), c("A01", x, y)])
}

Error_B06=UNIDOS_HOG[!(UNIDOS_HOG$B06 %in% c("1","2","3","4","5","6","7","8")),c("A01","B06")]

Error_B07=UNIDOS_HOG[!(UNIDOS_HOG$B07 %in% c("1","2","3","4")),c("A01","B07")]

############
#Capitulo C#
############

Error_C01=UNIDOS_HOG[!between(UNIDOS_HOG$C01,1,15)|
                      (UNIDOS_HOG$B01 %in% c("1","2") & UNIDOS_HOG$B02 %in% 7 & !UNIDOS_HOG$C01 %in% c("1","2","3"))|
                      (UNIDOS_HOG$B01 %in% 3 & !UNIDOS_HOG$C01 %in% c("1","2","3"))|
                      (UNIDOS_HOG$B01 %in% 4 & UNIDOS_HOG$C01>1),c("A01","B01","B02","C01")]
Error_C01_1=UNIDOS_HOG[(UNIDOS_HOG$C01_1>UNIDOS_HOG$C01),c("A01","C01","C01_1")]
Error_C02=UNIDOS_HOG[ (UNIDOS_HOG$C02>UNIDOS_HOG$C01_1),c("A01","C02","C01_1")]
Error_C02_1=UNIDOS_HOG[ (UNIDOS_HOG$C02_1>UNIDOS_HOG$C02),c("A01","C02","C02_1")]

Error_C03=UNIDOS_HOG[(UNIDOS_HOG$B01 %in% 4 & UNIDOS_HOG$C03 %in% c("1","2"))|
                     (UNIDOS_HOG$B01 %in% c("1","2") & UNIDOS_HOG$B02 %in% 7 & UNIDOS_HOG$C03 %in% c("1","2")) |
                     (UNIDOS_HOG$B01 %in% c("1","2") & UNIDOS_HOG$C03 %in% 5)|
                     (UNIDOS_HOG$B04B %in% 2 & UNIDOS_HOG$C03 %in% 1), c("A01","B01","B02","B04B","C03")]

Error_C03_1=UNIDOS_HOG[UNIDOS_HOG$C03 %in% 5 & (UNIDOS_HOG$C03_1 %in% c("1","2","Sin dato")),c("A01","C03","C03_1")]
Error_C03_2=UNIDOS_HOG[UNIDOS_HOG$C03 %in% 5 & (UNIDOS_HOG$C03_2 %in% c("1","2","3","Sin dato")),c("A01","C03","C03_2")]

for (i in 1:5) {

  x = paste0("C03_4",LETTERS[i])
  assign(paste0("Error_C03_4",LETTERS[i]), UNIDOS_HOG[(UNIDOS_HOG$C03 %in% 5 & UNIDOS_HOG[[x]] %in% c("1","2","Sin dato"))|
                                                      (!UNIDOS_HOG$C03 %in% 5 & !UNIDOS_HOG[[x]] %in% c("1","2","Sin dato")) , c("A01","C03",x)])

}

Error_C03_4F=UNIDOS_HOG[(UNIDOS_HOG$C03_1 %in% 2 & UNIDOS_HOG$C03_4F %in% 9)|
                        (!UNIDOS_HOG$C03_1 %in%  c("2","9") & !UNIDOS_HOG$C03_4F %in% 9),c("A01","C03","C03_1","C03_4F")]

Error_C03_5=UNIDOS_HOG[UNIDOS_HOG$C03 %in% 2 & !(UNIDOS_HOG$C03_5 %in% c("1","2","3","4","Sin dato")),c("A01","C03","C03_5")]
Error_C03_6=UNIDOS_HOG[(UNIDOS_HOG$C03 %in% 4 & !UNIDOS_HOG$C03_6 %in% c("1","2","Sin dato"))|
                         (!UNIDOS_HOG$C03 %in% 4 & UNIDOS_HOG$C03_6 %in% c("1","2")) , c("A01","C03","C03_6")]

Error_C04=UNIDOS_HOG[!(UNIDOS_HOG$C04 %in% c("1","2","3","4","5","6","7","8","9","Sin dato")) |
                      (UNIDOS_HOG$B04F  %in% 2 & UNIDOS_HOG$C04 %in% 1),c("A01","B04F","C04")]

Error_C04_2  = UNIDOS_HOG[(UNIDOS_HOG$C04 %in% 1 & !(UNIDOS_HOG$C04_2 %in% c("1","2","Sin dato"))) |
                          (UNIDOS_HOG$C04_2 %in% 2 & !between(UNIDOS_HOG$C04_2A,1,6)), c("A01","C04","C04_2","C04_2A")]
Error_C04_2A = UNIDOS_HOG[(UNIDOS_HOG$C04_2 %in% 2 & !between(UNIDOS_HOG$C04_2A,1,6)) |
                          (!UNIDOS_HOG$C04_2 %in% 2 & between(UNIDOS_HOG$C04_2A,1,6)), c("A01","C04","C04_2","C04_2A")]
Error_C04_2A = Error_C04_2A[!is.na(Error_C04_2A$A01),]

Error_C04_3  = UNIDOS_HOG[(UNIDOS_HOG$C04_2 %in% 1 & !(UNIDOS_HOG$C04_3 %in% c("1","2","Sin dato"))) |
                          (!UNIDOS_HOG$C04_2  %in% 1 & between(UNIDOS_HOG$C04_3,1,23)), c("A01","C04","C04_2","C04_2A","C04_3","C04_3A")]

Error_C04_3A = UNIDOS_HOG[(UNIDOS_HOG$C04_3 %in% 2 & !between(UNIDOS_HOG$C04_3A,1,23)) |
                          (!UNIDOS_HOG$C04_3  %in% 2 & between(UNIDOS_HOG$C04_3A,1,23)), c("A01","C04","C04_2","C04_2A","C04_3","C04_3A")]

Error_C04_4  = UNIDOS_HOG[!(UNIDOS_HOG$C04_4 %in% c("1","2","3","4","5","6","Sin dato")), c("A01","C04_4")]

Error_C04_5=UNIDOS_HOG[!(UNIDOS_HOG$C04_5 %in% c("1","2","Sin dato")),c("A01","C04_5")]
Error_C04_6=UNIDOS_HOG[UNIDOS_HOG$C04_5!=1 & (UNIDOS_HOG$C04_6 %in% c("1","2","3","4","5","6","7","8","Sin dato")),c("A01","C04_5","C04_6")]

UNIDOS_HOG$Orden_publico=rowSums(UNIDOS_HOG[grep("C05", names(UNIDOS_HOG), value = TRUE)] == "1")

for (i in 1:4) {

  x = paste0("C05_",LETTERS[i])
  assign(paste0("Error_C05_",LETTERS[i]), UNIDOS_HOG[!UNIDOS_HOG[[x]] %in% c("1","2"), c("A01",x)])

}

Error_C06=UNIDOS_HOG[UNIDOS_HOG$Orden_publico %in% 0 & (UNIDOS_HOG$C06 %in% c("1","2","3","4","5","Sin dato")),c("A01","C06","Orden_publico",grep("C05", names(UNIDOS_HOG), value = TRUE))]

Error_C07=UNIDOS_HOG[(UNIDOS_HOG$C06 %in% c("1","2","3","4","5") & !UNIDOS_HOG$C07 %in% c("1","2","3","4","Sin dato"))|
                     (!UNIDOS_HOG$C06 %in% c("1","2","3","4","5") & UNIDOS_HOG$C07 %in% c("1","2","3","4","Sin dato")), c("A01","C06","C07")]

Error_C08=UNIDOS_HOG[!(UNIDOS_HOG$C08 %in% c("1","2","3","4","5","6","7","Sin dato")) |
                      (UNIDOS_HOG$B04E %in% 2 & UNIDOS_HOG$C08 %in% "1"), c("A01","B04E","C08")]

Error_C09=UNIDOS_HOG[(!UNIDOS_HOG$C09 %in% c("1","2","Sin dato"))|
                      (UNIDOS_HOG$B01 %in% 3 & UNIDOS_HOG$C03 %in% c("1","2","3","4") & !UNIDOS_HOG$C09 %in% 2)|
                      (UNIDOS_HOG$B01 %in% c("1","2") & !UNIDOS_HOG$C09 %in% 1),c("A01","B01","C03","C09")]
Error_C10=UNIDOS_HOG[(!UNIDOS_HOG$C10 %in% c("1","2","3","4","5","Sin dato"))|
                     (UNIDOS_HOG$C09 %in% 1 & !UNIDOS_HOG$C10 %in% c("1","2","3","4","5","Sin dato"))|
                     (UNIDOS_HOG$C02==UNIDOS_HOG$C02_1 & UNIDOS_HOG$C10 %in% 2)|
                     (UNIDOS_HOG$C09 %in% 2 & UNIDOS_HOG$C10 %in% 1),c("A01","C02","C02_1","C09","C10")]

Error_C11=UNIDOS_HOG[(UNIDOS_HOG$B01 %in% c("1","2") & UNIDOS_HOG$B02 %in% 7 & UNIDOS_HOG$C11 %in% 2)|
                     (UNIDOS_HOG$B01 %in% 4 & UNIDOS_HOG$C11 %in% 2),c("A01","B01","B02","C11")]

Error_C11_1=UNIDOS_HOG[(UNIDOS_HOG$C09 %in% 2 & UNIDOS_HOG$C10 %in% 5) & (UNIDOS_HOG$C11_1A %in% c("1","2")),c("A01","C09","C10","C11_1A")]

for (i in 1:5) {

  x = paste0("C11_1",LETTERS[i])
  assign(paste0("Error_C11_1",LETTERS[i]), UNIDOS_HOG[(!UNIDOS_HOG[[x]] %in% c("1","2","9","Sin dato")), c("A01",x)])

}

Error_C12=UNIDOS_HOG[(UNIDOS_HOG$B01 %in% 4 & UNIDOS_HOG$C12 %in% 2)|
                    (UNIDOS_HOG$B04A %in% 2 & UNIDOS_HOG$C12 %in% 1)|
                    (UNIDOS_HOG$B04C %in% 2 & UNIDOS_HOG$C12 %in% 2),c("A01","B01","B04A","B04C","C12")]

for (i in 1:8) {

  x = paste0("C13_",LETTERS[i])
  assign(paste0("Error_C13_",LETTERS[i]), UNIDOS_HOG[(!UNIDOS_HOG[[x]] %in% c("1","2","Sin dato"))|
                                                    (UNIDOS_HOG$C09 %in% 2 & UNIDOS_HOG$C10 %in% 5 & !UNIDOS_HOG[[x]] %in% c("1","2","Sin dato")), c("A01",x)])

}

############
#Capitulo D#
############
Error_D01=UNIDOS_HOG[!(UNIDOS_HOG$D01 %in% c("1","2","3","4","5","Sin dato")), c("A01","D01")]
Error_D03=UNIDOS_HOG[!(UNIDOS_HOG$D01 %in% c("3","4","5")) & !between(UNIDOS_HOG$D03,0,10000000), c("A01","D01","D03")]
Error_D04=UNIDOS_HOG[(!UNIDOS_HOG$D01 %in% c("2","3") & UNIDOS_HOG$D04 %in% c("1","2","Sin dato")),c("A01","D01","D04")]
Error_D04_A=UNIDOS_HOG[(UNIDOS_HOG$D04 %in% 1 & !UNIDOS_HOG$D04_A %in% c("1","2","Sin dato"))|
                       (!UNIDOS_HOG$D04 %in% 1 & !UNIDOS_HOG$D04_A %in% 9),c("A01","D04","D04_A")]

############
#Capitulo E#
############
Error_IdIntegranteHogar = UNIDOS[(nchar(UNIDOS$IdIntegranteHogar)!=7) |
                                   grepl("[:alpha]", UNIDOS$IdIntegranteHogar), c("A01","IdIntegranteHogar")]

Error_E01_A=UNIDOS[is.na(UNIDOS$E01_A) | UNIDOS$E01_A %in% "" | !between(nchar(as.character(UNIDOS$E01_A)),2,42),c("IdIntegranteHogar","E01_A")]
Error_E01_B=UNIDOS[between(nchar(as.character(UNIDOS$E01_B)),1,2),c("IdIntegranteHogar","E01_B")]
Error_E01_B=Error_E01_B[!is.na(Error_E01_B$IdIntegranteHogar),]
Error_E01_C=UNIDOS[is.na(UNIDOS$E01_C) | UNIDOS$E01_C %in% "" | !between(nchar(as.character(UNIDOS$E01_C)),2,47),c("IdIntegranteHogar","E01_C")]
Error_E01_D=UNIDOS[between(nchar(as.character(UNIDOS$E01_D)),1,2),c("IdIntegranteHogar","E01_D")]
Error_E01_D=Error_E01_D[!is.na(Error_E01_D$IdIntegranteHogar),]

Error_E02= UNIDOS[!between(UNIDOS$EdadCaracterizacion,0,118),c("IdIntegranteHogar","EdadCaracterizacion")]
Error_E03=UNIDOS[!(UNIDOS$E03 %in% c("1","2","3")),c("IdIntegranteHogar","E03")]
Error_E03_1=UNIDOS[!UNIDOS$E03_1 %in% c("1","2"),c("IdIntegranteHogar","E03_1")]
Error_E03_A=UNIDOS[(UNIDOS$EdadCaracterizacion<12 & UNIDOS$E03_A %in% c("1","2","3","4")),c("IdIntegranteHogar","E03_A","EdadCaracterizacion")]

Error_E03_B=UNIDOS[(UNIDOS$EdadCaracterizacion<12 & UNIDOS$E03_B %in% c("1","2","3","4","5","6")),c("IdIntegranteHogar","E03_B","EdadCaracterizacion")]
Error_E04=UNIDOS[!(UNIDOS$E04 %in% c("1","2","3")),c("IdIntegranteHogar","E04")]
Error_E05=UNIDOS[(UNIDOS$E03_1 %in% 1 & !UNIDOS$E05 %in% c("1","2","3"))|
                (UNIDOS$E03_1 %in% 2 & !UNIDOS$E05 %in% c("4","5","6","7","8"))|
                (UNIDOS$E03_1 %in% 1 & UNIDOS$EdadCaracterizacion<7 & !UNIDOS$E05 %in% c("0","1"))|
                (UNIDOS$E03_1 %in% 1 & between(UNIDOS$EdadCaracterizacion,7,17) & !UNIDOS$E05 %in% c("0","1","2"))|
                (UNIDOS$E03_1 %in% 1 & UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$E05 %in% c("3","2","1","0")) |#Opciones para colombianos
                (UNIDOS$E03_1 %in% 1 & UNIDOS$EdadCaracterizacion<7 & !UNIDOS$E05 %in% 1) |#Opciones para extrangeros
                (UNIDOS$E03_1 %in% 2 & UNIDOS$EdadCaracterizacion<7 & !UNIDOS$E05 %in% c("0","5","6","8"))|
                (UNIDOS$E03_1 %in% 2 & UNIDOS$EdadCaracterizacion>=7 & !UNIDOS$E05 %in% c("0","4","5","6","7","8"))|
                (UNIDOS$E04 %in% 2 & !UNIDOS$E05 %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","E03_1","E04","E05")]

Error_E06=UNIDOS[(UNIDOS$E04 %in% 2 & !UNIDOS$E06 %in% 99) |
                 (UNIDOS$E06 %in% ""), c("A01","IdIntegranteHogar","E04","E05","E06","E01_A","E01_B","E01_C","E01_D")]#Revisar
Error_E07=UNIDOS[(UNIDOS$E07<=UNIDOS$E02)|
                 (UNIDOS$E07>UNIDOS$fechaInicio)|
                 (UNIDOS$E04 %in% 1 & UNIDOS$E07 %in% 9)|
                 (UNIDOS$E04 %in% c("2","3") & !UNIDOS$E07 %in% 9), c("IdIntegranteHogar","E04","E07","E02","fechaInicio")]#Revisar formato fecha

Error_E07_A1=UNIDOS[(UNIDOS$E03_1 %in% "1" & (UNIDOS$E07_A1 %in% 999))|
                      (UNIDOS$E03_1 %in% "2" & !(UNIDOS$E07_A1 %in% c("999","Sin dato"))),c("IdIntegranteHogar","E03_1","E07_A1")]
Error_E07_A2=UNIDOS[(UNIDOS$E03_1 %in% "1" & (UNIDOS$E07_A2 %in% 999))|
                      (UNIDOS$E03_1 %in% "2" & !(UNIDOS$E07_A2 %in% c("999","Sin dato"))),c("IdIntegranteHogar","E03_1","E07_A2")]
Error_E07_A3=UNIDOS[(UNIDOS$E03_1 %in% "1" & !(UNIDOS$E07_A3 %in% 999))|
                      (UNIDOS$E03_1 %in% "2" & (UNIDOS$E07_A3 %in% c("999","Sin dato"))),c("IdIntegranteHogar","E03_1","E07_A3")]

Error_E08=UNIDOS[!(UNIDOS$E08 %in% c("1","2","3","4","5","6")),c("IdIntegranteHogar","E08")]
Error_E09=UNIDOS[(UNIDOS$E08 %in% "1" & UNIDOS$E09 %in% 999),c("IdIntegranteHogar","E08","E09")]

#Revisar validación
Error_E11=UNIDOS[!(UNIDOS$E11 %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","Sin dato"))|
                   (UNIDOS$EdadCaracterizacion<=14 & UNIDOS$E11 %in% c("2","5","8","9"))|
                   (UNIDOS$EdadCaracterizacion<14 & UNIDOS$E11 %in% 1),c("IdIntegranteHogar","E11","EdadCaracterizacion")]

Error_JEFE_DUPLICADO= UNIDOS[UNIDOS$E11 %in% "1",] %>% group_by(A01) %>% mutate(JEFES=n())#Se construye dataframe con el conteo de jefes de hogar.
Error_JEFE_DUPLICADO= as.data.frame(Error_JEFE_DUPLICADO[Error_JEFE_DUPLICADO$JEFES!=1, c("A01","IdIntegranteHogar","E11","E05","E06")])#Si un hogar tiene más de un jefe de hogar o no tiene se asigna al error.
Error_SIN_JEFE= UNIDOS[c("A01","IdIntegranteHogar","E11","E05","E06")] %>% group_by(A01) %>% filter(all(E11!="1"))
Error_SIN_JEFE= as.data.frame(Error_SIN_JEFE[!duplicated(Error_SIN_JEFE$A01),])

#Revisar validación
Error_E12  =UNIDOS[(!UNIDOS$E12 %in% c("1","2","3","4","5","Sin dato")) |
                     (UNIDOS$E11 %in% "2" & UNIDOS$E12 %in% c("3","4","5"))|
                     (UNIDOS$EdadCaracterizacion<14 & !UNIDOS$E12 %in% "5"),c("IdIntegranteHogar","EdadCaracterizacion","E11","E12")]

Error_E12_1=UNIDOS[UNIDOS$E12 %in% c("3","4","5") & UNIDOS$E12_1 %in% c("1","2","Sin dato"),c("IdIntegranteHogar","E12","E12_1")]
#Revisar validación
Error_E13  =UNIDOS[(UNIDOS$E12_1 %in% 1 & UNIDOS$E13 %in% 999)|
                   (UNIDOS$E12_1 %in% 9 & !UNIDOS$E13 %in% 999),c("A01","IdIntegranteHogar","E12_1","E13")]
Error_E13_1=UNIDOS[!(UNIDOS$E13_1 %in% c("1","2","Sin dato")),c("IdIntegranteHogar","E13_1")]
Error_E13_2=UNIDOS[(UNIDOS$E13_1 %in% 1 & UNIDOS$E13_2 %in% 999)|
                   (!UNIDOS$E13_1 %in% 1 & !UNIDOS$E13_2 %in% 999), c("IdIntegranteHogar","E13_1","E13_2")]

UNIDOS$Discapacidad=rowSums(UNIDOS[grep("E15", names(UNIDOS), value = TRUE)]=="1")

Error_E15=UNIDOS[UNIDOS$Discapacidad>7 | (UNIDOS$Discapacidad>1 & UNIDOS$E15_S %in% "1"),c("IdIntegranteHogar",grep("E15", names(UNIDOS), value = TRUE),"Discapacidad")]

Error_E16=UNIDOS[(UNIDOS$E15_S %in% "1" & !UNIDOS$E16 %in% 9)|
                   (UNIDOS$E15_S %in% "2" & UNIDOS$E16 %in% 9),c("IdIntegranteHogar","E15_S","E16")]
Error_E17=UNIDOS[(UNIDOS$E15_S %in% "1" & UNIDOS$E17 %in% c("1","2")),c("IdIntegranteHogar","E15_R","E15_S","E17")]
Error_E18=UNIDOS[(UNIDOS$E15_S %in% "1" & !UNIDOS$E18 %in% 9)|
                  (UNIDOS$E15_S %in% "2" & UNIDOS$E18 %in% 9), c("IdIntegranteHogar","E18","Discapacidad","E15_S")]

Error_E18_1=UNIDOS[(UNIDOS$E15_S %in% "1" & UNIDOS$E18 %in% "1") & !(UNIDOS$E18_1 %in% c("1","2")), c("IdIntegranteHogar","E18_1")]
Error_E19=UNIDOS[UNIDOS$E15_S %in% "1" & (UNIDOS$E19 %in% c("1","2")),c("IdIntegranteHogar","E19","E19_1")]

Error_E19_1=UNIDOS[(UNIDOS$E15_S %in% "1" | UNIDOS$E19 %in% "2") & (UNIDOS$E19_1 %in% c("1","2")),c("IdIntegranteHogar","E19","E19_1")]

Error_E20=UNIDOS[UNIDOS$D04 %in% "1" & (UNIDOS$E20 %in% 999),c("IdIntegranteHogar","E20")]

############
#Capitulo F#
############
Error_F02=UNIDOS[!UNIDOS$F02 %in% c("0","1","2","3","9"),c("IdIntegranteHogar","F02")]

Error_F03=UNIDOS[!UNIDOS$F03 %in% c("1","2","Sin dato"),c("IdIntegranteHogar","F03")]
Error_F03_1=UNIDOS[(UNIDOS$F03 %in% 1 & !UNIDOS$F03_1 %in% c("1","2"))|
                   (!UNIDOS$F03 %in% 1 & UNIDOS$F03_1 %in% c("1","2")),c("IdIntegranteHogar","F03","F03_1")]
Error_F03_2=UNIDOS[(UNIDOS$F03_1 %in% 1 & !UNIDOS$F03_2 %in% c("1","2"))|
                   (!UNIDOS$F03_1 %in% 1 & UNIDOS$F03_2 %in% c("1","2")),c("IdIntegranteHogar","F03_1","F03_2")]

Error_F05=UNIDOS[UNIDOS$EdadCaracterizacion>=12 & !(UNIDOS$F05 %in% c("1","2")),c("IdIntegranteHogar","EdadCaracterizacion","F05")]
Error_F06=UNIDOS[(UNIDOS$E03 %in% 2 & between(UNIDOS$EdadCaracterizacion,8,59) & UNIDOS$F06 %in% 9)|
                 (!UNIDOS$E03 %in% 2 & !UNIDOS$F06 %in% 9), c("IdIntegranteHogar","E03","F06","EdadCaracterizacion")]
Error_F07=UNIDOS[(UNIDOS$E03 %in% 2 & UNIDOS$EdadCaracterizacion>=8 & UNIDOS$F07 %in% 9)|
                 (UNIDOS$E03 %in% 2 & !UNIDOS$EdadCaracterizacion>=8 & !UNIDOS$F07 %in% 9)|
                 (!UNIDOS$E03 %in% 2 & !UNIDOS$F07 %in% 9), c("IdIntegranteHogar","E03","EdadCaracterizacion","F07")]

############
#Capitulo G#
############
Edad_G=between(UNIDOS$EdadCaracterizacion,0,5)
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

UNIDOS$EdadCaracterizacion_Meses=elapsed_months(UNIDOS$fechaInicio,UNIDOS$E02)#Calculo de edad en meses

Error_G01=UNIDOS[(UNIDOS$EdadCaracterizacion>4 & UNIDOS$G01 %in% c("1","2","3","4","5","6","7","8"))|
                 (!UNIDOS$EdadCaracterizacion>4 & !UNIDOS$G01 %in% c("1","2","3","4","5","6","7","8")), c("IdIntegranteHogar","G01","EdadCaracterizacion")]

Error_G02_1 =UNIDOS[(UNIDOS$EdadCaracterizacion>4 & UNIDOS$G02_1 %in% c("1","2","Sin dato"))|
                    (!UNIDOS$EdadCaracterizacion>4 & !UNIDOS$G02_1 %in% c("1","2","Sin dato")),c("IdIntegranteHogar","G02_1","EdadCaracterizacion")]
Error_G03   =UNIDOS[(UNIDOS$EdadCaracterizacion>4 & UNIDOS$G03 %in% c("1","2","Sin dato"))|
                    (!UNIDOS$EdadCaracterizacion>4 & !UNIDOS$G03 %in% c("1","2", "Sin dato")),c("IdIntegranteHogar","G03","EdadCaracterizacion")]

for (i in 1:4) {
  x = paste0("G04_",LETTERS[i])
  assign(paste0("Error_G04_",LETTERS[i]), UNIDOS[(UNIDOS$EdadCaracterizacion>4 & UNIDOS[[x]] %in% c("1","2","Sin dato"))|
                                                 (!UNIDOS$EdadCaracterizacion>4 & !UNIDOS[[x]] %in% c("1","2","Sin dato")), c("IdIntegranteHogar","K07", x)])
}

Error_G05_1=UNIDOS[(between(UNIDOS$EdadCaracterizacion_Meses,7,59) & !UNIDOS$G05_1 %in% c("1","2","3","Sin dato"))|
                   (!between(UNIDOS$EdadCaracterizacion_Meses,7,59) & UNIDOS$G05_1 %in% c("1","2","3","Sin dato")),c("IdIntegranteHogar","EdadCaracterizacion","EdadCaracterizacion_Meses","G05_1")]

Error_G05_1A=UNIDOS[(between(UNIDOS$EdadCaracterizacion_Meses,7,59) & UNIDOS$G05_1 %in% 1 & !UNIDOS$G05_1A %in% 999)|
                    (between(UNIDOS$EdadCaracterizacion_Meses,7,59) & UNIDOS$G05_1 %in% 2 & UNIDOS$G05_1A %in% 999)|
                    (between(UNIDOS$EdadCaracterizacion_Meses,7,59) & UNIDOS$G05_1 %in% 2 & grepl("^[0-9]+$", UNIDOS$G05_1A))|
                    (nchar(as.character(UNIDOS$G05_1A))>150), c("IdIntegranteHogar","EdadCaracterizacion_Meses","G05_1","G05_1A")]

Error_G06=UNIDOS[(!UNIDOS$EdadCaracterizacion<5 & !UNIDOS$G06 %in% 99)|
                 (UNIDOS$EdadCaracterizacion<5& !UNIDOS$G06 %in% 99 & !between(UNIDOS$G06,6,27))|
                 (!UNIDOS$EdadCaracterizacion<5 & between(UNIDOS$G06,6,27)), c("IdIntegranteHogar","EdadCaracterizacion","G06")]
Error_G07=UNIDOS[(UNIDOS$EdadCaracterizacion<5 & !UNIDOS$G07 %in% c("1","2","3","Sin dato"))|
                 (!UNIDOS$EdadCaracterizacion<5 & UNIDOS$G07 %in% c("1","2","3","Sin dato")),c("IdIntegranteHogar","EdadCaracterizacion","G07")]

############
#Capitulo H#
############
############
#Capitulo I#
############
Error_I01=UNIDOS[(UNIDOS$EdadCaracterizacion>=5 & !UNIDOS$I01 %in% c("1","2","Sin dato"))|
                 (!UNIDOS$EdadCaracterizacion>=5 & UNIDOS$I01 %in% c("1","2","Sin dato")),c("IdIntegranteHogar","I01","EdadCaracterizacion")]
Error_I02=UNIDOS[(UNIDOS$EdadCaracterizacion>=5 & !UNIDOS$I02 %in% c("0","1","2","3","4","5","6","7"))|
                 (!UNIDOS$EdadCaracterizacion>=5 & UNIDOS$I02 %in% c("0","1","2","3","4","5","6","7"))|
                 (UNIDOS$I01 %in% 2 & UNIDOS$I02 %in% c("3","4","5","6","7"))|
                 (between(UNIDOS$EdadCaracterizacion,5,8) & UNIDOS$I02 %in% c("3","4","5","6","7"))|
                 (between(UNIDOS$EdadCaracterizacion,9,12) & UNIDOS$I02 %in% c("3","4","5","6","7")),c("IdIntegranteHogar","I02","EdadCaracterizacion")]

Error_I02_A=UNIDOS[(UNIDOS$EdadCaracterizacion>=5 & !UNIDOS$I02_A %in% 0:13)|
                   (!UNIDOS$EdadCaracterizacion>=5 & UNIDOS$I02_A %in% 0:13),c("IdIntegranteHogar","I02_A","I02","EdadCaracterizacion")]#Debería ser mayor o igual a 5

Error_I02_1=UNIDOS[(UNIDOS$I02 %in% 0 & !UNIDOS$I02_1 %in% 9)|
                   (UNIDOS$EdadCaracterizacion>=5 & UNIDOS$I02 %in% c("5","6","7") & UNIDOS$I02_1 %in% 9)|
                   (UNIDOS$EdadCaracterizacion>=5 & !UNIDOS$I02 %in% c("5","6","7") & !UNIDOS$I02_1 %in% 9),c("IdIntegranteHogar","EdadCaracterizacion","I02","I02_1")]#Debería ser mayor o igual a 18

Error_I03=UNIDOS[(!UNIDOS$EdadCaracterizacion>=5 & UNIDOS$I03 %in% c("1","2"))|
                 (UNIDOS$EdadCaracterizacion>=5 & (UNIDOS$I02 %in% c("1","2","3","4")  & UNIDOS$I02_1 %in% c("1","2")) & !UNIDOS$I03 %in% c("1","2"))|
                 (UNIDOS$EdadCaracterizacion>=5 & (!UNIDOS$I02 %in% c("1","2","3","4") & !UNIDOS$I02_1 %in% c("1","2")) & UNIDOS$I03 %in% c("1","2")),c("IdIntegranteHogar","EdadCaracterizacion","I02","I02_1","I03")]

Error_I06=UNIDOS[(!UNIDOS$EdadCaracterizacion>=18 & UNIDOS$I06 %in% c("1","2"))|
                 (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$I06 %in% c("1","2")), c("IdIntegranteHogar","I06","EdadCaracterizacion")]

############
#Capitulo J#
############
Error_J01=UNIDOS[(UNIDOS$EdadCaracterizacion>=8 & !UNIDOS$J01 %in% c("1","2","3","4","5","6","7","0"))|
                 (!UNIDOS$EdadCaracterizacion>=8 & UNIDOS$J01 %in% c("1","2","3","4","5","6","7","0"))|
                 (between(UNIDOS$EdadCaracterizacion,8,12) & UNIDOS$J01 %in% 5)|
                 (UNIDOS$EdadCaracterizacion<18 & UNIDOS$J01 %in% 6)|
                 (UNIDOS$E11 %in% 15 & !UNIDOS$J01 %in% 1), c("IdIntegranteHogar","E11","EdadCaracterizacion","J01")]

Error_J03_1 =UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & UNIDOS$J03_1 %in% c("1","2","3"))|
                    (!UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% c("1","2","3"))|
                    (UNIDOS$J01 %in% 1 & UNIDOS$J03_1 %in% c("1","2","3"))|
                    (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J01 %in% c("0","2","4") & UNIDOS$J03_1 %in% 9) , c("IdIntegranteHogar","EdadCaracterizacion","J01","J03_1")]

Error_J04   =UNIDOS[(!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$J04 %in% 999)|
                    (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J01 %in% 2 & !between(UNIDOS$J04,0,260))|
                    (!UNIDOS$J01 %in% 2 & !UNIDOS$J04 %in% 999)|
                    (UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS$J04 %in% 999), c("IdIntegranteHogar","EdadCaracterizacion","J01","J04")]

Error_J05   =UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS$J04 %in% 999)|
                    (UNIDOS$J01 %in% 1 & UNIDOS$J03_1 %in% c("1","2","3"))|
                    (!UNIDOS$J03_1 %in% 9 & !UNIDOS$J05 %in% c("1","2","Sin dato"))|
                    (!UNIDOS$J04 %in% 999 & !UNIDOS$J05 %in% c("1","2","Sin dato"))|
                    (UNIDOS$J03_1 %in% 9 & UNIDOS$J04 %in% 999 & UNIDOS$J05 %in% c("1","2","Sin dato")), c("IdIntegranteHogar","EdadCaracterizacion","J03_1","J04","J05")]

Error_J05_A =UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS$J04 %in% 999)|
                    (UNIDOS$J01 %in% 1 & UNIDOS$J03_1 %in% c("1","2","3"))|
                    (!UNIDOS$J05 %in% "1" & !UNIDOS$J05_A %in% 99)|
                    (UNIDOS$J05 %in% "1" & UNIDOS$J05_A %in% 99) , c("IdIntegranteHogar","EdadCaracterizacion","J05","J05_A")]

############
#Capitulo K#
############

#Ocupados#
##########

Error_K01= UNIDOS[(!UNIDOS$EdadCaracterizacion>=8 & UNIDOS$K01 %in% c("1","2","3","4","5","6","7","8","9","10"))|
                  (UNIDOS$J01 %in% 1 & !UNIDOS$K01 %in% c("1","2","3","4","5","6","7","8","9","10"))|
                  (UNIDOS$I01 %in% 2 & UNIDOS$K01 %in% 4)|
                  (UNIDOS$I02 %in% c("1","2","3","4","5") & UNIDOS$K01 %in% 4)|
                  (UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS$K01 %in% 99), c("IdIntegranteHogar","EdadCaracterizacion","I01","I02","J01","K01")]

Error_K01_1= UNIDOS[(!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K01_1 %in% 999)|
                    (UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS$K01_1 %in% 999), c("IdIntegranteHogar","EdadCaracterizacion","J01","K01","K01_1")]

#Asalariados#
#############

(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS$X %in% 99)|
(UNIDOS$K01 %in% c("4","5","6","7") & !UNIDOS$X %in% 99)

(UNIDOS$K01 %in% c("8","9") & !UNIDOS$X %in% 999)

Error_K02= UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS$K02 %in% 99)|
                  (UNIDOS$K01 %in% c("4","5","6","7") & !UNIDOS$K02 %in% 99)|
                  (UNIDOS$J01 %in% 1 & UNIDOS$K01 %in% c("1","2","3","8") & !between(UNIDOS$K02,0,30000000)), c("IdIntegranteHogar","EdadCaracterizacion","K01","K01_1","K02")]

Error_K03=UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS$K03 %in% 9)|
                  (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$K03 %in% 9)|
                  (!UNIDOS$J01 %in% 1 & UNIDOS$K03 %in% c("1","2","3")), c("IdIntegranteHogar","EdadCaracterizacion","K01","K02","K03")]

Error_K03_A=UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS$K03_A %in% 99)|
                   (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$K03_A %in% 99)|
                   (UNIDOS$K03 %in% 1 & !between(UNIDOS$K03_A,0,30000000)), c("IdIntegranteHogar","EdadCaracterizacion","K03","K03_A")]

Error_K03_B=UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS$K03_B %in% 9)|
                   (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$K03_B %in% 9)|
                   (UNIDOS$K03 %in% 1 & !UNIDOS$K03_B %in% c("1","2")), c("IdIntegranteHogar","EdadCaracterizacion","K03","K03_A","K03_B")]


# K03, K04, K05, K06 y K07
for (i in 3:7) {
  x = paste0("K0",i)
  assign(paste0("Error_K0",i), UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS[[x]] %in% 9) |
                                      (!UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                      (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9)|
                                      (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                      (UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9)|
                                      (!UNIDOS$K02 %in% 99 & UNIDOS[[x]] %in% 9)|
                                      (UNIDOS$K02 %in% 99 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","J01","K01", x)])
}

for (i in 3:7) {
  y = paste0("K0",i,"_A")
  x = paste0("K0",i)
  assign(paste0("Error_K0",i,"_A"), UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS[[y]] %in% 99)|
                                             (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS[[y]] %in% 99)|
                                             (UNIDOS[[x]]%in% "1") & !between(UNIDOS[[y]],0,30000000),c("IdIntegranteHogar","EdadCaracterizacion", x, y)])
}

# K08
for (i in 1:4) {
  x = paste0("K08",LETTERS[i])
  assign(paste0("Error_K08",LETTERS[i]), UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS[[x]] %in% 9) |
                                                  (!UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                                  (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                  (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                                  (UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                  (!UNIDOS$K02 %in% 99 & UNIDOS[[x]] %in% 9)|
                                                  (UNIDOS$K02 %in% 99 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","J01","K01","K02", x)])
}




for (i in 1:4) {
  x = paste0("K08",LETTERS[i])
  y = paste0("K08",LETTERS[i],"_A")
  assign(paste0("Error_K08",LETTERS[i],"_A"), UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS[[y]] %in% 99)|
                                                     (!UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                                     (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                     (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                                     (UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                     (UNIDOS$K07 %in% c("1","2","3") & UNIDOS[[x]] %in% "1" & !between(UNIDOS[[y]],0,30000000)), c("IdIntegranteHogar","EdadCaracterizacion","J01","K01","K07", x, y)])
}

for (i in 1:4) {
  x = paste0("K08",LETTERS[i])
  y = paste0("K08",LETTERS[i],"_B")
  assign(paste0("Error_K08",LETTERS[i],"_B"), UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS[[x]] %in% 9) |
                                                     (!UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                                     (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                     (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                                     (UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                     (UNIDOS$K07 %in% c("1","2","3") & UNIDOS[[x]] %in% "1" & !UNIDOS[[y]] %in% c("1","2")), c("IdIntegranteHogar","EdadCaracterizacion","J01","K01","K07", x, y)])
}


# K09
for (i in 1:2) {
  x = paste0("K09",LETTERS[i])
  assign(paste0("Error_K09",LETTERS[i]), UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS[[x]] %in% 9) |
                                                  (!UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                                  (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                  (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                                  (UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","J01","K01","K02", x)])
}

for (i in 1:2) {
  x = paste0("K09",LETTERS[i])
  y = paste0("K09",LETTERS[i],"_A")
  assign(paste0("Error_K09",LETTERS[i],"_A"), UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS[[y]] %in% 99) |
                                                     (!UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                                     (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                     (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                                     (UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                     (UNIDOS$K07 %in% c("1","2","3") & UNIDOS[[x]] %in% "1" & !between(UNIDOS[[y]],0,30000000)), c("IdIntegranteHogar","EdadCaracterizacion","J01","K01","K07", x, y)])
}

for (i in 1:2) {
  x = paste0("K09",LETTERS[i])
  y = paste0("K09",LETTERS[i],"_B")
  assign(paste0("Error_K09",LETTERS[i],"_B"), UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS[[x]] %in% 9) |
                                                     (!UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                                     (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                     (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                                     (UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                     (UNIDOS$K07 %in% c("1","2","3") & UNIDOS[[x]] %in% "1" & !UNIDOS[[y]] %in% c("1","2")), c("IdIntegranteHogar","EdadCaracterizacion","J01","K01","K07", x, y)])
}


# K10
for (i in 1:5) {
  x = paste0("K10",LETTERS[i])
  assign(paste0("Error_K10",LETTERS[i]), UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS[[x]] %in% 9) |
                                                (!UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                                (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                                (UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","J01","K01","K02", x)])
}

for (i in 1:5) {
  x = paste0("K10",LETTERS[i])
  y = paste0("K10",LETTERS[i],"_A")
  assign(paste0("Error_K10",LETTERS[i],"_A"), UNIDOS[(UNIDOS$J01 %in% c("3","5","6","7") & !UNIDOS[[y]] %in% 99) |
                                                     (!UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                                     (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                     (UNIDOS$K01 %in% c("4","5","6","7","8","9") & !UNIDOS$J01 %in% 1 & UNIDOS[[x]] %in% 9) |
                                                     (UNIDOS$K01 %in% c("4","5","6","7","8","9") & UNIDOS$J01 %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                     (UNIDOS$K07 %in% c("1","2","3") & UNIDOS[[x]] %in% "1" & !between(UNIDOS[[y]],0,30000000)), c("IdIntegranteHogar","EdadCaracterizacion","J01","K01","K07", x, y)])
}

#Independientes#
################
Error_K12=UNIDOS[(UNIDOS$K01 %in% c("4","5","6","7") & UNIDOS$A04 %in% 1 & UNIDOS$K12 %in% 99) |
                 (UNIDOS$K01 %in% c("4","5","6","7") & UNIDOS$A04 %in% c("2","3") & !between(UNIDOS$K12,0,30000000))|
                 (UNIDOS$K01 %in% c("4","5","6","7") & !UNIDOS$A04 %in% 1 & !UNIDOS$K12 %in% 99)|
                 (!UNIDOS$K01 %in% c("4","5","6","7") & UNIDOS$A04 %in% 1 & !UNIDOS$K12 %in% 99)|
                 (!UNIDOS$K01 %in% c("4","5","6","7") & !UNIDOS$A04 %in% 1 & !UNIDOS$K12 %in% 99), c("A01","A04","K01","K12")]

Error_K13=UNIDOS[(!(UNIDOS$K12 %in% 99 | is.na(UNIDOS$K12)) & !between(UNIDOS$K13,1,12))|
                 ((UNIDOS$K12 %in% 99 | is.na(UNIDOS$K12)) & between(UNIDOS$K13,1,12)), c("A01","K12","K13")]

Error_K13A=UNIDOS[(UNIDOS$K01 %in% c("4","5","6","7") & UNIDOS$A04 %in% c("2","3") & UNIDOS$K13A %in% 99) |
                  (UNIDOS$K01 %in% c("4","5","6","7") & UNIDOS$A04 %in% c("2","3") & !between(UNIDOS$K13A,0,30000000))|
                  (UNIDOS$K01 %in% c("4","5","6","7") & !UNIDOS$A04 %in% c("2","3") & !UNIDOS$K13A %in% 99)|
                  (!UNIDOS$K01 %in% c("4","5","6","7") & UNIDOS$A04 %in% c("2","3") & !UNIDOS$K13A %in% 99)|
                  (!UNIDOS$K01 %in% c("4","5","6","7") & !UNIDOS$A04 %in% c("2","3") & !UNIDOS$K13A %in% 99), c("A01","A04","K01","K13A")]

Error_K14=UNIDOS[(UNIDOS$EdadCaracterizacion<14 & UNIDOS$K14 %in% c("1","2","3")) |
                 (!UNIDOS$J01 %in% c("0","1","2","3","4","5","6","7") & UNIDOS$K14 %in% c("1","2","3"))|
                 (UNIDOS$J01 %in% 6 & UNIDOS$K14 %in% 3) |
                 (UNIDOS$K01 %in% 2 & UNIDOS$K14 %in% 2), c("IdIntegranteHogar","EdadCaracterizacion","J01","K14")]

#Desempleados#
##############
List=grep("^K15",names(UNIDOS),value = TRUE)[-23]
UNIDOS$K15_Sum=rowSums(UNIDOS[List]=="1")

Error_K15_Sum=UNIDOS[(UNIDOS$K15_Sum>3)|
                     ((UNIDOS$J03_1 %in% 1 & UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K15_Sum<1)), c("IdIntegranteHogar","K15_Sum",List)]

for (i in 1:22) {
  x = paste0("K15_",LETTERS[i])
  assign(paste0("Error_K15_",LETTERS[i]), UNIDOS[(UNIDOS$J03_1 %in% 1 & UNIDOS$EdadCaracterizacion>=18 & UNIDOS[[x]] %in% 9)|
                                                (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9)|
                                                (!UNIDOS$J03_1 %in% 1 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","J03_1","EdadCaracterizacion", x)])
}

Error_K15V_1=UNIDOS[(UNIDOS$K15_V %in% "1" & UNIDOS$K15V_1 %in% 999)|
                    (!UNIDOS$K15_V %in% "1" & !UNIDOS$K15V_1 %in% c(999,"Sin dato"))|
                    (nchar(as.character(UNIDOS$K15V_1))>50),c("IdIntegranteHogar","K15_V","K15V_1")]

Error_K16=UNIDOS[(UNIDOS$J03_1 %in% "1" & UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K16 %in% 9)|
                 (!UNIDOS$J03_1 %in% "1" & UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K16 %in% 9)|
                 (UNIDOS$J03_1 %in% "1" & !UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K16 %in% 9) ,c("IdIntegranteHogar","EdadCaracterizacion","J03_1","K16")]

Error_K17=UNIDOS[(UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K16 %in% 1 & UNIDOS$K17 %in% 999)|
                 (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K16 %in% 1 & !UNIDOS$K17 %in% 999)|
                 (UNIDOS$EdadCaracterizacion<18 & !UNIDOS$K17 %in% 999), c("IdIntegranteHogar","EdadCaracterizacion","J03_1","K16","K17")]

#K18

List=grep("^K18",names(UNIDOS),value = TRUE)
UNIDOS$K18_Sum=rowSums(UNIDOS[List]=="1")

Error_K18_Sum=UNIDOS[(UNIDOS$K18_Sum>3)|
                     ((UNIDOS$J03_1 %in% 1 & UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K18_Sum<1)), c("IdIntegranteHogar","K18_Sum",List)]

for (i in 1:13) {
  x = paste0("K18_",LETTERS[i])
  assign(paste0("Error_K18_",LETTERS[i]), UNIDOS[(UNIDOS$J03_1 %in% 1 & UNIDOS$EdadCaracterizacion>=18 & UNIDOS[[x]] %in% 9)|
                                                 (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9)|
                                                 (!UNIDOS$J03_1 %in% 1 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","J03_1","EdadCaracterizacion", x)])
}

#K19

for (i in 1:5) {
  x = paste0("K19_",LETTERS[i])
  assign(paste0("Error_K19_",LETTERS[i]), UNIDOS[(UNIDOS$J03_1 %in% 1 & UNIDOS$EdadCaracterizacion>=18 & UNIDOS[[x]] %in% 9)|
                                                 (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9)|
                                                 (!UNIDOS$J03_1 %in% 1 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","J03_1","EdadCaracterizacion", x)])
}

Error_K19E_1=UNIDOS[(UNIDOS$K19_E %in% 1 & UNIDOS$K19E_1 %in% 999)|
                    (!UNIDOS$K19_E %in% 1 & !UNIDOS$K19E_1 %in% 999)|
                    (nchar(as.character(UNIDOS$K19E_1))>50), c("IdIntegranteHogar","K19_E","K19E_1")]

#K20

List=grep("^K20",names(UNIDOS),value = TRUE)[-10]
UNIDOS$K20_Sum=rowSums(UNIDOS[List]=="1")

Error_K20_Sum=UNIDOS[UNIDOS$K20_Sum>3|
                     (UNIDOS$J03_1 %in% 1 & UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K20_Sum<1), c("IdIntegranteHogar","K20_Sum",List)]

for (i in 1:9) {
  x = paste0("K20_",LETTERS[i])
  assign(paste0("Error_K20_",LETTERS[i]), UNIDOS[(UNIDOS$J03_1 %in% 1 & UNIDOS$EdadCaracterizacion>=18 & UNIDOS[[x]] %in% 9)|
                                                 (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9)|
                                                 (!UNIDOS$J03_1 %in% 1 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","J03_1","EdadCaracterizacion", x)])
}

Error_K20I_1=UNIDOS[(UNIDOS$K20_I %in% "1" & UNIDOS$K20I_1 %in% 999)|
                    (!UNIDOS$K20_I %in% "1" & !UNIDOS$K20I_1 %in% c(999,"Sin dato"))|
                    (nchar(as.character(UNIDOS$K20I_1))>50), c("IdIntegranteHogar","K20_I","K20I_1")]

#Fortalecimiento productivo#
############################

UNIDOS$K01 %in% c("5","6","7")

Error_K21=UNIDOS[(!UNIDOS$K01 %in% c("5","6","7") & UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K21 %in% c("1","2"))|
                   (UNIDOS$K01 %in% c("5","6","7") & UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K21 %in% c("1","2"))|
                   (!UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K21 %in% c("1","2"))|
                   (!UNIDOS$K01 %in% c("5","6","7") & UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K21 %in% 9)|
                   (UNIDOS$K01 %in% c("5","6","7") & UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K21 %in% 9)|
                   (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K21 %in% 9),c("IdIntegranteHogar","EdadCaracterizacion","K01","K21")]

#K22

List=grep("^K22",names(UNIDOS),value = TRUE)[-23]
UNIDOS$K22_Sum=rowSums(UNIDOS[List]=="1")

Error_K22_Sum=UNIDOS[UNIDOS$K22_Sum>2|
                     (UNIDOS$K01 %in% c("5","6","7") & UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K20_Sum<1), c("IdIntegranteHogar","K01","EdadCaracterizacion","K22_Sum",List)]

for (i in 1:22) {
  x = paste0("K22_",LETTERS[i])
  assign(paste0("Error_K22_",LETTERS[i]), UNIDOS[(UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","K01","K21","EdadCaracterizacion", x)])
}

Error_K22V_1=UNIDOS[(UNIDOS$K22_V %in% 1 & UNIDOS$K22V_1 %in% 999)|
                    (!UNIDOS$K22_V %in% 1 & !UNIDOS$K22V_1 %in% c(999,"Sin dato"))|
                    (nchar(as.character(UNIDOS$K22V_1))>50), c("IdIntegranteHogar","K22_V","K22V_1")]

#K23
Error_K23=UNIDOS[(UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 2 & !UNIDOS$K23 %in% 1:4)|
                 (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 2 & UNIDOS$K23 %in% 1:4)|
                 (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & !UNIDOS$K23 %in% 1:4)|
                 (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & UNIDOS$K23 %in% 1:4)|
                 (!UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K23 %in% 1:4),c("IdIntegranteHogar","EdadCaracterizacion","K01","K21","K23")]

#K24
List=grep("^K24",names(UNIDOS),value = TRUE)
UNIDOS$K24_Sum=rowSums(UNIDOS[List]=="1")

Error_K24_Sum=UNIDOS[UNIDOS$K24_Sum>2|
                     (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & UNIDOS$K24_Sum<1), c("IdIntegranteHogar","K24_Sum",List)]

for (i in 1:12) {
  x = paste0("K24_",LETTERS[i])
  assign(paste0("Error_K24_",LETTERS[i]), UNIDOS[(UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","K01","K21", x)])
}

#K25
List=grep("^K25",names(UNIDOS),value = TRUE)
UNIDOS$K25_Sum=rowSums(UNIDOS[List]=="1")

Error_K25_Sum=UNIDOS[UNIDOS$K25_Sum>2|
                       (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & UNIDOS$K25_Sum<1), c("IdIntegranteHogar","K25_Sum",List)]

for (i in 1:4) {
  x = paste0("K25_",LETTERS[i])
  assign(paste0("Error_K25_",LETTERS[i]), UNIDOS[(UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","K01","K21", x)])
}

#K26
List=grep("^K26",names(UNIDOS),value = TRUE)[-(8:9)]
UNIDOS$K26_Sum=rowSums(UNIDOS[List]=="1")

Error_K26_Sum=UNIDOS[UNIDOS$K26_Sum>4|
                     (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & UNIDOS$K26_Sum<1), c("IdIntegranteHogar","K26_Sum",List)]

for (i in c(1:5,7)) {
  x = paste0("K26_",LETTERS[i])
  assign(paste0("Error_K26_",LETTERS[i]), UNIDOS[(UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                 (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                 (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & UNIDOS[[x]] %in% 9)|
                                                 (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                 (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","K01","K21", x)])
}

#K26_1
for (i in c(1:5,7)) {
  y = paste0("K26",LETTERS[i],"_1")
  x = paste0("K26_",LETTERS[i])
  assign(paste0("Error_K26",LETTERS[i],"_1"), UNIDOS[(UNIDOS[[x]] %in% 1 & UNIDOS[[y]] %in% 999)|
                                                     (!UNIDOS[[x]] %in% 1 & !UNIDOS[[y]] %in% 999)|
                                                     nchar(as.character(UNIDOS[[y]])>50), c("IdIntegranteHogar","EdadCaracterizacion", x, y)])
}

Error_K26G_1=UNIDOS[(UNIDOS$K26_G %in% 1 & UNIDOS$K26G_1 %in% 999)|
                      (!UNIDOS$K26_G %in% 1 & !UNIDOS$K26G_1 %in% 999)|
                      (nchar(as.character(UNIDOS$K26G_1))>50),c("IdIntegranteHogar","K26_G","K26G_2")]

Error_K26G_2=UNIDOS[(UNIDOS$K26_G %in% 1 & UNIDOS$K26G_2 %in% 999)|
                    (!UNIDOS$K26_G %in% 1 & !UNIDOS$K26G_2 %in% 999)|
                    (nchar(as.character(UNIDOS$K26G_2))>50),c("IdIntegranteHogar","K26_G","K26G_2")]

#K27
List=grep("^K27",names(UNIDOS),value = TRUE)[-6]
UNIDOS$K27_Sum=rowSums(UNIDOS[List]=="1")

Error_K27_Sum=UNIDOS[UNIDOS$K27_Sum>3|
                     (UNIDOS$K26_A %in% 1 & UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & UNIDOS$K27_Sum<1), c("IdIntegranteHogar","K27_Sum",List)]

Error_K27=UNIDOS[(!UNIDOS$K26_A %in% 1 & (!UNIDOS$K27_A %in% 9 |
                                            !UNIDOS$K27_B %in% 9 |
                                            !UNIDOS$K27_C %in% 9 |
                                            !UNIDOS$K27_D %in% 9 |
                                            !UNIDOS$K27_E %in% 9))|
                   (UNIDOS$K26_A %in% 1 & (UNIDOS$K27_A %in% 9 &
                                             UNIDOS$K27_B %in% 9 &
                                             UNIDOS$K27_C %in% 9 &
                                             UNIDOS$K27_D %in% 9 &
                                             UNIDOS$K27_E %in% 9)), c("IdIntegranteHogar","EdadCaracterizacion","K01","K21","K26_A",grep("K27",names(UNIDOS), value = T)[-(7:8)])]

for (i in 1:5) {
  x = paste0("K27_",LETTERS[i])
  assign(paste0("Error_K27_",LETTERS[i]), UNIDOS[(UNIDOS$K21 %in% c("2","9") & !UNIDOS[[x]] %in% 9)|
                                                 (!UNIDOS$K01 %in% c("5","6","7") & !UNIDOS[[x]] %in% 9)|
                                                 (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","K01","K21",grep("^K26",names(UNIDOS), value = T)[-8], x)])
}

Error_K27E_1=UNIDOS[(UNIDOS$K27_E %in% 1 & UNIDOS$K27E_1 %in% 999)|
                    (!UNIDOS$K27_E %in% 1 & !UNIDOS$K27E_1 %in% 999)|
                      (nchar(as.character(UNIDOS$K27E_1))>50),c("IdIntegranteHogar","K27_E","K27E_1")]

#K28
List=grep("^K28",names(UNIDOS),value = TRUE)[-9]
UNIDOS$K28_Sum=rowSums(UNIDOS[List]=="1")

Error_K28_Sum=UNIDOS[UNIDOS$K28_Sum>3|
                     (UNIDOS$K26_B %in% 1 & UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & UNIDOS$K28_Sum<1), c("IdIntegranteHogar","K28_Sum",List)]

Error_K28=UNIDOS[(!UNIDOS$K26_B %in% 1 & (!UNIDOS$K28_A %in% 9 |
                                          !UNIDOS$K28_B %in% 9 |
                                          !UNIDOS$K28_C %in% 9 |
                                          !UNIDOS$K28_D %in% 9 |
                                          !UNIDOS$K28_E %in% 9 |
                                          !UNIDOS$K28_F %in% 9 |
                                          !UNIDOS$K28_G %in% 9 |
                                          !UNIDOS$K28_H %in% 9))|
                   (UNIDOS$K26_B %in% 1 & (UNIDOS$K28_A %in% 9 &
                                           UNIDOS$K28_B %in% 9 &
                                           UNIDOS$K28_C %in% 9 &
                                           UNIDOS$K28_D %in% 9 &
                                           UNIDOS$K28_E %in% 9 &
                                           UNIDOS$K28_F %in% 9 &
                                           UNIDOS$K28_G %in% 9 &
                                           UNIDOS$K28_H %in% 9)), c("IdIntegranteHogar","EdadCaracterizacion","K01","K21","K26_B",grep("K28",names(UNIDOS), value = T)[-(9:10)])]

for (i in 1:8) {
  x = paste0("K28_",LETTERS[i])
  assign(paste0("Error_K28_",LETTERS[i]), UNIDOS[(UNIDOS$K21 %in% c("2","9") & !UNIDOS[[x]] %in% 9)|
                                                 (!UNIDOS$K01 %in% c("5","6","7") & !UNIDOS[[x]] %in% 9)|
                                                 (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","K01","K21", x)])
}

Error_K28H_1=UNIDOS[(UNIDOS$K28_H %in% 1 & UNIDOS$K28H_1 %in% 999)|
                   (!UNIDOS$K28_H %in% 1 & !UNIDOS$K28H_1 %in% 999)|
                     (nchar(as.character(UNIDOS$K28H_1))>50),c("IdIntegranteHogar","K28_H","K28H_1")]

#K29
List=grep("^K29",names(UNIDOS),value = TRUE)[-9]
UNIDOS$K29_Sum=rowSums(UNIDOS[List]=="1")

Error_K29_Sum=UNIDOS[UNIDOS$K29_Sum>3|
                     (UNIDOS$K26_F %in% 1 & UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & UNIDOS$K29_Sum<1), c("IdIntegranteHogar","K29_Sum",List)]

Error_K29=UNIDOS[(!UNIDOS$K26_F %in% 1 & (!UNIDOS$K29_A %in% 9 |
                                            !UNIDOS$K29_B %in% 9 |
                                            !UNIDOS$K29_C %in% 9 |
                                            !UNIDOS$K29_D %in% 9 |
                                            !UNIDOS$K29_E %in% 9 |
                                            !UNIDOS$K29_F %in% 9 |
                                            !UNIDOS$K29_G %in% 9 |
                                            !UNIDOS$K29_H %in% 9))|
                   (UNIDOS$K26_F %in% 1 & (UNIDOS$K29_A %in% 9 &
                                             UNIDOS$K29_B %in% 9 &
                                             UNIDOS$K29_C %in% 9 &
                                             UNIDOS$K29_D %in% 9 &
                                             UNIDOS$K29_E %in% 9 &
                                             UNIDOS$K29_F %in% 9 &
                                             UNIDOS$K29_G %in% 9 &
                                             UNIDOS$K29_H %in% 9)), c("IdIntegranteHogar","EdadCaracterizacion","K01","K21","K26_F",grep("K29",names(UNIDOS), value = T)[-(9:10)])]


for (i in 1:8) {
  x = paste0("K29_",LETTERS[i])
  assign(paste0("Error_K29_",LETTERS[i]), UNIDOS[(UNIDOS$K21 %in% c("2","9") & !UNIDOS[[x]] %in% 9)|
                                                (!UNIDOS$K01 %in% c("5","6","7") & !UNIDOS[[x]] %in% 9)|
                                                (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","K01","K21", x)])
}

Error_K29G_1=UNIDOS[(UNIDOS$K29_G %in% 1 & UNIDOS$K29G_1 %in% 999)|
                    (!UNIDOS$K29_G %in% 1 & !UNIDOS$K29G_1 %in% 999),c("IdIntegranteHogar","K29_G","K29G_1")]

#K30
Error_K30=UNIDOS[((!UNIDOS$K26_C %in% 1 & !UNIDOS$K26_D %in% 1 & !UNIDOS$K26_G %in% 1) & !UNIDOS$K30 %in% 9)|
                 ((UNIDOS$K26_C %in% 1 | UNIDOS$K26_D %in% 1 | UNIDOS$K26_G %in% 1) & UNIDOS$K30 %in% 9), c("IdIntegranteHogar","K26_C","K26_D","K26_G","K30")]

#K31
List=grep("^K31",names(UNIDOS),value = TRUE)[-7]
UNIDOS$K31_Sum=rowSums(UNIDOS[List]=="1")

Error_K31_Sum=UNIDOS[UNIDOS$K31_Sum>3|
                     (UNIDOS$K30 %in% 1 & UNIDOS$EdadCaracterizacion>=18 & UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 2 & UNIDOS$K31_Sum<1), c("IdIntegranteHogar","K31_Sum",List)]

for (i in 1:6) {
  x = paste0("K31_",LETTERS[i])
  assign(paste0("Error_K31_",LETTERS[i]), UNIDOS[(UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 1 & UNIDOS$K30 %in% 1 & UNIDOS[[x]] %in% 9)|
                                                 (!UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 1 & UNIDOS$K30 %in% 1 & !UNIDOS[[x]] %in%  c("9","Sin dato"))|
                                                 (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in%  c("9","Sin dato"))|
                                                 (UNIDOS$K21 %in% 2 & UNIDOS$K30 %in% 1 & !UNIDOS[[x]] %in%  c("9","Sin dato")) |
                                                 (UNIDOS$K21 %in% 1 & UNIDOS$K30 %in% 1 & UNIDOS[[x]] %in% 9)|
                                                 (!UNIDOS$K30 %in% 1 & !UNIDOS[[x]] %in%  9), c("IdIntegranteHogar","EdadCaracterizacion","K01","K21","K30", x)])
}

Error_K31F_1=UNIDOS[(UNIDOS$K31_F %in% 1 & UNIDOS$K31F_1 %in% 999)|
              (!UNIDOS$K31_F %in% 1 & !UNIDOS$K31F_1 %in% 999)|
                (nchar(as.character(UNIDOS$K31F_1))>50),c("IdIntegranteHogar","K31_F","K31F_1")]

#K32
Error_K32=UNIDOS[(UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 1 & UNIDOS$K30 %in% 1 & UNIDOS$K32 %in% 9)|
                 (UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 1 & !UNIDOS$K30 %in% 1 & !UNIDOS$K32 %in% 9)|
                 (UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 1 & UNIDOS$K30 %in% 1 & !UNIDOS$K32 %in% 9)|
                 (!UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 1 & UNIDOS$K30 %in% 1 & !UNIDOS$K32 %in% 9),c("IdIntegranteHogar","EdadCaracterizacion","K01","K30","K32")]

#K33
Error_K33=UNIDOS[(UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 1 & UNIDOS$K33 %in% 9)|
                 (UNIDOS$K01 %in% c("5","6","7") & !UNIDOS$K21 %in% 1 & !UNIDOS$K33 %in% 9)|
                 (!UNIDOS$K01 %in% c("5","6","7") & UNIDOS$K21 %in% 1 & !UNIDOS$K33 %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","K01","K21","K33")]

#Emprendimientos#
#################

Error_K34=UNIDOS[(UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & UNIDOS$K34 %in% 9)|
                 (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & !UNIDOS$K21 %in% 2 & !UNIDOS$K34 %in% 9)|
                 (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & !UNIDOS$K34 %in% 9)|
                 (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K34 %in% 9),c("IdIntegranteHogar","EdadCaracterizacion","J03_1","K21","K34")]

#K35

List=grep("^K35",names(UNIDOS),value = TRUE)[-24]
UNIDOS$K35_Sum=rowSums(UNIDOS[List]=="1")

Error_K35_Sum=UNIDOS[UNIDOS$K35_Sum>2|
                     (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & UNIDOS$K35_Sum<1), c("IdIntegranteHogar","EdadCaracterizacion","J03_1","K21","K35_Sum",List)]

Error_K35=UNIDOS[ (UNIDOS$K35_W %in% 1 & (UNIDOS$K35_A %in% 1 |
                                            UNIDOS$K35_B %in% 1 |
                                            UNIDOS$K35_C %in% 1 |
                                            UNIDOS$K35_D %in% 1 |
                                            UNIDOS$K35_E %in% 1 |
                                            UNIDOS$K35_F %in% 1 |
                                            UNIDOS$K35_G %in% 1 |
                                            UNIDOS$K35_H %in% 1 |
                                            UNIDOS$K35_I %in% 1 |
                                            UNIDOS$K35_J %in% 1 |
                                            UNIDOS$K35_K %in% 1 |
                                            UNIDOS$K35_L %in% 1 |
                                            UNIDOS$K35_M %in% 1 |
                                            UNIDOS$K35_N %in% 1 |
                                            UNIDOS$K35_O %in% 1 |
                                            UNIDOS$K35_P %in% 1 |
                                            UNIDOS$K35_Q %in% 1 |
                                            UNIDOS$K35_R %in% 1 |
                                            UNIDOS$K35_S %in% 1 |
                                            UNIDOS$K35_T %in% 1 |
                                            UNIDOS$K35_U %in% 1 |
                                            UNIDOS$K35_V %in% 1)), c("IdIntegranteHogar","EdadCaracterizacion",grep("K35",names(UNIDOS), value = T)[-(24:25)])]
for (i in 1:23) {
  x = paste0("K35_",LETTERS[i])
  assign(paste0("Error_K35_",LETTERS[i]), UNIDOS[(UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & UNIDOS[[x]] %in% 9)|
                                                 (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & !UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                 (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                 (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","J03_1","K21", x)])
}

Error_K35V_1=UNIDOS[(UNIDOS$K35_V %in% 1 & UNIDOS$K35V_1 %in% 999)|
                    (!UNIDOS$K35_V %in% 1 & !UNIDOS$K35V_1 %in% 999)|
                    (nchar(as.character(UNIDOS$K35V_1))>50), c("IdIntegranteHogar","K35_V","K35V_1")]


#K36

List=grep("^K36",names(UNIDOS),value = TRUE)[-13]
UNIDOS$K36_Sum=rowSums(UNIDOS[List]=="1")

Error_K36_Sum=UNIDOS[UNIDOS$K36_Sum>3|
                       (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & UNIDOS$K36_Sum<1), c("IdIntegranteHogar","EdadCaracterizacion","J03_1","K21","K36_Sum",List)]

Error_K36=UNIDOS[ (UNIDOS$K36_I %in% 1 & (UNIDOS$K36_A %in% 1 |
                                            UNIDOS$K36_B %in% 1 |
                                            UNIDOS$K36_C %in% 1 |
                                            UNIDOS$K36_D %in% 1 |
                                            UNIDOS$K36_E %in% 1 |
                                            UNIDOS$K36_F %in% 1 |
                                            UNIDOS$K36_G %in% 1 |
                                            UNIDOS$K36_H %in% 1 |
                                            UNIDOS$K36_I %in% 1 |
                                            UNIDOS$K36_J %in% 1 |
                                            UNIDOS$K36_K %in% 1 |
                                            UNIDOS$K36_L %in% 1)), c("IdIntegranteHogar","EdadCaracterizacion",grep("K36",names(UNIDOS), value = T)[-(13:14)])]


for (i in 1:12) {
  x = paste0("K36_",LETTERS[i])
  assign(paste0("Error_K36_",LETTERS[i]), UNIDOS[(UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & UNIDOS[[x]] %in% 9)|
                                                 (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & !UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                 (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                 (UNIDOS$K35_W %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                 (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","J03_1","K21", x)])
}

Error_K36K_1=UNIDOS[(UNIDOS$K36_K %in% 1 & UNIDOS$K36K_1 %in% 999)|
                      (!UNIDOS$K36_K %in% 1 & !UNIDOS$K36K_1 %in% 999)|
                      (nchar(as.character(UNIDOS$K36K_1))>50), c("IdIntegranteHogar","K36_K","K36K_1")]

#K37

List=grep("^K37",names(UNIDOS),value = TRUE)[-9]
UNIDOS$K37_Sum=rowSums(UNIDOS[List]=="1")

Error_K37_Sum=UNIDOS[UNIDOS$K37_Sum>3|
                       (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & !UNIDOS$K35_W %in% 1 & UNIDOS$K37_Sum<1), c("IdIntegranteHogar","EdadCaracterizacion","J03_1","K21","K37_Sum",List)]


Error_K37=UNIDOS[ (UNIDOS$K37_H %in% 1 & (UNIDOS$K37_A %in% 1 |
                                            UNIDOS$K37_B %in% 1 |
                                            UNIDOS$K37_C %in% 1 |
                                            UNIDOS$K37_D %in% 1 |
                                            UNIDOS$K37_E %in% 1 |
                                            UNIDOS$K37_F %in% 1 |
                                            UNIDOS$K37_G %in% 1)), c("IdIntegranteHogar","EdadCaracterizacion",grep("K37",names(UNIDOS), value = T)[-(9:10)])]

for (i in 1:8) {
  x = paste0("K37_",LETTERS[i])
  assign(paste0("Error_K37_",LETTERS[i]), UNIDOS[(UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & !UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$K35_W %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                   (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","J03_1","K21","K35_W", x)])
}

Error_K37G_1=UNIDOS[(UNIDOS$K37_G %in% 1 & UNIDOS$K37G_1 %in% 999)|
                      (!UNIDOS$K37_G %in% 1 & !UNIDOS$K37G_1 %in% 999)|
                      (nchar(as.character(UNIDOS$K37G_1))>50), c("IdIntegranteHogar","K37_G","K37G_1")]

#K38
List=grep("^K38",names(UNIDOS),value = TRUE)[-8]
UNIDOS$K38_Sum=rowSums(UNIDOS[List]=="1")

Error_K38_Sum=UNIDOS[UNIDOS$K38_Sum>4|
                    (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & !UNIDOS$K35_W %in% 1 & UNIDOS$K38_Sum<1), c("IdIntegranteHogar","K38_Sum",List)]

for (i in 1:7) {
  x = paste0("K38_",LETTERS[i])
  assign(paste0("Error_K38_",LETTERS[i]), UNIDOS[(UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & !UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$K35_W %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                   (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","J03_1","K21","K35_W", x)])
}

for (i in 1:6) {
  x = paste0("K38_",LETTERS[i])
  y = paste0("K38",LETTERS[i],"_1")
  assign(paste0("Error_K38",LETTERS[i],"_1"), UNIDOS[(UNIDOS[[x]] %in% 1 & UNIDOS[[y]] %in% 999)|
                                                 (!UNIDOS[[x]] %in% 1 & !UNIDOS[[y]] %in% 999)|
                                                 (nchar(as.character(UNIDOS[[y]]))>50), c("IdIntegranteHogar", x, y)])
}

Error_K38F_2=UNIDOS[(UNIDOS$K38_F %in% 1 & UNIDOS$K38F_2 %in% 999)|
                    (!UNIDOS$K38_F %in% 1 & !UNIDOS$K38F_2 %in% 999)|
                    (nchar(as.character(UNIDOS$K38F_2))>50),c("IdIntegranteHogar","K38_F","K38F_2")]

#K39
List=grep("^K39",names(UNIDOS),value = TRUE)[-6]
UNIDOS$K39_Sum=rowSums(UNIDOS[List]=="1")

Error_K39_Sum=UNIDOS[UNIDOS$K39_Sum>3|
                       (UNIDOS$K38_A %in% 1 & UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & !UNIDOS$K35_W %in% 1 & UNIDOS$K39_Sum<1), c("IdIntegranteHogar","K39_Sum",List)]

for (i in 1:5) {
  x = paste0("K39_",LETTERS[i])
  assign(paste0("Error_K39_",LETTERS[i]), UNIDOS[(UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & !UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$K35_W %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$K38_A %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                   (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","K01","K21","K35_W","K38_A", x)])
}

Error_K39E_1=UNIDOS[(UNIDOS$K39_E %in% 1 & UNIDOS$K39E_1 %in% 999)|
                      (!UNIDOS$K39_E %in% 1 & !UNIDOS$K39E_1 %in% 999)|
                      (nchar(as.character(UNIDOS$K39E_1))>50),c("IdIntegranteHogar","K39_E","K39E_1")]

#K40
List=grep("^K40",names(UNIDOS),value = TRUE)[-9]
UNIDOS$K40_Sum=rowSums(UNIDOS[List]=="1")

Error_K40_Sum=UNIDOS[UNIDOS$K40_Sum>3|
                       (UNIDOS$K38_B %in% 1 & UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & !UNIDOS$K35_W %in% 1 & UNIDOS$K40_Sum<1), c("IdIntegranteHogar","K40_Sum",List)]

for (i in 1:8) {
  x = paste0("K40_",LETTERS[i])
  assign(paste0("Error_K40_",LETTERS[i]), UNIDOS[(UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & !UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & !UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$K35_W %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                   (UNIDOS$K38_B %in% 1 & !UNIDOS[[x]] %in% 9)|
                                                   (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","K01","K35_W","K38_B","K21", x)])
}

Error_K40H_1=UNIDOS[(UNIDOS$K40_H %in% 1 & UNIDOS$K40H_1 %in% 999)|
                      (!UNIDOS$K40_H %in% 1 & !UNIDOS$K40H_1 %in% 999)|
                      (nchar(as.character(UNIDOS$K40H_1))>50),c("IdIntegranteHogar","K40_H","K40H_1")]

#K41

Error_K41=UNIDOS[((!UNIDOS$K38_C %in% 1 & !UNIDOS$K38_D %in% 1 & !UNIDOS$K38_E %in% 1 & !UNIDOS$K38_F %in% 1) & !UNIDOS$K41 %in% 9)|
                   ((UNIDOS$K38_C %in% 1 | UNIDOS$K38_D %in% 1 | UNIDOS$K38_E %in% 1 | UNIDOS$K38_F %in% 1) & UNIDOS$K41 %in% 9)|
                   (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & UNIDOS$K41 %in% 9)|
                   (UNIDOS$EdadCaracterizacion>=18 & UNIDOS$J03_1 %in% 2 & !UNIDOS$K21 %in% 2 & !UNIDOS$K41 %in% 9)|
                   (UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$J03_1 %in% 2 & UNIDOS$K21 %in% 2 & !UNIDOS$K41 %in% 9)|
                   (UNIDOS$K35_W %in% 1 & !UNIDOS$K41 %in% 9)|
                   (!UNIDOS$EdadCaracterizacion>=18 & !UNIDOS$K41 %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","J03_1","K21","K38_C","K38_D","K38_G","K35_W","K41")]
############
#Capitulo L#
############

Error_L01=UNIDOS[(UNIDOS$K14 %in% 3 & UNIDOS$L01 %in% 2)|
                 (!UNIDOS$EdadCaracterizacion>=8 & !UNIDOS$L01 %in% 9),c("IdIntegranteHogar","EdadCaracterizacion","K14","L01")]

#L02
for (i in 1:3) {
  x = paste0("L02",LETTERS[i])
  assign(paste0("Error_L02",LETTERS[i]), UNIDOS[(UNIDOS$L01 %in% 1 & UNIDOS[[x]] %in% 9)|
                                                (!UNIDOS$L01 %in% 1 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","L01", x)])
}

for (i in 1:3) {
  x = paste0("L02",LETTERS[i])
  y = paste0("L02",LETTERS[i],"_A")
  assign(paste0("Error_L02",LETTERS[i],"_A"), UNIDOS[(UNIDOS[[x]] %in% 1 & UNIDOS[[y]] %in% 99)|
                                                    (!UNIDOS[[x]] %in% 1 & !UNIDOS[[y]] %in% 99)|
                                                    (UNIDOS[[y]]>30000000), c("IdIntegranteHogar", x, y)])
}

#L03
Error_L03=UNIDOS[ (UNIDOS$L01 %in% c("1","2") & UNIDOS$L03 %in% 9)|
                  (!UNIDOS$L01 %in% c("1","2") & !UNIDOS$L03 %in% 9)|
                 (!UNIDOS$EdadCaracterizacion>=8 & !UNIDOS$L03 %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","L01","L03")]

#L04
for (i in 1:6) {
  x = paste0("L04",LETTERS[i])
  assign(paste0("Error_L04",LETTERS[i]), UNIDOS[(!UNIDOS$EdadCaracterizacion>=8 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion", x)])
}

for (i in 1:6) {
  x = paste0("L04",LETTERS[i])
  y = paste0("L04",LETTERS[i],"_A")
  assign(paste0("Error_L04",LETTERS[i],"_A"), UNIDOS[(UNIDOS[[x]] %in% 1 & UNIDOS[[y]] %in% 99)|
                                                       (!UNIDOS[[x]] %in% 1 & !UNIDOS[[y]] %in% 99)|
                                                       (UNIDOS[[y]]>30000000), c("IdIntegranteHogar", x, y)])
}

#L05

#Error_L05=UNIDOS[(UNIDOS$EdadCaracterizacion>=8 & UNIDOS$K01 %in% c("1","2","3","10") & UNIDOS$L05 %in% 9)|
#                   (UNIDOS$EdadCaracterizacion>=8 & !UNIDOS$K01 %in% c("1","2","3","10") & !UNIDOS$L05 %in% 9)|
#                   (!UNIDOS$EdadCaracterizacion>=8 & !UNIDOS$L05 %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","K01", "L05")]

#Error_L05_A=UNIDOS[(UNIDOS$L05 %in% 1 & UNIDOS$L05_A %in% 99)|
#                     (!UNIDOS$L05 %in% 1 & !UNIDOS$L05_A %in% 99)|
#                     (UNIDOS$L05_A>30000000), c("IdIntegranteHogar", "L05", "L05_A")]

#L06

#Error_L06=UNIDOS[(UNIDOS$EdadCaracterizacion>=8 & UNIDOS$K01 %in% c("4","5","6","7") & UNIDOS$L06 %in% 9)|
#                   (UNIDOS$EdadCaracterizacion>=8 & !UNIDOS$K01 %in% c("4","5","6","7") & !UNIDOS$L06 %in% 9)|
#                   (!UNIDOS$EdadCaracterizacion>=8 & !UNIDOS$L06 %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","K01", "L06")]

#Error_L06_A=UNIDOS[(UNIDOS$L06 %in% 1 & UNIDOS$L06_A %in% 99)|
#                     (!UNIDOS$L06 %in% 1 & !UNIDOS$L06_A %in% 99)|
#                     (UNIDOS$L06_A>30000000), c("IdIntegranteHogar", "L06", "L06_A")]

#L07

#Error_L07=UNIDOS[(UNIDOS$EdadCaracterizacion>=8 & UNIDOS$A04 %in% c("2","3") & UNIDOS$L06 %in% 1 & UNIDOS$L07 %in% 9)|
#                   (UNIDOS$EdadCaracterizacion>=8 & !UNIDOS$A04 %in% c("2","3")  & UNIDOS$L06 %in% 1 & !UNIDOS$L07 %in% 9)|
#                   (UNIDOS$EdadCaracterizacion>=8 & UNIDOS$A04 %in% c("2","3")  & !UNIDOS$L06 %in% 1 & !UNIDOS$L07 %in% 9)|
#                   (!UNIDOS$EdadCaracterizacion>=8 & !UNIDOS$L07 %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","A04","L06", "L07")]

#Error_L07_A=UNIDOS[(UNIDOS$L07 %in% 1 & UNIDOS$L07_A %in% 99)|
#                     (!UNIDOS$L07 %in% 1 & !UNIDOS$L07_A %in% 99), c("IdIntegranteHogar", "L07", "L07_A")]

#Error_L07_B=UNIDOS[(UNIDOS$L07 %in% 1 & UNIDOS$L07_B %in% 99)|
#                     (!UNIDOS$L07 %in% 1 & !UNIDOS$L07_B %in% 99)|
#                     (UNIDOS$L07_B>30000000), c("IdIntegranteHogar", "L07", "L07_B")]

#L08

#Error_L08A=UNIDOS[(UNIDOS$EdadCaracterizacion>=8 & UNIDOS$K14 %in% 3 & UNIDOS$L08A %in% 9)|
#                    (UNIDOS$EdadCaracterizacion>=8 & !UNIDOS$K14 %in% 3 & !UNIDOS$L08A %in% 9)|
#                    (!UNIDOS$EdadCaracterizacion>=8 & !UNIDOS$L08A %in% 9),c("IdIntegranteHogar","EdadCaracterizacion","K14","L08A")]

# L08B hasta L08E
#for (i in 2:5) {
#  x = paste0("L08",LETTERS[i])
#  assign(paste0("Error_L08",LETTERS[i]), UNIDOS[(UNIDOS$EdadCaracterizacion>=8 & UNIDOS[[x]] %in% 9)|
#                                                  (!UNIDOS$EdadCaracterizacion>=8 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion", x)])
#}

#Post preguntas L08A_A hasta L08E_A
#for (i in 1:5) {
#  x = paste0("L08",LETTERS[i])
#  y = paste0("L08",LETTERS[i],"_A")
#  assign(paste0("Error_L08",LETTERS[i],"_A"), UNIDOS[(UNIDOS[[x]] %in% 1 & UNIDOS[[y]] %in% c("99","0"))|
#                                                       (!UNIDOS[[x]] %in% 1 & !UNIDOS[[y]] %in% c("99","0"))|
#                                                       (UNIDOS[[y]]>30000000), c("IdIntegranteHogar", x, y)])
#}

#L08F
#Error_L08F=UNIDOS[(UNIDOS$EdadCaracterizacion>=8 & UNIDOS$L08F %in% 9)|
#                    (!UNIDOS$EdadCaracterizacion>=8 & !UNIDOS$L08F %in% 9),c("IdIntegranteHogar","EdadCaracterizacion","L08F")]

#L08F1 hasta L08F3
#for (i in 1:3) {
#  x = paste0("L08F",i)
#  assign(paste0("Error_L08F",i), UNIDOS[(UNIDOS$EdadCaracterizacion>=8 & UNIDOS$L08F %in% 1 & UNIDOS[[x]] %in% 9)|
#                                          (UNIDOS$EdadCaracterizacion>=8 & !UNIDOS$L08F %in% 1 & !UNIDOS[[x]] %in% 9)|
#                                          (!UNIDOS$EdadCaracterizacion>=8 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","L08F", x)])
#}

#for (i in 1:3) {
#  x = paste0("L08F",i)
#  y = paste0("L08F",i,"_A")
#  assign(paste0("Error_L08F",i,"_A"), UNIDOS[(UNIDOS[[x]] %in% 1 & UNIDOS[[y]] %in% c("99","0"))|
#                                               (!UNIDOS[[x]] %in% 1 & !UNIDOS[[y]] %in% c("99","0"))|
#                                               (UNIDOS[[y]]>30000000), c("IdIntegranteHogar", x, y)])
#}


#L09

#for (i in 1:8) {
#  x = paste0("L09",LETTERS[i])
#  assign(paste0("Error_L09",LETTERS[i]), UNIDOS[(UNIDOS$E11 %in% 1 & UNIDOS[[x]] %in% 9)|
#                                                  (!UNIDOS$E11 %in% 1 & !UNIDOS[[x]] %in% 9), c("IdIntegranteHogar","EdadCaracterizacion","E11", x)])
#}

#for (i in 1:3) {
#  x = paste0("L09",LETTERS[i])
#  y = paste0("L09",LETTERS[i],"_A")
#  assign(paste0("Error_L09",LETTERS[i],"_A"), UNIDOS[(UNIDOS[[x]] %in% 1 & UNIDOS[[y]] %in% c("99","0"))|
#                                                       (!UNIDOS[[x]] %in% 1 & !UNIDOS[[y]] %in% c("99","0"))|
#                                                       (UNIDOS[[y]]>30000000), c("IdIntegranteHogar", x, y)])
#}
