# Generación de campos

UNIDOS$niv_educativo=recode_factor(UNIDOS$I02,
                                   `Ninguno` = "0",
                                   `Preescolar` = "1",
                                   `Básica primaria (1° - 5°)` = "2",
                                   `Básica secundaria (6° - 9°)` = "3",
                                   `Media (10° - 13°)` = "4",
                                   `Técnico o tecnológico (1 - 4)` = "5",
                                   `Universitario` = "6",
                                   `Posgrado` = "7")

UNIDOS$I02_A=as.numeric(as.character(UNIDOS$I02_A))

UNIDOS$anio_educ=ifelse(UNIDOS$niv_educativo %in% c(0,1),0,
                        ifelse((UNIDOS$niv_educativo %in% 2) & (UNIDOS$I02_A>=1 & UNIDOS$I02_A <=5),UNIDOS$I02_A,
                               ifelse((UNIDOS$niv_educativo %in% 2) & (UNIDOS$I02_A>5),5,
                                      ifelse((UNIDOS$niv_educativo %in% 3) & (UNIDOS$I02_A>0),UNIDOS$I02_A,
                                             ifelse((UNIDOS$niv_educativo %in% 4) & (UNIDOS$I02_A>0 & UNIDOS$I02_A<=11),UNIDOS$I02_A,
                                                    ifelse((UNIDOS$niv_educativo %in% 4) & (UNIDOS$I02_A>0 & UNIDOS$I02_A>11),11,
                                                           ifelse((UNIDOS$niv_educativo %in% 5) & (UNIDOS$I02_A>0),UNIDOS$I02_A+11,
                                                                  ifelse((UNIDOS$niv_educativo %in% 6) & (UNIDOS$I02_A>0),UNIDOS$I02_A+16,
                                                                         ifelse((UNIDOS$niv_educativo %in% 7) & (UNIDOS$I02_A>0),UNIDOS$I02_A+16,
                                                                                99)))))))))
