############### Tableros de control_Movimientos PE ###########
############### Procesamiento de datos ###############
### INE-DERFE-COC-DE-EVALUACION DEMOGRAFICA ###
### Autor: Miguel David Alvarez Hernández
### Ultima versión : 16/06/2020


#----------------------------------------------------------------------------------#
################################### Paquetes y setup ###############################
#----------------------------------------------------------------------------------#

library(pacman)
p_load(tidyverse,
       lubridate,
       plotly,
       ggthemes,
       RColorBrewer)

#Prevenir notación científica
options(scipen=999) 
#Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
print(getwd())



#----------------------------------------------------------------------------------#
####################################### Datos ######################################
#----------------------------------------------------------------------------------#

#Inscripciones
Datos_inscripciones <- read_csv("Datos_inscripciones_1994-2020.csv", 
                                          col_types = cols(fecha = col_date(format = "%d/%m/%Y")))
View(Datos_inscripciones)

#Cambios de domicilio
Datos_cambiodom <- read_csv("Datos_cambiodom_1994-2020.csv", 
                                      col_types = cols(fecha = col_date(format = "%d/%m/%Y")))
View(Datos_cambiodom)

#Reposiciones
Datos_reposiciones <- read_csv("Datos_reposiciones_1994-2020.csv", 
                                         col_types = cols(fecha = col_date(format = "%d/%m/%Y")))
View(Datos_reposiciones)

#Error en datos
Datos_errordatos <- read_csv("Datos_errordatos_1994-2020.csv", 
                                       col_types = cols(fecha = col_date(format = "%d/%m/%Y")))
View(Datos_errordatos)

#Corrección dirección
Datos_correcdireccion <- read_csv("Datos_correcdireccion_1994-2020.csv", 
                                            col_types = cols(fecha = col_date(format = "%d/%m/%Y")))
View(Datos_correcdireccion)

#Reincorporaciones
Datos_reincorporacion <- read_csv("Datos_reincorporacion_1994-2020.csv", 
                                            col_types = cols(AGUASCALIENTES = col_integer(), 
                                                             `BAJA CALIFORNIA` = col_integer(), 
                                                             `BAJA CALIFORNIA SUR` = col_integer(), 
                                                             CAMPECHE = col_integer(), CHIAPAS = col_integer(), 
                                                             CHIHUAHUA = col_integer(), `CIUDAD DE MEXICO` = col_integer(), 
                                                             COAHUILA = col_integer(), COLIMA = col_integer(), 
                                                             DURANGO = col_integer(), GUANAJUATO = col_integer(), 
                                                             GUERRERO = col_integer(), HIDALGO = col_integer(), 
                                                             JALISCO = col_integer(), `MICHOACÁN` = col_integer(), 
                                                             MORELOS = col_integer(), `MÉXICO` = col_integer(), 
                                                             NAYARIT = col_integer(), `NUEVO LEÓN` = col_integer(), 
                                                             OAXACA = col_integer(), PUEBLA = col_integer(), 
                                                             `QUERÉTARO` = col_integer(), `QUINTANA ROO` = col_integer(), 
                                                             `SAN LUIS POTOSÍ` = col_integer(), 
                                                             SINALOA = col_integer(), SONORA = col_integer(), 
                                                             TABASCO = col_integer(), TAMAULIPAS = col_integer(), 
                                                             TLAXCALA = col_integer(), VERACRUZ = col_integer(), 
                                                             `YUCATÁN` = col_integer(), ZACATECAS = col_integer(), 
                                                             fecha = col_date(format = "%d/%m/%Y")))
View(Datos_reincorporacion)

#Reemplazos
Datos_reemplazos <- read_csv("Datos_reemplazos_1994-2020.csv", 
                                       col_types = cols(AGUASCALIENTES = col_integer(), 
                                                        `BAJA CALIFORNIA` = col_integer(), 
                                                        `BAJA CALIFORNIA SUR` = col_integer(), 
                                                        CAMPECHE = col_integer(), CHIAPAS = col_integer(), 
                                                        CHIHUAHUA = col_integer(), `CIUDAD DE MEXICO` = col_integer(), 
                                                        COAHUILA = col_integer(), COLIMA = col_integer(), 
                                                        DURANGO = col_integer(), GUANAJUATO = col_integer(), 
                                                        GUERRERO = col_integer(), HIDALGO = col_integer(), 
                                                        JALISCO = col_integer(), `MICHOACÁN` = col_integer(), 
                                                        MORELOS = col_integer(), `MÉXICO` = col_integer(), 
                                                        NAYARIT = col_integer(), `NUEVO LEÓN` = col_integer(), 
                                                        OAXACA = col_integer(), PUEBLA = col_integer(), 
                                                        `QUERÉTARO` = col_integer(), `QUINTANA ROO` = col_integer(), 
                                                        `SAN LUIS POTOSÍ` = col_integer(), 
                                                        SINALOA = col_integer(), SONORA = col_integer(), 
                                                        TABASCO = col_integer(), TAMAULIPAS = col_integer(), 
                                                        TLAXCALA = col_integer(), VERACRUZ = col_integer(), 
                                                        `YUCATÁN` = col_integer(), ZACATECAS = col_integer(), 
                                                        fecha = col_date(format = "%d/%m/%Y")))
View(Datos_reemplazos)

#Credenciales entregadas
Datos_entregacredencial<- read_csv("Datos_entregacredencial_1994-2020.csv", 
                                              col_types = cols(fecha = col_date(format = "%d/%m/%Y")))
View(Datos_entregacredencial)




#----------------------------------------------------------------------------------#
######################## Procesamiento Datos Inscripciones #########################
#----------------------------------------------------------------------------------#

#se eliminan las filas sobrantes
#Datos_inscripciones2 <- Datos_inscripciones %>% drop_na(fecha)

#se añade el número de semana
Datos_inscripciones2 <- Datos_inscripciones %>% arrange(fecha) %>% 
  group_by(año) %>%
  mutate(Semana = 1:n()) %>% 
  ungroup()

#se renombran las columnas
Datos_inscripciones2 <- Datos_inscripciones2 %>%
  rename(
    Fecha = fecha)

#transformar de formato ancho a formato largo
Datos_inscripciones3 <- gather(Datos_inscripciones2, Edo, Numero, c(colnames(Datos_inscripciones2[,1:33])), factor_key=TRUE)

#añadir columna con tipo de tramite
Datos_inscripciones3 <- Datos_inscripciones3 %>%
  mutate(Tramite = 'Inscripción')

#View(Datos_inscripciones3)

#se obtienen las claves de los estados presentes 
clv_edo <- unique(unlist(Datos_inscripciones3$Edo))
#View(clv_edo)

#numero de valores vacíos en inscripciones
sum(is.na(Datos_inscripciones3$Numero))

#cambiar nan en columna Inscripciones por ceros
Datos_inscripciones3 <- Datos_inscripciones3 %>% replace_na(list(Numero = 0))

sum(is.na(Datos_inscripciones3$Numero))
View(Datos_inscripciones3)


#----------------------------------------------------------------------------------#
#################### Procesamiento Datos Entrega Credenciales ######################
#----------------------------------------------------------------------------------#


#se añade el número de semana
Datos_entregacredencial2 <- Datos_entregacredencial %>% arrange(fecha) %>% 
  group_by(año) %>%
  mutate(Semana = 1:n()) %>% 
  ungroup()

#se renombran las columnas
Datos_entregacredencial2 <- Datos_entregacredencial2 %>%
  rename(
    Fecha = fecha)

#transformar de formato ancho a formato largo
Datos_entregacredencial3 <- gather(Datos_entregacredencial2, Edo, Numero, c(colnames(Datos_entregacredencial2[,1:33])), factor_key=TRUE)

#añadir columna con tipo de tramite
Datos_entregacredencial3 <- Datos_entregacredencial3 %>%
  mutate(Tramite = 'Entrega_credencial')

#View(Datos_entregacredencial3)

#numero de valores vacíos en inscripciones
sum(is.na(Datos_entregacredencial3$Numero))

#cambiar nan en columna Inscripciones por ceros
Datos_entregacredencial3 <- Datos_entregacredencial3 %>% replace_na(list(Numero = 0))

sum(is.na(Datos_entregacredencial3$Numero))

View(Datos_entregacredencial3)


#----------------------------------------------------------------------------------#
##################### Procesamiento Datos Cambio de domicilio ######################
#----------------------------------------------------------------------------------#


#se añade el número de semana
Datos_cambiodom2 <- Datos_cambiodom %>% arrange(fecha) %>% 
  group_by(año) %>%
  mutate(Semana = 1:n()) %>% 
  ungroup()

#se renombran las columnas
Datos_cambiodom2 <- Datos_cambiodom2 %>%
  rename(
    Fecha = fecha)

#transformar de formato ancho a formato largo
Datos_cambiodom3 <- gather(Datos_cambiodom2, Edo, Numero, c(colnames(Datos_cambiodom2[,1:33])), factor_key=TRUE)

#añadir columna con tipo de tramite
Datos_cambiodom3 <- Datos_cambiodom3 %>%
  mutate(Tramite = 'Cambio_domicilio')

#View(Datos_cambiodom3)

#numero de valores vacíos en inscripciones
sum(is.na(Datos_cambiodom3$Numero))

#cambiar nan en columna Inscripciones por ceros
Datos_cambiodom3 <- Datos_cambiodom3 %>% replace_na(list(Numero = 0))

sum(is.na(Datos_cambiodom3$Numero))

View(Datos_cambiodom3)


#----------------------------------------------------------------------------------#
#################### Procesamiento Datos Correccion direccion ######################
#----------------------------------------------------------------------------------#


#se añade el número de semana
Datos_correcdireccion2 <- Datos_correcdireccion %>% arrange(fecha) %>% 
  group_by(año) %>%
  mutate(Semana = 1:n()) %>% 
  ungroup()

#se renombran las columnas
Datos_correcdireccion2 <- Datos_correcdireccion2 %>%
  rename(
    Fecha = fecha)

#transformar de formato ancho a formato largo
Datos_correcdireccion3 <- gather(Datos_correcdireccion2, Edo, Numero, c(colnames(Datos_correcdireccion2[,1:33])), factor_key=TRUE)

#añadir columna con tipo de tramite
Datos_correcdireccion3 <- Datos_correcdireccion3 %>%
  mutate(Tramite = 'Corrección_domicilio')

#View(Datos_correcdireccion3)

#numero de valores vacíos en inscripciones
sum(is.na(Datos_correcdireccion3$Numero))

#cambiar nan en columna Inscripciones por ceros
Datos_correcdireccion3 <- Datos_correcdireccion3 %>% replace_na(list(Numero = 0))

sum(is.na(Datos_correcdireccion3$Numero))

View(Datos_correcdireccion3)


#----------------------------------------------------------------------------------#
######################### Procesamiento Datos Error datos ##########################
#----------------------------------------------------------------------------------#


#se añade el número de semana
Datos_errordatos2 <- Datos_errordatos %>% arrange(fecha) %>% 
  group_by(año) %>%
  mutate(Semana = 1:n()) %>% 
  ungroup()

#se renombran las columnas
Datos_errordatos2 <- Datos_errordatos2 %>%
  rename(
    Fecha = fecha)

#transformar de formato ancho a formato largo
Datos_errordatos3 <- gather(Datos_errordatos2, Edo, Numero, c(colnames(Datos_errordatos2[,1:33])), factor_key=TRUE)

#añadir columna con tipo de tramite
Datos_errordatos3 <- Datos_errordatos3 %>%
  mutate(Tramite = 'Corrección_datos')

#View(Datos_errordatos3)

#numero de valores vacíos en numero
sum(is.na(Datos_errordatos3$Numero))

#cambiar nan en columna numero por ceros
Datos_errordatos3 <- Datos_errordatos3 %>% replace_na(list(Numero = 0))

sum(is.na(Datos_errordatos3$Numero))

View(Datos_errordatos3)



#----------------------------------------------------------------------------------#
######################### Procesamiento Datos Reemplazo ############################
#----------------------------------------------------------------------------------#


#se añade el número de semana
Datos_reemplazos2 <- Datos_reemplazos %>% arrange(fecha) %>% 
  group_by(año) %>%
  mutate(Semana = 1:n()) %>% 
  ungroup()

#se renombran las columnas
Datos_reemplazos2 <- Datos_reemplazos2 %>%
  rename(
    Fecha = fecha)

#transformar de formato ancho a formato largo
Datos_reemplazos3 <- gather(Datos_reemplazos2, Edo, Numero, c(colnames(Datos_reemplazos2[,1:33])), factor_key=TRUE)

#añadir columna con tipo de tramite
Datos_reemplazos3 <- Datos_reemplazos3 %>%
  mutate(Tramite = 'Reemplazo')

#View(Datos_reemplazos3)

#numero de valores vacíos en numero
sum(is.na(Datos_reemplazos3$Numero))

#cambiar nan en columna numero por ceros
Datos_reemplazos3 <- Datos_reemplazos3 %>% replace_na(list(Numero = 0))

sum(is.na(Datos_reemplazos3$Numero))

View(Datos_reemplazos3)


#----------------------------------------------------------------------------------#
######################### Procesamiento Datos Reincorporación ######################
#----------------------------------------------------------------------------------#


#se añade el número de semana
Datos_reincorporacion2 <- Datos_reincorporacion %>% arrange(fecha) %>% 
  group_by(año) %>%
  mutate(Semana = 1:n()) %>% 
  ungroup()

#se renombran las columnas
Datos_reincorporacion2 <- Datos_reincorporacion2 %>%
  rename(
    Fecha = fecha)

#transformar de formato ancho a formato largo
Datos_reincorporacion3 <- gather(Datos_reincorporacion2, Edo, Numero, c(colnames(Datos_reincorporacion2[,1:33])), factor_key=TRUE)

#añadir columna con tipo de tramite
Datos_reincorporacion3 <- Datos_reincorporacion3 %>%
  mutate(Tramite = 'Reincorporación')

#View(Datos_reincorporacion3)

#numero de valores vacíos en numero
sum(is.na(Datos_reincorporacion3$Numero))

#cambiar nan en columna numero por ceros
Datos_reincorporacion3 <- Datos_reincorporacion3 %>% replace_na(list(Numero = 0))

sum(is.na(Datos_reincorporacion3$Numero))

View(Datos_reincorporacion3)


#----------------------------------------------------------------------------------#
######################### Procesamiento Datos Reposiciones #########################
#----------------------------------------------------------------------------------#


#se añade el número de semana
Datos_reposiciones2 <- Datos_reposiciones %>% arrange(fecha) %>% 
  group_by(año) %>%
  mutate(Semana = 1:n()) %>% 
  ungroup()

#se renombran las columnas
Datos_reposiciones2 <- Datos_reposiciones2 %>%
  rename(
    Fecha = fecha)

#transformar de formato ancho a formato largo
Datos_reposiciones3 <- gather(Datos_reposiciones2, Edo, Numero, c(colnames(Datos_reposiciones2[,1:33])), factor_key=TRUE)

#añadir columna con tipo de tramite
Datos_reposiciones3 <- Datos_reposiciones3 %>%
  mutate(Tramite = 'Reposiciones')

#View(Datos_reposiciones3)

#numero de valores vacíos en numero
sum(is.na(Datos_reposiciones3$Numero))

#cambiar nan en columna numero por ceros
Datos_reposiciones3 <- Datos_reposiciones3 %>% replace_na(list(Numero = 0))

sum(is.na(Datos_reposiciones3$Numero))

View(Datos_reposiciones3)




#----------------------------------------------------------------------------------#
################################ Agrupamiento de datos #############################
#----------------------------------------------------------------------------------#

#agrupamiento por mes
total <- rbind(Datos_inscripciones3, Datos_cambiodom3)
total <- rbind(total, Datos_correcdireccion3)
total <- rbind(total, Datos_errordatos3)
total <- rbind(total, Datos_reemplazos3)
total <- rbind(total, Datos_reposiciones3)
total <- rbind(total, Datos_reincorporacion3)
total <- rbind(total, Datos_entregacredencial3)

View(total)

#se guardan los resultados
#write.csv(total,"Datos_Movimientos_PE_proc.csv", row.names = FALSE)


