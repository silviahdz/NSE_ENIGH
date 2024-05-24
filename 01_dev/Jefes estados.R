library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(openxlsx)
library(reshape2)
library(tidyr)
library(readxl)



# t <- readxl::read_xlsx( '../Practicas 2024/Jefe_ENIGH.xlsx')
# Cargamos las bases de datos 
setwd("../00_resources")
# setwd("C:/Users/52238/OneDrive/Documentos/Practicas 2024")

# Tabla original de los jef@s de familia del nivel de edcucación
jefes <- read.xlsx(xlsxFile = 'Jefe_ENIGH.xlsx')

Estado <- read.xlsx(xlsxFile = 'Cod_Estado.xlsx')


for(i in 1:length(jefes$folioviv)){
  if(nchar(jefes$folioviv[i])==9){
    jefes$folioviv[i] <- paste(0,jefes$folioviv[i],sep = "")  
    }
}

nchar(jefes$folioviv)

jefes$ENTIDAD <- substring(jefes$folioviv,1,2)

jefes_mod <- merge(
  y = jefes, 
  x = Estado, 
  by = 'ENTIDAD'
)

unique(jefes_mod$NSE_NUEVO)

nueva_tabla <- 0
condicion <- !is.na(jefes_mod$NSE_NUEVO)

nueva_tabla <- jefes_mod[condicion,]

unique(nueva_tabla$NSE_NUEVO)

jefes_mod <- nueva_tabla


# writexl::write_xlsx(jefes_mod,"BASE_jefes.xlsx")

### Muestra 

unique(jefes_mod$NOM_ENT)
muestra_ESTADO <- data.frame(table(jefes_mod$NOM_ENT))

sum(table(jefes_mod$NOM_ENT))

# writexl::write_xlsx(muestra_ESTADO, "Muestras_ESTADO.xlsx")


jefes_mod <- jefes_mod[,-c(3,5,6,8)]

table(jefes_mod$NOM_ENT)
t<- data.frame(table(jefes_mod$NOM_ENT))

# sum(resultado[1,-1], na.rm  = TRUE) 


vector <- unique(jefes_mod$NOM_ENT)  
vector
#abd <- nueva_tabla[40:500, colnames(nueva_tabla)]

# Cramos una nueva tabla donde solo se encuentran lso datos que no son NA 
nueva_tabla <- data.frame()

nueva_tabla <- jefes_mod
Tab <- jefes_mod
vector


#for (i in 1:nrow(Tab)) {
#  for (j in 1:length(vector)) {
#   if (!is.na(Tab$NOM_ENT[i]) && !is.na(vector[j]) && Tab$NOM_ENT[i] == vector[j]) {
#     nueva_tabla <- rbind(nueva_tabla, Tab[i, ])
#   }
#  }
#}



orden_filas <- c("Posgrado", "Profesional completa", "Profesional incompleta",
                 "Preparatoria completa", "Preparatoria incompleta", 
                 "Secundaria completa", "Secundaria incompleta", "Primaria completa", 
                 "Primaria incompleta", "Preescolar", "Sin instrucci—n")  
orden_columnas <- c("Nivel_de_educacion","AB", "C+", "C", "C-","D+","D","E")  


columns <- orden_columnas # Define row names
row_names <- orden_filas
# Create an empty dataframe with specified row names and column 
namesdf <- data.frame(matrix(NA, nrow = length(row_names), ncol = length(columns)))
rownames(namesdf) <- c()
colnames(namesdf) <- columns

namesdf$Nivel_de_educacion <- orden_filas

resultados_env <- new.env()
validacion <- c()

# Iterar sobre los nombres en vector
for (i in seq_along(vector)) {
  # Filtrar las filas donde ZM coincide con el nombre actual
  filtro <- nueva_tabla %>% filter(NOM_ENT == vector[i])
  
  
  # Verificar si hay filas para el nombre actual
  if (nrow(filtro) > 0) {
    resultado <- filtro %>%
      group_by(Nivel_de_educacion, NSE_NUEVO) %>%
      summarise(Suma_factor = sum(factor)) %>%
      group_by(NSE_NUEVO) %>%
      mutate(Porcentaje_total = Suma_factor / sum(Suma_factor)) %>%
      pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_factor, Porcentaje_total))
    
    nombre_objeto <- paste0(vector[i])
    assign(nombre_objeto, resultado, envir = resultados_env)
    
    
  }
  
  # Validación 
  condicion_2 <- grepl("Porc",colnames(resultado))
  resultado_2 <- resultado[,condicion_2]
  resultado_2 <- apply(resultado_2, MARGIN = 2, FUN = sum, na.rm = TRUE)
  validacion <- append(validacion,resultado_2)
}

validacion


table(validacion)
7*32 #224

valid <- c()
for(i in 1:length(vector)){
  valid <- append(valid,rep(vector[i],7))
}


nse <- rep(orden_columnas[-1],32)

chivis <- data.frame(
  "Estado" = valid, 
  "NSE" = nse, 
  "Validacion"=validacion
  
)

writexl::write_xlsx(chivis,"validacion_estados.xlsx")
#rep(vector[2],7)

# resultado[,(ncol(resultado)-6):ncol(resultado)]

condicion_2 <- grepl("Porc",colnames(resultado))
resultado[,condicion_2]

# Obtener los nombres de los objetos de resultados_env
nombres_objetos <- ls(resultados_env)


# Iterar sobre los nombres y cambia los NA por un cero 
for (i in nombres_objetos) {
  resultados_env[[i]][is.na(resultados_env[[i]])] <- 0
}




columnas <- c('Suma_factor_A/B', 'Suma_factor_C+', 'Suma_factor_C', 
              'Suma_factor_C-', 'Suma_factor_D+', 'Suma_factor_D', 
              'Suma_factor_E', 'Suma_factor_NA', 'Porcentaje_total_NA')

# Iterar sobre los nombres de los objetos en el entorno
for (i in ls(resultados_env)) {
  
  if (is.data.frame(resultados_env[[i]])) {
    
    resultados_env[[i]] <- resultados_env[[i]][, !colnames(resultados_env[[i]]) %in% columnas]
  }
}
View(resultados_env$Aguascalientes)

# Validacion 




# Iterar sobre los nombres de los objetos en el entorno
for (i in ls(resultados_env)) {
  # Verificar si el objeto es un data.frame
  if (is.data.frame(resultados_env[[i]])) {
    # Obtener los nombres de las columnas
    nombres_columnas <- colnames(resultados_env[[i]])
    
    # Verificar si la columna 'Porcentaje_total_A/B' existe en el data.frame
    if ('Porcentaje_total_A/B' %in% nombres_columnas) {
      # Cambiar el nombre de la columna
      colnames(resultados_env[[i]])[colnames(resultados_env[[i]]) == 'Porcentaje_total_A/B'] <- "AB"
    }
    if ('Porcentaje_total_C+' %in% nombres_columnas) {
      
      colnames(resultados_env[[i]])[colnames(resultados_env[[i]]) == 'Porcentaje_total_C+'] <- "C+"
    }
    if ('Porcentaje_total_C' %in% nombres_columnas) {
      
      colnames(resultados_env[[i]])[colnames(resultados_env[[i]]) == 'Porcentaje_total_C'] <- "C"
    }
    if ('Porcentaje_total_C-' %in% nombres_columnas) {
      
      colnames(resultados_env[[i]])[colnames(resultados_env[[i]]) == 'Porcentaje_total_C-'] <- "C-"
    }
    if ('Porcentaje_total_D+' %in% nombres_columnas) {
      
      colnames(resultados_env[[i]])[colnames(resultados_env[[i]]) == 'Porcentaje_total_D+'] <- "D+"
    }
    if ('Porcentaje_total_D' %in% nombres_columnas) {
      
      colnames(resultados_env[[i]])[colnames(resultados_env[[i]]) == 'Porcentaje_total_D'] <- "D"
    }
    if ('Porcentaje_total_E' %in% nombres_columnas) {
      
      colnames(resultados_env[[i]])[colnames(resultados_env[[i]]) == 'Porcentaje_total_E'] <- "E"
    }
  }
}

View(resultados_env$Aguascalientes)



## tablas para el dash
orden_filas <- c("Posgrado", "Profesional completa", "Profesional incompleta",
                 "Preparatoria completa", "Preparatoria incompleta", 
                 "Secundaria completa", "Secundaria incompleta", "Primaria completa", 
                 "Primaria incompleta", "Preescolar", "Sin instrucci—n")  
orden_columnas <- c("Nivel_de_educacion","AB", "C+", "C", "C-","D+","D","E")


# En el environment "Nombre_Tablas" esta nombrada cada tabla por el nombre de su ZM
Tablas <- new.env()

Tablas <- resultados_env
tabla_nombres <- ls(Tablas)

Nombre_tablas <- new.env()
Nombre_tablas <- Tablas



prueba <- data.frame()




#######################################
# Agrega las filas y columnas faltantes 
for (nombre_tabla in ls(Nombre_tablas)) {
  # Obtener la tabla del environment
  tabla_actual <- Nombre_tablas[[nombre_tabla]]
  
  # Iterar sobre el orden de las filas
  for (i in orden_filas) {
    if (!(i %in% tabla_actual$Nivel_de_educacion)) {
      tabla_actual <- rbind(tabla_actual, c(i, rep(0, ncol(tabla_actual) - 1)))
    }
  }
  
  # Iterar sobre el orden de las columnas
  for (i in orden_columnas) {
    if (!(i %in% colnames(tabla_actual))) {
      tabla_actual <- cbind(tabla_actual, rep(0, nrow(tabla_actual)))
    }
  }
  
  
  # Actualizar la tabla en el environment
  Nombre_tablas[[nombre_tabla]] <- tabla_actual
  
}

View(resultados_env$Aguascalientes)

b <- c(2)
for(i in ls(Nombre_tablas)){
  a <- colnames(Nombre_tablas[[i]])[length(Nombre_tablas[[i]])]
  b <- append(b,a)
}

for(i in ls(Nombre_tablas)){
  a <- colnames(Nombre_tablas[[i]])[length(Nombre_tablas[[i]])]
  b <- append(b,a)
}




# Ordena las columnas y las filas de las tablas 

for (nombre_tabla in ls(Nombre_tablas)) {
  
  # Obtener la tabla del environment
  tabla_actual <- Nombre_tablas[[nombre_tabla]]
  
  # Reordenar las columnas según tu especificación
  tabla_actual <- tabla_actual[, c("Nivel_de_educacion", "AB", "C+", "C", "C-", "D+", "D", "E")]
  
  # Factorizar la columna "Nivel_de_educacion"
  tabla_actual$Nivel_de_educacion <- factor(tabla_actual$Nivel_de_educacion, levels = 
                                              c("Posgrado", "Profesional completa", 
                                                "Profesional incompleta", "Preparatoria completa",
                                                "Preparatoria incompleta", "Secundaria completa", 
                                                "Secundaria incompleta", "Primaria completa",
                                                "Primaria incompleta","Preescolar", "Sin instrucci—n"))
  
  tabla_actual <- tabla_actual[order(tabla_actual$Nivel_de_educacion),]
  
  #order(tabla_actual$Nivel_de_educacion)
  
  # Actualizar la tabla en el environment
  Nombre_tablas[[nombre_tabla]] <- tabla_actual
  
}


a <- data.frame(
  Nivel_de_educacion = character(),
  NSE = numeric(),
  Porcentaje = numeric(),
  ESTADO = character(),
  variable = character(),
  stringsAsFactors = FALSE  # Esto evita que las cadenas se conviertan en factores por defecto
)



# Agregar un renglón vacío al data frame
a <- rbind(a, c("", NA, NA, "", ""))
View(a)

colnames(a)<-c ("Nivel_de_educacion", "NSE", "Porcentaje", 
                "ESTADO", "variable")

for( nombre_tabla in ls(Nombre_tablas)){
  tabla_actual <- Nombre_tablas[[nombre_tabla]]
  tabla_actual$variable <- "Nivel educacion jefe de familia"
  
  tabla_actual$ESTADO <- nombre_tabla
  
  
  long_df <- gather(tabla_actual, key = "NSE",
                    value = "Porcentaje",
                    -c(ESTADO, Nivel_de_educacion, variable))
  
  
  long_df <- long_df[, c("Nivel_de_educacion", "NSE", "Porcentaje", 
                         "ESTADO", "variable")]
  a <- rbind(a,long_df)
}


a <- a[-1,]

### Total país

# TOTAL 

Tabla<-jefes_mod

# Creamos la tabla dinamica 
resultado <- Tabla %>%
  group_by(Nivel_de_educacion, NSE_NUEVO) %>%
  summarise(Suma_factor = sum(factor)) %>%
  group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_factor / sum(Suma_factor)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_factor, Porcentaje_total))


# Limpiamos la base de datos y nos quedamos solo con los porcentajes
condicion <- grepl("Porc",colnames(resultado)) | grepl("Nivel",
                                                        colnames(resultado))
resultado <- resultado[,condicion]

# Todos los valores que tenga NA se van a reemplazar con un cero
resultado[is.na(resultado)] <- 0

# Redondeamos todos los resultados a dos digitos
resultado <- resultado %>%
  mutate(across(where(is.numeric), ~round(., digits = 8)))

total <- resultado

colnames(total)<-c("Nivel_de_educacion", "AB","C","C+","C-","D+","D","E")

# Reordenar las columnas según tu especificación
total <- total[, c("Nivel_de_educacion", "AB", "C+", "C", "C-", "D+", "D", "E")]

# Factorizar la columna "Nivel_de_educacion"
total$Nivel_de_educacion <- factor(total$Nivel_de_educacion, levels = 
                                     c("Posgrado", "Profesional completa", 
                                       "Profesional incompleta", "Preparatoria completa",
                                       "Preparatoria incompleta", "Secundaria completa", 
                                       "Secundaria incompleta", "Primaria completa",
                                       "Primaria incompleta","Preescolar", "Sin instrucci—n"))

total<- total[order(total$Nivel_de_educacion),]



total$variable <- "Nivel educacion jefe de familia"

total$ESTADO <- "Total país"


long <- gather(total, key = "NSE",
               value = "Porcentaje",
               -c(ESTADO, Nivel_de_educacion, variable))


long <- long[, c("Nivel_de_educacion", "NSE", "Porcentaje", 
                 "ESTADO", "variable")]
a <- rbind(a,long)

for(i in 1:length(a$Nivel_de_educacion)){
  if(a$Nivel_de_educacion[i]=="Sin instrucci—n"){
    a$Nivel_de_educacion[i]<- "Sin instrucción"
  }
}



writexl::write_xlsx(a,"BASE_NIVEL_EDUCACION_ESTADOS_3.xlsx")

