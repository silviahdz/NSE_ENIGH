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
#setwd("C:/Users/52238/OneDrive/Documentos/Practicas 2024")

# Zonas metropolitanas
zonas_met <- read.xlsx(xlsxFile = 'Composicion de ZMs.xlsx', sheet = "oficial")

# Tabla original de los jef@s de familia del nivel de edcucación
jefes <- read.xlsx(xlsxFile = 'Jefe_ENIGH.xlsx')

# Eliminamos lo datos repetidos de los municipios
zonas_met_no_rep <- zonas_met %>%
  group_by(zonas_met$ubic_geo) %>%
  summarise(across(everything(), ~toString(unique(.))))

zonas_met_no_rep <- subset(zonas_met_no_rep, select = c(ubic_geo,ZM))

# Generamos la tabla de jefes con la zona metropolitana
Tabla <- merge(
  x = jefes,
  y = zonas_met_no_rep, 
  by.x = "ubic_geo",
  by.y = "ubic_geo", 
  all.x = TRUE)


# Tabla sin NA en NSE
unique(Tabla$NSE_NUEVO)

nueva_tabla <- 0
condicion <- !is.na(Tabla$NSE_NUEVO)

nueva_tabla <- Tabla[condicion,]

unique(nueva_tabla$NSE_NUEVO)

Tabla <- nueva_tabla




# Modificamos la tabla "Tabla" para dejar solo las columnas que estarmeos utilizando
Tabla <- subset(Tabla, select = c(ubic_geo,ZM,Nivel_de_educacion,NSE_NUEVO,factor))

muestra_ZM <- data.frame(table(Tabla$ZM))

total <- length(Tabla$ZM)-sum(table(Tabla$ZM))
total
 vec <- c("sin zona",total)

muestra_ZM<-rbind(muestra_ZM,vec)

sum(table(Tabla$ZM))

#writexl::write_xlsx(muestra_ZM, "Muestras_ZM.xlsx")


# Tablas de cada Zona Metropolitana 
vector <- c("AGUASCALIENTES","ENSENADA","MEXICALI","TIJUANA","LA PAZ","CAMPECHE","SALTILLO",
            "MONCLOVA-FRONTERA","LA LAGUNA","PIEDRAS NEGRAS","TECOMÁN","COLIMA-VILLA DE ÁLVAREZ",
            "TUXTLA GUTIÉRREZ","TAPACHULA","CHIHUAHUA", "DELICIAS","HIDALGO DEL PARRAL" ,"JUÁREZ", 
            "VALLE DE MÉXICO","DURANGO","QUERÉTARO",  "CELAYA","GUANAJUATO","LEÓN",
            "MOROLEÓN-URIANGATO","LA PIEDAD-PÉNJAMO","SAN FRANCISCO DEL RINCÓN","ACAPULCO",
            "CHILPANCINGO" ,"TULA","TULANCINGO","PACHUCA" ,"GUADALAJARA","OCOTLÁN","PUERTO VALLARTA",
            "TOLUCA","TIANGUISTENCO","ZAMORA","MORELIA","CUAUTLA","CUERNAVACA","TEPIC" ,"MONTERREY",
            "OAXACA","TEHUANTEPEC","PUEBLA-TLAXCALA","TEHUACÁN","TEZIUTLÁN","CANCÚN","CHETUMAL",
            "RÍOVERDE","SAN LUIS POTOSÍ","CULIACÁN","MAZATLÁN","GUAYMAS","HERMOSILLO","NOGALES",
            "VILLAHERMOSA","TAMPICO","MATAMOROS","NUEVO LAREDO","REYNOSA","CIUDAD VICTORIA",
            "TLAXCALA-APIZACO","VERACRUZ","CÓRDOBA","XALAPA","ORIZABA", "POZA RICA","COATZACOALCOS",
            "MINATITLÁN","ACAYUCAN","MÉRIDA","ZACATECAS-GUADALUPE")  

#abd <- nueva_tabla[40:500, colnames(nueva_tabla)]

# Cramos una nueva tabla donde solo se encuentran lso datos que no son NA 
nueva_tabla <- data.frame()

Tab <- Tabla

for (i in 1:nrow(Tab)) {
  for (j in 1:length(vector)) {
    if (!is.na(Tab$ZM[i]) && !is.na(vector[j]) && Tab$ZM[i] == vector[j]) {
       nueva_tabla <- rbind(nueva_tabla, Tab[i, ])
    }
  }
}

nueva_tabla <- Tab %>%
  filter(!is.na(ZM) & ZM %in% vector)

orden_filas <- c("Posgrado", "Profesional completa", "Profesional incompleta",
                 "Preparatoria completa", "Preparatoria incompleta", 
                 "Secundaria completa", "Secundaria incompleta", "Primaria completa", 
                 "Primaria incompleta", "Preescolar", "Sin instrucci—n") 
orden_filas <- c(0,1,2,3,4,5)  

orden_columnas <- c("Nivel_de_educacion","AB", "C+", "C", "C-","D+","D","E")  


nueva_tabla$Nivel_de_educacion <- ifelse(nueva_tabla$Nivel_de_educacion>4,5,
                                         nueva_tabla$Nivel_de_educacion)


columns <- orden_columnas # Define row names
row_names <- orden_filas
# Create an empty dataframe with specified row names and column 
namesdf <- data.frame(matrix(NA, nrow = length(row_names), ncol = length(columns)))
rownames(namesdf) <- c()
colnames(namesdf) <- columns

namesdf$Nivel_de_educacion <- orden_filas
head(nueva_tabla)
resultados_env <- new.env()
validacion <- c()
#resp <- Base_resultante

#nueva_tabla <- Base_resultante
#nueva_tabla$Nivel_de_educacion <- nueva_tabla$num_auto / nueva_tabla$factor

# Iterar sobre los nombres en vector
for (i in seq_along(vector)) {
  # Filtrar las filas donde ZM coincide con el nombre actual
   filtro <- nueva_tabla %>% filter(ZM == vector[i])
  
  
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
   
  
}



# Obtener los nombres de los objetos de resultados_env
nombres_objetos <- ls(resultados_env)


# Iterar sobre los nombres y cambia los NA por un cero 
for (i in nombres_objetos) {
  resultados_env[[i]][is.na(resultados_env[[i]])] <- 0
}


View(resultados_env$ACAPULCO)

columnas <- c('Suma_factor_A/B', 'Suma_factor_C+', 'Suma_factor_C', 
                         'Suma_factor_C-', 'Suma_factor_D+', 'Suma_factor_D', 
                         'Suma_factor_E', 'Suma_factor_NA', 'Porcentaje_total_NA')

# Iterar sobre los nombres de los objetos en el entorno
for (i in ls(resultados_env)) {
  
  if (is.data.frame(resultados_env[[i]])) {
    
    resultados_env[[i]] <- resultados_env[[i]][, !colnames(resultados_env[[i]]) %in% columnas]
  }
}



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


orden_filas <- c("Posgrado", "Profesional completa", "Profesional incompleta",
                 "Preparatoria completa", "Preparatoria incompleta", 
                 "Secundaria completa", "Secundaria incompleta", "Primaria completa", 
                 "Primaria incompleta", "Preescolar", "Sin instrucci—n")  
orden_columnas <- c("Nivel_de_educacion","AB", "C+", "C", "C-","D+","D","E")  

View(resultados_env$tabla_ACAYUCAN)


# En el environment "Nombre_Tablas" esta nombrada cada tabla por el nombre de su ZM
Tablas <- new.env()

Tablas <- resultados_env
tabla_nombres <- ls(Tablas)

 Nombre_tablas <- new.env()
Nombre_tablas <- Tablas

#for (nombre_tabla in tabla_nombres) {
 
 #  entidad_nombre <- sub("tabla_", "", nombre_tabla, ignore.case = TRUE)  
  
#  entidad_nombre <- tolower(entidad_nombre)
  
 # assign(entidad_nombre, get(nombre_tabla, envir = Tablas), envir = Nombre_tablas)
#}


prueba <- data.frame()



View(Nombre_tablas$acayucan)
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

b <- c(2)
for(i in ls(Nombre_tablas)){
  a <- colnames(Nombre_tablas[[i]])[length(Nombre_tablas[[i]])]
  b <- append(b,a)
}

for(i in ls(Nombre_tablas)){
  a <- colnames(Nombre_tablas[[i]])[length(Nombre_tablas[[i]])]
  b <- append(b,a)
}


rm("ACAYUCAN", envir = Nombre_tablas)
rm("OCOTLÁN", envir = Nombre_tablas)
rm("TEHUANTEPEC", envir = Nombre_tablas)


# Ordena las columnas y las filas de las tablas 

for (nombre_tabla in ls(Nombre_tablas)) {
  
  # Obtener la tabla del environment
  tabla_actual <- Nombre_tablas[[nombre_tabla]]
  
  # Reordenar las columnas según tu especificación
  tabla_actual <- tabla_actual[, c("Nivel_de_educacion", "AB", "C+", "C", "C-", "D+", "D", "E")]
  
  # Factorizar la columna "Nivel_de_educacion"
  #tabla_actual$Nivel_de_educacion <- factor(tabla_actual$Nivel_de_educacion, levels = 
  #                                            c("Posgrado", "Profesional completa", 
  #                                              "Profesional incompleta", "Preparatoria completa",
  #                                              "Preparatoria incompleta", "Secundaria completa", 
  #                                              "Secundaria incompleta", "Primaria completa",
  #                                              "Primaria incompleta","Preescolar", "Sin instrucci—n"))
  tabla_actual$Nivel_de_educacion <- factor(tabla_actual$Nivel_de_educacion, levels = 
                                                                                         c(0,1,2,3,4,5))
  

  tabla_actual <- tabla_actual[order(tabla_actual$Nivel_de_educacion),]
  
  #order(tabla_actual$Nivel_de_educacion)
  
  # Actualizar la tabla en el environment
  Nombre_tablas[[nombre_tabla]] <- tabla_actual
  
}


#{"A": "hola", "B":"adios"}


#a <- a[1,c("Nivel_de_educacion", "NSE", "Porcentaje", 
  #         "ZM", "variable")]





## nace a 

a <- data.frame(
  Nivel_de_educacion = character(),
  NSE = numeric(),
  Porcentaje = numeric(),
  ZM = character(),
  variable = character(),
  stringsAsFactors = FALSE  # Esto evita que las cadenas se conviertan en factores por defecto
)



# Agregar un renglón vacío al data frame
a <- rbind(a, c("", NA, NA, "", ""))
View(a)

colnames(a)<-c ("Nivel_de_educacion", "NSE", "Porcentaje", 
"ZM", "variable")

for( nombre_tabla in ls(Nombre_tablas)){
  tabla_actual <- Nombre_tablas[[nombre_tabla]]
  tabla_actual$variable <- "Nivel educacion jefe de familia"
  
  tabla_actual$ZM <- nombre_tabla
  
  
  long_df <- gather(tabla_actual, key = "NSE",
                    value = "Porcentaje",
                    -c(ZM, Nivel_de_educacion, variable))
  
  
  long_df <- long_df[, c("Nivel_de_educacion", "NSE", "Porcentaje", 
                         "ZM", "variable")]
  a <- rbind(a,long_df)
}
#length(orden_filas)
#ls(Nombre_tablas)

#  unique(Tabla$ZM)
  
a <- a[-1,]

# ACAYUCAN 

vector <- c("ACAYUCAN", "TEHUANTEPEC", "OCOTLÁN")
Tabla <- nueva_tabla
# Hay datos que aparecen como "NA", no hay coincidencia con la ZM y acá checa cuantas veces aparece "ENSENADA"
cont <- sum(!is.na(Tabla$ZM) & Tabla$ZM == "ACAYUCAN")
cont # 20 

resp2 <- nueva_tabla
nueva_tabla <- data.frame()

Tab <- resp2
# Crea la nueva tabla para "AGUASCALIENTES"
for (i in 1:nrow(Tab)) {
  for (j in 1:1) {
    if (!is.na(Tab$ZM[i]) && !is.na(vector[j]) && Tab$ZM[i] == vector[j]) {
      nueva_tabla <- rbind(nueva_tabla, Tab[i, ])
    }
  }
}

re <- nueva_tabla %>%
  group_by(Nivel_de_educacion, NSE_NUEVO) %>%
  summarise(Suma_factor = sum(factor)) %>%
  group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_factor / sum(Suma_factor)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_factor, Porcentaje_total))


re[is.na(re)] <- 0

# Limpiamos la base de datos y nos quedamos solo con los porcentajes
condicion <- grepl("Porc",colnames(re)) | grepl("Nivel",
                                                       colnames(re))
re <- re[,condicion]



# AGREGAMOS LAS FILAS Y COLUMNAS 
acayucan <- re
colnames(acayucan)<- c("Nivel_de_educacion", "C-","D","D+","C+","C")
  
  # Iterar sobre el orden de las filas
  for (i in orden_filas) {
    if (!(i %in% acayucan$Nivel_de_educacion)) {
      acayucan <- rbind(acayucan, c(i, rep(0, ncol(acayucan) - 1)))
    }
  }
  
  # Iterar sobre el orden de las columnas
  for (i in orden_columnas) {
    if (!(i %in% colnames(acayucan))) {
      acayucan <- cbind(acayucan, rep(0, nrow(acayucan)))
    }
  
  }

colnames(acayucan)<-c("Nivel_de_educacion", "C-","D","D+","C+","C","AB","E")



  # Reordenar las columnas según tu especificación
acayucan <- acayucan[, c("Nivel_de_educacion", "AB", "C+", "C", "C-", "D+", "D", "E")]
  
  # Factorizar la columna "Nivel_de_educacion"
acayucan$Nivel_de_educacion <- factor(acayucan$Nivel_de_educacion, levels = 
                                              c("Posgrado", "Profesional completa", 
                                                "Profesional incompleta", "Preparatoria completa",
                                                "Preparatoria incompleta", "Secundaria completa", 
                                                "Secundaria incompleta", "Primaria completa",
                                                "Primaria incompleta","Preescolar", "Sin instrucci—n"))
  
acayucan$Nivel_de_educacion <- factor(acayucan$Nivel_de_educacion, levels = 
                                       c(0,1,2,3,4,5))

acayucan <- acayucan[order(acayucan$Nivel_de_educacion),]
  


acayucan$variable <- "Nivel educacion jefe de familia"
  
acayucan$ZM <- "ACAYUCAN"
  
  
long <- gather(acayucan, key = "NSE",
                    value = "Porcentaje",
                    -c(ZM, Nivel_de_educacion, variable))
  
  
long <- long[, c("Nivel_de_educacion", "NSE", "Porcentaje", 
                         "ZM", "variable")]
a <- rbind(a,long)

  
# TEHUANTEPEC

vector <- c("ACAYUCAN", "TEHUANTEPEC", "OCOTLÁN")
# Hay datos que aparecen como "NA", no hay coincidencia con la ZM y acá checa cuantas veces aparece "ENSENADA"
cont <- sum(!is.na(Tabla$ZM) & Tabla$ZM == "TEHUANTEPEC")
cont # 103

nueva_tabla <- data.frame()

# Crea la nueva tabla para "AGUASCALIENTES"
for (i in 1:nrow(Tab)) {
  for (j in 2:2) {
    if (!is.na(Tab$ZM[i]) && !is.na(vector[j]) && Tab$ZM[i] == vector[j]) {
      nueva_tabla <- rbind(nueva_tabla, Tab[i, ])
    }
  }
}

re <- nueva_tabla %>%
  group_by(Nivel_de_educacion, NSE_NUEVO) %>%
  summarise(Suma_factor = sum(factor)) %>%
  group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_factor / sum(Suma_factor)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_factor, Porcentaje_total))


re[is.na(re)] <- 0


# Limpiamos la base de datos y nos quedamos solo con los porcentajes
condicion <- grepl("Porc",colnames(re)) | grepl("Nivel",
                                                colnames(re))
re <- re[,condicion]

# AGREGAMOS LAS FILAS Y COLUMNAS 
tehuantepec <- re
colnames(tehuantepec)<- c("Nivel_de_educacion", "C-","D","D+","C+","E","C")

# Iterar sobre el orden de las filas
for (i in orden_filas) {
  if (!(i %in% tehuantepec$Nivel_de_educacion)) {
    tehuantepec <- rbind(tehuantepec, c(i, rep(0, ncol(tehuantepec) - 1)))
  }
}

# Iterar sobre el orden de las columnas
for (i in orden_columnas) {
  if (!(i %in% colnames(tehuantepec))) {
    tehuantepec <- cbind(tehuantepec, rep(0, nrow(tehuantepec)))
  }
  
}

colnames(tehuantepec)<-c("Nivel_de_educacion", "C-","D","D+","C+","E","C","AB")



# Reordenar las columnas según tu especificación
tehuantepec <- tehuantepec[, c("Nivel_de_educacion", "AB", "C+", "C", "C-", "D+", "D", "E")]

# Factorizar la columna "Nivel_de_educacion"
tehuantepec$Nivel_de_educacion <- factor(tehuantepec$Nivel_de_educacion, levels = 
                                        c("Posgrado", "Profesional completa", 
                                          "Profesional incompleta", "Preparatoria completa",
                                          "Preparatoria incompleta", "Secundaria completa", 
                                          "Secundaria incompleta", "Primaria completa",
                                          "Primaria incompleta","Preescolar", "Sin instrucci—n"))

tehuantepec$Nivel_de_educacion <- factor(tehuantepec$Nivel_de_educacion, levels = 
                                           c(0,1,2,3,4,5))

tehuantepec<- tehuantepec[order(tehuantepec$Nivel_de_educacion),]



tehuantepec$variable <- "Nivel educacion jefe de familia"

tehuantepec$ZM <- "TEHUANTEPEC"


long <- gather(tehuantepec, key = "NSE",
               value = "Porcentaje",
               -c(ZM, Nivel_de_educacion, variable))


long <- long[, c("Nivel_de_educacion", "NSE", "Porcentaje", 
                 "ZM", "variable")]
a <- rbind(a,long)


# OCOTLÁN

vector <- c("ACAYUCAN", "TEHUANTEPEC", "OCOTLÁN")
# Hay datos que aparecen como "NA", no hay coincidencia con la ZM y acá checa cuantas veces aparece "ENSENADA"
cont <- sum(!is.na(Tabla$ZM) & Tabla$ZM == "OCOTLÁN")
cont # 44

nueva_tabla <- data.frame()

# Crea la nueva tabla para "AGUASCALIENTES"
for (i in 1:nrow(Tab)) {
  for (j in 3:3) {
    if (!is.na(Tab$ZM[i]) && !is.na(vector[j]) && Tab$ZM[i] == vector[j]) {
      nueva_tabla <- rbind(nueva_tabla, Tab[i, ])
    }
  }
}

re <- nueva_tabla %>%
  group_by(Nivel_de_educacion, NSE_NUEVO) %>%
  summarise(Suma_factor = sum(factor)) %>%
  group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_factor / sum(Suma_factor)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_factor, Porcentaje_total))


re[is.na(re)] <- 0


# Limpiamos la base de datos y nos quedamos solo con los porcentajes
condicion <- grepl("Porc",colnames(re)) | grepl("Nivel",
                                                colnames(re))
re <- re[,condicion]

# AGREGAMOS LAS FILAS Y COLUMNAS 
ocotlan <- re
colnames(ocotlan)<- c("Nivel_de_educacion", "C-","C+","D","C","D+","E")

# Iterar sobre el orden de las filas
for (i in orden_filas) {
  if (!(i %in% ocotlan$Nivel_de_educacion)) {
    ocotlan <- rbind(ocotlan, c(i, rep(0, ncol(ocotlan ) - 1)))
  }
}

# Iterar sobre el orden de las columnas
for (i in orden_columnas) {
  if (!(i %in% colnames(ocotlan ))) {
    ocotlan  <- cbind(ocotlan , rep(0, nrow(ocotlan )))
  }
  
}

colnames(ocotlan)<-c("Nivel_de_educacion", "C-","C+","D","C","D+","E","AB")



# Reordenar las columnas según tu especificación
ocotlan <- ocotlan[, c("Nivel_de_educacion", "AB", "C+", "C", "C-", "D+", "D", "E")]

# Factorizar la columna "Nivel_de_educacion"
ocotlan$Nivel_de_educacion <- factor(ocotlan$Nivel_de_educacion, levels = 
                                           c("Posgrado", "Profesional completa", 
                                             "Profesional incompleta", "Preparatoria completa",
                                             "Preparatoria incompleta", "Secundaria completa", 
                                             "Secundaria incompleta", "Primaria completa",
                                             "Primaria incompleta","Preescolar", "Sin instrucci—n"))

ocotlan$Nivel_de_educacion <- factor(ocotlan$Nivel_de_educacion, levels = 
                                       c(0,1,2,3,4,5))

ocotlan<- ocotlan[order(ocotlan$Nivel_de_educacion),]



ocotlan$variable <- "Nivel educacion jefe de familia"

ocotlan$ZM <- "OCOTLÁN"


long <- gather(ocotlan, key = "NSE",
               value = "Porcentaje",
               -c(ZM, Nivel_de_educacion, variable))


long <- long[, c("Nivel_de_educacion", "NSE", "Porcentaje", 
                 "ZM", "variable")]
a <- rbind(a,long)


# TOTAL 

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

total$Nivel_de_educacion <- factor(total$Nivel_de_educacion, levels = 
                                     c(0,1,2,3,4,5))

total<- total[order(total$Nivel_de_educacion),]



total$variable <- "Nivel educacion jefe de familia"

total$ZM <- "TOTAL PAÍS"


long <- gather(total, key = "NSE",
               value = "Porcentaje",
               -c(ZM, Nivel_de_educacion, variable))


long <- long[, c("Nivel_de_educacion", "NSE", "Porcentaje", 
                 "ZM", "variable")]
a <- rbind(a,long)


# write.csv(a,"Base_CASI.csv")

abc <- Tabla[is.na(Tabla$ZM),] 

abc$ZM <- "Sin zona"

re <- abc %>%
  group_by(Nivel_de_educacion, NSE_NUEVO) %>%
  summarise(Suma_factor = sum(factor)) %>%
  group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_factor / sum(Suma_factor)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_factor, Porcentaje_total))

re[is.na(re)] <- 0


# Limpiamos la base de datos y nos quedamos solo con los porcentajes
condicion <- grepl("Porc",colnames(re)) | grepl("Nivel",
                                                       colnames(re))
re <- re[,condicion]

# AGREGAMOS LAS FILAS Y COLUMNAS 
sinzona <- re
colnames(sinzona)<- c("Nivel_de_educacion", "AB","C","C+","C-","D+","D","E")

# Iterar sobre el orden de las filas
for (i in orden_filas) {
  if (!(i %in% sinzona$Nivel_de_educacion)) {
    sinzona <- rbind(sinzona, c(i, rep(0, ncol(sinzona) - 1)))
  }
}

# Iterar sobre el orden de las columnas
for (i in orden_columnas) {
  if (!(i %in% colnames(sinzona))) {
    sinzona  <- cbind(sinzona, rep(0, nrow(sinzona)))
  }
  
}

colnames(sinzona)<-c("Nivel_de_educacion", "AB","C","C+","C-","D+","D","E")



# Reordenar las columnas según tu especificación
sinzona <- sinzona[, c("Nivel_de_educacion", "AB", "C+", "C", "C-", "D+", "D", "E")]

# Factorizar la columna "Nivel_de_educacion"
sinzona$Nivel_de_educacion <- factor(sinzona$Nivel_de_educacion, levels = 
                                       c("Posgrado", "Profesional completa", 
                                         "Profesional incompleta", "Preparatoria completa",
                                         "Preparatoria incompleta", "Secundaria completa", 
                                         "Secundaria incompleta", "Primaria completa",
                                         "Primaria incompleta","Preescolar", "Sin instrucci—n"))

sinzona$Nivel_de_educacion <- factor(sinzona$Nivel_de_educacion, levels = 
                                       c(0,1,2,3,4,5))

sinzona<- sinzona[order(ocotlan$Nivel_de_educacion),]



sinzona$variable <- "Nivel educacion jefe de familia"

sinzona$ZM <- "SIN ZONA"


long <- gather(sinzona, key = "NSE",
               value = "Porcentaje",
               -c(ZM, Nivel_de_educacion, variable))


long <- long[, c("Nivel_de_educacion", "NSE", "Porcentaje", 
                 "ZM", "variable")]
a <- rbind(a,long)
colnames(a)[1] <- "Nivel de educación"

a$variable <- "Nivel educación jefe de familia"

for(i in 1:length(a$`Nivel de educación`)){
  if(a$`Nivel de educación`[i] == "Sin instrucci—n"){
    a$`Nivel de educación`[i] <- "Sin instrucción"
  }
}



a$ZM == "TOTAL PAÍS"
chivis <- a[a$ZM == "TOTAL PAÍS",]

colnames(chivis)<-c("educacion","NSE","Porcentaje","ZM","variable")

chivis$Porcentaje <- as.double(chivis$Porcentaje)
p<- mutate(group_by(chivis,NSE), suma_chivis  = sum(Porcentaje)) 



ggplot(chivis, aes(x = NSE, y = Porcentaje, fill = educacion)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(round(chivis$Porcentaje,4) > 0.001,
                               paste0(round(chivis$Porcentaje * 100, 2)),
                               "")),
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(title = "Número de autos (%)",
       y = "Porcentaje",
       fill = "Autos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
  

anti_join(nueva_tabla)

View(Nombre_tablas$ACAPULCO)

ifelse(round(chivis$Porcentaje,4)>0.001,round(chivis$Porcentaje,4),"")
library(writexl)
writexl::write_xlsx(a,"BASE_NIVEL_DE_EDUCACION_2.xlsx")


length(table(Tabla$ZM))
76*11*7
table(a$ZM)
