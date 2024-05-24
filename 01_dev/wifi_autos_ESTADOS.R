library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(openxlsx)
library(reshape2)
library(tidyr)
library(readxl)


# Cargamos las bases de datos 
setwd("../00_resources")
#setwd("C:/Users/52238/OneDrive/Documentos/Practicas 2024")

Estado <- read.xlsx(xlsxFile = 'Cod_Estado.xlsx')


Base_wifi <- readxl::read_xlsx('wifi_autos.xlsx')

for(i in 1:length(Base_wifi$folioviv)){
  if(nchar(Base_wifi$folioviv[i])==9){
    Base_wifi$folioviv[i] <- paste(0,Base_wifi$folioviv[i],sep = "")  
  }
}

table(nchar(Base_wifi$folioviv))

Base <- Base_wifi

Base$ENTIDAD <- substring(Base$folioviv,1,2)

Base_resultante <- merge(
  y = Base, 
  x = Estado, 
  by = 'ENTIDAD'
)
unique(Base_resultante$NOM_ENT)

# Tabla sin NA en NSE
unique(Base_resultante$NOM_ENT)

nueva_tabla <- 0
condicion <- !is.na(Base_resultante$NSE_NUEVO)

nueva_tabla <- Base_resultante[condicion,]

unique(nueva_tabla$NSE_NUEVO)

Base_resultante <- nueva_tabla

### Muestra 


muestra_ESTADO <- data.frame(table(Base_resultante$NOM_ENT))

sum(table(Base_resultante$NOM_ENT))
# writexl::write_xlsx(muestra_ESTADO, "Muestras_ESTADO_wifi.xlsx")


# Creamos la tabla dinamica 
resultado_wifi <- Base_resultante %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_wifi = sum(conex_inte)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_wifi / sum(Suma_wifi)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_wifi, Porcentaje_total))

resultado_autos <- Base_resultante %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_autos = sum(num_auto)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_autos / sum(Suma_autos)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_autos, Porcentaje_total))

resultado_wifi <- resultado_wifi[,-c(2,3,4,5,6,7,8)]
resultado_autos <- resultado_autos[,-c(2,3,4,5,6,7,8)]

resultado_wifi <- resultado_wifi[,c(1,2,4,3,5,7,6,8)]
resultado_autos <- resultado_autos[,c(1,2,4,3,5,7,6,8)]

colnames(resultado_autos)
colnames(resultado_wifi) <- c("ESTADO","AB","C+","C","C-","D+","D","E")
colnames(resultado_autos) <- c("ESTADO","AB","C+","C","C-","D+","D","E")


# Validacion
pa <- apply(resultado_wifi, MARGIN = 2, FUN=as.double)
r<-apply(pa,MARGIN = 1,FUN=sum,na.rm=TRUE)
pa
r
p_2 <- apply(resultado_autos, MARGIN = 2, FUN=as.double)
r_2 <- apply(p_2,MARGIN = 1,FUN=sum,na.rm=TRUE)

valid_3 <- data.frame()
valid_3 <- data.frame(
  "Internet"=r,
  "Autos"=r_2
)

rrr <- resultado_wifi$ESTADO
rrr


#row.names(valid_2)<- rrr
valid_3$ESTADO<- rrr

valid_3 <-valid_3[,c(3,1,2)]

# writexl::write_xlsx(valid_3,"Validacion_Autos_ESTADOS.xlsx")


resultado_wifi$variable <- "Internet"


long_df <- gather(resultado_wifi, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

long_df$valor <- "Conexión a internet"

long_df <- long_df[order(long_df$ESTADO),]


Dash_wifi <- data.frame(
  ZM = character(),
  variable = character(),
  NSE = character(),
  Porcentaje = numeric(),
  valor = character(),
  stringsAsFactors = FALSE  
)



# Agregar un renglón vacío al data frame
Dash_wifi<- rbind(Dash_wifi, c("", NA, NA, "", ""))
View(Dash_wifi)

colnames(Dash_wifi)<-c ("ESTADO", "variable", "NSE", 
                        "Porcentaje", "valor")

Dash_wifi <- rbind(Dash_wifi,long_df)

Dash_wifi <- Dash_wifi[-1,]


####### Transporte 
resultado_autos$variable <- "Autos"


long_df <- gather(resultado_autos, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

long_df$valor <- "Número de autos"

long_df <- long_df[order(long_df$ESTADO),]


Dash_wifi<- rbind(Dash_wifi,long_df)


## Total WIFI 
resultado_wifi <- Base_resultante %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_wifi = sum(conex_inte)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_wifi / sum(Suma_wifi)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_wifi, Porcentaje_total))

resultado_wifi <- resultado_wifi[,-c(9,10,11,12,13,14,15)]
colnames(resultado_wifi)
colnames(resultado_wifi)<- c("ESTADO","AB", "C","C+","C-","D","D+","E")

# ordenamos las columnas
resultado_wifi <- resultado_wifi[,c("ESTADO","AB","C+","C","C-","D+","D","E")]

#  colnames(resultado_wifi)

suma_wifi <- apply(resultado_wifi[,-1], MARGIN = 2, FUN=sum,na.rm = TRUE)
suma_wifi


Total_wifi <- sum(suma_wifi)
Total_wifi
Porc_wifi <- suma_wifi/Total_wifi

# Porcentajes del total de wifi
Porc_wifi

#### Autos 
resultado_autos <- Base_resultante %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_autos = sum(num_auto)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_autos / sum(Suma_autos)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_autos, Porcentaje_total))

resultado_autos <- resultado_autos[,-c(9,10,11,12,13,14,15)]
colnames(resultado_autos)
colnames(resultado_autos)<- c("ESTADO","AB", "C","C+","C-","D","D+","E")

# ordenamos las columnas
resultado_wifi <- resultado_wifi[,c("ESTADO","AB","C+","C","C-","D+","D","E")]

#  colnames(resultado_wifi)

suma_autos <- apply(resultado_autos[,-1], MARGIN = 2, FUN=sum,na.rm = TRUE)
suma_autos


Total_autos <- sum(suma_autos)
Total_autos
Porc_autos <- suma_autos/Total_autos
Porc_autos



# Agregamos el wifi total 


e <- data.frame(
  AB = character(),
  C = character(),
  'C+' = numeric(),
  'C-' = character(),
  D = character(),
  'D+' = character(),
  E = character(),
  stringsAsFactors = FALSE  
)


e <- rbind(e,Porc_wifi)
colnames(e)<- c("AB","C+","C","C-","D+","D","E")


e$ESTADO <- "TOTAL PAÍS"
e$variable <- "Internet"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

long_df$valor <- "Conexión a internet"

Dash_wifi <- rbind(Dash_wifi,long_df)

# Agregamos el total de autos 
e <- data.frame(
  AB = character(),
  C = character(),
  'C+' = numeric(),
  'C-' = character(),
  D = character(),
  'D+' = character(),
  E = character(),
  stringsAsFactors = FALSE  
)


e <- rbind(e,Porc_autos)
colnames(e)<- c("AB","C+","C","C-","D+","D","E")


e$ESTADO <- "TOTAL PAÍS"
e$variable <- "Autos"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

long_df$valor <- "Número de autos"

Dash_wifi <- rbind(Dash_wifi,long_df)

for(i in 1:length(Dash_wifi$Porcentaje)){
  if(is.na(Dash_wifi$Porcentaje[i])){
    Dash_wifi$Porcentaje[i]<-0
  }
}

33*2*7
writexl:: write_xlsx(Dash_wifi,"BASE_WIFI_AUTOS_ESTADOS.xlsx")
