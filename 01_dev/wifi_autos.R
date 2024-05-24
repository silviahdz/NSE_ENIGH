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

zonas_met <- readxl::read_xlsx('Composicion de ZMs.xlsx', 
                               sheet = "oficial")
Base_wifi <- readxl::read_xlsx('wifi_autos.xlsx')

# Zonas metropolitanas
#zonas_met <- read.xlsx(xlsxFile = '../Practicas 2024/Composicion de ZMs.xlsx', sheet = "oficial")

# Base_wifi <- read.xlsx(xlsxFile = '../Practicas 2024/wifi_autos.xlsx')

zonas_met_no_rep <- c()
zonas_met_no_rep <- zonas_met %>%
  group_by(zonas_met$ubic_geo) %>%
  summarise(across(everything(), ~toString(unique(.))))

zonas_met_no_rep <- subset(zonas_met_no_rep, select = c(ubic_geo,ZM))


Base <- Base_wifi

colnames(Base)[1] <- "ubic_geo"
table(nchar(Base$ubic_geo))

for(i in 1:length(Base$ubic_geo)){
  if(nchar(Base$ubic_geo[i])==4){
    Base$ubic_geo[i]<- paste0("0", Base$ubic_geo[i])
  }else{
    Base$ubic_geo[i] <- Base$ubic_geo[i]
  }
}

Base_resultante <- merge(
  x = Base, 
  y = zonas_met_no_rep,
  by.x = "ubic_geo",
  by.y = "ubic_geo",
  all.x = TRUE
)

# Tabla sin NA en NSE
unique(Base_resultante$ZM)

nueva_tabla <- 0
condicion <- !is.na(Base_resultante$NSE_NUEVO)

nueva_tabla <- Base_resultante[condicion,]

unique(nueva_tabla$NSE_NUEVO)

Base_resultante <- nueva_tabla

# Modificamos la tabla "Tabla" para dejar solo las columnas que estarmeos utilizando
Base_resultante <- subset(Base_resultante, select = c(ubic_geo,ZM,conex_inte, num_auto
                                                  ,NSE_NUEVO,factor))


# Muestra 
muestra_autos_ZM <- data.frame(table(Base_resultante$ZM))

total <- length(Base_resultante$ZM)-sum(table(Base_resultante$ZM))
total
vec <- c("sin zona",total)


muestra_autos_ZM<-rbind(muestra_autos_ZM,vec)

#sum(table(Tabla$ZM))

# writexl::write_xlsx(muestra_autos_ZM, "Muestras_autos_ZM.xlsx")


# summarise(group_by(Base_original,NSE_NUEVO),suma_alimentos = sum(alimentos))


# Creamos la tabla dinamica 
resultado_wifi <- Base_resultante %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_wifi = sum(conex_inte)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_wifi / sum(Suma_wifi)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_wifi, Porcentaje_total))

resultado_autos <- Base_resultante %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_autos = sum(num_auto)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_autos / sum(Suma_autos)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_autos, Porcentaje_total))

resultado_wifi <- resultado_wifi[,-c(2,3,4,5,6,7,8)]
resultado_autos <- resultado_autos[,-c(2,3,4,5,6,7,8)]
colnames(resultado_autos)

resultado_wifi <- resultado_wifi[,c(1,2,4,3,5,7,6,8)]
resultado_autos <- resultado_autos[,c(1,2,4,3,5,7,6,8)]

colnames(resultado_wifi) <- c("ZM","AB","C+","C","C-","D+","D","E")
colnames(resultado_autos) <- c("ZM","AB","C+","C","C-","D+","D","E")


# Validacion
pa <- apply(resultado_wifi, MARGIN = 2, FUN=as.double)
r<-apply(pa,MARGIN = 1,FUN=sum,na.rm=TRUE)
p_2 <- apply(resultado_autos, MARGIN = 2, FUN=as.double)
r_2 <- apply(p_2,MARGIN = 1,FUN=sum,na.rm=TRUE)

valid_3 <- data.frame()
valid_3 <- data.frame(
  "Internet"=r,
  "Autos"=r_2
)

rrr <- resultado_wifi$ZM

rrr <-na.omit(rrr)
rrr <- append(rrr,"sin zona")

#row.names(valid_2)<- rrr
valid_3$ZM<- rrr

valid_3 <-valid_3[,c(3,1,2)]

# writexl::write_xlsx(valid_3,"Validacion_Autos.xlsx")

resultado_wifi$variable <- "Internet"


long_df <- gather(resultado_wifi, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Conexión a internet"

long_df <- long_df[order(long_df$ZM),]


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

colnames(Dash_wifi)<-c ("ZM", "variable", "NSE", 
                "Porcentaje", "valor")

Dash_wifi <- rbind(Dash_wifi,long_df)

Dash_wifi <- Dash_wifi[-1,]


####### Transporte 
resultado_autos$variable <- "Autos"


long_df <- gather(resultado_autos, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Número de autos"

long_df <- long_df[order(long_df$ZM),]


Dash_wifi<- rbind(Dash_wifi,long_df)

for(i in 1:length(Dash_wifi$ZM)){
  if(is.na(Dash_wifi$ZM[i])){
    Dash_wifi$ZM[i]<- "SIN ZONA"
  }
}


## Total WIFI 
resultado_wifi <- Base_resultante %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_wifi = sum(conex_inte)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_wifi / sum(Suma_wifi)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_wifi, Porcentaje_total))

resultado_wifi <- resultado_wifi[,-c(9,10,11,12,13,14,15)]
colnames(resultado_wifi)<- c("ZM","AB", "C","C+","C-","D","D+","E")

# ordenamos las columnas
resultado_wifi <- resultado_wifi[,c("ZM","AB","C+","C","C-","D+","D","E")]

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
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_autos = sum(num_auto)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_autos / sum(Suma_autos)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_autos, Porcentaje_total))

resultado_autos <- resultado_autos[,-c(9,10,11,12,13,14,15)]
colnames(resultado_autos)
colnames(resultado_autos)<- c("ZM","AB", "C","C+","C-","D","D+","E")

# ordenamos las columnas
resultado_wifi <- resultado_wifi[,c("ZM","AB","C+","C","C-","D+","D","E")]

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


e$ZM <- "TOTAL PAÍS"
e$variable <- "Internet"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

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


e$ZM <- "TOTAL PAÍS"
e$variable <- "Autos"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Número de autos"

Dash_wifi <- rbind(Dash_wifi,long_df)

for(i in 1:length(Dash_wifi$Porcentaje)){
  if(is.na(Dash_wifi$Porcentaje[i])){
    Dash_wifi$Porcentaje[i]<-0
  }
     }

2*76*7



Base_resultante$condicion <- ifelse(Base_resultante$conex_inte/Base_resultante$factor
                                    > 1,0,1)

x<-group_by(Base_resultante,NSE_NUEVO)

y<-summarise(x,"suma"=sum(condicion),"n"=length(condicion))

y$porc <- y$suma/y$n

y$no <- 1-y$porc

#condicion <- Dash_wifi$variable == "Internet" & Dash_wifi$ZM == "TOTAL PAÍS"
#wifi<-Dash_wifi[condicion,]

#wifi$Porcentaje <- as.double(wifi$Porcentaje)
#wifi$NO <- 1-wifi$Porcentaje 


chivis_g <- c()
chivis_g2 <-c()
for(i in 1:nrow(y)){
  chivis <- c()
  chivis_2 <- c()
  chivis<-append(chivis,y$porc[i])
  chivis<-append(chivis,y$no[i])
  #chivis_2<-append(chivis_2,"Si")
  #chivis_2<-append(chivis_2,"No")
  
  chivis_g <- append(chivis_g,chivis[1])
  chivis_g <- append(chivis_g,chivis[2])
  #chivis_g2 < append(chivis_g2,chivis_2)
  
  
}

sino <- rep(c("Si","No"),nrow(y))


nse_g<-c()
for(i in 1:nrow(y)){
  nse <- c()
  nse <- rep(y$NSE_NUEVO[i],2)
  nse_g <- append(nse_g,nse)
}


grafica <- data.frame(
  "NSE"=nse_g,
  "Porcentaje"=chivis_g,
  "Internet"=sino
)

orden_filas <- c("A/B","C+","C","C-","D+","D","E")
grafica$NSE2 <- factor(grafica$NSE,levels = orden_filas)

#grafica <- grafica[condicion,]

ggplot(grafica, aes(x = NSE2, y = Porcentaje, fill = Internet)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label =ifelse(round(Porcentaje,4)>0.001,
                              100*round(Porcentaje,4),"")),
            position = position_stack(vjust = 0.5),
            size = 3)+
  labs(title = "Conexión de internet (%)",
       y = "Porcentaje",
       x = "NSE",
       fill = "Internet") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) 


# Gráfica número de autos


Base_resultante$condicion <- Base_resultante$num_auto / Base_resultante$factor
table(Base_resultante$condicion)
Base_resultante$division <- Base_resultante$num_auto / Base_resultante$factor
Base_resultante$condicion_2 <- ifelse(Base_resultante$condicion>4,5,
                                    Base_resultante$condicion)

#Base_resultante$num_auto

table(Base_resultante$condicion_2)

x<-group_by(Base_resultante,NSE_NUEVO, condicion_2)

colnames(Base_resultante)


y2<-summarise(x,"n"=sum(condicion_2),"n2"=length(condicion_2))
y<-summarise(x,"n"=length(condicion))

#write.csv(y,"Sumas_autos.csv")

r<-group_by(y2,NSE_NUEVO)

r <- mutate(r,"suma"=sum(n2))

r$porc <- round(r$n2/r$suma,3)



orden_filas <- c("A/B","C+","C","C-","D+","D","E")
r$NSE2 <- factor(r$NSE_NUEVO,levels = orden_filas)


#grafica <- grafica[condicion,]


ggplot(r, aes(x = NSE2, y = porc, fill = condicion_2)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label =ifelse(round(porc,4)>0.001,
                              100*round(porc,4),"")),
            position = position_stack(vjust = 0.5),
            size = 3)+
  labs(title = "Validacion",
       y = "Porcentaje",
       x = "NSE",
       fill = "Número de autos") +
  scale_fill_viridis_c(option = "H")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) 

write.csv(r,"verbaños.csv")

table(r$division)

writexl:: write_xlsx(Dash_wifi,"BASE_WIFI_AUTOS.xlsx")
