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

Base_baños <- readxl::read_xlsx('Base_original.xlsx')


for(i in 1:length(Base_baños$folioviv)){
  if(nchar(Base_baños$folioviv[i])==9){
    Base_baños$folioviv[i] <- paste(0,Base_baños$folioviv[i],sep = "")  
  }
}

table(nchar(Base_baños$folioviv))


Base <- Base_baños

Base$ENTIDAD <- substring(Base$folioviv,1,2)

Base_resultante <- merge(
  y = Base, 
  x = Estado, 
  by = 'ENTIDAD'
)

# Tabla sin NA en NSE
unique(Base_resultante$NOM_ENT)

nueva_tabla <- 0
condicion <- !is.na(Base_resultante$NSE_NUEVO)

nueva_tabla <- Base_resultante[condicion,]

unique(nueva_tabla$NSE_NUEVO)

Base_resultante <- nueva_tabla


### Muestra 

unique(Base_resultante$NOM_ENT)
muestra_ESTADO <- data.frame(table(Base_resultante$NOM_ENT))

sum(table(Base_resultante$NOM_ENT))
# writexl::write_xlsx(muestra_ESTADO, "Muestras_ESTADO_dorm.xlsx")


# Modificamos la tabla "Tabla" para dejar solo las columnas que estarmeos utilizando
Base_resultante <- subset(Base_resultante, select = c(ENTIDAD,NOM_ENT,cuart_dorm, bano_comp,
                                                      bano_excus, bano_regad,NSE_NUEVO,factor))


# summarise(group_by(Base_original,NSE_NUEVO),suma_alimentos = sum(alimentos))


# Creamos la tabla dinamica 
resultado_dorm <- Base_resultante %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_dorm = sum(cuart_dorm)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_dorm / sum(Suma_dorm)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_dorm, Porcentaje_total))

resultado_bcomp <- Base_resultante %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_bcomp = sum(bano_comp)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_bcomp / sum(Suma_bcomp)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_bcomp, Porcentaje_total))

resultado_bexcus <- Base_resultante %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_bexcus = sum(bano_excus)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_bexcus / sum(Suma_bexcus)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_bexcus, Porcentaje_total))


resultado_bregad <- Base_resultante %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_bregad = sum(bano_regad)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_bregad / sum(Suma_bregad)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_bregad, Porcentaje_total))


resultado_dorm <- resultado_dorm[,-c(2,3,4,5,6,7,8)]
resultado_bregad <- resultado_bregad[,-c(2,3,4,5,6,7,8)]
resultado_bcomp <- resultado_bcomp[,-c(2,3,4,5,6,7,8)]
resultado_bexcus <- resultado_bexcus[,-c(2,3,4,5,6,7,8)]

colnames(resultado_dorm)
colnames(resultado_dorm)<- c("ESTADO","AB","C","C+","C-","D","D+","E")
colnames(resultado_bregad)
colnames(resultado_bregad)<- c("ESTADO","AB","C","C+","C-","D","D+","E")
colnames(resultado_bexcus)
colnames(resultado_bexcus)<- c("ESTADO","AB","C","C+","C-","D","D+","E")
colnames(resultado_bcomp)
colnames(resultado_bcomp)<- c("ESTADO","AB","C","C+","C-","D","D+","E")

# Validacion
p <- apply(resultado_bcomp, MARGIN = 2, FUN=as.double)
r<-apply(p,MARGIN = 1,FUN=sum,na.rm=TRUE)

p_2 <- apply(resultado_bexcus, MARGIN = 2, FUN=as.double)
r_2 <- apply(p_2,MARGIN = 1,FUN=sum,na.rm=TRUE)

p_3 <- apply(resultado_bregad, MARGIN = 2, FUN=as.double)
r_3 <- apply(p_3,MARGIN = 1,FUN=sum,na.rm=TRUE)

p_4<- apply(resultado_dorm,MARGIN=2,FUN=as.double)
r_4 <- apply(p_4,MARGIN=1,FUN=sum, na.rm=TRUE)
r_3

valid_2 <- data.frame(
  "Número de baños completos"=r,
  "Número de baños con excusado"=r_2,
  "Número de baños con regadera"=r_3,
  "Número de dormitorios"=r_4
)

rrr <- resultado_bcomp$ESTADO

valid_2$ESTADO<- rrr

valid_2 <-valid_2[,c(5,1,2,3,4)]


# writexl::write_xlsx(valid_2,"Validacion_Dorm_ESTADOS.xlsx")


## DORMITORIOS
resultado_dorm$variable <- "Dormitorios"

long_df <- gather(resultado_dorm, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

long_df$valor <- "Número de dormitorios"

long_df <- long_df[order(long_df$ESTADO),]

d <- data.frame(
  ESTADO = character(),
  variable = character(),
  NSE = character(),
  Porcentaje = numeric(),
  valor = character(),
  stringsAsFactors = FALSE  
)



# Agregar un renglón vacío al data frame
d <- rbind(d, c("", NA, NA, "", ""))
View(d)

colnames(d)<-c ("ESTADO", "variable", "NSE", 
                "Porcentaje", "valor")

d <- rbind(d,long_df)

d<- d[-1,]



## BAÑO CON REGADERAS 
resultado_bregad$variable <- "Baño con regadera"

long_df <- gather(resultado_bregad, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

long_df$valor <- "Número de baños con regadera"

long_df <- long_df[order(long_df$ESTADO),]


d <- rbind(d,long_df)

# Baño con excusado 
resultado_bexcus$variable <- "Baño con excusado"

long_df <- gather(resultado_bexcus, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

long_df$valor <- "Número de baños con excusado"

long_df <- long_df[order(long_df$ESTADO),]


d <- rbind(d,long_df)

# Baño completo 
resultado_bcomp$variable <- "Baño completo"

long_df <- gather(resultado_bcomp, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

long_df$valor <- "Número de baños completos"

long_df <- long_df[order(long_df$ESTADO),]


d <- rbind(d,long_df)

## Total de baños completos 
resultado_bcomp <- Base_resultante %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_bcomp = sum(bano_comp)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_bcomp / sum(Suma_bcomp)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_bcomp, Porcentaje_total))

resultado_bcomp <- resultado_bcomp[,-c(9,10,11,12,13,14,15)]
colnames(resultado_bcomp)
colnames(resultado_bcomp)<- c("ESTADO","AB","C","C+","C-","D","D+","E")


suma_bcomp<-apply(resultado_bcomp[,-1], MARGIN = 2,FUN=sum,na.rm=TRUE)
suma_bcomp

Total_bcomp<-sum(suma_bcomp)
Porc_bcomp <- suma_bcomp/Total_bcomp
Porc_bcomp

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


# Agregar un renglón vacío al data frame

e <- rbind(e,Porc_bcomp)
colnames(e)<- c("AB","C","C+","C-","D","D+","E")


e$ESTADO <- "TOTAL PAÍS"
e$variable <- "Baño completo"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

long_df$valor <- "Número de baños completos"

# Validacion
pp <- apply(long_df, MARGIN = 2, FUN=as.double)
rr<-apply(pp,MARGIN = 1,FUN=sum,na.rm=TRUE)
sum(rr)

d <- rbind(d,long_df)

## Baños con regadera
resultado_bregad <- Base_resultante %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_bregad = sum(bano_regad)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_bregad / sum(Suma_bregad)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_bregad, Porcentaje_total))

resultado_bregad <- resultado_bregad[,-c(9,10,11,12,13,14,15)]
colnames(resultado_bregad)
colnames(resultado_bregad)<- c("ESTADO","AB","C","C+","C-","D","D+","E")

suma_bregad<-apply(resultado_bregad[,-1], MARGIN = 2,FUN=sum,na.rm=TRUE)
suma_bregad

Total_bregad<-sum(suma_bregad)
Porc_bregad <- suma_bregad/Total_bregad
Porc_bregad

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


# Agregar un renglón vacío al data frame

e <- rbind(e,Porc_bregad)
colnames(e)<- c("AB","C","C+","C-","D","D+","E")


e$ESTADO <- "TOTAL PAÍS"
e$variable <- "Baño con regadera"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

long_df$valor <- "Número de baños con regadera"

# Validacion
pp <- apply(long_df, MARGIN = 2, FUN=as.double)
rr<-apply(pp,MARGIN = 1,FUN=sum,na.rm=TRUE)
sum(rr)

d <- rbind(d,long_df)


# Baño con excusado
resultado_bexcus <- Base_resultante %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_bexcus = sum(bano_excus)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_bexcus / sum(Suma_bexcus)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_bexcus, Porcentaje_total))

resultado_bexcus <- resultado_bexcus[,-c(9,10,11,12,13,14,15)]
colnames(resultado_bexcus)
colnames(resultado_bexcus)<- c("ESTADO","AB","C","C+","C-","D","D+","E")

suma_bexcus<-apply(resultado_bexcus[,-1], MARGIN = 2,FUN=sum,na.rm=TRUE)
suma_bexcus

Total_bexcus<-sum(suma_bexcus)
Porc_bexcus <- suma_bexcus/Total_bexcus
Porc_bexcus

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


# Agregar un renglón vacío al data frame

e <- rbind(e,Porc_bexcus)
colnames(e)<- c("AB","C","C+","C-","D","D+","E")


e$ESTADO<- "TOTAL PAÍS"
e$variable <- "Baño con excusado"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

long_df$valor <- "Número de baños con excusado"

d <- rbind(d,long_df)

# Dormitorios
resultado_dorm <- Base_resultante %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_dorm = sum(cuart_dorm)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_dorm / sum(Suma_dorm)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_dorm, Porcentaje_total))

resultado_dorm <- resultado_dorm[,-c(9,10,11,12,13,14,15)]
colnames(resultado_dorm)
colnames(resultado_dorm)<- c("ESTADO","AB","C","C+","C-","D","D+","E")

suma_dorm<-apply(resultado_dorm[,-1], MARGIN = 2,FUN=sum,na.rm=TRUE)
suma_dorm

Total_dorm<-sum(suma_dorm)
Porc_dorm <- suma_dorm/Total_dorm
Porc_dorm

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


# Agregar un renglón vacío al data frame

e <- rbind(e,Porc_dorm)
colnames(e)<- c("AB","C","C+","C-","D","D+","E")


e$ESTADO <- "TOTAL PAÍS"
e$variable <- "Dormitorios"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

long_df$valor <- "Número de dormitorios"

d <- rbind(d,long_df)

for(i in 1:length(d$Porcentaje)){
  if(is.na(d$Porcentaje[i])){
    
    d$Porcentaje[i]<-0
  }
}

4*33*7
writexl::write_xlsx(d,"BASE_BAÑOS_DORMITORIOS_ESTADOS.xlsx")


