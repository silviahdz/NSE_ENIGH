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
Base <- readxl::read_xlsx('Base de datos AL_TR_ED.xlsx')



zonas_met_no_rep <- zonas_met %>%
  group_by(zonas_met$ubic_geo) %>%
  summarise(across(everything(), ~toString(unique(.))))

zonas_met_no_rep <- subset(zonas_met_no_rep, select = c(ubic_geo,ZM))


Base_de_datos <- Base

for(i in 1:length(Base_de_datos$ubic_geo)){
  if(nchar(Base_de_datos$ubic_geo[i])==4){
    Base_de_datos$ubic_geo[i]<- paste0("0", Base_de_datos$ubic_geo[i])
  }else{
    Base_de_datos$ubic_geo[i] <- Base_de_datos$ubic_geo[i]
  }
}

table(nchar(Base$ubic_geo))
table(nchar(Base_de_datos$ubic_geo))


# Generamos la tabla de jefes con la zona metropolitana
Base_original <- merge(
  x = Base_de_datos,
  y = zonas_met_no_rep, 
  by.x = "ubic_geo",
  by.y = "ubic_geo", 
  all.x = TRUE)

# Tabla sin NA en NSE
unique(Base_original$ZM)

nueva_tabla <- 0
condicion <- !is.na(Base_original$NSE_NUEVO)

nueva_tabla <- Base_original[condicion,]

unique(nueva_tabla$NSE_NUEVO)

Base_original <- nueva_tabla



# Modificamos la tabla "Tabla" para dejar solo las columnas que estarmeos utilizando
Base_original <- subset(Base_original, select = c(ubic_geo,ZM,alimentos, transporte,educacion
                                                  ,NSE_NUEVO,factor))

# Muestra 
muestra_alim_ZM <- data.frame(table(Base_original$ZM))

total <- length(Base_original$ZM)-sum(table(Base_original$ZM))
total
vec <- c("sin zona",total)


muestra_alim_ZM<-rbind(muestra_alim_ZM,vec)

sum(table(Base_original$ZM))
# writexl::write_xlsx(muestra_alim_ZM, "Muestras_alim_ZM.xlsx")

# summarise(group_by(Base_original,NSE_NUEVO),suma_alimentos = sum(alimentos))


# Creamos la tabla dinamica 
resultado <- Base_original %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_alimentos = sum(alimentos)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_alimentos / sum(Suma_alimentos)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_alimentos, Porcentaje_total))

# Creamos la tabla dinamica 
resultado_2 <- Base_original %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_transporte = sum(transporte)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_transporte / sum(Suma_transporte)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_transporte, Porcentaje_total))

resultado_3 <- Base_original %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_educacion = sum(educacion)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_educacion / sum(Suma_educacion)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_educacion, Porcentaje_total))


resultado <- resultado[,-c(2,3,4,5,6,7,8)]
resultado_2 <- resultado_2[,-c(2,3,4,5,6,7,8)]
resultado_3 <- resultado_3[,-c(2,3,4,5,6,7,8)]

resultado <- resultado[,c(1,2,4,3,5,7,6,8)]
resultado_2 <- resultado_2[,c(1,2,4,3,5,7,6,8)]
resultado_3 <- resultado_3[,c(1,2,4,3,5,7,6,8)]

colnames(resultado_3)

colnames(resultado) <- c("ZM","AB","C+","C","C-","D+","D","E")
colnames(resultado_2) <- c("ZM","AB","C+","C","C-","D+","D","E")
colnames(resultado_3) <- c("ZM","AB","C+","C","C-","D+","D","E")


# Validacion
p <- apply(resultado, MARGIN = 2, FUN=as.double)
r<-apply(p,MARGIN = 1,FUN=sum,na.rm=TRUE)
p_2 <- apply(resultado_2, MARGIN = 2, FUN=as.double)
r_2 <- apply(p_2,MARGIN = 1,FUN=sum,na.rm=TRUE)
p_3 <- apply(resultado_3, MARGIN = 2, FUN=as.double)
r_3 <- apply(p_3,MARGIN = 1,FUN=sum,na.rm=TRUE)
r_3

valid_2 <- data.frame(
  "Alimentos"=r,
  "Transporte"=r_2,
  "Educación"=r_3
)

rrr <- resultado$ZM

rrr <-na.omit(rrr)
rrr <- append(rrr,"sin zona")

row.names(valid_2)<- rrr
valid_2$ZM<- rrr

valid_2 <-valid_2[,c(4,1,2,3)]
 
# writexl::write_xlsx(valid_2,"Validacion_AL.xlsx")

######

resultado$variable <- "Gasto"


long_df <- gather(resultado, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Alimentos"

long_df <- long_df[order(long_df$ZM),]


d <- data.frame(
  ZM = character(),
  variable = character(),
  NSE = character(),
  Porcentaje = numeric(),
  valor = character(),
  stringsAsFactors = FALSE  
)



# Agregar un renglón vacío al data frame
d <- rbind(d, c("", NA, NA, "", ""))
View(d)

colnames(d)<-c ("ZM", "variable", "NSE", 
                "Porcentaje", "valor")

d <- rbind(d,long_df)

d <- d[-1,]


####### Transporte 
resultado_2$variable <- "Gasto"


long_df <- gather(resultado_2, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Transporte"

long_df <- long_df[order(long_df$ZM),]


d <- rbind(d,long_df)


####### Educacion

resultado_3$variable <- "Gasto"


long_df <- gather(resultado_3, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Educación"

long_df <- long_df[order(long_df$ZM),]


d <- rbind(d,long_df)


for(i in 1:length(d$ZM)){
  if(is.na(d$ZM[i])){
    d$ZM[i]<-"SIN ZONA"
  }
}


# TOTAL 

resultado <- Base_original %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_alimentos = sum(alimentos)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_alimentos / sum(Suma_alimentos)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_alimentos, Porcentaje_total))

# Creamos la tabla dinamica 
resultado_2 <- Base_original %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_transporte = sum(transporte)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_transporte / sum(Suma_transporte)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_transporte, Porcentaje_total))

resultado_3 <- Base_original %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_educacion = sum(educacion)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_educacion / sum(Suma_educacion)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_educacion, Porcentaje_total))


resultado <- resultado[,-c(9,10,11,12,13,14,15)]
colnames(resultado)
colnames(resultado)<- c("ZM","AB","C","C+","C-","D","D+","E")

# Alimentos 

AB <- sum(resultado$AB,na.rm = TRUE)
C_mas <- sum(resultado$`C+`,na.rm = TRUE)
C_menos <- sum(resultado$`C-`,na.rm = TRUE)
C <- sum(resultado$C,na.rm = TRUE)
D_mas <- sum(resultado$`D+`,na.rm = TRUE)
D <- sum(resultado$D,na.rm = TRUE)
E <- sum(resultado$E,na.rm = TRUE)

TotalAL <- AB + C_mas + C_menos + C + D_mas + D + E

PAB <- AB/TotalAL
PC_mas <- C_mas/TotalAL
PC_menos <- C_menos/TotalAL
PC <- C/TotalAL
PD_mas <- D_mas/TotalAL
PE <- E/TotalAL
PD <- D/TotalAL

PAL<- c(PAB,PC,PC_mas,PC_menos,PD,PD_mas,PE)
PAL

# Transporte
resultado_2 <- resultado_2[,-c(9,10,11,12,13,14,15)]
colnames(resultado_2)
colnames(resultado_2)<- c("ZM","AB","C","C+","C-","D","D+","E")

ncol(resultado_2)

suma_2 <- apply(resultado_2[,-1], MARGIN = 2,
                FUN=sum,na.rm = TRUE)
suma_2
TotalTR <- sum(suma_2)


Porc_T <- suma_2/TotalTR


# Educacion


resultado_3 <- resultado_3[,-c(9,10,11,12,13,14,15)]
colnames(resultado_3)
colnames(resultado_3)<- c("ZM","AB","C","C+","C-","D","D+","E")
suma_3 <- apply(resultado_3[,-1], MARGIN=2 ,FUN=sum, na.rm=TRUE)

suma_3

TotalED <- sum(suma_3)
Porc_ED <- suma_3/TotalED

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

e <- rbind(e,PAL)
colnames(e)<- c("AB","C","C+","C-","D","D+","E")

e<- rbind(e,Porc_T)
Porc_T
e<-rbind(e,Porc_ED)

e <- e[-c(2,3),]

e$ZM <- "TOTAL PAÍS"
e$variable <- "Gasto"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Alimentos"

d <- rbind(d,long_df)

# Transporte
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


e <- rbind(e,Porc_T)
colnames(e)<- c("AB","C","C+","C-","D","D+","E")

Porc_T
e<- rbind(e,Porc_T)
#e<-rbind(e,Porc_T)

e <- e[-2,]

e$ZM <- "TOTAL PAÍS"
e$variable <- "Gasto"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Transporte"

d <- rbind(d,long_df)


# Educacion

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


e <- rbind(e,Porc_ED)
Porc_ED
colnames(e)<- c("AB","C","C+","C-","D","D+","E")

e$ZM <- "TOTAL PAÍS"
e$variable <- "Gasto"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Educación"

d <- rbind(d,long_df)

for(i in 1:length(d$Porcentaje)){
  if(is.na(d$Porcentaje[i])){
    d$Porcentaje[i]<- 0
  }
}

d$Porcentaje <- as.double(d$Porcentaje)

#k <- aggregate(
#  x =list(suma = d[,'Porcentaje']),
#  by = list(ZM = d[,'ZM']),
#  FUN = sum
  
#)

writexl::write_xlsx(d,"BASE_AL_TR_ED.xlsx")

76*7*3

table(d$ZM)

length(unique(d$Porcentaje))


########



base_2 <- read.xlsx("BASE_AL_TR_ED.xlsx")

vector <- c()
cont <- 0

for(i in 1:length(base_2$ZM)-1){
  cont <- 0
  if(base_2$ZM[i+1]==base_2$ZM[i]){
    cont <- cont + base_2$Porcentaje[i]
  }
  vector <- append(vector,cont)
}

base_alimentos <- base_2[base_2$valor == "Alimentos",]
base_transporte <- base_2[base_2$valor == "Transporte",]
base_educacion <- base_2[base_2$valor == "Educación",]


valid_al <- summarise(group_by(base_alimentos,ZM),validacion=sum(Porcentaje))
valid_tr <- summarise(group_by(base_transporte,ZM),validacion=sum(Porcentaje))
valid_ed <- summarise(group_by(base_educacion,ZM),validacion=sum(Porcentaje))

View(resultados_env[["SAN FRANCISCO DEL RINCÓN"]])

vector_2 <- c()
vector<-c()
vector_3 <- c()

for(i in ls(resultados_env)){
  p <- apply(resultados_env[[i]][,-1], MARGIN = 2, FUN = as.double)
  vector <- apply(p, MARGIN = 2, FUN = sum)
  vector_2 <- append(vector_2,vector)
  vector_3<-append(vector_3,i)
}


table(vector_2)

l<- resultados_env[[i]][-1,-1]
p <- apply(resultados_env[[i]][,-1], MARGIN = 2, FUN = as.double)
apply(p, MARGIN = 2, FUN = sum)
?group_by
i

# long_df <- long_df[, c("Nivel_de_educacion", "NSE", "Porcentaje", 
#                       "ZM", "variable")]


