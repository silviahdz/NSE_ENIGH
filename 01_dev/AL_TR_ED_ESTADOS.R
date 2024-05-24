library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(openxlsx)
library(reshape2)
library(tidyr)
library(readxl)

# Cargamos las bases de datos 
#setwd("C:/Users/52238/OneDrive/Documentos/Practicas 2024")
setwd("../00_resources")

Estado <- read.xlsx(xlsxFile = 'Cod_Estado.xlsx')

Base <- readxl::read_xlsx('Base de datos AL_TR_ED.xlsx')


for(i in 1:length(Base$folioviv)){
  if(nchar(Base$folioviv[i])==9){
    Base$folioviv[i] <- paste(0,Base$folioviv[i],sep = "")  
  }
}

table(nchar(Base$folioviv))

Base$ENTIDAD <- substring(Base$folioviv,1,2)

Base_mod <- merge(
  y = Base, 
  x = Estado, 
  by = 'ENTIDAD'
)

Base_mod <- Base_mod[,-c(3,4,5,6,8,9,10)]

#writexl::write_xlsx(Base_mod,"BASE_Alim.xlsx")

unique(Base_mod$NOM_ENT)

# Tabla sin NA en NSE
unique(Base_mod$NOM_ENT)

nueva_tabla <- 0
condicion <- !is.na(Base_mod$NSE_NUEVO)

nueva_tabla <- Base_mod[condicion,]

unique(nueva_tabla$NSE_NUEVO)

Base_mod <- nueva_tabla

### Muestra 

unique(Base_mod$NOM_ENT)
muestra_ESTADO_al_ed_tr <- data.frame(table(Base_mod$NOM_ENT))

sum(table(Base_mod$NOM_ENT))
#writexl::write_xlsx(muestra_ESTADO_al_ed_tr, "Muestras_ESTADO_al_tr_ed.xlsx")

##### total de la muestra por estado 



#######
Base_original <- Base_mod
# Creamos la tabla dinamica 
resultado <- Base_original %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_alimentos = sum(alimentos)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_alimentos / sum(Suma_alimentos)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_alimentos, Porcentaje_total))

# Creamos la tabla dinamica 
resultado_2 <- Base_original %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_transporte = sum(transporte)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_transporte / sum(Suma_transporte)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_transporte, Porcentaje_total))

resultado_3 <- Base_original %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
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
colnames(resultado) <- c("ESTADO","AB","C+","C","C-","D+","D","E")
colnames(resultado_2) <- c("ESTADO","AB","C+","C","C-","D+","D","E")
colnames(resultado_3) <- c("ESTADO","AB","C+","C","C-","D+","D","E")



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

rrr <- resultado$ESTADO

#rrr <-na.omit(rrr)
#rrr <- append(rrr,"sin zona")

row.names(valid_2)<- rrr
valid_2$ESTADO<- rrr

valid_2 <-valid_2[,c(4,1,2,3)]

# writexl::write_xlsx(valid_2,"Validacion_AL_ESTADOS.xlsx")



resultado$variable <- "Gasto"


long_df <- gather(resultado, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

long_df$valor <- "Alimentos"

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

d <- d[-1,]


####### Transporte 
resultado_2$variable <- "Gasto"


long_df <- gather(resultado_2, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

long_df$valor <- "Transporte"

long_df <- long_df[order(long_df$ESTADO),]


d <- rbind(d,long_df)


####### Educacion

resultado_3$variable <- "Gasto"


long_df <- gather(resultado_3, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

long_df$valor <- "Educación"

long_df <- long_df[order(long_df$ESTADO),]


d <- rbind(d,long_df)

### TOTAL PAÍS

resultado <- Base_original %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_alimentos = sum(alimentos)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_alimentos / sum(Suma_alimentos)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_alimentos, Porcentaje_total))

# Creamos la tabla dinamica 
resultado_2 <- Base_original %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_transporte = sum(transporte)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_transporte / sum(Suma_transporte)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_transporte, Porcentaje_total))

resultado_3 <- Base_original %>%
  group_by(NOM_ENT, NSE_NUEVO) %>%
  summarise(Suma_educacion = sum(educacion)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_educacion / sum(Suma_educacion)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_educacion, Porcentaje_total))


resultado <- resultado[,-c(9,10,11,12,13,14,15)]
colnames(resultado)
colnames(resultado)<- c("ESTADO","AB","C","C+","C-","D","D+","E")

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

# Transporte
resultado_2 <- resultado_2[,-c(9,10,11,12,13,14,15)]
colnames(resultado_2)
colnames(resultado_2)<- c("ESTADO","AB","C","C+","C-","D","D+","E")

ncol(resultado_2)

suma_2 <- apply(resultado_2[,-1], MARGIN = 2,
                FUN=sum,na.rm = TRUE)
suma_2

TotalTR <- sum(suma_2)


Porc_T <- suma_2/TotalTR
Porc_T

# Educacion


resultado_3 <- resultado_3[,-c(9,10,11,12,13,14,15)]
colnames(resultado_3)
colnames(resultado_3)<- c("ESTADO","AB","C","C+","C-","D","D+","E")
suma_3 <- apply(resultado_3[,-1], MARGIN=2 ,FUN=sum, na.rm=TRUE)

TotalED <- sum(suma_3)

Porc_ED <- suma_3/TotalED
Porc_ED

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
PAL
colnames(e)<- c("AB","C","C+","C-","D","D+","E")

e<- rbind(e,Porc_T)
Porc_T
e<-rbind(e,Porc_ED)

e <- e[-c(2,3),]

e$ESTADO <- "TOTAL PAÍS"
e$variable <- "Gasto"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

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

e$ESTADO <- "TOTAL PAÍS"
e$variable <- "Gasto"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

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

e$ESTADO <- "TOTAL PAÍS"
e$variable <- "Gasto"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ESTADO, variable))

long_df$valor <- "Educación"

d <- rbind(d,long_df)

for(i in 1:length(d$Porcentaje)){
  if(is.na(d$Porcentaje[i])){
    d$Porcentaje[i]<- 0
  }
}

d$Porcentaje <- as.double(d$Porcentaje)

for(i in 1:length(d$ESTADO)){
  if(d$ESTADO[i]=="TOTAL PAÍS"){
    d$ESTADO[i]<-"Total país"
  }
}


#1-valid_2$Alimentos



#sum(1-valid_2$Alimentos)
#sum(resultado$Porcentaje_total_NA)

#sum(1-valid_2$Transporte)
#sum(resultado_2$Porcentaje_total_NA)

#sum(1-valid_2$Educación)
#sum(resultado_3$Porcentaje_total_NA)


# writexl::write_xlsx(d,"BASE_AL_TR_ED_ESTADOS.xlsx")

