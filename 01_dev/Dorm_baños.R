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
Base_baños <- readxl::read_xlsx('Base_original.xlsx')

Base_baños[Base_baños$cuart_dorm==0,]
table(Base_baños$cuart_dorm/Base_baños$factor)

# Zonas metropolitanas
# zonas_met <- read.xlsx(xlsxFile = '../Practicas 2024/Composicion de ZMs.xlsx', sheet = "oficial")

# Base_baños <- read.xlsx(xlsxFile = '../Practicas 2024/Base_original.xlsx')

zonas_met_no_rep <- c()
zonas_met_no_rep <- zonas_met %>%
  group_by(zonas_met$ubic_geo) %>%
  summarise(across(everything(), ~toString(unique(.))))

zonas_met_no_rep <- subset(zonas_met_no_rep, select = c(ubic_geo,ZM))


Base <- Base_baños

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
Base_resultante <- subset(Base_resultante, select = c(ubic_geo,ZM,cuart_dorm, bano_comp,
                                                      bano_excus, bano_regad,NSE_NUEVO,factor))

# Muestra 
muestra_dorm_ZM <- data.frame(table(Base_resultante$ZM))

total <- length(Base_resultante$ZM)-sum(table(Base_resultante$ZM))
total
vec <- c("sin zona",total)


muestra_dorm_ZM<-rbind(muestra_dorm_ZM,vec)

# sum(table(Tabla$ZM))

# writexl::write_xlsx(muestra_dorm_ZM, "Muestras_dorm_ZM.xlsx")

# summarise(group_by(Base_original,NSE_NUEVO),suma_alimentos = sum(alimentos))


# Creamos la tabla dinamica 
resultado_dorm <- Base_resultante %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_dorm = sum(cuart_dorm)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_dorm / sum(Suma_dorm)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_dorm, Porcentaje_total))

resultado_bcomp <- Base_resultante %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_bcomp = sum(bano_comp)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_bcomp / sum(Suma_bcomp)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_bcomp, Porcentaje_total))

resultado_bexcus <- Base_resultante %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_bexcus = sum(bano_excus)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_bexcus / sum(Suma_bexcus)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_bexcus, Porcentaje_total))


resultado_bregad <- Base_resultante %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_bregad = sum(bano_regad)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_bregad / sum(Suma_bregad)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_bregad, Porcentaje_total))


resultado_dorm <- resultado_dorm[,-c(2,3,4,5,6,7,8)]
resultado_bregad <- resultado_bregad[,-c(2,3,4,5,6,7,8)]
resultado_bcomp <- resultado_bcomp[,-c(2,3,4,5,6,7,8)]
resultado_bexcus <- resultado_bexcus[,-c(2,3,4,5,6,7,8)]

colnames(resultado_dorm)
colnames(resultado_dorm)<- c("ZM","AB","C","C+","C-","D","D+","E")
colnames(resultado_bregad)
colnames(resultado_bregad)<- c("ZM","AB","C","C+","C-","D","D+","E")
colnames(resultado_bexcus)
colnames(resultado_bexcus)<- c("ZM","AB","C","C+","C-","D","D+","E")
colnames(resultado_bcomp)
colnames(resultado_bcomp)<- c("ZM","AB","C","C+","C-","D","D+","E")

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

rrr <- resultado_bcomp$ZM

rrr <-na.omit(rrr)
rrr <- append(rrr,"sin zona")

row.names(valid_2)<- rrr
valid_2$ZM<- rrr

valid_2 <-valid_2[,c(5,1,2,3,4)]


writexl::write_xlsx(valid_2,"Validacion_Dorm.xlsx")


## DORMITORIOS
resultado_dorm$variable <- "Dormitorios"

long_df <- gather(resultado_dorm, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Número de dormitorios"

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

d<- d[-1,]



## BAÑO CON REGADERAS 
resultado_bregad$variable <- "Baño con regadera"

long_df <- gather(resultado_bregad, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Número de baños con regadera"

long_df <- long_df[order(long_df$ZM),]


d <- rbind(d,long_df)

# Baño con excusado 
resultado_bexcus$variable <- "Baño con excusado"

long_df <- gather(resultado_bexcus, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Número de baños con excusado"

long_df <- long_df[order(long_df$ZM),]


d <- rbind(d,long_df)

# Baño completo 
resultado_bcomp$variable <- "Baño completo"

long_df <- gather(resultado_bcomp, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Número de baños completos"

long_df <- long_df[order(long_df$ZM),]


d <- rbind(d,long_df)

## Total de baños completos 
resultado_bcomp <- Base_resultante %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_bcomp = sum(bano_comp)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_bcomp / sum(Suma_bcomp)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_bcomp, Porcentaje_total))

resultado_bcomp <- resultado_bcomp[,-c(9,10,11,12,13,14,15)]
colnames(resultado_bcomp)
colnames(resultado_bcomp)<- c("ZM","AB","C","C+","C-","D","D+","E")


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


e$ZM <- "TOTAL PAÍS"
e$variable <- "Baño completo"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Número de baños completos"

# Validacion
pp <- apply(long_df, MARGIN = 2, FUN=as.double)
rr<-apply(pp,MARGIN = 1,FUN=sum,na.rm=TRUE)
sum(rr)

d <- rbind(d,long_df)

## Baños con regadera
resultado_bregad <- Base_resultante %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_bregad = sum(bano_regad)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_bregad / sum(Suma_bregad)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_bregad, Porcentaje_total))

resultado_bregad <- resultado_bregad[,-c(9,10,11,12,13,14,15)]
colnames(resultado_bregad)
colnames(resultado_bregad)<- c("ZM","AB","C","C+","C-","D","D+","E")

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


e$ZM <- "TOTAL PAÍS"
e$variable <- "Baño con regadera"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Número de baños con regadera"

# Validacion
pp <- apply(long_df, MARGIN = 2, FUN=as.double)
rr<-apply(pp,MARGIN = 1,FUN=sum,na.rm=TRUE)
sum(rr)

d <- rbind(d,long_df)


# Baño con excusado
resultado_bexcus <- Base_resultante %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_bexcus = sum(bano_excus)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_bexcus / sum(Suma_bexcus)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_bexcus, Porcentaje_total))

resultado_bexcus <- resultado_bexcus[,-c(9,10,11,12,13,14,15)]
colnames(resultado_bexcus)
colnames(resultado_bexcus)<- c("ZM","AB","C","C+","C-","D","D+","E")

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


e$ZM <- "TOTAL PAÍS"
e$variable <- "Baño con excusado"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Número de baños con excusado"

d <- rbind(d,long_df)

# Dormitorios
resultado_dorm <- Base_resultante %>%
  group_by(ZM, NSE_NUEVO) %>%
  summarise(Suma_dorm = sum(cuart_dorm)) %>%
  # group_by(NSE_NUEVO) %>%
  mutate(Porcentaje_total = Suma_dorm / sum(Suma_dorm)) %>%
  pivot_wider(names_from = NSE_NUEVO, values_from = c(Suma_dorm, Porcentaje_total))

resultado_dorm <- resultado_dorm[,-c(9,10,11,12,13,14,15)]
colnames(resultado_dorm)
colnames(resultado_dorm)<- c("ZM","AB","C","C+","C-","D","D+","E")

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


e$ZM <- "TOTAL PAÍS"
e$variable <- "Dormitorios"

long_df <- gather(e, key = "NSE",
                  value = "Porcentaje",
                  -c(ZM, variable))

long_df$valor <- "Número de dormitorios"

d <- rbind(d,long_df)

for(i in 1:length(d$Porcentaje)){
  if(is.na(d$Porcentaje[i])){
    
    d$Porcentaje[i]<-0
  }
}

for(i in 1:length(d$ZM)){
  if(is.na(d$ZM[i])){
    d$ZM[i]<-"SIN ZONA"
  }
}


4*76*7
#condicion_4 <- !is.na(Base_resultante$ZM)

#Base_resultante <- Base_resultante[condicion_4,]

Base_resultante$condicion <- Base_resultante$bano_comp / Base_resultante$factor
Base_resultante$condicion <- ifelse(Base_resultante$condicion>4,5,
                                    Base_resultante$condicion )

Base_resultante$condicion_2 <- Base_resultante$bano_excus / Base_resultante$factor
Base_resultante$condicion_2 <- ifelse(Base_resultante$condicion_2>4,5,
                                    Base_resultante$condicion_2 )

Base_resultante$condicion_3 <- Base_resultante$bano_regad / Base_resultante$factor
Base_resultante$condicion_3 <- ifelse(Base_resultante$condicion_3>4,5,
                                      Base_resultante$condicion_3 )

#Base_resultante$suma <-   Base_resultante$condicion

#Base_resultante$condicion <- Base_resultante$suma

#Base_resultante$condicion <- ifelse(Base_resultante$condicion>4,5,
#                                    Base_resultante$condicion )

table(Base_resultante$condicion)



x<-group_by(Base_resultante,NSE_NUEVO,condicion)

y<-summarise(x,"n"=length(condicion))

r<-group_by(y,NSE_NUEVO)

r <- mutate(r,"suma"=sum(n))

r$porc <- round(r$n/r$suma,3)


orden_filas <- c("A/B","C+","C","C-","D+","D","E")
r$NSE2 <- factor(r$NSE_NUEVO,levels = orden_filas)

write.csv(r,"baños_ver.csv")


#grafica <- grafica[condicion,]

ggplot(r, aes(x = NSE2, y = porc, fill = condicion)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label =ifelse(round(porc,4)>0.001,
                              100*round(porc,4),"")),
            position = position_stack(vjust = 0.5),
            size = 3)+
  labs(title = "Número de baños (%)",
       y = "Porcentaje",
       x = "NSE",
       fill = "Número de baños completos") +
  scale_fill_viridis_c(option = "H")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.7)))


# Gráfica de número de dormitorios

Base_resultante$condicion <- Base_resultante$cuart_dorm / Base_resultante$factor
Base_resultante$condicion <- ifelse(Base_resultante$condicion>4,5,
                                    Base_resultante$condicion )
table(Base_resultante$condicion)

x<-group_by(Base_resultante,NSE_NUEVO,condicion)

y<-summarise(x,"n"=length(condicion))

r<-group_by(y,NSE_NUEVO)

r <- mutate(r,"suma"=sum(n))

r$porc <- round(r$n/r$suma,3)


orden_filas <- c("A/B","C+","C","C-","D+","D","E")
r$NSE2 <- factor(r$NSE_NUEVO,levels = orden_filas)




#grafica <- grafica[condicion,]

write.csv(r,"dormitorios.csv")


ggplot(r, aes(x = NSE2, y = porc, fill = condicion)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(round(porc, 4) > 0.001,
                               paste0(100 * round(porc, 4)),
                               "")),
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(title = "Número de dormitorios (%)",
       y = "Porcentaje",
       x = "NSE",
       fill = "Dormitorios") +
  scale_fill_viridis_c(option = "H", aesthetics = "fill") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  guides(fill = guide_legend(override.aes = list(alpha = .8)))




writexl::write_xlsx(d,"BASE_BAÑOS_DORMITORIOS.xlsx")
