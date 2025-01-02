-----#### JORGE RAMIREZ #####
### Fecha 23/02/2023 
### Resumen de Check Zonales ### 


rm(list=ls()) # Borrar los datos de entorno cargados en memoria 
setwd("C:/Users/asus-tuf/Documents/GitHub/CheckList")


# instalando librerias o paquetes-------
#install.packages("tidyverse") # instalo el paquete
#install.packages("tidyverse")
library(tidyverse)

tidyverse_packages()


# Importo el Excel


CheckT <- readxl::read_excel("Check list - visita GERENTE ZONAL.xlsx", sheet = 1, col_names = T, col_types = NULL) #leo el archivo con datos de checks lo convierto DF

### Le doy formato a las columnas en referencia a sus datos


CheckT$`Hora de inicio` <- as.Date(CheckT$`Hora de inicio`)

class(CheckT$`Hora de inicio`)

### Convertir fecha inicio en fecha para R 
############################################
###########################################
desde <- as.Date("2023-10-15")           ##
#hasta <- as.Date("2023-12-31")          ##
                                         ##
###########################################
############################################

Fecha1 <- CheckT$`Hora de inicio`< desde
print(Fecha1)
Checkdf <- CheckT[!Fecha1,] 

Checkdf$Nombre <-as.factor(Checkdf$Nombre)
levels(Checkdf$Nombre)


#### contar apariciones de ";" que significa 1 tilde

Checkdf <- mutate(Checkdf, QEle = str_count(Checkdf$`CARPETA DE GESTIÓN ELECTRO`, pattern = ";"))
Checkdf <- mutate(Checkdf, QAfil = str_count(Checkdf$`CARPETA DE GESTIÓN AFILIACIONES`, pattern = ";"))
Checkdf <- mutate(Checkdf, QSup = str_count(Checkdf$`CARPETA DE GESTIÓN SUPER`, pattern = ";"))
Checkdf <- mutate(Checkdf, QControles = str_count(Checkdf$`RESPECTO A LOS CONTROLES EN SUCURSAL, esta ok ....  ¿?`, pattern = ";"))


### Gráfico descargo librerias -----
### ggthemr es una de muchas para darle color a los gráficos
#install.packages('patchwork')-

#install.packages("ggthemes")


library(dplyr)
library(ggplot2)
library(patchwork)
library(ggthemes)
library(scales)
library(forcats)

Checkdf2 <- Checkdf [c(4, 18, 19, 20, 21 )]

Checkdf2[is.na(Checkdf2)] <- 0

str(Checkdf2)

### genero objetos con Themes para Charts.

tm1 <- theme( plot.background = element_rect(fill = "light grey", ),
  panel.background = element_rect(fill = "#EEAD0E"),
  axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) 

tm2 <- theme(plot.background = element_rect(color = "blue"),
  panel.background = element_rect(fill = "light blue", color = "grey"))



### Genero un resumen agrupado por Gte Zonal
Checkdf3<- Checkdf2 %>%
  group_by(Nombre) %>%
  summarise(
    QEle = sum(QEle),
    QAfil = sum(QAfil),
    QSup = sum(QSup),
    QControles = sum(QControles)
  )


QEMax  <- round(max(Checkdf3$QEle)*1.2, digits = 0)
QAMax  <- round(max(Checkdf3$QAfil)*1.2, digits = 0)
QSMax  <- round(max(Checkdf3$QSup)*1.2, digits = 0)
print(QEMax)
      print(QAMax)
            print(QSMax)


### Generar los charts

ChE <- ggplot(Checkdf3, aes(Nombre, QEle)) + geom_bar(stat = "identity", fill = "green") +
  labs(title = print(paste('Cant Total de items Revisados desde', desde)),
       subtitle = 'Carpeta de Gestión Electro',
       tag = NULL,
       caption = NULL,
       y = 'Cantidad',
       x = 'Zonales') +   
  ylim(c(0, QEMax))+
  geom_text(aes(label=QEle), stat = "identity", vjust=-0.3, color="black",
            position = position_dodge(0.9),  size=4.0)+ tm1

ChE
ChA <- ggplot(Checkdf3, aes(Nombre, QAfil)) + geom_bar(stat = "identity", fill = "purple")+
  labs(subtitle = 'Carpeta de Gestión Afiliaciones',
       tag = NULL,
       caption = NULL,
       y = 'Cantidad',
       x = 'Zonales') +
  ylim(c(0, QAMax))+
  geom_text(aes(label=QAfil), vjust=-0.3, color="black",
            position = position_dodge(0.9),  size=4.0)+ tm1

ChS <- ggplot(Checkdf3, aes(Nombre, QSup)) + geom_bar(stat = "identity", fill = "blue")+
  labs(subtitle = 'Carpeta de Gestión Super',
       tag = NULL,
       caption = NULL,
       y = 'Cantidad',
       x = 'Zonales') +
  ylim(c(0, QSMax))+
  geom_text(aes(label=QSup), stat = "identity", vjust=-0.3, color="black",
            position = position_dodge(0.9),  size=4.0)+ tm1

Chedf4 <- ggplot(Checkdf2, aes(Nombre, QControles)) + 
  geom_col(fill = "#8FBC8F", color = "dark green") +
  labs(title = 'Cant de Controles realizados',
       subtitle = 'Sucursales',
       tag = NULL,
       caption = 'En el gráf: cada cuadro = 1 Archivo y dentro la cant. de controles',
       y = 'Cantidad Controles',
       x = 'Zonales') +
  geom_text(aes(label = QControles), color="blue",
            position = position_stack(0.6),  size=4.0) + tm2

df_QCZ  <- Checkdf2%>% count (Nombre)


QCZ  <- ggplot(Checkdf2, aes(Nombre)) + geom_dotplot(aes(fill = Nombre), dotsize = 1.7) + 
  labs(title = 'Cant de Checks realizados',
       subtitle = NULL,
            tag = NULL,
        caption = 'En el gráf: cada bolita = 1 Check',
              y = NULL,
              x = 'Cant de Checks')+ 
  theme(legend.position='none', 
        plot.background = element_rect(fill = "light grey", ))

  (ChE/ChS/ChA) | QCZ


### Proyecto diferenciar controles
colnames(Checkdf) [c(10)] <- 'ControlesHechos'

DFCZ <- Checkdf [c(2,5,6,10)]
DFCZ <- mutate(DFCZ, Ele_OpDudosas = str_count(Checkdf$ControlesHechos, pattern = "operaciones dudosas"))
DFCZ <- mutate(DFCZ, Ele_Remitos = str_count(Checkdf$ControlesHechos, pattern = "remitos de clientes"))
DFCZ <- mutate(DFCZ, Ele_Caja = str_count(Checkdf$ControlesHechos, pattern = "ultimo arqueo"))
DFCZ <- mutate(DFCZ, Ele_StockHist = str_count(Checkdf$ControlesHechos, pattern = "control de stock"))
DFCZ <- mutate(DFCZ, Ele_Posventa = str_count(Checkdf$ControlesHechos, pattern = "post venta"))



#rm(DFCZ_largo)
DFCZ_largo <- DFCZ %>% tidyr::gather(key = "controlado", valores, c(5:9))
DFCZ_largo [is.na(DFCZ_largo)] <- 0



RCZ_dud <- DFCZ %>%   ggplot(., aes(Ele_OpDudosas, Nombre, fill = Nombre))+
  geom_col() + 
  labs(title = print(paste('Cant Total de items Revisados desde', desde)),x="Control de Operaciones dudosas", y= "Cantidad") #+ RSform

RCZ_rem <- DFCZ %>%   ggplot(., aes(Ele_Remitos, Nombre, fill = Nombre))+
  geom_col() + 
  labs(x="Control de Remitos", y= "Cantidad") #+ RSform

RCZ_caja <- DFCZ %>%   ggplot(., aes(Ele_Caja, Nombre, fill = Nombre))+
  geom_col() + 
  labs(x="Control de Arqueo de Caja", y= "Cantidad") #+ RSform

  RCZ_posv <- DFCZ %>%   ggplot(., aes(Ele_Posventa, Nombre, fill = Nombre))+
  geom_col() + 
  labs(x="Control de Clasif y Stock Pos Venta", y= "Cantidad") #+ RSform

RCZ_stock <- DFCZ %>%   ggplot(., aes(Ele_StockHist, Nombre, fill = Nombre))+
  geom_col() + 
  labs(x="Revisión de controles stock", y= "Cantidad") #+ RSform



RCZ_dud/RCZ_caja/RCZ_posv/RCZ_rem/RCZ_stock




summarise(Checkdf3$Nombre = count(Checkdf3$Nombre))

### ver valores "a medias" en Infraestructura

am1 <- Checkdf$`INFRAESTRUCTURA,  el local se encuentra en condiciones respecto a las luces ,  techos , paredes , pisos , baños, humedad `!= "a medias"
print(am1)

Ch_Infra <- Checkdf[!am1,] 
Ch_Infra <- Ch_Infra [c(2,5,6, 12, 17)]

print(Ch_Infra)


Ch_OBS_Suc <- Checkdf [c(2,5,6, 12, 17)]
comentarios <- Checkdf [c(2,5,6, 12:17)]

#install.packages("writexl")
library("writexl")

write_xlsx(Ch_Infra, "Infra.xlsx")
write_xlsx(Ch_OBS_Suc, "Observaciones en Sucursales.xlsx")
write_xlsx(comentarios, "Comentartios.xlsx")



#### TAblas.. presentar, posibilidades -----

a <- gtable(unit(1:3, c("cm")), unit(5, "cm"))
a
gtable_show_layout(a)
print.data.frame(Ch_Infra)

#install.packages(gridExtra)
library(gridExtra)
pdf("test.pdf", height=3, width=5)
write.(Ch_Infra, file ="")
grid.table(Ch_Infra)
dev.off()
       
