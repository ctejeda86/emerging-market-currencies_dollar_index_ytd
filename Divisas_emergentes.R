dolar<-read.table(file="dol_1409.txt",header=TRUE,sep="\t", dec=".")
head(dolar)


library(dplyr)
#dolar<-dolar[-1,]
títulos<-c("fecha","cierre")
names(dolar)=títulos

library(lubridate)
dolar$fecha<-dmy(dolar$fecha)

#ver últimos datos del df
tail(dolar)

#Ver desde cuándo es su menor nivel
dolar$fecha[which(dolar$cierre<21.089)]
#en la fila 48 está el más reciente
dolar$cierre[48]

#ver máximo
dolar$fecha[which.max(dolar$cierre)]
#57
#ver mínimo
which.min(dolar$cierre)
#31
#crear columna para etiquetas
dolar$lab<-""
indexlab<-c(1,31,57,179)
dolar$lab[indexlab]<-dolar$cierre[indexlab]

###Establecer tema para mis gráficas
My_theme<-theme(panel.grid.major = element_blank(),    #strip major gridlines
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),#strip minor gridlines
                axis.ticks = element_blank(), 
                axis.title.x = element_text(size = 10,family="gochi"),
                axis.text.x = element_text(size = 8,family="gochi"),
                axis.title.y = element_text(size = 10,family="gochi"),
                axis.text.y = element_text(size=8,family="gochi"),
                plot.title = element_text(size=12,family="gochi"),
                plot.caption = element_text(size=9,family ="gochi"))


#librerias
library(scales)
library(showtext)
library(sysfonts)
library(ggrepel)
showtext_auto()
#gráfica
x11()
gdolar<-ggplot(data=dolar,aes(x=fecha,y=cierre,label=lab))+
  geom_line(color="lightblue3",size=1)+
  labs(x="",y="Pesos por dólar",
       caption = "Fuente: Banxico",
       title = "USD/MXN en 2020")+
  #agregar theme propio
  My_theme+
  ####esto se agrega a my_theme
  #axis ticks para meses en eje x
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%B"))+
  #etiquetas para datos a resaltar
  geom_label_repel(family="gochi",size=4)

########
#IMPORTAR DATOS DE ÍNDICE DÓLAR
index_dol<-read.table(file="index_dolar.txt",header=TRUE,sep="\t",dec=".")
head(index_dol)
nombres2<-c("fecha","open","high","low","ultimo")
names(index_dol)=nombres2
index_dol$fecha<-dmy(index_dol$fecha)
#seleccionar solo datos del 2020
index20<-index_dol %>%
  filter( fecha >=("2019-12-31") & fecha <=("2020-09-14")) %>%
  select(fecha, ultimo)

#crear columna para etiquetas
index20$lab<-""
which.min(index20$ultimo)
#175
which.max(index20$ultimo)
#59
lab2<-c(1,59,179)
index20$lab[lab2]<-index20$ultimo[lab2]

#gráfico
index_dolar<- ggplot(data=index20,aes(x=fecha,y=ultimo,label=lab))+
    geom_line(color="lightblue3",size=1)+
    labs(title="Índice dolar",
         caption="Fuente:Refinitiv",
         x="Mes",y="Puntos")+
    My_theme+
    geom_label_repel(family="gochi",size=4)+
    scale_x_date(breaks = date_breaks("1 month"),
                 labels = date_format("%B"))

em<-read.table(file="emergentes.txt",sep="\t",dec=".",
               header=TRUE)
#cambiar el primer nombre porque no se leía correctamente
em$Moneda[1]<-"Real brasileño"

#gráfica
gem<-ggplot(data=em,aes(x=reorder(Moneda,-Var_porce),y=Var_porce,
                   label=Var_porce))+
  geom_bar(stat="identity",fill="lightblue3")+
  labs(x="Divisa",y="Var. %",
       title="Depreciación de monedas emergentes (al 14/09)",
       caption="Fuente: Refinitiv")+
  My_theme+
  geom_text(family="gochi")


#arreglar varias gráficas en una sola ventana
library(grid)
library(gridExtra)
x11()
grid.arrange(gdolar,                                    # bar plot spaning two columns
             index_dolar, gem,                               # box plot and scatter plot
             ncol = 2, nrow = 2, 
             layout_matrix = rbind(c(1,1), c(2,3)),
             top = textGrob("Divisas emergentes e índice dólar en 2020",
                            gp=gpar(fontsize=15,font=2,
                                    fontfamily="gochi")))
