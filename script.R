# Instalar paquetes ####

## readxl para importar planillas excel ####
if (!require('readxl'))
  install.packages("readxl")
library(readxl) 

if (!require('dplyr'))
  install.packages("dplyr")
library(dplyr) 

if (!require('tidyr'))
  install.packages("tidyr")
library(tidyr) 

if (!require('data.table'))
  install.packages("data.table")
library(data.table) 



# Importar datos ###

# Se deben descargar las planillas del repositorio https://www.sedeco.gov.py/index.php/publicaciones/monitoreo-canasta-familiar #

## Importamos los datos de la planilla del 2015 ####

datos2015 <- read_excel("tp/data/Monitoreo de Precios 2015.xlsx", sheet = 1, col_names=TRUE, skip = 9)

# Borramos la primera columna #

datos2015 <- datos2015 %>% select(-1)

# Modificamos el nombre de la columna por el que corresponde #

datos2015 <-datos2015 %>% rename("Enero"="42005")
datos2015 <-datos2015 %>% rename("Febrero"="42036")
datos2015 <-datos2015 %>% rename("Marzo"="42064")
datos2015 <-datos2015 %>% rename("Abril"="42095")
datos2015 <-datos2015 %>% rename("Mayo"="42125")
datos2015 <-datos2015 %>% rename("Junio"="42156")
datos2015 <-datos2015 %>% rename("Julio"="42186")
datos2015 <-datos2015 %>% rename("Agosto"="42217")
datos2015 <-datos2015 %>% rename("Septiembre"="42248")
datos2015 <-datos2015 %>% rename("Octubre"="42278")
datos2015 <-datos2015 %>% rename("Noviembre"="42309")
datos2015 <-datos2015 %>% rename("Diciembre"="42339")

# Convertimos datos anchos a datos altos #

datos2015<-datos2015%>%gather(key="Mes",value="Precio", -RUBRO)%>% 
  #Ordenamos por proceso
  arrange(RUBRO)

datos2015$Año <- "2015"

## Importamos los datos de la planilla del 2016 ####

datos2016 <- read_excel("tp/data/Monitoreo de Precios 2016.xlsx", sheet = 1, col_names=TRUE, skip = 9)

# Borramos la primera columna #

datos2016 <- datos2016 %>% select(-1)

# Modificamos el nombre de la columna por el que corresponde #

datos2016 <-datos2016 %>% rename("Enero"="42370")
datos2016 <-datos2016 %>% rename("Febrero"="42401")
datos2016 <-datos2016 %>% rename("Marzo"="42430")
datos2016 <-datos2016 %>% rename("Abril"="42461")
datos2016 <-datos2016 %>% rename("Mayo"="42491")
datos2016 <-datos2016 %>% rename("Junio"="42522")
datos2016 <-datos2016 %>% rename("Julio"="42552")
datos2016 <-datos2016 %>% rename("Agosto"="42583")
datos2016 <-datos2016 %>% rename("Septiembre"="42614")
datos2016 <-datos2016 %>% rename("Octubre"="42644")
datos2016 <-datos2016 %>% rename("Noviembre"="42675")
datos2016 <-datos2016 %>% rename("Diciembre"="42705")

# Convertimos datos anchos a datos altos #

datos2016<-datos2016%>%gather(key="Mes",value="Precio", -RUBRO)%>% 
  #Ordenamos por proceso
  arrange(RUBRO)

datos2016$Año <- "2016"

datos2016 <- transform(datos2016, Precio = as.numeric(Precio))

## Importamos los datos de la planilla del 2017 ####

datos2017 <- read_excel("tp/data/Monitoreo de Precios 2017.xlsx", sheet = 1, col_names=TRUE, skip = 9)

# Borramos la primera columna #

datos2017 <- datos2017 %>% select(-1)

# Modificamos el nombre de la columna por el que corresponde #

datos2017 <-datos2017 %>% rename("Enero"="42736")
datos2017 <-datos2017 %>% rename("Febrero"="42767")
datos2017 <-datos2017 %>% rename("Marzo"="42795")
datos2017 <-datos2017 %>% rename("Abril"="42826")
datos2017 <-datos2017 %>% rename("Mayo"="42856")
datos2017 <-datos2017 %>% rename("Junio"="42887")
datos2017 <-datos2017 %>% rename("Julio"="42917")
datos2017 <-datos2017 %>% rename("Agosto"="42948")
datos2017 <-datos2017 %>% rename("Septiembre"="42979")
datos2017 <-datos2017 %>% rename("Octubre"="43009")
datos2017 <-datos2017 %>% rename("Noviembre"="43040")
datos2017 <-datos2017 %>% rename("Diciembre"="43070")

# Convertimos datos anchos a datos altos #

datos2017<-datos2017%>%gather(key="Mes",value="Precio", -RUBRO)%>% 
  #Ordenamos por proceso
  arrange(RUBRO)

datos2017$Año <- "2017"

## Unimos en un solo dataset ####

canasta_familiar=bind_rows(datos2015, datos2016, datos2017)

## Extraemos valores nulos ####

canasta_familiarsinnulos <- canasta_familiar[!is.na(canasta_familiar$Precio),]

st_options(lang = "es") #Translations
summarytools::view(dfSummary(canasta_familiarsinnulos), 
                   footnote = NA, 
                   valid.col = FALSE)


missing_glimpse(canasta_familiar) # valores perdidos para cada variable 
missing_glimpse(canasta_familiarsinnulos) # valores perdidos para cada variable 


ff_glimpse(canasta_familiar) # sumario estadístico para cada variable 

summary(canasta_familiar)


## Obteniendo Precios maximos y mínimos por producto ####

canasta_familiarsinnulosmax = canasta_familiarsinnulos %>% filter(Año %in% c(2017)) %>% group_by(RUBRO, Año) %>% summarise(PrecioMax = max(Precio))

canasta_familiarsinnulosmin = canasta_familiarsinnulos %>% filter(Año %in% c(2015)) %>% group_by(RUBRO, Año) %>% summarise(PrecioMin = min(Precio))

df2 <- canasta_familiarsinnulosmax %>% inner_join( canasta_familiarsinnulosmin, by=c('RUBRO'='RUBRO')) #, 'Año'='Año'))

canasta_familiar_con_DIferencias = mutate(df2, DiferenciaPrecio = PrecioMax - PrecioMin)

canasta_familiar_con_DIferencias = mutate(canasta_familiar_con_DIferencias, DiferenciaPrecioPorcentaje = (DiferenciaPrecio*100)/PrecioMax)


RUBROSPrecioMax = canasta_familiar_con_DIferencias %>%
  arrange(desc(DiferenciaPrecioPorcentaje)) %>%
  head(3)
RUBROSPrecioMax<-RUBROSPrecioMax["RUBRO"]

RUBROSPrecioMin = canasta_familiar_con_DIferencias %>%
  arrange(DiferenciaPrecioPorcentaje) %>%
  head(3)
RUBROSPrecioMin<-RUBROSPrecioMin["RUBRO"]

## Histograma ####

canasta_familiar_con_DIferencias %>%
  arrange(desc(DiferenciaPrecio)) %>%
  group_by(RUBRO,Año) %>%
  head(3)%>%
  ggplot(aes(x = DiferenciaPrecio)) +       # remember aes() (estetica)
  geom_histogram(bins = 20) +      # histogram with 20 bars (geometría)
  facet_grid(RUBRO ~ Año, scales = "free")  # optional: add scales="free"

# QQ plot ####

canasta_familiar %>% 
  filter(Año %in% c(2015, 2017)) %>%
  ggplot(aes(sample = Precio)) +      # Q-Q plot requires 'sample'
  geom_qq() +                          # defaults to normal distribution
  geom_qq_line(colour = "blue") +      # add the theoretical line
  #facet_grid(Año ~ RUBRO, scales = "free") 
  facet_wrap(Año ~ RUBRO, scales = "free") 

# Diagrama de caja ####

canasta_familiar %>% 
  filter(Año %in% c(2015, 2017)) %>%
  ggplot(aes(x = RUBRO, y = Precio)) + # elementos de estética 
  geom_boxplot() + # elementos de geometría 
  facet_wrap(~ Año) # elementos de diagramación 


# ver elementos de estética, geometría y diagramación 

canasta_familiar %>%  
  filter(Año %in% c(2015, 2017)) %>%
  ggplot(aes(x = factor(Año), y = Precio)) +
  geom_boxplot(aes(fill = RUBRO)) +     # add colour to boxplots
  geom_jitter(alpha = 0.4) +                # alpha = transparency
  facet_wrap(~ RUBRO, ncol = 5) +       # spread by continent
  theme(legend.position = "none") +         # remove legend
  xlab("Año") +                            # label x-axis
  ylab("Precio (Año)") +         # label y-axis
  ggtitle(
    "Precios entre 2015 y 2017") # add title
