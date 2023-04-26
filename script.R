# Instalar paquetes ####

# readxl para importar planillas excel
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

if (!require('tidyverse'))
  install.packages("tidyverse")
library(tidyverse) 




# Importar datos ####

# Se deben descargar las planillas del repositorio https://www.sedeco.gov.py/index.php/publicaciones/monitoreo-canasta-familiar #

## Importamos los datos de la planilla del 2015 ####

datos2015 <- read_excel("tp/data/Monitoreo de Precios 2015.xlsx", sheet = 1, col_names=TRUE, skip = 9)

# Borramos la primera columna #

datos2015 <- datos2015 %>% select(-1)

# Modificamos el nombre de la columna por el que corresponde #

datos2015 <-datos2015 %>% rename("1"="42005")
datos2015 <-datos2015 %>% rename("2"="42036")
datos2015 <-datos2015 %>% rename("3"="42064")
datos2015 <-datos2015 %>% rename("4"="42095")
datos2015 <-datos2015 %>% rename("5"="42125")
datos2015 <-datos2015 %>% rename("6"="42156")
datos2015 <-datos2015 %>% rename("7"="42186")
datos2015 <-datos2015 %>% rename("8"="42217")
datos2015 <-datos2015 %>% rename("9"="42248")
datos2015 <-datos2015 %>% rename("10"="42278")
datos2015 <-datos2015 %>% rename("11"="42309")
datos2015 <-datos2015 %>% rename("12"="42339")

# Convertimos datos anchos a datos altos #

datos2015<-datos2015%>%gather(key="Mes",value="Precio", -RUBRO)%>% 
  #Ordenamos por proceso
  arrange(RUBRO)

datos2015$Anho <- "2015"

## Importamos los datos de la planilla del 2016 ####

datos2016 <- read_excel("tp/data/Monitoreo de Precios 2016.xlsx", sheet = 1, col_names=TRUE, skip = 9)

# Borramos la primera columna #

datos2016 <- datos2016 %>% select(-1)

# Modificamos el nombre de la columna por el que corresponde #

datos2016 <-datos2016 %>% rename("1"="42370")
datos2016 <-datos2016 %>% rename("2"="42401")
datos2016 <-datos2016 %>% rename("3"="42430")
datos2016 <-datos2016 %>% rename("4"="42461")
datos2016 <-datos2016 %>% rename("5"="42491")
datos2016 <-datos2016 %>% rename("6"="42522")
datos2016 <-datos2016 %>% rename("7"="42552")
datos2016 <-datos2016 %>% rename("8"="42583")
datos2016 <-datos2016 %>% rename("9"="42614")
datos2016 <-datos2016 %>% rename("10"="42644")
datos2016 <-datos2016 %>% rename("11"="42675")
datos2016 <-datos2016 %>% rename("12"="42705")

# Convertimos datos anchos a datos altos #

datos2016<-datos2016%>%gather(key="Mes",value="Precio", -RUBRO)%>% 
  #Ordenamos por proceso
  arrange(RUBRO)

datos2016$Anho <- "2016"

datos2016 <- transform(datos2016, Precio = as.numeric(Precio))

## Importamos los datos de la planilla del 2017 ####

datos2017 <- read_excel("tp/data/Monitoreo de Precios 2017.xlsx", sheet = 1, col_names=TRUE, skip = 9)

# Borramos la primera columna #

datos2017 <- datos2017 %>% select(-1)

# Modificamos el nombre de la columna por el que corresponde #

datos2017 <-datos2017 %>% rename("1"="42736")
datos2017 <-datos2017 %>% rename("2"="42767")
datos2017 <-datos2017 %>% rename("3"="42795")
datos2017 <-datos2017 %>% rename("4"="42826")
datos2017 <-datos2017 %>% rename("5"="42856")
datos2017 <-datos2017 %>% rename("6"="42887")
datos2017 <-datos2017 %>% rename("7"="42917")
datos2017 <-datos2017 %>% rename("8"="42948")
datos2017 <-datos2017 %>% rename("9"="42979")
datos2017 <-datos2017 %>% rename("10"="43009")
datos2017 <-datos2017 %>% rename("11"="43040")
datos2017 <-datos2017 %>% rename("12"="43070")

# Convertimos datos anchos a datos altos #

datos2017<-datos2017%>%gather(key="Mes",value="Precio", -RUBRO)%>% 
  #Ordenamos por proceso
  arrange(RUBRO)

datos2017$Anho <- "2017"


## Importamos los datos de la planilla del 2018 ####

datos2018 <- read_excel("tp/data/Monitoreo Cerrado 2018.xlsx", sheet = 1, col_names=TRUE, skip = 9)

# Borramos la primera columna #

datos2018 <- datos2018 %>% select(-1)

# Modificamos el nombre de la columna por el que corresponde #

datos2018 <-datos2018 %>% rename("RUBRO"="Productos")
datos2018 <-datos2018 %>% rename("1"="43101")
datos2018 <-datos2018 %>% rename("2"="43132")
datos2018 <-datos2018 %>% rename("3"="43160")
datos2018 <-datos2018 %>% rename("4"="43191")
datos2018 <-datos2018 %>% rename("5"="43221")
datos2018 <-datos2018 %>% rename("6"="43252")
datos2018 <-datos2018 %>% rename("7"="43282")
datos2018 <-datos2018 %>% rename("8"="43313")
datos2018 <-datos2018 %>% rename("9"="43344")
datos2018 <-datos2018 %>% rename("10"="43374")
datos2018 <-datos2018 %>% rename("11"="43405")
datos2018 <-datos2018 %>% rename("12"="43435")


datos2018 <- datos2018 %>% select(-14)
datos2018 <- datos2018 %>% select(-14)

# Convertimos datos anchos a datos altos #

datos2018<-datos2018%>%gather(key="Mes",value="Precio", -RUBRO)%>% 
  #Ordenamos por proceso
  arrange(RUBRO)

datos2018$Anho <- "2018"


## Importamos los datos de la planilla del 2019 ####

datos2019 <- read_excel("tp/data/Año 2019_CONSOLIDADO.xlsx", sheet = 1, col_names=TRUE, skip = 7)

# Borramos la primera columna #

datos2019 <- datos2019 %>% select(-1)

# Modificamos el nombre de la columna por el que corresponde #

datos2019 <-datos2019 %>% rename("RUBRO"="Productos")
datos2019 <-datos2019 %>% rename("1"="43466")
datos2019 <-datos2019 %>% rename("2"="43497")
datos2019 <-datos2019 %>% rename("3"="43525")
datos2019 <-datos2019 %>% rename("4"="43556")
datos2019 <-datos2019 %>% rename("5"="43586")
datos2019 <-datos2019 %>% rename("6"="43617")
datos2019 <-datos2019 %>% rename("7"="43647")
datos2019 <-datos2019 %>% rename("8"="43678")
datos2019 <-datos2019 %>% rename("9"="43709")
datos2019 <-datos2019 %>% rename("10"="43739")
datos2019 <-datos2019 %>% rename("11"="43770")
datos2019 <-datos2019 %>% rename("12"="43800")

# Convertimos datos anchos a datos altos #

datos2019<-datos2019%>%gather(key="Mes",value="Precio", -RUBRO)%>% 
  #Ordenamos por proceso
  arrange(RUBRO)

datos2019$Anho <- "2019"


## Importamos los datos de la planilla del 2021 ####

datos2020 <- read_excel("tp/data/Consolidado Mensual Enero a Diciembre 2020.xlsx", sheet = 1, col_names=TRUE, skip = 6)

# Borramos la primera columna #

datos2020 <- datos2020 %>% select(-1)

# Modificamos el nombre de la columna por el que corresponde #

datos2020 <-datos2020 %>% rename("RUBRO"="Productos")
datos2020 <-datos2020 %>% rename("1"="43831")
datos2020 <-datos2020 %>% rename("2"="43862")
datos2020 <-datos2020 %>% rename("3"="43891")
datos2020 <-datos2020 %>% rename("4"="43922")
datos2020 <-datos2020 %>% rename("5"="43952")
datos2020 <-datos2020 %>% rename("6"="43983")
datos2020 <-datos2020 %>% rename("7"="44013")
datos2020 <-datos2020 %>% rename("8"="44044")
datos2020 <-datos2020 %>% rename("9"="44075")
datos2020 <-datos2020 %>% rename("10"="44105")
datos2020 <-datos2020 %>% rename("11"="44136")
datos2020 <-datos2020 %>% rename("12"="44166")

datos2020 <- datos2020 %>% select(-14)

# Convertimos datos anchos a datos altos #

datos2020<-datos2020%>%gather(key="Mes",value="Precio", -RUBRO)%>% 
  #Ordenamos por proceso
  arrange(RUBRO)

datos2020$Anho <- "2020"


## Importamos los datos de la planilla del 2020 ####

datos2021 <- read_excel("tp/data/Canasta Basica Enero a Diciembre 2021_final_pp.xlsx", sheet = 1, col_names=TRUE, skip = 6)

# Borramos la primera columna #

datos2021 <- datos2021 %>% select(-1)

# Modificamos el nombre de la columna por el que corresponde #

datos2021 <-datos2021 %>% rename("RUBRO"="Productos")
datos2021 <-datos2021 %>% rename("1"="44197")
datos2021 <-datos2021 %>% rename("2"="44228")
datos2021 <-datos2021 %>% rename("3"="44256")
datos2021 <-datos2021 %>% rename("4"="44287")
datos2021 <-datos2021 %>% rename("5"="44317")
datos2021 <-datos2021 %>% rename("6"="44348")
datos2021 <-datos2021 %>% rename("7"="44378")
datos2021 <-datos2021 %>% rename("8"="44409")
datos2021 <-datos2021 %>% rename("9"="44440")
datos2021 <-datos2021 %>% rename("10"="oct.-21")
datos2021 <-datos2021 %>% rename("11"="nov.-21")
datos2021 <-datos2021 %>% rename("12"="dic.-21")

datos2021 <- datos2021 %>% select(-14)

# Convertimos datos anchos a datos altos #

datos2021<-datos2021%>%gather(key="Mes",value="Precio", -RUBRO)%>% 
  #Ordenamos por proceso
  arrange(RUBRO)

datos2021$Anho <- "2021"


## Importamos los datos de la planilla del 2020 ####

datos2020 <- read_excel("tp/data/Consolidado Mensual Enero a Diciembre 2020.xlsx", sheet = 1, col_names=TRUE, skip = 6)

# Borramos la primera columna #

datos2020 <- datos2020 %>% select(-1)

# Modificamos el nombre de la columna por el que corresponde #

datos2020 <-datos2020 %>% rename("RUBRO"="Productos")
datos2020 <-datos2020 %>% rename("1"="43831")
datos2020 <-datos2020 %>% rename("2"="43862")
datos2020 <-datos2020 %>% rename("3"="43891")
datos2020 <-datos2020 %>% rename("4"="43922")
datos2020 <-datos2020 %>% rename("5"="43952")
datos2020 <-datos2020 %>% rename("6"="43983")
datos2020 <-datos2020 %>% rename("7"="44013")
datos2020 <-datos2020 %>% rename("8"="44044")
datos2020 <-datos2020 %>% rename("9"="44075")
datos2020 <-datos2020 %>% rename("10"="44105")
datos2020 <-datos2020 %>% rename("11"="44136")
datos2020 <-datos2020 %>% rename("12"="44166")

datos2020 <- datos2020 %>% select(-14)

# Convertimos datos anchos a datos altos #

datos2020<-datos2020%>%gather(key="Mes",value="Precio", -RUBRO)%>% 
  #Ordenamos por proceso
  arrange(RUBRO)

datos2020$Anho <- "2020"


## Importamos los datos de la planilla del 2022 ####

datos2022 <- read_excel("tp/data/Canasta Basica Enero a Diciembre 2022_final_pp.xlsx", sheet = 1, col_names=TRUE, skip = 6)

# Borramos la primera columna #

datos2022 <- datos2022 %>% select(-1)

# Modificamos el nombre de la columna por el que corresponde #

datos2022 <-datos2022 %>% rename("RUBRO"="Productos")
datos2022 <-datos2022 %>% rename("1"="44562")
datos2022 <-datos2022 %>% rename("2"="44593")
datos2022 <-datos2022 %>% rename("3"="44621")
datos2022 <-datos2022 %>% rename("4"="44652")
datos2022 <-datos2022 %>% rename("5"="44682")
datos2022 <-datos2022 %>% rename("6"="44713")
datos2022 <-datos2022 %>% rename("7"="44743")
datos2022 <-datos2022 %>% rename("8"="44774")
datos2022 <-datos2022 %>% rename("9"="44805")
datos2022 <-datos2022 %>% rename("10"="44835")
datos2022 <-datos2022 %>% rename("11"="44866")
datos2022 <-datos2022 %>% rename("12"="44896")

datos2022 <- datos2022 %>% select(-14)

# Convertimos datos anchos a datos altos #

datos2022<-datos2022%>%gather(key="Mes",value="Precio", -RUBRO)%>% 
  #Ordenamos por proceso
  arrange(RUBRO)

datos2022$Anho <- "2022"

# Unimos en un solo dataset ####

canasta_familiar=bind_rows(datos2015, datos2016, datos2017, datos2018, datos2019, datos2020, datos2021, datos2022)

## Extraemos valores nulos ####

#canasta_familiarsinnulos <- canasta_familiar[!is.na(canasta_familiar$Precio),]

summary(canasta_familiar)

# Obteniendo Precios maximos y mínimos por producto ####

canasta_familiarsinnulosmax2022 = canasta_familiarsinnulos %>% filter(Anho %in% c(2022)) %>% group_by(RUBRO, Anho) %>% summarise(PrecioMax2022 = max(Precio))

canasta_familiarsinnulosmin2019 = canasta_familiarsinnulos %>% filter(Anho %in% c(2019)) %>% group_by(RUBRO, Anho) %>% summarise(PrecioMax2019 = max(Precio))

df2 <- canasta_familiarsinnulosmax2022 %>% inner_join( canasta_familiarsinnulosmin2019, by=c('RUBRO'='RUBRO')) #, 'Anho'='Anho'))

canasta_familiar_con_DIferencias = mutate(df2, DiferenciaPrecio = PrecioMax - PrecioMin)

canasta_familiar_con_DIferencias = mutate(canasta_familiar_con_DIferencias, DiferenciaPrecioPorcentaje = (DiferenciaPrecio*100)/PrecioMax)

# Obteniendo Precios mas aumentados por producto ####

RUBROSPrecioMax = canasta_familiar_con_DIferencias %>%
  arrange(desc(DiferenciaPrecioPorcentaje)) %>%
  head(3)
RUBROSPrecioMax<-RUBROSPrecioMax["RUBRO"]

RUBROSPrecioMax

# Obteniendo Precios menos variados por producto ####

RUBROSPrecioMin = canasta_familiar_con_DIferencias %>%
  arrange(DiferenciaPrecioPorcentaje) %>%
  head(3)
RUBROSPrecioMin<-RUBROSPrecioMin["RUBRO"]

RUBROSPrecioMin

# Diagrama de caja para rubros con precios mas altos ####

canasta_familiar %>% 
  filter(RUBRO %in% c("Aceite de soja - 900cc"))  %>%
  ggplot(aes(x = RUBRO, y = Precio)) + # elementos de estética 
  geom_boxplot() + # elementos de geometría 
  facet_wrap( RUBRO ~ Anho, ncol = 8)  # elementos de diagramación 

canasta_familiar %>% 
  filter(RUBRO %in% c("Mandioca (Kg.)"))  %>%
  ggplot(aes(x = RUBRO, y = Precio)) + # elementos de estética 
  geom_boxplot() + # elementos de geometría 
  facet_wrap( RUBRO ~ Anho, ncol = 8)  # elementos de diagramación 

canasta_familiar %>% 
  filter(RUBRO %in% c("Banana karape (Kg.)"))  %>%
  ggplot(aes(x = RUBRO, y = Precio)) + # elementos de estética 
  geom_boxplot() + # elementos de geometría 
  facet_wrap( RUBRO ~ Anho, ncol = 8)  # elementos de diagramación 

# Extraemos el pvalor de las comparaciones ####

x1 <- canasta_familiar %>%
  filter(Anho %in% c("2019")) %>%
  filter(RUBRO %in% c("Aceite de soja - 900cc"))  %>%
  select("Precio")

x2 <- canasta_familiar %>%
  filter(Anho %in% c("2022")) %>%
  filter(RUBRO %in% c("Aceite de soja - 900cc"))  %>%
  select("Precio")

t.test(x1,x2)



x3 <- canasta_familiar %>%
  filter(Anho %in% c("2019")) %>%
  filter(RUBRO %in% c("Banana karape (Kg.)"))  %>%
  select("Precio")

x4 <- canasta_familiar %>%
  filter(Anho %in% c("2022")) %>%
  filter(RUBRO %in% c("Banana karape (Kg.)"))  %>%
  select("Precio")

t.test(x3,x4)


x5 <- canasta_familiar %>%
  filter(Anho %in% c("2019")) %>%
  filter(RUBRO %in% c("Aceite de soja - 900cc"))  %>%
  select("Precio")

x6 <- canasta_familiar %>%
  filter(Anho %in% c("2022")) %>%
  filter(RUBRO %in% c("Aceite de soja - 900cc"))  %>%
  select("Precio")

t.test(x5,x6)
