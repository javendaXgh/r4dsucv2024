##########################################################################################
######## Script clase 12 del 25-11-24                                              #######
# Objetivo: usar funciones de dplyr para poder agrupar datos y obtener valores al 
# aplicar una función para cada grupo
##########################################################################################
# Se trabaja con las funciones:
# select
# group_by
# reframe
# mutate
# case_when
# filter
# quantile
# pull
# read.csv

##########################################################################################
######## Limpiar ambiente de trabajo                                               #######
##########################################################################################
rm(list= ls())

##########################################################################################
######## Cargar librerías                                                          #######
##########################################################################################
library(tidyverse)

##########################################################################################
######## Cargar conjuntos de datos                                                 #######
##########################################################################################
df_gapminder_csv <- read.csv('https://raw.githubusercontent.com/javendaXgh/datos/refs/heads/master/gapminder.csv')%>%
  select(-X)%>%
  as_tibble()

df_gapminder <- gapminder::gapminder

df_gastosaludmundial <- read.csv('https://raw.githubusercontent.com/UCVeconomia2024-2/scripts/refs/heads/main/data_in/clase_12/IHME/IHME_HEALTH_SPENDING_1995_2021/IHME_HEALTH_SPENDING_1995_2021_Y2024M07D23.CSV')

# info sobre este conjunto de datos disponible en
#https://ghdx.healthdata.org/record/ihme-data/global-health-spending-1995-2021
#https://github.com/UCVeconomia2024-2/scripts/tree/main/data_in/clase_12/IHME

##########################################################################################
######## Procesamientos. Cada enunciado es un problema a resolver                  #######
##########################################################################################
# 0- Funcionamiento group_by
df_gapminder_csv%>%
  group_by(country)

# 1- Obtener cantidad observaciones por país
df_gapminder_csv%>%
  group_by(country)%>%
  reframe(cdad_obs=n())

# 2- Obtener cantidad observaciones por continente 
df_gapminder_csv%>%
  group_by(continent)%>%
  reframe(cdad_obs=n())

# equivalente, aunque no es la misma estructura de datos
df_gapminder_csv%>%
  group_by(continent)%>%
  count()

# 3- Obtener gdpPercap promedio por período y continente
df_gapminder_csv%>%
  group_by(continent, year)%>%
  reframe(gdpPercap_promedio= mean(gdpPercap))#%>%
  #print(n=50)

# 4- Obtener gdpPercap total por período
df_gapminder_csv%>%
  group_by(year)%>%
  reframe(gdpPercap_promedio= mean(gdpPercap))

# 5- Obtener población promedio por país para todos los períodos 
df_gapminder_csv%>%
  group_by(country)%>%
  reframe(poblacion_promedio= mean(pop))#%>%
  # filter(country=='Venezuela')

# 6- Crear una serie de valores estadísticos por país para todos los períodos 
df_gapminder_csv%>%
  group_by(country)%>%
  reframe(poblacion_prom= mean(pop), 
            gdpPercap_sum= sum(gdpPercap),
            gdp_prom= mean(gdpPercap),
            gdp_max= max(gdpPercap),
            gdp_min= min(gdpPercap))#%>%
  # filter(country=='Venezuela')

# 7- Encadenamiento para clasificar por continente a los países según su PIB

# 7.a- crear columna pib_mm
df_gapminder_csv%>%
  filter(year==2007)%>%
  mutate(pib_mm=(gdpPercap*pop)/1000000) # cifras en mm

# 7.b- cuantiles según agrupamiento por continente
df_gapminder_csv%>%
  filter(year==2007)%>%
  mutate(pib_mm=(gdpPercap*pop)/1000000)%>%
  group_by(continent)%>%
  reframe(cuantil_1=quantile(pib_mm ,p=0.25),
          cuantil_3=quantile(pib_mm, p=0.75),
          country,
          pib_mm)

# 7.c- crear columna para asignación de categorías con función case_when
df_gapminder_csv%>%
  filter(year==2007)%>%
  mutate(pib_mm=(gdpPercap*pop)/1000000)%>%
  group_by(continent)%>%
  reframe(cuantil_1=quantile(pib_mm,p=0.25),
          cuantil_3=quantile(pib_mm,p=0.75),
          country,
          pib_mm)%>%
  mutate(pib_clasificacion=case_when(pib_mm < cuantil_1~'bajo',
                                     pib_mm>= cuantil_1 & pib_mm <= cuantil_3 ~ 'medio',
                                     pib_mm> cuantil_3 ~ 'alto'))

# 7.d- asignar a un nuevo objeto
df_gap_clasi_pibcontinent <- df_gapminder_csv%>%
  filter(year==2007)%>%
  mutate(pib_mm=(gdpPercap*pop)/1000000)%>%
  group_by(continent)%>%
  reframe(cuantil_1=quantile(pib_mm,p=0.25),
          cuantil_3=quantile(pib_mm,p=0.75),
          country, # se coloca dentro del reframe para que sean extraídos los valores 
          pib_mm)%>%  # se coloca dentro del reframe para que sean extraídos los valores 
  mutate(pib_clasificacion=case_when(pib_mm < cuantil_1~'bajo',
                                     pib_mm>= cuantil_1 & pib_mm <= cuantil_3 ~ 'medio',
                                     pib_mm> cuantil_3 ~ 'alto'))%>%
  select(-cuantil_1, -cuantil_3)

View(df_gap_clasi_pibcontinent%>%
  filter(continent=='Americas'))

# 7.e- subsetting países de América reordenados por criterio de pib_mm
df_gap_clasi_pibcontinent_ame <- df_gap_clasi_pibcontinent%>%
  filter(continent=='Americas')%>%
  arrange(desc(pib_mm))

# visualizar los datos
ggplot(data=df_gap_clasi_pibcontinent_ame)+
  geom_col(mapping = aes(x=country, 
                         y= pib_mm,
                         fill = pib_clasificacion))

# 7.f- crear factor para dato categórico y resolver orden de visualización de las columnas
df_gap_clasi_pibcontinent_ame <- df_gap_clasi_pibcontinent%>%
  filter(continent=='Americas')%>%
  arrange(desc(pib_mm))%>%
  mutate(country =  factor(country, levels = country))

ggplot(data=df_gap_clasi_pibcontinent_ame)+
  geom_col(mapping = aes(x=country, 
                         y= pib_mm,
                         fill = pib_clasificacion))


# 8. Salidas distintas en group_by según más de un criterio
df_gapminder_csv%>%
  group_by( country, continent)%>%
  reframe(gdpPercap_promedio= mean(gdpPercap))

df_gapminder_csv%>%
  group_by(continent, country)%>%
  reframe(gdpPercap_promedio= mean(gdpPercap))
##########################################################################################
####### 2 da parte                                                                 #######
##########################################################################################

#1. Global Health Data Exchange. Revisar contenido pre EDA (Exploratory Data Analysis)
dim(df_gastosaludmundial)
names(df_gastosaludmundial)
summary(df_gastosaludmundial[,1:10])
str(df_gastosaludmundial[,1:10])
unique(df_gastosaludmundial$location_name)
# Aplica leer la documentación técnica cuyo enlace se encuentra en la sección de carga
# de datos

#2. obtener listado de países de América según gapminder
paises_america <- df_gapminder_csv%>%
  filter(continent=='Americas')%>%
  select(country)%>%
  distinct(country)%>% #.keep_all
  pull(country)

#3. seleccionar sólo las columnas asociadas a valores promedios
df_gsm_mean <- df_gastosaludmundial%>%
  filter(location_name %in% paises_america)%>%
  select(location_name,year, ends_with('mean'))

#select(storms, contains("essure"))

contains()	Selecciona variables cuyo nombre contiene la cadena de texto
ends_with()	Selecciona variables cuyo nombre termina con la cadena de caracteres
everything()	Selecciona todas las columnas
matches()	Selecciona las variables cuyos nombres coinciden con una expresión regular
num_range()	Selecciona las variables por posición
one_of()	Selecciona variables cuyos nombres están en un grupo de nombres
start_with()	Selecciona variables cuyos nombres empiezan con la cadena de caracteres


names(df_gsm_mean)
unique(df_gsm_mean$location_name)
dim(df_gsm_mean)

string_paises <- paste0(paises_america,collapse = '|')

df_gsm_mean <- df_gastosaludmundial%>%
  filter(str_detect(location_name, string_paises))%>%
  select(location_name,year, ends_with('mean'))

unique(df_gsm_mean$location_name)

#faltantes
paises1 <- df_gastosaludmundial%>%
  filter(location_name %in% paises_america)%>%
  distinct(location_name)%>%
  pull(location_name)

paises2 <- df_gastosaludmundial%>%
  filter(str_detect(location_name, string_paises))%>%
  distinct(location_name)%>%
  pull(location_name)

paises2[paises2 %in% paises1]
paises2[!paises2 %in% paises1]

## con este conjunto de datos se volverá a trabajar

##########################################################################################
#######Variación cuando existe un vector con factores                                 ####
##########################################################################################
library(forcats) # fue cargada previamente con tidyverse

paises_america2 <- df_gapminder%>%
  filter(continent=='Americas')%>%
  select(country)%>%
  distinct(country)%>% #.keep_all
  pull(country)


levels(paises_america2)

levels(paises_america)

fct_count(paises_america, sort = FALSE,
          prop = FALSE)

fct_drop(paises_america)

as.numeric(vector_factores)

#2. Vectores con factores 
#2.a crear un vector con factores 

vector_factores <- factor(sample(sample(1:10), 20, replace = TRUE))
vector_factores

#2.b remover factores mediante RBase
as.numeric(levels(vector_factores))[vector_factores]

#2.c remover factores mediante paquete varhandle
# install.packages('varhandle')
library(varhandle)
unfactor(vector_factores)


#3 versión factor categorías
#3.a crear un vector con factores 

categorias <- c('muy bajo','bajo','medio','alto','muy alto')
vector_fact_categorias <- factor(sample(categorias, 30, replace = TRUE))
vector_fact_categorias

#3.b remover factores mediante RBase
as.character(levels(vector_fact_categorias))[vector_fact_categorias]

#3.c remover factores mediante paquete varhandle
unfactor(vector_fact_categorias)

#4 remover factores de paises_america2
as.character(levels(paises_america2))[paises_america2]
