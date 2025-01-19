##########################################################################################
######## Script clase 14 del 02-12-24                                              #######
# Objetivo: usar funciones de dplyr para poder agrupar datos y obtener valores al 
# aplicar una función para cada grupo
##########################################################################################
# Se trabaja con las funciones:
# select
# group_by
# mutates (mutata_all, start_with)

##########################################################################################
######## Limpiar ambiente de trabajo                                               #######
##########################################################################################
rm(list= ls())

##########################################################################################
######## Cargar librerías                                                          #######
##########################################################################################
library(tidyverse)
library(gapminder)

##########################################################################################
######## Primera Parte: 
# binds_rows, 
# str_detect
# select, 
# ends_with, 
# factores
##########################################################################################

##########################################################################################
######## Cargar conjuntos de datos                                                 #######
##########################################################################################
df_gapminder_csv <- read.csv('https://raw.githubusercontent.com/javendaXgh/datos/refs/heads/master/gapminder.csv')%>%
  select(-X)%>%
  as_tibble()

df_gapminder <- gapminder

df_gastosaludmundial <- read.csv('https://raw.githubusercontent.com/UCVeconomia2024-2/scripts/refs/heads/main/data_in/clase_12/IHME/IHME_HEALTH_SPENDING_1995_2021/IHME_HEALTH_SPENDING_1995_2021_Y2024M07D23.CSV')


# info sobre este conjunto de datos disponible en
#https://ghdx.healthdata.org/record/ihme-data/global-health-spending-1995-2021
#https://github.com/UCVeconomia2024-2/scripts/tree/main/data_in/clase_12/IHME


df_ventas <- read.csv('https://raw.githubusercontent.com/UCVeconomia2024-2/scripts/refs/heads/main/data_in/df_ventas.csv')

##########################################################################################
######## Parte 1: DPLYR
##########################################################################################

##########################################################################################
######## Procesamientos. Cada enunciado es un problema a resolver                  #######
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
df_paises_america <- df_gapminder_csv%>%
  filter(continent=='Americas')

#3. crear df con valores duplicados uniendo dos df's
df_paises_america2 <- bind_rows(df_paises_america,
                                df_paises_america%>%
                                  sample_n(30)
                                )%>%
  arrange(country, year)

df_paises_america2%>%
  print(n=30) # presencia de valores duplicados

dim(df_paises_america)
dim(df_paises_america2) #presencia de valores duplicados

#4. remover filas duplicadas
df_paises_america_limpio <- df_paises_america2%>%
  filter(continent=='Americas')%>%
  distinct(country, lifeExp) #, .keep_all=TRUE

dim(df_paises_america_limpio)
str(df_paises_america_limpio)
df_paises_america_limpio

#4.1 remover filas duplicadas conservando todos los atributos
df_paises_america_limpio <- df_paises_america2%>%
  filter(continent=='Americas')%>%
  distinct(country, lifeExp, .keep_all=TRUE) 

dim(df_paises_america_limpio)
str(df_paises_america_limpio)
df_paises_america_limpio


#5. obtener listado de países de América según gapminder
paises_america <- df_gapminder_csv%>%
  filter(continent=='Americas')%>%
  select(country)%>%
  distinct(country)%>% #.keep_all
  pull(country)

length(paises_america) # cdad de paises

#6. seleccionar sólo las columnas asociadas a valores promedios
df_gsm_mean <- df_gastosaludmundial%>%
  filter(location_name %in% paises_america)%>%
  select(location_name,year, ends_with('mean'))

names(df_gsm_mean)
unique(df_gsm_mean$location_name) # 22 valores Vs. 25 en paises_america
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

library(stringr)
?str_detect # consultar función

paises2 <- df_gastosaludmundial%>%
  filter(str_detect(location_name, string_paises))%>%
  distinct(location_name)%>%
  pull(location_name)

paises2[paises2 %in% paises1]
paises2[!paises2 %in% paises1]

## con este conjunto de datos se volverá a trabajar
##########################################################################################
######## Parte 2: Factores
##########################################################################################

##########################################################################################
#######Variación cuando existe un vector con factores                                 ####
##########################################################################################
library(forcats) # fue cargada previamente con tidyverse

paises_america2 <- df_gapminder%>%
  filter(continent=='Americas')%>%
  select(country)%>%
  distinct(country)%>% 
  pull(country)


levels(paises_america2)

levels(paises_america)

fct_count(paises_america, sort = FALSE,
          prop = FALSE)

fct_drop(paises_america)

as.numeric(vector_factores)

## se comprende la función fct_drop con este ejemplo

f <- factor(c("a", "b"), 
            levels = c("a", "b", "c"))
f

fct_drop(f)

#2. Vectores con factores 
#2.a crear un vector con factores 

vector_factores <- factor(sample(1:10, 20, replace = TRUE),
                          levels = 1:11)
vector_factores

levels(vector_factores)

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

##########################################################################################
######## Parte 3: Coerce
##########################################################################################

# Coerción implícita: Suma de un número entero y un número decimal
entero <- 5L
decimal <- 3.2

resultado <- entero + decimal
resultado  


# Coerción explícita: Convertir un carácter a un número decimal
numero_como_cadena <- "3.2"
numero_decimal <- as.numeric(numero_como_cadena)
numero_decimal  # Salida: 3.2

# Coerción explícita: Convertir un número entero a un carácter
entero <- 5L
cadena <- as.character(entero)
cadena  # Salida: "5"

# Crear un data frame con diferentes tipos de datos
df <- data.frame(
  edad = c(25L, 30L, 35L),
  nombre = c("Juan", "Ana", "Carlos"),
  altura = c(1.75, 1.68, 1.80)
)

df
##########################################################################################
######## Parte 4: Recycle
##########################################################################################
# Ejemplo sumas
vector_corto <- c(1, 2)
vector_largo <- c(3, 4, 5, 6)

resultado_suma <- vector_corto + vector_largo
resultado_suma


# Ejemplo multiplicaciones
vector_corto <- c(2, 3)
vector_largo <- c(5, 7, 9, 11)

resultado_mult <- vector_corto * vector_largo
resultado_mult

# Ejemplo asignaciones
vector_largo <- c(0, 0, 0, 0)
vector_corto <- c(1, 2)
vector_largo[] <- vector_corto
vector_largo

# Cuando no son múltiplos ambos vectores

vector_corto <- c(1, 2)
vector_largo <- c(3, 4, 5)
resultado <- vector_corto + vector_largo

# se genera una warning
resultado

# Operaciones lógicas
vector_corto <- c(TRUE, FALSE)
vector_largo <- c(1, 2, 3, 4)
resultado_logico <- vector_corto & (vector_largo > 2)
resultado_logico

# en una DF
df_recycle <- data.frame(vector_corto = '1',
                         vector_largo = 1:5)

df_recycle
##########################################################################################
######## Parte 5: NA's
##########################################################################################
# Crear un vector con algunos valores NA
datos <- c(1, 2, NA, 4, NA, 6)

datos
# Identificar los valores NA
is.na(datos)

# Contar los valores NA en el vector 'datos'
sum(is.na(datos))

# Eliminar filas con valores NA en un data frame
df <- data.frame(a = c(1, 2, NA), b = c(4, NA, 6))
df_sin_na <- na.omit(df)
df_sin_na

# Reemplazar valores NA con la media de la columna 'a'
df$a[is.na(df$a)] <- mean(df$a, na.rm = TRUE)
df

mean(df$a, na.rm = TRUE)

df_completos <- df[complete.cases(df), ]
df_completos

# Coercion en NA´s

## Ejemplo df_ventas
View(head(df_ventas))
df_ventas%>%
  filter(is.na(VAT))

df_ventas%>%
  filter(VAT==NA)

df_ventas%>%
  filter(VAT!=NA)

df_ventas%>%
  filter(is.na(VAT))
##########################################################################################
######## Parte 6: tidyr formatos long y wide
##########################################################################################

# Crear un tibble ancho (wide)
datos_ancho <- tribble(
  ~nombre, ~edad_2015, ~edad_2016, ~edad_2017,
  "Juan",   20,         21,         22,
  "Ana",    23,         24,         25
)


datos_ancho


# Crear un tibble largo (long)
datos_largo <- tribble(
  ~nombre, ~año, ~edad,
  "Juan",   2015, 20,
  "Juan",   2016, 21,
  "Juan",   2017, 22,
  "Ana",    2015, 23,
  "Ana",    2016, 24,
  "Ana",    2017, 25
)

datos_largo

# Convertir datos_ancho a datos_largo
datos_largo <- datos_ancho %>%
  pivot_longer(cols = starts_with("edad"),
               names_to = "año",
               values_to = "edad")

datos_largo


# Convertir datos_largo a datos_ancho
datos_ancho <- datos_largo %>%
  pivot_wider(names_from = año,
              values_from = edad)

datos_ancho

# Datos ancho (ejemplo)
ventas_ancho <- tribble(
  ~producto, ~enero, ~febrero, ~marzo,
  "A",        100,    200,      150,
  "B",        120,    220,      180
)

ventas_ancho

# Convertir a formato largo
ventas_largo <- ventas_ancho %>%
  pivot_longer(cols = starts_with("enero"),
               names_to = "mes",
               values_to = "venta")

ventas_largo

# Calcular promedio de ventas por producto
promedio_ventas <- ventas_largo %>%
  group_by(producto) %>%
  summarize(promedio = mean(venta))

promedio_ventas

# Ejemplo promedio acciones
precio_acciones <- tibble(
  fecha = as.Date("2024-01-01") + 0:9,
  precio_x = rnorm(10, 0, 1),
  precio_y = rnorm(10, 0, 2),
  precio_z = rnorm(10, 0, 4)
)

precio_acciones

# versión anterior con gather
precio_acciones %>% 
  gather("accion_nombe",
         "precio_accion",
         -fecha)

# caso relig income
head(relig_income)

relig_income %>%
  pivot_longer(cols =!religion, 
               names_to = "income", 
               values_to = "count")


# caso éxitos Billboard
head(billboard)
dim(billboard)

billboard %>%
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    # names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE
  )

## remover prefijos wk
billboard %>%
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE
  )

# Caso WHO
names(who)
dim(who)

who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = c("diagnosis", "gender", "age"),
    names_pattern = "new_?(.*)_(.)(.*)",
    values_to = "count"
  )

## análisis patrón en string
library(stringr) # ya está cargada en tidyverse
str_split("new_sp_m2534", "new_?(.*)_(.)(.*)")

# Hacer consultar a LLM sobre este patrón
cadena <- "new_user_123"
patron <- "new_?(.*)_(.)(.*)"

resultados <- str_match(cadena, patron)
resultados

# Ejercicio pivot wider
# fish encounter
######## pivot_wider()
head(fish_encounters, 8)

fish_encounters %>%
  pivot_wider(names_from = station, 
              values_from = seen)




