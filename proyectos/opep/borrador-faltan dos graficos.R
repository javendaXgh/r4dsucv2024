rm(list=ls())

##borrador final faltan 2 cuadros####

library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
tabla_produccion<-read_excel(path="Crude_oil_production_in_OPEC_and_Price_Average_Annual_DoC (2).xlsx", range = "A3:BM18")

tabla_canasta <- read_excel(path="cesta_opep.xlsx", range= "A3:AP15")

#formato largo canasta

# Transformar la tabla y renombrar columnas en español
tabla_canasta_long <- tabla_canasta %>%
  rename(paises = Contries) %>%  
  pivot_longer(
    cols = -paises,              
    names_to = "Ano",            
    values_to = "Tipo_Petroleo" )
    
  
   

###eliminamos las columnas innecesarias####
tabla_produccion_corregida<-tabla_produccion%>%
  select(-(2:21), -(63:65))

####transformamos de wide a longer donde damos###
###un nuevo nombre a las columnas y organizamos por year###
tabla_produccion_largo<-tabla_produccion_corregida%>%
  pivot_longer(
    cols =-Contries,
    names_to = "ano",
    values_to = "produccion_pais"
  )%>%
  rename(paises=Contries)%>%
  arrange(ano)

View(tabla_produccion_largo)

### en este caso vamos a filtrar unas filas, transformarlas a wide###
### donde tomaremos los tittulos de la columna "paises" y los valores###
### de la columna "produccion", luego con ! eliminaremos esas filas y vamos###
### a unir con nuestras nuevas columnas con left_join y lo haremos por orden de year###

filas_a_transformar1 <- tabla_produccion_corregida %>%
  filter(Contries %in% c("OPEC","Price OPEP","Price Average Annual"))


filas_long<-filas_tranformada_long%>%
  pivot_wider(
    names_from = Contries,
    values_from = value
  )





rm(tabla_produccion_larga)


# Transformar las filas usando pivot_wider
# Transformar la tabla a formato largo y crear la columna produccion_pais


produccion_final <- tabla_produccion_largo %>%
  filter(!paises %in% c("OPEC", "Price OPEP", "Price Average Annual")) %>%  
  left_join(filas_long, by = c("ano" = "year")) %>%
  rename(produccion_opep = OPEC)

# Renombrar las columnas
# Convertir las columnas a formato numérico
# Convertir las columnas a formato numérico
#Convertir las columnas a formato numérico

produccion_final <- produccion_final %>%
  mutate(
    ano = as.numeric(ano),
    `Price Average Annual` = round(as.numeric(`Price Average Annual`), 1)
  ) 



library(tidiverse)
library(ggplot2)




#Convertir la columna "ano" a tipo numérico
tabla_canasta_long <- tabla_canasta_long %>%
  mutate(Ano = as.numeric(Ano)) 


# Unir los dataframes "produccion_final" y "tabla_canasta_long" usando left_join
produccion_final_unida <- produccion_final %>%
  left_join(tabla_canasta_long, by = c("paises" = "paises", "ano" = "Ano"))

 ####se creo una columna de porcentaje en la tabla df produccion_final_unida
produccion_final_unida <- produccion_final_unida %>%
  mutate(porcentaje_produccion = (produccion_pais / produccion_opep) * 100,2)

produccion_final_unida <- produccion_final_unida %>%
  mutate(porcentaje_produccion = round((produccion_pais / produccion_opep) * 100, 2))
 

produccion_final_unida <- produccion_final_unida %>%
  select(-`2`)

View(produccion_final)
View(produccion_final_unida)

# resumen estadistico 
getwd()
produccion_final_unida %>%
  select(produccion_opep, `Price OPEP`, `Price Average Annual`) %>%
  summary()

# Producción promedio por país ###reframe no sumarise ya lo dijo en diciembre 86
produccion_final_unida %>%
  group_by(paises) %>%
  reframe(
    produccion_promedio_paises = mean(produccion_pais)
  ) %>%
  arrange(desc(produccion_promedio_paises))
#Precios promedios por año
precios_anuales <- produccion_final_unida %>%
  group_by(ano) %>%
  reframe(
    precio_opep = mean(`Price OPEP`, na.rm = TRUE),
    precio_global = mean(`Price Average Annual`, na.rm = TRUE)
  ) %>%
  ungroup() # Desagrupar después de la operación

##Tendencia de precios

ggplot(produccion_final_unida, aes(x = ano)) +
  # Puntos para Precio OPEP
  geom_point(aes(y = `Price OPEP`, color = "Precio OPEP")) +
  # Puntos para Precio Global
  geom_point(aes(y = `Price Average Annual`, color = "Precio Global")) +
  # Línea de tendencia para Precio OPEP
  geom_smooth(aes(y = `Price OPEP`, color = "Precio OPEP"), method = "loess", se = FALSE) +
  # Línea de tendencia para Precio Global
  geom_smooth(aes(y = `Price Average Annual`, color = "Precio Global"), method = "loess", se = FALSE) +
  # Etiquetas y título del gráfico
  labs(
    title = "Tendencias de precios petroleros (1980-2020)",
    x = "Año",
    y = "Precio (USD)",
    color = "Tipo de precio"
  ) +
  # Tema minimalista
  theme_minimal() +
  # Colores personalizados para las series de datos
  scale_color_manual(values = c("Precio OPEP" = "red", "Precio Global" = "blue")) +
  # Posición de la leyenda
  theme(legend.position = "bottom") 


#Producción por país (series temporales)
# Crear el gráfico
ggplot(produccion_final, aes(x = ano, y = produccion_pais, color = paises)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Producción petrolera por país (1980-2020)",
    x = "Año",
    y = "Producción (1,000 b/d)",
    color = "País"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")






# Comparación de Producción Anual por País
produccion_final %>%
 # filter(year == 2020) %>%
  ggplot(aes(x = reorder(paises, produccion_pais), y = produccion_pais, fill = paises)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Comparación de producción petrolera por país en 2020",
    x = "País",
    y = "Producción (1,000 b/d)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ano)


# Promedio de Producción por Década 

produccion_final %>%
  mutate(decade = floor(ano / 10) * 10) %>%
  group_by(decade, paises) %>%
  reframe(mean_produccion_pais = mean(produccion_pais, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(decade), y = mean_produccion_pais, fill = paises)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Promedio de producción petrolera por década (1980-2020)",
    x = "Década",
    y = "Producción Promedio (1,000 b/d)",
    fill = "País"
  ) +
  theme_minimal()


library(leaflet)

# Crear un dataframe con los países y sus coordenadas
paises_location <- data.frame(
  countries = c("Algeria", "Congo", "Equatorial Guinea", "Gabon", "IR Iran", "Iraq", 
                "Kuwait", "Libya", "Nigeria", "Saudi Arabia", "United Arab Emirates", "Venezuela"),
  lat = c(28.0339, -0.2280, 1.6508, -0.8037, 32.4279, 33.2232, 29.3759, 26.3351, 9.0820, 23.8859, 23.4241, 6.4238),
  lng = c(1.6596, 15.8277, 10.2679, 11.6094, 53.6880, 43.6793, 47.9774, 17.2283, 8.6753, 45.0792, 53.8478, -66.5897),
  produccion_pais=produccion_final$produccion_pais
)

mapa_interactivo <- leaflet(paises_location) %>%
  addTiles() %>%  # Agregar capa base de OpenStreetMap
  addCircleMarkers(
    lng = ~lng, lat = ~lat,  # Coordenadas de los países
    popup = ~countries,           # Mostrar el nombre del país al hacer clic
    label = ~countries,
    color = "yellow",                   # Color del borde del círculo                # Color de relleno del círculo
    fillOpacity = 0.8  # Mostrar el nombre del país al pasar el cursor
  )

# Mostrar el mapa
mapa_interactivo

#############
library(DT)

# Primera tabla: Producción Anual por País
tabla1 <- datatable(
  produccion_final_unida %>% select(paises, ano, produccion_pais, porcentaje_produccion, Tipo_Petroleo, produccion_opep),
  options = list(
    pageLength = 10,
    dom = 'Bfrtip', # Agrega botones y filtros
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    autoWidth = TRUE,
    scrollX = TRUE,
    scrollY = "400px"
  ),
  filter = 'top',
  caption = 'Producción Anual por País'
)

# Mostrar la tabla
tabla1

######################
# Segunda tabla: Precio Anual OPEP (Eliminando duplicados por año)
tabla2.2.1 <-datatable(
  produccion_final_unida %>%
    select(ano, "Price OPEP", "Price Average Annual") %>%
    distinct(ano, "Price OPEP", "Price Average Annual"),# Esto se asegura que por año solo haya un registro único
  , extensions = c('Buttons'),
  options = list( buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  pageLength = 10),
  caption = 'Precio Anual OPEP'
)

#########


tabla2.21 <- datatable(
  produccion_final %>%
    select(ano, `Price OPEP`, `Price Average Annual`) %>%
    distinct(ano, `Price OPEP`, `Price Average Annual`), # Esto se asegura que por año solo haya un registro único
  extensions = c('Buttons'),
  options = list(
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    pageLength = 10
  ),
  caption = 'Precio Anual OPEP'
)

# Mostrar la tabla
tabla2.21


###############
#Graficos lineales###

library(ggplot2)
library(patchwork)
install.packages("patchwork")
# Gráfico 1: Producción OPEP
grafico_produccion <- ggplot(produccion_final_unida, aes(x = ano, y = produccion_opep)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(title = "Producción Anual de la OPEP", x = "Año", y = "Producción (Millones de barriles diarios)") +
  theme_minimal()

# Gráfico 2: Precio Internacional
grafico_precio <- ggplot(produccion_final_unida, aes(x = ano, y = `Price Average Annual`)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Precio Internacional del Petróleo (Promedio Anual)", x = "Año", y = "Precio (USD por barril)") +
  theme_minimal()

# Enlazar ambos gráficos (Stacked)
grafico_final <- grafico_produccion / grafico_precio

# Mostrar gráficos
grafico_final
