################################################################################
#                         CARGAMOS LAS LIBRERIAS                               #
################################################################################


library(tidyverse)
library(ggalt)
library(bbplot)
library(viridis)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(treemap)
library(treemapify)


df_recolectado <- readRDS("04_datos_limpios/df_recolectado.rds")
df_educación <- readRDS("04_datos_limpios/df_educación.rds")
df_experiencia <- readRDS("04_datos_limpios/df_experiencia.rds")
df_puestos <- readRDS("04_datos_limpios/df_puestos.rds")

################################################################################
#         HACEMOS UN HISTOGRAMA DE FRECUENCIA DE HABILIDADES                   #
################################################################################



# Supongamos que 'df_recolectado' es el df con las variables:
# puesto_de_trabajo, ID, Finanzas, Software_especializados, 
# Planificación_estratégica, Negociación, Contabilidad_y_Tributación

# Calcular la cantidad de TRUE en cada categoría
df_counts <- df_recolectado %>%
  summarise(
    Finanzas = sum(Finanzas, na.rm = TRUE),
    Software_especializados = sum(Software_especializados, na.rm = TRUE),
    Planificación_estratégica = sum(Planificación_estratégica, na.rm = TRUE),
    Negociación = sum(Negociación, na.rm = TRUE),
    Contabilidad_y_Tributación = sum(Contabilidad_y_Tributación, na.rm = TRUE)
  )

# Convertir a formato largo para ggplot
df_counts_long_habilidades <- df_counts %>%
  pivot_longer(
    cols = everything(),
    names_to = "Categoría",
    values_to = "Cantidad"
  )

saveRDS(df_counts_long_habilidades, "05_script_visualización/df_counts_long_habilidades.rds")
# Crear gráfico de barras
histrograma_de_frecuencia <- ggplot(data=df_counts_long_habilidades,
                                    aes(x= Cantidad,
                                        y= reorder(Categoría, -Cantidad),
                                        fill = Categoría)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +  # Puedes cambiar "Set1" por otra paleta
  labs(title = "Conteo de habilidades establecidas por Categoría", 
       x = "Cantidad", 
       y = "Categoría")

histrograma_de_frecuencia <- histrograma_de_frecuencia%>%
  ggplotly()

histrograma_de_frecuencia

################################################################################
#         HACEMOS UN MOSAICO DE HABILIDADES COMBINADAS                         #
################################################################################

cols_categorias <- c("Finanzas", "Software_especializados", "Planificación_estratégica", 
                     "Negociación", "Contabilidad_y_Tributación")

df_recolectado$Combinacion <- apply(df_recolectado[, cols_categorias], 1, function(x) {
  # Extraer los nombres de las categorías que son TRUE
  categorias_true <- names(x)[x == TRUE]
  # Si no hay ninguna, se puede asignar "Ninguna" o dejar vacio
  if(length(categorias_true) == 0) {
    return("Ninguna")
  } else {
    # Concatenar los nombres separados por coma
    return(paste(categorias_true, collapse = ", "))
  }
})

df_combinaciones <- df_recolectado %>%
  group_by(ID) %>%
  summarise(
    Combinacion = paste(unique(Combinacion), collapse = ", "),
    .groups = "drop"
  )

df_frecuencia <- df_combinaciones %>%
  count(Combinacion, sort = TRUE)%>%
  filter(Combinacion != "Ninguna")

saveRDS(df_frecuencia, "05_script_visualización/df_frecuencia.rds")

# Función para generar colores pastel
colores_pastel <- function(n) {
  hsv(h = seq(0, 1, length.out = n), s = 0.3, v = 0.9)
}

# Crea el gradiente de colores pastel
gradiente_pastel <- colorRampPalette(colores_pastel(33))

# Obtén los 33 colores del gradiente
colores <- gradiente_pastel(33)


df_frecuencia$texto_etiqueta <- paste0(df_frecuencia$Combinacion, "\n Freq=", df_frecuencia$n)

grafico_mosaico <- ggplot(df_frecuencia, aes(area = n, fill = Combinacion, label = texto_etiqueta)) +
  geom_treemap(colour = "white", size = 2) +
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE, reflow = TRUE, min.size = 1, family = "Tahoma", fontface = "bold") +
  scale_fill_manual(values = colores) +
  theme(legend.position = "none")+
  labs(title = "Combinaciones de habilidades más demandadas")+
  theme(plot.title = element_text(size = 20,  
                                  family = "Arial",  
                                  color = "black",  
                                  hjust = 0.5))  # Centrar el título
grafico_mosaico

################################################################################
#         HACEMOS UN HISTOGRAMA DE FRECUENCIA DE PUESTOS                       #
################################################################################

df_counts <- readRDS("04_datos_limpios/df_puestos.rds") %>%
  summarise(
    Finanzas = sum(Finanzas, na.rm = TRUE),
    Investigación = sum(Investigación, na.rm = TRUE),
    Administrador = sum(Administrador, na.rm = TRUE),
    Gerencia = sum(Gerencia, na.rm = TRUE),
    Contabilidad_y_Tributación = sum(Contabilidad_y_Tributación, na.rm = TRUE)
  )
# Convertir a formato largo para ggplot

df_counts_long_puestos <- df_counts %>%
  pivot_longer(
    cols = everything(),
    names_to = "Categoría",
    values_to = "Cantidad"
  )


saveRDS(df_counts_long_puestos, "05_script_visualización/df_counts_long_puestos.rds")

# Crear gráfico de barras
histrograma_de_frecuencia <- ggplot(data=df_counts_long_puestos,
                                    aes(x= Cantidad,
                                        y= reorder(Categoría, -Cantidad),
                                        fill = Categoría)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +  # Puedes cambiar "Set1" por otra paleta
  labs(title = "Conteo de Puestos establecidos por Categoría", 
       x = "Cantidad", 
       y = "Categoría")

histrograma_de_frecuencia_puestos <- histrograma_de_frecuencia%>%
  ggplotly()

histrograma_de_frecuencia_puestos

################################################################################
#         HACEMOS UN HISTOGRAMA DE FRECUENCIA DE EXPERIENCIA                   #
################################################################################

df_counts <- readRDS("04_datos_limpios/df_experiencia.rds") %>%
  summarise(
    Experiencia_1_año = sum(Experiencia_1_año, na.rm = TRUE),
    Experiencia_5_años = sum(Experiencia_5_años, na.rm = TRUE),
    Experiencia_10_años = sum(Experiencia_10_años, na.rm = TRUE),
    No_especifica_experiencia = sum(No_especifica_experiencia, na.rm = TRUE)
  )
# Convertir a formato largo para ggplot
df_counts_long_experiencia <- df_counts %>%
  pivot_longer(
    cols = everything(),
    names_to = "Categoría",
    values_to = "Cantidad"
  )

saveRDS(df_counts_long_experiencia, "05_script_visualización/df_counts_long_experiencia.rds")


# Crear gráfico de barras
histrograma_de_frecuencia <- ggplot(data=df_counts_long_experiencia,
                                    aes(x= Cantidad,
                                        y= reorder(Categoría, -Cantidad),
                                        fill = Categoría)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +  # Puedes cambiar "Set1" por otra paleta
  labs(title = "Nivel de experiencia mínima", 
       x = "Cantidad", 
       y = "Categoría")

histrograma_de_frecuencia_experiencia <- histrograma_de_frecuencia%>%
  ggplotly()

histrograma_de_frecuencia_experiencia

################################################################################
#         HACEMOS UN HISTOGRAMA DE FRECUENCIA DE EDUCACIÓN                     #
################################################################################

df_counts <- readRDS("04_datos_limpios/df_educación.rds") %>%
  summarise(
    Estudiante_de_pregrado = sum(Estudiante_de_pregrado, na.rm = TRUE),
    Pregrado = sum(Pregrado, na.rm = TRUE),
    Postgrado = sum(Postgrado, na.rm = TRUE),
    Maestria_o_Doctorado = sum(Maestria_o_Doctorado, na.rm = TRUE),
    No_especifíca = sum(No_especifíca, na.rm = TRUE)
  )

  
# Convertir a formato largo para ggplot
df_counts_long_educación <- df_counts %>%
  pivot_longer(
    cols = everything(),
    names_to = "Categoría",
    values_to = "Cantidad"
  )

saveRDS(df_counts_long_educación, "05_script_visualización/df_counts_long_educación.rds")

# Crear gráfico de barras
histrograma_de_frecuencia <- ggplot(data=df_counts_long_educación,
                                    aes(x= Cantidad,
                                        y= reorder(Categoría, -Cantidad),
                                        fill = Categoría)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +  # Puedes cambiar "Set1" por otra paleta
  labs(title = "Nivel de educación mínimo", 
       x = "Cantidad", 
       y = "Categoría")

histrograma_de_frecuencia_educación <- histrograma_de_frecuencia%>%
  ggplotly()

histrograma_de_frecuencia_educación







library(dplyr)
library(DT)

df_completo <- readRDS("C:/Users/USUARIO/Documents/Trabajo_final/02_datos_sin_limpiar/df_completo.rds")

df_completo <- df_completo%>%
  select(-c(nivel_educativo, edad, experiencia, habilidades, conocimientos_tecnicos, ID, salario_por_hora, fecha))%>%
  arrange(empresa)

datatable(
  df_completo,
  rownames = FALSE,
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    scrollX = TRUE,          
    scrollY = "300px",       
    autoWidth = TRUE,
    buttons = list(
      list(
        extend = 'collection',
        buttons = c('csv', 'excel', 'pdf'),
        text = 'Descargar'
      )
    ),
    lengthChange = FALSE,
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    pageLength = 5,
    initComplete = JS(
      "function(settings, json) {",
      "  $(this.api().table().header()).css({'background-color': '#224eb2', 'color': '#fff'});",
      "}"
    ),
    Filter = 0
  ),
  escape = FALSE
)



serif