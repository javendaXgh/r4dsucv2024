##########################################################################################
######## Script clase 13 del 27-11-24                                              #######
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

##########################################################################################
######## Procesamientos. Cada enunciado es un problema a resolver                  #######
##########################################################################################

#1) obtener keys del agrupamiento
df_gapminder_csv%>%
  group_by(country)%>%
  group_keys()%>%
  pull(country)

#2) extraer valores de las keys en un vector
df_gapminder_csv%>%
  group_by(country)%>%
  group_keys()%>%
  pull(country)

#3) Índices de filas por grupo
df_gap_country <- df_gapminder_csv%>%
  group_by(country)

df_gap_country%>%
  group_rows()

#4) Añadir índice general e índice por grupo
df_gapminder_csv%>%
  select(country, year, pop)%>%
  mutate(idx_gral= 1:n())%>% # índice general v1
  mutate(idx_gral2 =1:nrow(.))%>% # índice general v2
  mutate(idx_gral3 =row_number())%>% # índice general v3
  group_by(country)%>%
  mutate(idx_group1= row_number())%>%
  mutate(idx_group2= 1:n())%>%
  # mutate(idx_group3= 1:nrow(.))%>%
  print(n=25)
  

# 5) comparación Reframe-Mutate
#5.1) Usando Mutate
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

#5.2) Usando reframe
df_gap_clasi_pibcontinent1 <- df_gapminder_csv%>%
  filter(year==2007)%>%
  mutate(pib_mm=(gdpPercap*pop)/1000000)%>%
  group_by(continent)%>%
  reframe(cuantil_1=quantile(pib_mm,p=0.25),
          cuantil_3=quantile(pib_mm,p=0.75),
          country,
          pib_mm)%>%
  reframe(continent, country,  pib_mm, pib_clasificacion=case_when(pib_mm < cuantil_1~'bajo',
                                     pib_mm>= cuantil_1 & pib_mm <= cuantil_3 ~ 'medio',
                                     pib_mm> cuantil_3 ~ 'alto'))

head(df_gap_clasi_pibcontinent,2)
head(df_gap_clasi_pibcontinent1,2)
# Tener muy claro cuando tienen un efecto distinto o el mismo

# 6) obtener por país maximo gdp indicando el año en que lo tuvo

# 6.1) Aproximaciones para resolver el problema

df_gapminder_csv%>%
  group_by(country)%>%
  reframe(valor=max(gdpPercap))


df_gapminder_csv %>% 
  group_by(country) %>% 
  reframe(max_gdp = max(gdpPercap), year) %>% 
  arrange(desc(max_gdp))

# 6.2) Mezcla RBase (which.max) con dplyr 
df_gapminder_csv%>%
  group_by(country)%>%
  reframe(valor=max(gdpPercap),
          year_max_gdpPercap = year[which.max(gdpPercap)])%>%
  arrange(desc(valor))#%>%
  # filter(year_max_gdpPercap!=2007)

# 6.3) Enfoque alternativo con mezcla RBase (which) con dplyr 
df_gapminder_csv%>%
  group_by(country)%>%
  reframe(valor=max(gdpPercap),
          year_max_gdpPercap = year[which(gdpPercap==max(gdpPercap))])%>%
  arrange(desc(valor))%>%
  filter(year_max_gdpPercap!=2007)

# 6.4) Enfoque con con `filter`
df_gapminder_csv%>%
  group_by(country)%>%
  filter(gdpPercap==max(gdpPercap))%>%
  select(country, year, gdpPercap)%>%
  arrange(desc(gdpPercap))%>%
  filter(year!=2007)

# 6.5) Obtener índices%>%
df_gapminder_csv%>%
  group_by(country)%>%
  filter(gdpPercap==max(gdpPercap))%>%
  select(country, year, gdpPercap)%>%
  arrange(desc(gdpPercap))%>%
  filter(year!=2007)%>%
  group_indices()

# 7) Regresión Lineal : predecir lifeExp dado el gdpPercap

# 7.1) Aplicar formula y revisar resultado
df_venezuela <- df_gapminder_csv%>%
  filter(country=='Venezuela')

rl_ven <- lm(df_venezuela$gdpPercap ~ df_venezuela$lifeExp)

#7.2) estructura y revisión objeto `rl_ven`
summary(rl_ven)
class(rl_ven)
str(rl_ven)
length(rl_ven)
rl_ven$model
rl_ven$coefficients
rl_ven$residuals

# 7.3) Versión DF
df_venezuela_rl <- df_gapminder_csv%>%
  filter(country=='Venezuela')%>%
  nest_by(country) %>% # función clave ya que anida los datos
  mutate(lm_result= list(lm(gdpPercap ~ lifeExp, data = data)))

# 7.4) revisar resultados
df_venezuela_rl$data # datos de entrada
df_venezuela_rl$lm_result # columna que contiene los resultados
df_venezuela_rl$lm_result[[1]] # elemento (casilla) con los resultados en la primera fila
df_venezuela_rl$lm_result[[1]]$coefficients #acceso a los elementos de los coeficientes
df_venezuela_rl$lm_result[[1]]$coefficients[[1]] #Intercept
intercept_pred <- df_venezuela_rl$lm_result[[1]]$coefficients[[1]] 
lifeExp_pred <- df_venezuela_rl$lm_result[[1]]$coefficients[[2]] #lifeExp

# 7.5) graficar datos entrada
plot(df_venezuela_rl$data[[1]]$gdpPercap,
     df_venezuela_rl$data[[1]]$lifeExp,
     pch = 16,
     cex = 1.3, 
     col = "blue", 
     main = "RL Venezuela", 
     xlab = "gdpPercap", 
     ylab = "lifeExp")



# 7.6) Versión gráfico GGplot2
ggplot(data = df_venezuela_rl$data[[1]], 
       aes(x = gdpPercap, 
           y = lifeExp)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", 
              se = TRUE, # intervalo de confianza
              color='red')
  
## 7.7) Obtener RL para Todos los Países

df_rl_gapminder <- df_gapminder_csv%>%
  nest_by(country) %>% # función clave ya que anida los datos
  mutate(lm_result= list(lm(gdpPercap ~ lifeExp, data = data)))
