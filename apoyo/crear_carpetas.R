clases <- c(paste0('clase_0',1:9),
                 paste0('clase_',10:30))
carpetas_scripts <- paste0('_site/scripts/',clases,'/')

for(i in  carpetas_scripts){
  dir.create(i)
}
dir.create(carpetas_scripts[2])
