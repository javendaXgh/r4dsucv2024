##########################################################################################
# Clase 2:
# Script: instalación paquete offline (sólo windows)
##########################################################################################

#descomprimir archivo
unzip('winbinarios.zip')

# listar paquetes a instalar
pkgFilenames <- read.csv("'winbinarios/pkgFilenames.csv", stringsAsFactors = FALSE)[, 1]

# instalar distintos paquetes
install.packages(pkgFilenames, repos = NULL, type = "win.binary")

