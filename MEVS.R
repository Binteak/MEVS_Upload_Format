##################
## FORMATO MEVS ##
##################
#Ejecutar Mevs() en consola para iniciar

Mevs=function(){
#PARAMETROS----
XI="cl763lj"

#CARGA----
path_in=paste0("C:/Users/",XI,"/Desktop/MEVS_FORMAT")

#LIBRERIAS----
library(data.table)



value = readline(prompt="Bienvenido, Qu� quieres hacer? (1) Cambiar formato, (2) A�adir nuevas Mevs, (3) Ambas :")

if(value == 1){
  print("Ha elegido cambiar formato.")
  source("C:/Users/cl763lj/Desktop/MEVS_FORMAT/MEVS_Format.R")
}else if(value == 2){
  print("Ha elegido a�adir nuevas Mevs.")
  source("C:/Users/cl763lj/Desktop/MEVS_FORMAT/MEVS_Upload.R")
}else if(value == 3){
  print("Ha elegido Ambas.")
  source("C:/Users/cl763lj/Desktop/MEVS_FORMAT/MEVS_Format.R")
  source("C:/Users/cl763lj/Desktop/MEVS_FORMAT/MEVS_Upload.R")
}

}#Fin Mevs function
