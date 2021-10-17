library(dplyr)

#Vamos a hacer dos operaciones: cuantos planetas hay en total,

#Cuantos personajes hay de cada origen y varias medidas de dispersion y altura.

starwars_redux<-starwars %>% 
  select(name,homeworld,height)

# Cuantos homeworld hay, dos formas (3)

starwars_redux %>% 
  distinct(homeworld)

starwars %>% 
  select(name,homeworld,height) %>% 
  distinct(homeworld)

# Tambien podemos hacerlo asi, pero luego no podremos reenganchar operaciones

distinct(starwars_redux,homeworld)

# Calculamos ahora la altura media de todos los personajes

mean(starwars_redux$height, na.rm=TRUE)

# Como lo hacemos con dplyr. Están los decimales, pero no los muestra
# Si queremo verlos, se crea una variable y le damos a View().
# Summarise sirve para hacer agregaciones, pero no tiene porque ser sobre el data
# frame entero

starwars_redux %>% 
  summarise(altura_media=mean(height, na.rm=TRUE))

# Altura media de la gente de cada planeta. Usamos group_by

starwars_redux %>% 
  group_by(homeworld) %>% 
  summarise(altura_media=mean(height, na.rm=TRUE))

# Además, queremos saber cuantos personajes vienen de esos planetas y que nos muestre 49 filas
# Vemos que hay 10 que no se sabe de donde vienen.

starwars_redux %>% 
  group_by(homeworld) %>% 
  summarise(altura_media=mean(height, na.rm=TRUE),
            cuantos_personajes = n ()) %>% 
  print(n=49)

# Podemos crear muchas columnas en un solo summarise

starwars_redux %>% 
  group_by(homeworld) %>% 
  summarise(altura_media=mean(height, na.rm=TRUE),
            cuantos_personajes = n (),
            altura_mediana=median(height, na.rm = TRUE))  

