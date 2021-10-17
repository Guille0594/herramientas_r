###########################
## data.frames and dplyr ##
## Leonardo Hansa        ## 
## September 2021        ##
###########################

library(dplyr)
library(data.table)

# Filtering rows ----------------------------------------------------------

glimpse(starwars)     # Personajes de starwars

# Filter in the base-R way

starwars$height > 175 
starwars$name[starwars$height > 175]   #Construimos un vector, lo llevamos a 
# los corchetes y nos quedamos con los personajes con estas alturas

# Filter in the dplyr way, aqui es para quedarnos con la tabla con toda la 
# información de los personajes. 
# Selección de una columna con select() y el pipe (cmnd shift M), lo hacemos
# aqui abajo. En SQL seria SLECT (en sql es tanto select como mutate en r)
# From hace referencia a la tabla, y ahora trabajamos con el where, vamos a
# hacer algo parecido en dplyr.

starwars %>% 
  select(name, skin_color, birth_year)

# Sintaxis habitual, cuando estamos dentro de los parentesis de un dplyr,
# no hace falta repetir starwars: con %>%  ya le decimos que todo lo que le 
# decimos de buscar esta dentro de ese dataframe. 

starwars %>%               
  filter(height > 175)

# Ahora queremos seleccionar solo una columna para ver los nombres mejor: 
# Data frame, pipe, filtro, pipe, y otra selección. Filter-filas, 
# select-columnas. El FILTRO en deplyr siempre hace referencia a las filas.

starwars_filtrado<-starwars %>% 
  filter(height > 175) %>% 
  select(name)

# En R base seria: En caso de que haya millones de filas, el de antes sería
# mucho mejor. Para casos muy pequeños como iris, quizas da igual.
# Corchetes a la izquierda significa que filas queremos, a la derecha que 
# "columnas" (en r base). El numero aqui da 54, es porque coge los NA.
# Dplyr un valor donde no hay dato no es > 175, ni menor. 
# El base si se los queda

starwars_filtrado_mal<-starwars[starwars$height > 175, c("name","height")]

starwars_df<-as.data.frame(starwars)
class(starwars_df)
class(starwars)
starwars_df[starwars_df$height>175, "name"]


starwars %>% 
  filter(height != 202) %>% 
  nrow()

# characters

starwars %>% 
  filter(hair_color == "brown")

# AND operator. Quiero personajes con piel clara y altura mayor a 165, aqui 
# lo hacemos con una coma también se podria hacer con un &, que es la
# operacion, intesrección de dos conjuntos. 

starwars %>% 
  filter(skin_color == "light", height >= 165)

starwars %>% 
  filter(skin_color=="light" & "height" >= 165 )

# OR operator |||||| Con este operador me quedo con la unión |, 
# hace que al menos una de las dos condiciones se cumpla. Si se cumplen las 
# dos, tambien

starwars_filtrado_2 <- starwars %>% 
  filter(skin_color == "light" | height < 100)

starwars_filtrado_2

# Vamos a ver si hay casos que cumplan las dos condiciones

starwars_filtrado_2 %>% 
  filter(skin_color=="light", height < 100)


Distinct

# Distinct  ---------------------------------------------------------------
# Para ver todos los colores de pelo que hay

# con r base seria asi:

unique(starwars$hair_color)

# con dplyr seria asi:

starwars %>% 
  distinct(hair_color)

# No queremos un vector ahora, pero una tabla. Con disctinct 

starwars_distintos <-starwars %>% 
  distinct(hair_color, skin_color)
starwars_distintos

# NA ----------------------------------------------------------------------
unique_hair_color <- unique(starwars$hair_color)
unique_hair_color

# This NA is the only value not written within ". But the vector is a character vector.

class(unique_hair_color)

class(unique_hair_color[1])

# A NA value
class(unique_hair_color[2])

# Exercise. Repeat the same procedure with height column and see what class NA belong to this time.
# Exercise. Apply the class() function to NA and see the result.
# Calculamos la altura media de todo el conjunto, que no funciona por los NA. 
# Por que cuando hay NA no calcula la media ? valor + NA no es ese valor, NA no es 0, es Not available
# 

mean(starwars$height)

1 + NA

# Despues del vector ponemos una coma, y rm que viene de remove todos los NA. 
# Tendremos la media eliminando los registros de NA, no dandoles valor 0.
# Los NA se quitan y esto es muy importante cuando estemos modelizando. Ya veremos que hacer con los NA. 

mean(starwars$height, na.rm = TRUE)


# is.na() is a R base function that allows us detecting NA values.
# Simple example:

vector_with_NA <- c(1, NA, 3)
is.na(vector_with_NA)

!is.na(vector_with_NA)

# Quedate donde esas filas donde la altura sea NA

starwars %>% 
  filter(is.na(height))

# Lo mismo, pero que NO sea NA (en la altura)

starwars %>% 
  filter(!is.na(height))

starwars %>% 
  filter(is.na(height) | is.na(hair_color))


# Exercise. Keep only the rows with both height and mass available and create a 
# new column with the BMI (m/h2). Finally select the name of the character and 
# the new column.


# Row numbers --------------------------------------------------------------

# Quedasnos con las filas 1,2,3,100,101,102,103 (como no hay tantas, solo nos coge las 3 primeras.)
starwars %>% 
  slice(1:3, 100:103)

# Cambiamos a homewolrd.