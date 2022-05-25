#.     .       .  .   . .   .   . .    +  .
#.     .  :     .    ..   :. .___---------___.
#.       .   .    .  :.  :. _".^ .^ ^.  '.. :"-_. .
#   .  :       .  .   .:../:            . .^  :.:\.
#       .   . :: +.  :.:/: .   .    .        . . .:\
#.  :    .     .  _ :::/:               .  ^ .  . .:\
#..  . .   . -  : :.:./.                        .  .:\
#.       .     .  :..|:                    .  .  ^. .:|
#    .       . : : ..||        .                . . !:|
#  .     . . . ::. ::\(                           . :)/
# .   .     : . : .:.|. ######              .#######::|
#:.. .    :-  : .:  ::|.#######           ..########:|
#.  .  .   ..  .  .. :\ ########          :######## :/
#.          .+ :: : -.:\ ########       . ########.:/
#.  .+       . . . . :.:\. #######       #######..:/
#:: . . .       . ::.:..:.\           .   .   ..:/
#    .      .  .. :  -::::.\.       | |     . .:/
#    .  :  .    .  .-:.":.::.\             ..:/
# .      -.   . . . .: .:::.:.\.           .:/
#.   .   .  :      : ....::_:..:\   ___.  :/
#   .   .  .   .:. .. .  .: :.:.:\       :/
#     +   .   .   : . ::. :.:. .:.|\  .:/|
#     .         +   .  .  ...:: ..|  --.:|
#SOCIAL DATA SCIENCE FINAL PROJECT: "I WANT TO BELIEVE"
  
  install.packages("forcats")
  install.packages("sf")
  install.packages("tidytext")
  install.packages("wordcloud")
  library(sf)
  library(tidyverse)
  library(ggplot2)
  library(forcats)
  library(tidytext)
  library(wordcloud)
  
# *  .  . *       *    .    Exploracion de la base   .        .   *    .. 
  
  nuforc_reports <- read.csv("C:/Users/ivani/Downloads/nuforc_reports.csv")
  View(nuforc_reports)
  
  is.data.frame(nuforc_reports)
  dim(nuforc_reports)
  names(nuforc_reports)
  glimpse(nuforc_reports)
  

  ##Posibles preguntas:
  #1. Cuantos casos se registraron en los Estados con el mayor y el menor numero de avistamientos?
  #2. Cuales son las 3 ciudades con mas avistamientos?
  #3. Que porcentaje representa cada forma entre los objetos observados?
  #4. Hubo algun cambio en la forma reportada de los objetos con el pasar de los anios?
  #5. Se produjeron mas avistamientos en horario diurno o nocturno?
  #6. En que anio se reportaron mas avistamientos?
  #7. Los reportes de avistamientos aumentaron o disminuyeron a lo largo del tiempo?
  #8. Hay algun patron en la distribucion geografica de los avistamientos?
  #9. Que palabras se utilizan con mayor frecuencia para describir estos incidentes? 
  #10. pregunta pendiente: en que ciudad se registro la observacion mas duradera?
  
  
# *  .  . *       *    .    Limpieza y orden del df    .        .   *    ..
  
#En un recorte inicial, voy a tomar las columnas que sirven a las preguntas planteadas y eliminar los casos que conengan datos perdidos. Por otro lado, voy a tomar unicamente los casos registrados en los ultimos 20 anios, para lo cual fue necesario separar considerar los anis de la columna "date_time" de manera independiente.
  
df <- nuforc_reports %>% select(summary, city, state, date_time, shape, duration, city_latitude, city_longitude) %>% drop_na() %>% rename(lat=city_latitude, long=city_longitude)

df <- mutate(df, anio = str_sub(df$date_time, 1,4))
             
df <- mutate(df, anio2 = as.numeric(df$anio))

is.numeric(df$anio2)

df <- filter(df, anio2 >= 2002)

view(df)

  
# *  .  . *       *    .    Preguntas    .        .   *    ..  
    
#Pregunta 1: observamos que el estado con menos casos solo tiene 6 reportes, mientras que el mayor alcanza los 11004. 
estados <- df %>%  group_by(state) %>% count()
view(estados)
min(estados$n)
max(estados$n)

#En un segundo momento, para saber cuales eran esos Estados, ordene la base de modo descendente y ascendente. El que presenta mas reportes es California. Ahora bien, al mirar en orden ascendente veo que los que tienen menos reportes (6) no coinciden con el codigo estandar de ningun Estado, por lo que son probablemente errores de carga. Entonces, el Estado con menos casos es en realidad Pensilvania, con 7 registros.

estados <- estados %>% arrange(desc(n))
head(estados)

estados <- estados %>% arrange(n)
 
head(estados)
     
     
#pregunta 2: para contestar esta pregunta agrupe la base por ciudad, conte los casos y los ordene de forma descendiente. Luego pedi los mas altos. Las ciudades con mas reportes fueron Nueva York (629), Phoenix (607) y Seattle (539).

ciudades <- df %>%  group_by(city) %>% count() %>% arrange(desc(n)) 
view(ciudades)
head(ciudades)
  
#Pregunta 3: en terminos porcentuales, las formas mas reportadas son las luces (20.58%), los triangulos (9.18%) y las "bolas de fuego" (7.19%). Las menos frecuentes son hexagonales, llamaradas, piramides, media luna y de forma cambiante (representan un 0.0009% respectivamente). Para hacerlo mas claro, reduje las categorias a formas generales: la m√°s popular es "lights" (31.14%) seguido por "round" (27%), "triangle" (11.25%), y finalmente "squares". La categoria "others" (23.52%) es de poco interes ya que reune frecuencias muy bajas e identificaciones dudosas. 
 
 df$shape <-factor(df$shape)
 forma <- table(df$shape)
prop.table(forma)*100

df <- mutate(df, general_shape = fct_recode(df$shape, "round" = "circle", "round" = "disk", "round" = "egg", "round" = "oval", "round" = "sphere", "square" = "diamond", "square" = "rectangle", "square" = "cross", "triangle" = "chevron", "triangle" = "triangle", "triangle" = "teardrop", "triangle" = "cone", "triangle" = "pyramid", "lights" = "fireball", "lights" = "flash", "lights" = "flare", "lights" = "light", "others" = "changing", "others" = "unknown", "others" = "cigar", "others" = "cylinder", "others" = "formation", "others" = "changed", "others" = "crescent", "others" = "delta", "others" = "hexagon", "others" = "other"))

view(df)

forma_general <- table(df$general_shape)
prop.table(forma_general)*100

#Pregunta: para esta pregunta opte por generar un grafico de barras que indique el tipo de formas reportadas a lo largo del tiempo. Podemos ver que en general hay una cantidad similar de reportes de objetos redondeados y luces. Aunque entre el 2010 y 2015 se observa un leve pero estable predominio de las luces, en los ultimos aÒos tiende a equilibrarse.

df2 <- df %>% group_by(general_shape, anio2) %>% count(general_shape, anio2)

ggplot(df2) + geom_bar(aes(x = anio2, weight = n, fill = general_shape)) + coord_flip() + labs(title = "Forma de OVNIs reportados por a√±o", caption = "Fuente: nuforc reports", x = "Anio", y = "Cantidad total de reportes", fill = "Forma del objeto reportado")


#Pregunta 5: para contestar esta pregunta primero delimite las horas en las que se produjeron los reportes. Luego las reagrupe en horarios dirunos/nocturnos (de 7 a 18 y de 19 a 6 respectivamente). Hay mas avistamientos en horario nocturno (68927 vs 21215 diurno).

df <- mutate(df, horario =  
               str_sub(df$date_time, 12,13))
        

is.character(df$horario)
df <- mutate(df, horario2 = as.numeric(df$horario))
is.numeric(df$horario2)
df <- mutate(df, franja_horario = case_when(horario2  >= 7 & horario2 <= 18 ~ 'Diurno', 
                              horario2 >= 19 | horario2 <=6 ~ 'Nocturno'))
view(df)
table(df$franja_horario)

#Pregunta 7: El anio con mas reportes de avistamientos fue el 2014 (7344). Al considerar una linea temporal, pareciera que la mayoria de los reportes se dieron en los ultimos 10 anios. Para ver esta evolucion mas en detalle, solicite un grafico. A simple vista, se observa una tendencia creciente con un salto marcado entre los anios 2012-2014.

df3 <- df %>% group_by(anio2) %>% count(anio2)
view(df3) 
arrange(df3, desc(n))
ggplot(df3) + geom_point(aes(x = anio2, y = n)) + labs(title = "Cantidad de avistamientos por anio", caption = "Fuente: nuforc reports", x = "cantidad de reportes", y = "anio")

#Para profundizar en el tema, intente relacionar las variables mediante una regresion lineal, luego la incorpore en el grafico. Si bien en un primer momento las variables parecieran tener una relacion posotiva fuerte, la dispersion es grande. 

regresion <- lm(n ~ anio2, data = df3)
regresion

ggplot(df3) + geom_point(aes(x = anio2, y = n)) + labs(title = "Cantidad de avistamientos por anio", caption = "Fuente: nuforc reports", x = "cantidad de reportes", y = "anio")+ geom_abline(aes(intercept = -182247.82, slope =  92.84), color = "blue")

#Como hipotesis frente a los saltos abruptos en reportes, es interesante recordar que en 2012 se difundio informacion acerca de un presunto "apocalipsis maya", que fue representado ampliamente en la cultura popular. No es necesario relatar lo que sucedio durante el 2020, no? Quizas estos contextos favorezcan un estado de paranoia colectiva en el cual la gente se encuentre mas susceptible a interpretar experiencias con un tinte sobrenatural o catastr√≥fico, lo cual puede incluir el contacto extraterrestre.

ggplot(df3) + geom_point(aes(x = anio2, y = n)) + geom_vline(aes(xintercept = 2012), color = "red") + geom_vline(aes(xintercept = 2020), color = "red") + labs(title = "Cantidad de avistamientos por anio", y = "cantidad de reportes", caption = "En rojo, los anios 2012 y 2020, caracterizados por contextos favorables a teorias conspirativas y escenarios apocalipticos")

#Pregunta 8: Observando el mapa podemos ver que hay un claro predominio de avistamientos en la zona de California (y quizas en la costa oeste en general), y un descenso en los reportes en la franja media del pais. Considerando la distribucion geologicade EEUU, pareciera que hay mas avistamieintos en zonas deserticas y-o de montania.

df4 <- df %>% group_by(state) %>% count(state) %>% rename("values" = n) %>% ungroup()

view(df4)
mapa <- st_read("https://d2ad6b4ur7yvpq.cloudfront.net/naturalearth-3.3.0/ne_110m_admin_1_states_provinces_shp.geojson")
summary(mapa)
ggplot() + geom_sf(data = mapa)

mapa <- mapa %>% mutate(mapa, state = postal)
view(mapa)

df5 <- mapa %>% left_join(df4, by="state")
view(df5)

ggplot() + geom_sf(data = df5)

ggplot() + geom_sf(data = df5, aes(fill = values)) + labs(title = "Reportes de avistamientos por Estado", fill = "reportes/estado")

#Pregunta 9: usando count podemos ver que las palabras mas usadas son "luz" o "luces" (sumando ambas superan ampliamente al resto, en total 42835). Tambien encontramos la palabra "brillo" haciendo alusion a esta cualidad. Esto condice con la prevalencia de reportes de objetos caratcerizados como luces, llamaradas o destellos. Ademas, encontramos algunos atributos de color como "naranja", "blanco" y "rojo", tambien facilmente asociablesa este tipo de estimulos.

reportes <- df$summary
reportes <- tibble(text = reportes)
view(reportes)
palabras <- reportes %>% unnest_tokens(word, text) 
palabras <- palabras %>%anti_join(stop_words)
palabras %>% count(word, sort = TRUE)

## Para profundizar e ilustrar, genere un grafico de nube que incluyera las 50 palabras mas usadas, en tanto hayan sido mencionadas por lo menos 100 veces. Podemos deducir que "objetos o luces brillantes en el cielo" es una descripcion comun del fenomeno, incluyendo algunas otras caracteristicas del objeto como su movimiento, forma o color. 

palabras2 <- palabras %>% count(word) %>% group_by(n)
view(palabras2)

wordcloud(words = palabras2$word, freq = palabras2$n, min.freq = 100, max.words = 50,
          colors =  c("Blue","Purple", "Black"),
          random.order = F,
          random.color = F,
          scale = c(3 ,0.1),
          rot.per = 0.3)


#10. Pregunta pendiente: originalmente me habia planteado esta pregunta pero no logre organizar la base a tiempo para poder contestarla. Los reportes de duracion estan escribos a en formato chr con diversas expresiones. De todos modos, se me ocurrio que una posibildad era considerar unicamente los casos donde haya una frecuencia reportado utilizando palabras o indices denominadores como "seconds, "s", "minutes", "min", etc., haciendo una escala ordinal donde 1=segundos, 2=minutos, 3=horas. Si bien se pierde la precision de la informacion, podria haberse indagado quizas a nivel estado en cuales tienden a registrarse observaciones mas largas. Es una posibilidad para otro momento!

#*  .  . *       *    .    Algunas conclusiones    .        .   *    ..
#*  
# En terminos generales, el fenomeno OVNI en EEUU parece consistir en el reporte de objetos brillantes, mayormente en el cielo nocturno, en la zona montaÒosa del oeste del pais. La distribucion geografica tiene sentido en tanto, si la mayor parte de los reportes incluyen luces durante la noche, son zonas con menor contaminacion luminica que permiten observar con mayor claridad el cielo nocturno. Por otro lado, si bien en las ultimas decadas hubo un predominio de objetos redondeadas o luces en las descripciones de los testigos, aunque hay una gran diversidad de formas reportadas a lo largo de la historia. Quizas este predominio sea un sesgo debido a las representaciones de vida extraterrestre presentes en la cultura popular (si vieramos un objeto volador no identificado similar a lo que se ha presentado como "alien" en el imaginario colectivo, tenderiamos a interpretarlo como tal). Otro argumento del impacto cultural en nuestras percepciones podria derivarse del aumento de reportes alededor de "eventos del fin del mundo" como la pandemia de COVID o el mitico apocalipsis del 2012. Seguramente este tipo de estado de hipervigilancia frente a posibles fenomenos catastroficos hace que los testigos esten predispuestos a interpretar los eventos que presencian desde una postura paranoide. Ahora bien, esto quiere decir que el fenomeno OVNI es pura sugestion de los testigos? Es necesario seguir reuniendo datos para poder tomar una afirmacion al respecto. Para esto es crucial que los gobiernos habiliten los expedientes correspondientes al publico.

#Conclusion: no se puede afirmar ni negar que nada de esto sea cierto pero...THE TRUTH IS OUT THERE!

#                         .-.
#         .-""`""-.    |(@ @)
#     _/`oOoOoOoOo`\_  \ \-/
#    '.-=-=-=-=-=-=-.'  \/ \
#BYE   `-=.=-.-=.=-'     \ /\
#         ^  ^  ^        _H_ \

