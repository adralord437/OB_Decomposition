#Cargando Librerias
library(tidyverse)
library(oaxaca)
library(kableExtra)

#Importando datos
EPHPM_2001 <- read.csv("C:/Users/user/Desktop/LIBRARY/Academic Leveling/Economic Lectures/AEH/Mi primer paper/Datos/CSV/2001 - EPHPM.csv")



## Limpiando dataset----
 

# Renombrando columna 1 y p13 para facilitar trabajo (tiene un nombre con un caracter extraño)
names(EPHPM_2001)[1] <- "hogar" 
names(EPHPM_2001)[names(EPHPM_2001) == "p13"] <- "hrs_trab_semana"

# Vector con nombre de variables de interes
Interes_vars <- c("hogar", "ur", "sexo", "edad", "civil", "p06a", "p06b", "hrs_trab_semana", "p28", "p29a", "p29b", "p29c", "p29d",
                  "e02a", "e02b", "totper", "ysmop", "ytraop", "ysmophg", "ytraophg", "ytothg", "yperhg", "totpervi", 
                  "anosest", "rama", "ocupaop"
                  )

# Reduciendo el dataset solo a las variables de interes
EPHPM_2001_reducido <- EPHPM_2001 %>% select(Interes_vars)


# Sustiuyendo variables categoricas a numeros binarios (Lo exige la funcion de oaxaca)
EPHPM_2001_reducido$ur <- case_when(EPHPM_2001_reducido$ur == 1 ~ 0, EPHPM_2001_reducido$ur == 2 ~ 1) # Urbano = 0, Rural = 1.
EPHPM_2001_reducido$sexo <- case_when(EPHPM_2001_reducido$sexo == 1 ~ 0, EPHPM_2001_reducido$sexo == 2 ~ 1) #Hombre = 0, Mujer = 1.
EPHPM_2001_reducido$p28 <- case_when(EPHPM_2001_reducido$p28 == 1 ~ 0, EPHPM_2001_reducido$p28 == 2 ~ 1) # Emp.publico = 0, Emp.privado = 1.

# Creando nueva variable dummy para identificar si es trabajador a medio tiempo o tiempo completo
 EPHPM_2001_reducido <- EPHPM_2001_reducido %>% mutate(tipo_trabajo = ifelse(hrs_trab_semana <= 35, 0, 1)) # Menos de 35hrs de trabajo a la semana se considera tiempo parcial (OIT)

### Creando nuevas variables binarias para señalar el estado civil al que pertenece el encuestado

## Funcion que hara el trabajo de crear la nueva columna binaria para cada estado civil
civil_bin <- function(x, n){
        new.col <- EPHPM_2001_reducido %>% transmute(x = if_else(civil == n, 1, 0)) #Asigna 1 si el indiv tiene el estado civil indicado, 0 de lo contrario.

names(new.col)[1] <- x

return(new.col)
}

## iterando la funcion para agregar una nueva columna binaria para cada estado civil
ests_civs <- c("casado", "viudo", "divorciado", "separado", "soltero", "Union_libre", "Menor", "NS/NR")

list_civ <- vector("list", length(ests_civs))

for(i in seq_along(ests_civs)){
        list_civ[[i]] <- civil_bin(ests_civs[i], i)
}

#Uniendo las diferentes columnas binarias del estado civil en un solo df
df_civil <- cbind(list_civ[[1]],
                  list_civ[[2]],
                  list_civ[[3]],
                  list_civ[[4]],
                  list_civ[[5]],
                  list_civ[[6]],
                  list_civ[[7]],
                  list_civ[[8]])

#Uniendo df_civil al df principal
EPHPM_2001_reducido <- cbind.data.frame(EPHPM_2001_reducido,df_civil)


## Generando Variables dummys para la rama de actividad economica

rama_bin <- function(x,n){
        new.col <- EPHPM_2001_reducido %>% transmute(x =  if_else(rama == n, 1, 0))
        names(new.col)[1] <- x
        return(new.col)
}

ramas_econ <- c("A.S.C.P"             ,         # Agricultura, Silvicultura, caza y pesca  
                "Exp.Min"             ,         # Explotacion de Minas y canteras
                "Manuf"               ,         # Manufactura
                "Elec.g.a"            ,         # Electricidad,gas y agua 
                "Contrucc"            ,         # Construccion 
                "Com.Hot.Rest"        ,         # Comercio, hoteles y restaurantes 
                "Trans.Alm.Comu"      ,         # Transporte, almacenamiento y comunicaciones
                "Banc.Seg.BieRai.Serv",         # Establecimientos Financieros, seguros, bienes raices y servicios
                "SerCom.Soc.Pers"     ,         # Servicios comunales, sociales y personales 
                "NS/NR"               , 
                "Busca empleo por primera vez")

# Lista que servira de contenedor para a iteracion siguiente
list_rama <- vector("list", length(ramas_econ))

for(i in seq_along(list_rama)){
        list_rama[[i]] <- rama_bin(ramas_econ[i], i)
}


# Uniendo el vector binario de cada rama en un solo df
df_rama <- cbind(list_rama[[1]],
                  list_rama[[2]],
                  list_rama[[3]],
                  list_rama[[4]],
                  list_rama[[5]],
                  list_rama[[6]],
                  list_rama[[7]],
                  list_rama[[8]])


# Incorporando el nuevo df al df principal
EPHPM_2001_reducido <- cbind.data.frame(EPHPM_2001_reducido, df_rama)


# Creando variable proxy para representar la experiencia
EPHPM_2001_reducido <- EPHPM_2001_reducido %>% mutate(experiencia = edad - anosest - 6) # Experiencia Potencial

# Creando nueva variable ingreso logaritmizada para que sea la var dependiente
EPHPM_2001_reducido <- EPHPM_2001_reducido %>% mutate(log_ysmop = log(ysmop))


### Modelo

# Excluyendo personas con edad < 16
EPHPM_2001_reducido <- EPHPM_2001_reducido %>% filter(edad >= 25 & edad <= 60) # Incluyendo solo personas mayores de 16 años



DF_reG <- EPHPM_2001_reducido %>% select(log_ysmop, anosest, ur, sexo, tipo_trabajo, casado, viudo, p28,
                                         divorciado, soltero, Union_libre, A.S.C.P,
                                         Exp.Min, Manuf, Elec.g.a,
                                         Contrucc, Com.Hot.Rest, Trans.Alm.Comu,
                                         Banc.Seg.BieRai.Serv, experiencia)


DF_reG <- DF_reG %>% mutate(educ.2 = anosest*anosest)

oaxaca(data = DF_reG, formula = log_ysmop ~. | sexo)

# Creando conexion para guardar los resultados de la descomposicion en un archivo de texto ya que es
# demasiado largo para ser mostrado completo en la consola de Rstudio.

setwd("C:/Users/user/Desktop/LIBRARY/Academic Leveling/Economic Lectures/AEH/Mi primer paper/Datos")
sink(file='myoutput.txt')
oaxaca(data = DF_reG, formula = log_ysmop ~. | sexo)
sink()




#### Analisis de Datos Exploratorio----

# Estadisticas Basicas de cada variable
summary(EPHPM_2001_reducido %>% select(edad, hrs_trab_semana, ysmop, yperhg, anosest, experiencia))

### Histogramas

# Funcion que creara el histograma para la variable indicada

# Edad
ggplot(data = EPHPM_2001_reducido, aes(x = edad)) +
        geom_bar()

# Horas de trabajo semanal
ggplot(data = EPHPM_2001_reducido, aes(x = hrs_trab_semana)) +
        geom_histogram(bins = 25)

# Ingreso salarial al mes por ocupacion principal
ggplot(data = EPHPM_2001_reducido, aes(x = ysmop)) +
        geom_histogram()

#Años de estudios
ggplot(data = EPHPM_2001_reducido, aes(x = anosest)) +
        geom_histogram(bins = 20)

# Experiencia potencial
ggplot(data = EPHPM_2001_reducido, aes(x = experiencia)) +
        geom_histogram(bins = 50, binwidth = 2)

## Estadisticas Descriptivas

# Funcion que calculara las estadisticas descriptivas

# ED Totales
ed <- function(fun){
        b <- c("edad", "hrs_trab_semana", "ysmop", "anosest", "experiencia")
        a <- EPHPM_2001_reducido %>% select(b) %>% sapply(FUN = fun, na.rm = T )
        
        return(round(a, 2))
}


media.c <- ed(mean)
sd.c <- ed(sd)

# ED por Genero
ed <- function(fun, sex){
        b <- c("edad", "hrs_trab_semana", "ysmop", "anosest", "experiencia")
        a <- EPHPM_2001_reducido %>% filter(sexo == sex) %>% select(b) %>% sapply(FUN = fun, na.rm = T )
        
        return(round(a, 2))
}

media.m <- ed(mean, 1) 
sd.m <- ed(sd, 1)

media.h <- ed(mean, 0)
sd.h <- ed(mean, 0)


# Creando tabla con estadisticas descriptivas
ED_DF <- cbind(media.m, sd.m, media.h, sd.h, media.c, sd.c)

#Estilizando tabla
ED_DF %>% kable(caption = "Tabla 1", align = "c") %>%
        kable_classic(full_width = F, html_font = "Times New Roman") %>%
        add_header_above(c(" " = 1, "Mujeres" = 2, "Hombres" = 2, "Combinado" = 2)) %>%
        footnote(number = c("Fuente: EPHPM 2001", "Los valores calculados representan a los hombres y mujeres entre 25 y 60 años")) 