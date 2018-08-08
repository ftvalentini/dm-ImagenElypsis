# carga librerias
source("src/load_librerias.R")

# carga base raw
path_base <- here("data","raw","base_pba.csv")
base_raw <- read.csv(path_base,header=T,stringsAsFactors=F)

# eliminamos columnas con valores distintos para un mismo callid (imagen_reduc y pondera2)
# y pasamos de formato long a wide, quedando un solo "callid" por fila
basea <- base_raw %>%
  dplyr::select(-c(imagen_reduc,pondera2)) %>% 
  tidyr::spread(quien,imagen)
  

# chequeo telefonos (un poco enquilombado):
  # nota: duplicated solo pone F a partir de la segunda ocurrencia de valor repetido
tels_dup <- basea$phone[duplicated(basea$phone)] %>% unique
# selecciono registros con tels repetidos y con valenzuela sin NA/8/6/7
  # vamos a conservar los registros que tienen tel único una vez que incorporo filtro NA/8/6/7
  # la idea es no eliminar los registros duplicados que sirvan (1/2/3/4/5)
ids_keep <- basea[basea$phone %in% tels_dup, ] %>% 
  dplyr::filter(!(valenzuela %in% c(NA,6,7,8))) %>% 
  dplyr::filter(!(phone %in% .$phone[duplicated(.$phone)])) %>% 
  dplyr::select(callid) %>% unlist
# seleccionamos los registros con los telefonos de ids_keep que hay que borrar
tels_de_keep <- unique(basea[basea$callid %in% ids_keep,"phone"])
ids_drop_1 <- basea[(basea$phone%in%tels_de_keep)&(!(basea$callid%in%ids_keep)),"callid"]
# finalmente elimino los registros duplicados restantes al azar
  # la idea es evitar mantener solo los primeros registros, que sería arbitrario
  # para eso desordenamos la base
set.seed(1)
base <- basea %>% 
  dplyr::filter(!(callid %in% ids_drop_1)) %>% 
  dplyr::arrange(sample(nrow(.),nrow(.))) %>% 
  dplyr::filter(!(duplicated(.$phone))) %>% 
  dplyr::arrange(callid)

# chequeo que no se perdió ningun telefono:
(unique(sort(basea$phone))==unique(sort(base$phone))) %>% all
# chequeo: hay un solo callid por fila
(unique(base$callid) %>% length)==(nrow(base))

# variables *nombre-candidato* indican imagen con codigos:
    # 1: Muy buena 
    # 2: Buena 
    # 3: Regular 
    # 4: Mala
    # 5: Muy Mala 
    # 6: No sabe 
    # 7: No sabe quién es 
    # 8: Repetir la pregunta

# cargo funcion para tabla de frecuencias
source(here("src","funciones.R"))
tabfreq(base,"valenzuela")

# sacamos 8s y NAs (¿y 6 y 7?) de valenzuela de la base
# y generamos nueva variable "valen_res" en nueva base "base_reduc":
  # "a": muy buena/buena
  # "b": regular
  # "c": mala/muy mala

base_reduc <- base %>% dplyr::filter(!(valenzuela %in% c(NA,8,6,7))) %>% 
  dplyr::mutate(valen_y=dplyr::case_when(
    valenzuela %in% c(1,2) ~ "a",
    valenzuela %in% 3 ~ "b",
    valenzuela %in% c(4,5) ~ "c",
    TRUE ~ NA_character_
  ))
# quedan 3 clases equilibradas:
tabfreq(base_reduc,"valen_y")
# tamaño de la muestra:
n_reduc <- nrow(base_reduc)

# sacamos variables con muchos datos faltantes (más del 0%)
  # creamos "fecha" y "mesanio" en base a endtime
  # y sacamos otras variables:
falta_reduc <- map_dbl(base_reduc, function(x) sum(is.na(x))) %>% 
  subset(./n_reduc>0) %>% names

base_reduc_clean <- base_reduc[!(names(base_reduc) %in% falta_reduc)] %>% 
  dplyr::mutate(fecha=
                  dplyr::case_when(
                    # casos con fechas-nros de excel
                    !(grepl(":",endtime)) ~
                      (as.numeric(substr(endtime,1,5)) %>% as.Date(origin='1899-12-30')),
                    # casos con fechas como strings
                    TRUE ~ (substr(endtime,start=1,stop=nchar(endtime)-6) %>% 
                      strptime(format="%d/%m/%Y",tz="GMT") %>% as.Date)
                  )) %>% 
  dplyr::mutate(mesanio=format(fecha,"%Y-%m")) %>% 
  dplyr::select(-c(campaignid,
                   surveyid,
                   starttime,
                   phone,
                   duration,
                   retries,
                   semana,
                   semana2,
                   endtime))

# chequeo de fechas correctas:
summary(base_reduc_clean$fecha)
plot(base_reduc_clean$fecha,type="l")
  # las fechas están bien, pero no en orden

# distribucion temporal de las observaciones
table(base_reduc_clean$mesanio,useNA="always") %>% barplot
# evolucion de la imagen media de valenzuela en el tiempo
base_reduc_clean %>% dplyr::group_by(fecha) %>% 
  dplyr::summarise(imagenmedia=mean(valenzuela)) %>% 
  plot(type="l")
# proporcion de a,b,c en cada mes
sum_mes <- base_reduc_clean %>% 
  dplyr::group_by(mesanio) %>% 
  dplyr::summarise(n=n(),
                   prop_a=sum(valen_y=="a")/n,
                   prop_b=sum(valen_y=="b")/n,
                   prop_c=sum(valen_y=="c")/n)
plot(1:nrow(sum_mes),sum_mes$prop_c,type="l",xaxt="n",ylim=c(0.25,0.5),
     col="red")
lines(sum_mes$prop_b,col="gold")
lines(sum_mes$prop_a,col="green")
axis(1,at=seq_along(sum_mes$mesanio),labels=sum_mes$mesanio)
  # alguna cagada se mandó valenzuela (igual ojo q en las puntas hay mucha var x n chico)

# variables que quedaron con NAs u 8s
map_dbl(base_reduc_clean, function(x) sum(is.na(x)))
map_dbl(base_reduc_clean, function(x) sum(x %in% 8))

# base final
  # sacamos variables que no sirven para arboles
    # qué es status??? no es lo mismo que ocupación (tiene 0 y 1)
  # el atributo zona no sirve porque solo hay 3 obs de "OTRO"
    # o sea, solo tenemos una muestra de GBA
  # sacamos los "4" de gestion_nac y gestion_prov
  # recodeamos imagenes de otros en a(1-2) b(3) c(4-5) d(6-7)

base_final <- base_reduc_clean %>% 
  dplyr::select(-c(callid,fecha,mesanio,valenzuela,zona,status)) %>% 
  # dplyr::filter( !(gestion_nac %in% 4 | gestion_prov %in% 4) ) %>% 
 # aca van los candidatos que hayan quedado (poner test mas piola)
  # CREO QUE NO TIENE SENTIDO PONER DESCONOCIDOS EN CANDIDATOS QUE NO SEA VIDAL
   dplyr::mutate_at(c("cfk","massa","mm",
                     "randazzo","stolbizer","vidal"),
                   dplyr::funs(dplyr::recode(.,`1`="a",`2`="a",
                                             `3`="b",
                                             `4`="c",`5`="c",
                                             `6`="d",`7`="d")))
n_final <- nrow(base_final)

# aca van candidatos restantes (poner test mas piola)
otros <- dplyr::select(base_final,c("cfk","massa","mm",
                                    "randazzo","stolbizer","vidal"))

pmap(list(otros), function(x) table(base_final$valen_y,x)/n_final*100)
  # claramente es macrista...

# una truchada: tiramos random forest para ver importancia de variables
base_rf <- base_final %>% na.omit %>% dplyr::mutate_all(factor) #%>% dplyr::select(-c(mm,vidal,cfk))
modelo_rf <- randomForest::randomForest(valen_y~.,data=base_rf)
randomForest::importance(modelo_rf) %>% "["(order(.,decreasing=T),)
modelo_rf
# el error rate es una garompa

# randomizacion de las edades en base_final (medio cabeza)
set.seed(1)
vec_edad_old <- base_final$edad # correr una sola vez (es para no perder los originales)
vec_edad <- base_final$edad
for (i in seq_along(vec_edad)) {
  if (vec_edad[i] %in% 1) vec_edad[i] <- sample(10:15,1)
  else if (vec_edad[i] %in% 2) vec_edad[i] <- sample(16:30,1)
  else if (vec_edad[i] %in% 3) vec_edad[i] <- sample(31:50,1)
  else if (vec_edad[i] %in% 4) vec_edad[i] <- sample(51:65,1)
  else if (vec_edad[i] %in% 5) vec_edad[i] <- sample(66:85,1)
  else vec_edad[i] <- NA_integer_
}
base_final$edad <- as.double(vec_edad)

# guarda base para weka
  # la clase debe ser la ultima columna
write.csv(base_final,file=here("data","final","valenzuela_imagen_final.csv"),row.names=F)

# cosas pendientes:
  # sacar los 6/7 ("d") de otros candidatos
  # ver si con randazzo/stolbizer se recupera variable "zona"
  # una alternativa es conoce/no conoce a valenzuela

