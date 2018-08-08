### separa base de data/final en train-test y guarda como csv en data/working

# parametros: -------------------------------------------------------------
# nombre del "tema" (lo que guarda como [tema]_final.csv en archivos procesa_[tema].R)
nombre_tt <- nombre # hereda 'nombre' de algun script anterior
# semilla
semilla <- 1
# proporcion de testing data
test_prop <- 0.2

# cuerpo ------------------------------------------------------------------
# carga librerias y funciones
source("src/load_librerias.R")
source("src/funciones.R")

# carga base segun 'nombre' y renombra var. respuesta (la ultima) como "class"
path_base <- here("data","final",nombre_tt%+%"_final.csv")
base <- read.csv(path_base,header=T,stringsAsFactors=F)
names(base)[length(base)] <- "class"

# genera vector con indices de test
set.seed(semilla)
n <- nrow(base)
test_i <- sample(1:n,size=test_prop*n,rep=F)

# bases testing y training
base_test <- base[test_i,]
base_train <- base[-test_i,]

# exporta como csv
  # (NA como queda como NA)
  # (NA se transforma en "?" solo al pasar a arff con funcion csv_to_arff)
write.csv(base_train, file=here("data","working",nombre_tt%+%"_train.csv"), row.names=F)
write.csv(base_test, file=here("data","working",nombre_tt%+%"_test.csv"), row.names=F)
