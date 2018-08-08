### punto 3 - sobreajuste y poda CF

# parametros --------------------------------------------------------------
nombre <- "valenzuela_imagen"
weka_file <- "C:/Program Files/Weka-3-8/weka.jar"
ejercicio <- 3

# cuerpo ------------------------------------------------------------------
# carga librerias y funciones
source("src/load_librerias.R")
source("src/funciones.R")

# genera y guarda base_test y base_train (segun 'nombre')
source("src/train_test.R")

# rutas/nombres de archivos csv y arff
train_file_csv <- here("data","working",nombre%+%"_train.csv")
test_file_csv <- here("data","working",nombre%+%"_test.csv")
train_file_arff <- dirname(train_file_csv)%+%'/'%+%nombre%+%"_train.arff"
test_file_arff <- dirname(test_file_csv)%+%'/'%+%nombre%+%"_test.arff"

# usa funcion propia para convertir .csv en .arff para test-train (para que Weka corra bien)
csv_to_arff(csv_file=train_file_csv, arff_file=train_file_arff, jar_file=weka_file)
csv_to_arff(csv_file=test_file_csv, arff_file=test_file_arff, jar_file=weka_file)

# corre el modelo con funcion propia con varios valores de ConfidenceFactor (-C)
  # y guarda resultados en data.frame
cfs <- seq(0.05,0.5,by=0.05)
dat <- data.frame(confidence_factor=cfs,
                    Nº_de_hojas=rep(NA,length(cfs)),
                    Nº_de_nodos=rep(NA,length(cfs)),
                    Training=rep(NA,length(cfs)),
                    Test=rep(NA,length(cfs))) 
for (i in seq_along(cfs)) {
  arbol <- run_j48weka(jar_file=weka_file, conf=cfs[i], minobj=2,
                       train_file=train_file_arff, test_file=test_file_arff)
  hojas <- arbol[grepl("Number of Leaves",arbol)] %>% 
    substring(regexpr("\t",.)+1) %>% as.numeric
  nodos <- arbol[grepl("Size of the tree",arbol)] %>% 
    substring(regexpr("\t",.)+1) %>% as.numeric %>% "-"(hojas)
  acctrain <- arbol[grepl("Correctly Classified Instances",arbol)][1] %>% 
    substring(regexpr("%",.)-8,regexpr("%",.)-2) %>% as.numeric
  acctest <- arbol[grepl("Correctly Classified Instances",arbol)][2] %>% 
    substring(regexpr("%",.)-8,regexpr("%",.)-2) %>% as.numeric
  dat[i,"Nº_de_hojas"] <- hojas
  dat[i,"Nº_de_nodos"] <- nodos
  dat[i,"Training"] <- acctrain
  dat[i,"Test"] <- acctest
}

# lista para guardar graficos
glist <- vector("list")
# convierte nodos/hojas en una columna para ggplot
dat_1 <- dat[-c(4,5)] %>% 
  reshape2::melt(id.vars=1,value.name="tamaño_arbol",variable.name="tipo")
# grafico 1: x-confidence factor / y1-#nodos y2-#hojas
(glist[[1]] <- ggplot(dat_1) +
    geom_line(aes(x=confidence_factor,y=tamaño_arbol,color=tipo),size=1) +
    theme(legend.title=element_blank())
)
# convierte test/train en una columna para ggplot
dat_2 <- dat[-c(2,3)] %>% 
  reshape2::melt(id.vars=1,value.name="accuracy",variable.name="tipo")
# grafico 2: x-confidence factor / y-accuracy [training vs test]
(glist[[2]] <- ggplot(dat_2) +
    geom_line(aes(x=confidence_factor,y=accuracy,color=tipo),size=1) +
    theme(legend.title=element_blank())
)
# guarda .png en "output/charts"
walk2("output/charts/p0"%+%ejercicio%+%"_g0"%+%seq_along(glist)%+%".png", glist, ggsave)

