### punto 6 - tolerancia al ruido

# parametros --------------------------------------------------------------
nombre <- "valenzuela_imagen"
weka_file <- "D:/Programas/Weka/Weka-3-8/weka.jar"
semilla_6 <- 1
ejercicio <- 6

# cuerpo ------------------------------------------------------------------
# carga librerias y funciones
source("src/load_librerias.R")
source("src/funciones.R")

# genera y guarda base_test y base_train (segun 'nombre')
source("src/train_test.R")

# transforma test en arff para usar en todos los ejercicios
write.csv(base_test, file=here("data","working",nombre%+%"_test_ruido.csv"), 
          row.names=F, na="?")
path_test_csv <- here("data","working",nombre%+%"_test_ruido.csv")
path_test_arff <- dirname(path_test_csv)%+%'/'%+%nombre%+%"_test"%+%"_ruido"%+%".arff"
csv_to_arff(csv_file=path_test_csv, arff_file=path_test_arff, jar_file=weka_file)

# Clase a agregar ruido
var_class <- "class"

# parte (a) ---------------------------------------------------------------
# J48 con varios CFs para distintas proporciones de Ruido en Clase
# introduce Ruido al azar secuencialmente en la clase
nt <- nrow(base_train)
porcs <- seq(0,35,by=1)
cfs <- seq(0.05,0.5,by=0.05)
res_list <- vector("list",length(porcs))
for (i in seq_along(porcs)) {
  # pone Ruido en clase
  set.seed(semilla_6)
  ruido_i <- sample(1:nt, size=porcs[i]/100*nt, rep=F)
  base_ruido <- base_train
  # Clases Posibles
  factores_clase <- as.vector(levels(as.factor(base_train$class)))
  # Clase original para samplear sobre el resto
  indice <- match(base_ruido$class, factores_clase)
  # Loopeo sobre las filas a agregarle ruido y les cambio la clase de manera random
  clase_ruido <- base_ruido[,var_class]
  for (k in ruido_i){
    clase_ruido[k] <- sample(factores_clase[-indice[k]],1)
  }
  base_ruido$class <- clase_ruido
  # guarda csv train que se va pisando en cada corrida
  path_train_csv <- here("data","working",nombre%+%"_train"%+%"_ruido_temp"%+%".csv")
  write.csv(base_ruido, file=path_train_csv, row.names=F)
  # transforma en arff
  path_train_arff <- dirname(path_train_csv)%+%'/'%+%nombre%+%"_train"%+%"_ruido_temp"%+%".arff"
  csv_to_arff(csv_file=path_train_csv, arff_file=path_train_arff, jar_file=weka_file)
  # dataframe para guardar resultados dentro de res_list
  dat_temp <- data.frame(porcentaje_ruido=rep(porcs[i], length(cfs)),
                         confidence_factor=cfs,
                         tamaño=rep(NA, length(cfs)),
                         Training=rep(NA, length(cfs)),
                         Test=rep(NA, length(cfs)))
                         
# corre j48 para cada CF y guarda resultados en dat_temp
for (j in seq_along(cfs)) {
  arbol <- run_j48weka(jar_file=weka_file, conf=cfs[j], minobj=2,
                       train_file=path_train_arff,test_file=path_test_arff)
  size <- arbol[grepl("Size of the tree",arbol)] %>% 
    substring(regexpr("\t",.)+1) %>% as.numeric
  acctrain <- arbol[grepl("Correctly Classified Instances",arbol)][1] %>% 
    substring(regexpr("%",.)-8,regexpr("%",.)-2) %>% as.numeric
  acctest <- arbol[grepl("Correctly Classified Instances",arbol)][2] %>% 
    substring(regexpr("%",.)-8,regexpr("%",.)-2) %>% as.numeric
  dat_temp[j,"tamaño"] <- size
  dat_temp[j,"Training"] <- acctrain
  dat_temp[j,"Test"] <- acctest
}
# guarda cada dat_temp en lista de resultados
res_list[[i]] <- dat_temp
}
# genera data.frame con todos los resultados
dat_g <- Reduce(rbind,res_list)


#### Aplico graficos a modo de prueba

datg_2 <- dat_g[-3] %>% 
  reshape2::melt(id.vars=1:2,value.name="accuracy",variable.name="tipo")
# chart: x-%ruido / y-#nodos [modelos con CF 0-0.5 by 0.05]
(g6_01 <- ggplot(dat_g) +
    geom_point(aes(x=porcentaje_ruido,y=tamaño,color=confidence_factor),
               position="jitter") +
    theme(legend.text=element_text(size=8),legend.title=element_text(size=8))
)
# chart: x-%ruido / y-accuracy training [modelos con CF 0-0.5 by 0.05]
(g6_02 <- ggplot(datg_2) +
    geom_point(aes(x=porcentaje_ruido,y=accuracy,color=tipo,alpha=confidence_factor),
               position="jitter") +
    guides(color=guide_legend(title = NULL)) +
    theme(legend.text=element_text(size=8),legend.title=element_text(size=8))
)

ggsave(filename = "p06_g01.png",plot = g6_01, width = 5.4 , height = 3.3)
ggsave(filename = "p06_g02.png",plot = g6_02,width = 5.4 , height = 3.3)
