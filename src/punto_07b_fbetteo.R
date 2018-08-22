## Discretizacion supervisada
install.packages("discretization")
library(discretization)

# parametros --------------------------------------------------------------
nombre <- "valenzuela_imagen"
weka_file <- "D:/Programas/Weka/Weka-3-8/weka.jar"
semilla_5 <- 1
ejercicio <- 7

# cuerpo ------------------------------------------------------------------
# carga librerias y funciones
source("src/load_librerias.R")
source("src/funciones.R")

# genera y guarda base_test y base_train (segun 'nombre')
source("src/train_test.R")
#J48 para distinta cantidad de bins y distintos CFs
nt <- nrow(base_train)
bins = 1
cfs <- seq(0.05,0.5,by=0.05)
res_list <- vector("list",length(bins))

base2 <- base

cut.edad <- discretization::cutPoints(base2$edad, base2$class)

# 1 y 0 lo valores de la nueva variable para no perder que sea numerico
edad_disc <- ifelse(base2$edad > cut.edad, 1,0)

base$edad <- edad_disc


for (i in 1) {
  # crea nuevas bases testing y training con dichos bins
  base_test <- base[test_i,]
  base_train <- base[-test_i,]
  
  # guarda csv test que se va pisando en cada corrida
  write.csv(base_test, file=here("data","working",nombre%+%"_test_disc.csv"), 
            row.names=F, na="?")
  path_test_csv <- here("data","working",nombre%+%"_test_disc.csv")
  path_test_arff <- dirname(path_test_csv)%+%'/'%+%nombre%+%"_test"%+%"_disc"%+%".arff"
  csv_to_arff(csv_file=path_test_csv, arff_file=path_test_arff, jar_file=weka_file)
  
  # guarda csv train que se va pisando en cada corrida
  path_train_csv <- here("data","working",nombre%+%"_train"%+%"_disc_temp"%+%".csv")
  write.csv(base_train, file=path_train_csv, row.names=F)
  # transforma en arff
  path_train_arff <- dirname(path_train_csv)%+%'/'%+%nombre%+%"_train"%+%"_disc_temp"%+%".arff"
  csv_to_arff(csv_file=path_train_csv, arff_file=path_train_arff, jar_file=weka_file)
  
  # dataframe para guardar resultados dentro de res_list
  dat_temp <- data.frame(cantidad_bins=rep(bins[i], length(cfs)),
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
dat_width <- Reduce(rbind,res_list)

# hardcodeo, la cantidad de bins en realidad es 2, pero tuve que poner "1" para que loopee bien
dat_width$cantidad_bins = 2

dat7_sup <- dat_width
dat7_sup <- dat7_sup %>% dplyr::rename(cantidad_de_nodos = tamaño )

dat7_sup_2 <- dat7_sup[-3] %>% 
  reshape2::melt(id.vars=1:2,value.name="accuracy",variable.name="tipo")
#### Nuevos charts

(g7_01_sup <- ggplot(dat7_sup) +  geom_line(aes(x = confidence_factor, y = tamaño), color = "red") +
    theme(legend.text=element_text(size=8),legend.title=element_text(size=8)))


(g7_02_sup <- ggplot(dat7_sup_2) +
    geom_line(aes(x=confidence_factor,y=accuracy,color=tipo)) +
    guides(color=guide_legend(title = NULL)) +
    theme(legend.text=element_text(size=8),legend.title=element_text(size=8))
)

ggsave(filename = "p07_tamaño_supervisado.png",plot = g7_01_sup, width = 5.4 , height = 3.3)
ggsave(filename = "p07_accuracy_supervisado.png",plot = g7_02_sup,width = 5.4 , height = 3.3)
