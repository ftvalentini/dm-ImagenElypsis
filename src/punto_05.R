### punto 5 - datos faltantes

# parametros --------------------------------------------------------------
nombre <- "valenzuela_imagen"
weka_file <- "C:/Program Files/Weka-3-8/weka.jar"
semilla_5 <- 1
ejercicio <- 5

# cuerpo ------------------------------------------------------------------
# carga librerias y funciones
source("src/load_librerias.R")
source("src/funciones.R")

# genera y guarda base_test y base_train (segun 'nombre')
source("src/train_test.R")

# transforma test en arff para usar en todos los ejercicios
write.csv(base_test, file=here("data","working",nombre%+%"_test_falt.csv"), 
          row.names=F, na="?")
path_test_csv <- here("data","working",nombre%+%"_test_falt.csv")
path_test_arff <- dirname(path_test_csv)%+%'/'%+%nombre%+%"_test"%+%"_falt"%+%".arff"
csv_to_arff(csv_file=path_test_csv, arff_file=path_test_arff, jar_file=weka_file)

# se elige atributo con gain.ratio mediano en base_train (porque si)
var_f <- FSelector::gain.ratio(class~., base_train) %>% dplyr::mutate(var=rownames(.)) %>% 
  dplyr::arrange(-attr_importance) %>% "["(floor(nrow(.)/2),"var")

# corridas ---------------------------------------------------------------
# J48 con varios CFs para distintas proporciones de NAs en atributo var_f
# tres cosas adentro del loop:
# introduce NA al azar (a), reemplaza por moda (b), reemplaza por moda segun clase (c)
nt <- nrow(base_train)
porcs <- seq(0,75,by=5)
cfs <- seq(0.05,0.5,by=0.05)
# lista de resultados: una lista por parte (a,b,c)
  # dentro de cada una se guarda un data frame para cada porcentaje de faltante
res_list <- rep(list(NA),3) %>% lapply(function(x) as.list(rep(x,length(porcs))))
# indices de loops: i-porcentajes, p-partes, j-cfs
for (i in seq_along(porcs)) {
  # genera base_na (parte a), base_moda (b) y base_modacl (c)
  set.seed(semilla_5)
  # a
  na_i <- sample(1:nt, size=porcs[i]/100*nt, rep=F)
  base_na <- base_train
  base_na[na_i,var_f] <- NA
  # b
  moda <- get_moda(base_na[var_f])
  base_moda <- base_na
  base_moda[is.na(base_moda[var_f]),var_f] <- moda
  # c
  modacl <- split(base_na,base_na$class) %>% map("["(var_f)) %>% map_chr(get_moda)
  base_modacl <- base_na
  for (l in names(modacl)) {
    base_modacl[is.na(base_modacl[var_f]) & base_modacl$class==l,var_f] <- modacl[l]
  }
  # lista de 3 dataframes para guardar resultados de cada parte
  dat_temps <- list(data.frame(porcentaje_faltantes=rep(porcs[i], length(cfs)),
                               confidence_factor=cfs,
                               tamaño=rep(NA, length(cfs)),
                               Training=rep(NA, length(cfs)),
                               Test=rep(NA, length(cfs)))) %>% 
    rep(3) %>% setNames(c("na","moda","modacl"))
  # lista con las tres bases sobre la cual corre loop:
  partes <- list("na"=base_na, "moda"=base_moda, "modacl"=base_modacl)
  noms <- names(partes)
  for (p in seq_along(partes)) {
    # guarda csv train que se va pisando en cada corrida 
    assign("path_train_csv_"%+%noms[p],
           here("data","working",nombre%+%"_train_"%+%noms[p]%+%"_temp"%+%".csv"))
    write.csv(partes[[p]],
              file=get("path_train_csv_"%+%noms[p]), row.names=F)
    # transforma en arff
    assign("path_train_arff_"%+%noms[p],
           dirname(get("path_train_csv_"%+%noms[p]))%+%'/'%+%nombre%+%
             "_train_"%+%noms[p]%+%"_temp"%+%".arff")
    csv_to_arff(csv_file=get("path_train_csv_"%+%noms[p]),
                arff_file=get("path_train_arff_"%+%noms[p]), jar_file=weka_file)
    # corre j48 para cada CF y guarda resultados en dat_temp_[p]
    for (j in seq_along(cfs)) {
      arbol <- run_j48weka(jar_file = weka_file, conf = cfs[j], minobj = 2,
                           train_file = get("path_train_arff_"%+%noms[p]),
                           test_file = path_test_arff)
      size <- arbol[grepl("Size of the tree",arbol)] %>% 
        substring(regexpr("\t",.)+1) %>% as.numeric
      acctrain <- arbol[grepl("Correctly Classified Instances",arbol)][1] %>% 
        substring(regexpr("%",.)-8,regexpr("%",.)-2) %>% as.numeric
      acctest <- arbol[grepl("Correctly Classified Instances",arbol)][2] %>% 
        substring(regexpr("%",.)-8,regexpr("%",.)-2) %>% as.numeric
      dat_temps[[p]][j,"tamaño"] <- size
      dat_temps[[p]][j,"Training"] <- acctrain
      dat_temps[[p]][j,"Test"] <- acctest
    }
    # guarda cada dat_temps[[p]] en lista de resultados
    res_list[[p]][[i]] <- dat_temps[[p]]
  }
}
# para cada parte de res_list genera data.frame unico para usar en graficos
dat_g <- res_list %>% map(function(x) Reduce(f=rbind,x))

# graficos ----------------------------------------------------------------
# lista para guardar graficos de cada parte
glist <- vector("list",3)
for (p in seq_along(glist)) {
  # chart 1: x-%faltantes / y-tamaño [varios CF]
  (glist[[p]][[1]] <- plot3v(dat_g[[p]],"porcentaje_faltantes","tamaño","confidence_factor"))
  # chart 2: x-%faltantes / y-tamaño [accuracy y varios CF]
  (glist[[p]][[2]] <- plot4v(dat_g[[p]],"porcentaje_faltantes",
                      c("Training","Test"),"accuracy","confidence_factor"))
}
# lista con tantos elementos como graficos
gvec <- Reduce(c,glist)
# guarda .png en "output/charts"
walk2("output/charts/p0"%+%ejercicio%+%"_g0"%+%seq_along(gvec)%+%".png", gvec, ggsave)


