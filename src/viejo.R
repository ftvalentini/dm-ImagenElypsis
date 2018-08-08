### exploracion vieja

# carga librerias
source("src/load_librerias.R")
# carga base raw
path_base <- here("data","raw","base_pba.csv")
base <- read.csv(path_base,header=T,stringsAsFactors=F)

table(base$quien,useNA="always")
repes <- vector("list")
for (i in unique(base$quien)) {
  repes[[i]] <- base[base$quien==i,c("callid")] %>% anyDuplicated()
}
unlist(repes)
# resultado: no hay "callid" repetido por candidato
length(unique(base$callid))
# 47670 encuestados
base_new <- tidyr::spread(base[!(names(base)%in%"imagen_reduc")],quien,imagen)
# hay varios valores de "imagen_reduc" para cada id (y la cant de valores varía)
any(duplicated(base_new$callid))
# aun sacando "imagen_reduc" se repiten callids
callidrep <- base_new[which(duplicated(base_new$callid))[1],"callid"]
base_new[base_new$callid==callidrep,]
# otra vble con valores repetidos es "pondera2"