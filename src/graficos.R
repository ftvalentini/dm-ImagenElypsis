library(ggplot2)

# punto 6 -----------------------------------------------------------------
# Datasets con "ruido" (0-35 by 1 -- 20%validation sin tocar):
dat6 <- data.frame(porcentaje_ruido=rep(seq(0,35,by=1),each=11),
                   confidence_factor=seq(0,0.5,by=0.05),
                   Nº_de_nodos=rexp(396),
                   Training=rexp(396),
                   Test=runif(396))
dat6_2 <- dat6[-3] %>% 
  reshape2::melt(id.vars=1:2,value.name="accuracy",variable.name="tipo")
    # chart: x-%ruido / y-#nodos [modelos con CF 0-0.5 by 0.05]
(g6_01 <- ggplot(dat6) +
    geom_point(aes(x=porcentaje_ruido,y=Nº_de_nodos,color=confidence_factor),
               position="jitter") +
    theme(legend.text=element_text(size=8),legend.title=element_text(size=8))
)
    # chart: x-%ruido / y-accuracy training [modelos con CF 0-0.5 by 0.05]
(g6_02 <- ggplot(dat6_2) +
    geom_point(aes(x=porcentaje_ruido,y=accuracy,color=tipo,alpha=confidence_factor),
               position="jitter") +
    guides(color=guide_legend(title = NULL)) +
    theme(legend.text=element_text(size=8),legend.title=element_text(size=8))
)
    # chart: x-%ruido / y-accuracy test [modelos con CF 0-0.5 by 0.05]
    # ESTA HECHO 2EN1 CON EL DE ARRIBA; SEPARARLOS SI ES CHOCLO


# punto 7 -----------------------------------------------------------------
  # Datasets con edad en bins equalwidth (1-20 bins by 1):
dat7 <- data.frame(Nº_bins=rep(seq(1,20,by=1),each=11),
                   confidence_factor=seq(0,0.5,by=0.05),
                   Nº_de_nodos=rexp(220),
                   Training=rexp(220),
                   Test=runif(220))
dat7_2 <- dat6[-3] %>% 
  reshape2::melt(id.vars=1:2,value.name="accuracy",variable.name="tipo")
    # chart: x-#bins / y-#nodos [modelos con CF 0-0.5 by 0.05]
(g7_01 <- ggplot(dat7) +
    geom_point(aes(x=Nº_bins,y=Nº_de_nodos,color=confidence_factor),
               position="jitter") +
    theme(legend.text=element_text(size=8),legend.title=element_text(size=8))
)
    # chart: x-#bins / y-accuracy training [modelos con CF 0-0.5 by 0.05]
(g7_02 <- ggplot(dat7_2) +
    geom_point(aes(x=Nº_bins,y=accuracy,color=tipo,alpha=confidence_factor),
               position="jitter") +
    guides(color=guide_legend(title = NULL)) +
    theme(legend.text=element_text(size=8),legend.title=element_text(size=8))
)
    # chart: x-#bins / y-accuracy test [modelos con CF 0-0.5 by 0.05]
    # ESTA HECHO 2EN1 CON EL DE ARRIBA; SEPARARLOS SI ES CHOCLO
  # Datasets con edad en bins equalfreq (1-20 bins by 1):
    # chart: x-#bins / y-#nodos [modelos con CF 0-0.5 by 0.05]
    # chart: x-#bins / y-accuracy training [modelos con CF 0-0.5 by 0.05]
    # chart: x-#bins / y-accuracy test [modelos con CF 0-0.5 by 0.05]
  # edad discretiz supervisada (un solo #bins que da el algoritmo):
dat7b <- data.frame(confidence_factor=rnorm(10),
                   Nº_de_hojas=rnorm(10),
                   Nº_de_nodos=runif(10),
                   Training=rexp(10),
                   Test=runif(10)) 
dat7b_2 <- dat7b[-c(2,3)] %>% 
  reshape2::melt(id.vars=1,value.name="accuracy",variable.name="tipo")
    # chart: x-CFactor / y-#nodos
(g7b_01 <- ggplot(dat7b) +
    geom_line(aes(x=confidence_factor,y=Nº_de_nodos),size=1) +
    theme(legend.title=element_blank())
)
    # chart: x-CFactor / y-accuracy [train vs test] 
(g7b_02 <- ggplot(dat7b_2) +
    geom_line(aes(x=confidence_factor,y=accuracy,color=tipo),size=1) +
    theme(legend.title=element_blank())
)
