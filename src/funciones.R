source("src/load_librerias.R")
# operador para concatenar texto:
"%+%" <- function(a,b) paste(a,b,sep="")

# tabla frecuencias para var categórica:
tabfreq <- function(database,var) {
  dat <- database[[var]]
  f <- table(dat, useNA="always")
  fr <- f/length(dat)*100
  f_cum <- cumsum(f)
  fr_cum <- cumsum(fr)
  return(rbind(f,fr,f_cum,fr_cum) %>% round(1))
}

# convierte .csv a .arff usando weka
csv_to_arff <- function(csv_file, arff_file, jar_file) {
  # lee csv
  df <- read.csv(csv_file,header=T,stringsAsFactors=F)
  # arma texto para indicar a weka los atributos con strings
  str_i <- (map_chr(df, class)=="character") %>% which
  str_i_fl <- str_i[[1]]%+%"-"%+%str_i[[length(str_i)]]
  str_levels <- map(df[str_i], function(x) paste0(sort(unique(x)),collapse=","))
  cmd_str <- (' -L ' %+% '"' %+% str_i %+% ':' %+% str_levels %+% '"') %>% Reduce("%+%",.)
  # guarda un csv temporal en la raiz con "?" en lugar de NA
  temp_csv <- here("convert_temp.csv")
  write.csv(df, file=temp_csv, row.names=F, na="?")
  # genera sentencia para cmd
  cmd_line <- 'java -cp ' %+% '"'%+% jar_file %+%'"' %+% ' weka.core.converters.CSVLoader ' %+%  
    '"'%+% temp_csv %+%'"' %+% ' > ' %+% '"' %+% arff_file %+% '"' %+%
    ' -N ' %+% '"'%+% str_i_fl %+%'"' %+% cmd_str
  # corre en cmd con shell
  shell(cmd_line, intern=T)
  # borra data frame y csv temporal
  rm(df)
  file.remove(temp_csv)
}

# corre arbol J48 desde weka --parametros: ConfidenceFactor(0.25) y Min#ObjetoxHoja(2)
run_j48weka <- function(jar_file,conf=0.25,minobj=2,train_file,test_file) {
  cmd_line <- 'java -cp ' %+% '"'%+% jar_file %+%'"' %+% ' weka.classifiers.trees.J48' %+%
    ' -C ' %+% conf %+% ' -c ' %+% length(base) %+% ' -M ' %+% minobj %+% 
    ' -t '%+% '"'%+%train_file%+%'"' %+% ' -T ' %+% '"'%+%test_file%+%'"'
  tree <- shell(cmd_line,intern=T)
  return(tree)
}

# genera ggplot para 3 variables (las variables entran quoted)
plot3v <- function(dframe,x_var,y_var,color_var) {
  xq <- as.name(x_var); yq <- as.name(y_var); cq <- as.name(color_var)
  g <- ggplot(dframe) + 
    geom_point(aes_q(x=xq, y=yq, color=cq), position="jitter") +
    theme(legend.text=element_text(size=8), legend.title=element_text(size=8))
  return(g)
}

# genera ggplot para 4 variables (las variables entran quoted)
plot4v <- function(dframe,x_var,y_color_vars,y_name,intensi_var) {
 
  dat_melt <- dframe[c(x_var,y_color_vars,intensi_var)] %>% 
    reshape2::melt(id.vars=c(x_var,intensi_var),value.name=y_name,variable.name="tipo")
  
  xq=as.name(x_var); yq=as.name(y_name); cq=as.name("tipo"); iq=as.name(intensi_var) 
  
  g <- ggplot(dat_melt) + 
    geom_point(aes_q(x=xq, y=yq, color=cq, alpha=iq), position="jitter") +
    guides(color=guide_legend(title = NULL)) +
    theme(legend.text=element_text(size=8),legend.title=element_text(size=8))
  
  return(g)
}

# calcula moda de un vector atomico
get_moda <- function(x){ 
  tab <- table(na.omit(x))
  mx <- max(tab)
  # si todas las clases tienen = frecuencia devuelve NA
  if (all(tab == mx)) mod <- NA
  else if (is.numeric(x)) mod <- as.numeric(names(tab)[tab==mx])
  else mod <- names(tab)[tab==mx]
  return(mod)
}
