# Funcion para la conversion de coordenadas a 
to_dec<- function(x){
  mult <- rep(1,length(x))
  mult[grepl("S|s|O|o|-",x)] <-  mult[grepl("S|s|O|o|-",x)] *-1
  
  x <- gsub("[O\"]|[S\"]","",x)
  grados<- as.numeric(sapply((strsplit(x,"°")),"[",1))
  minutos <- as.numeric(sapply(strsplit(sapply(strsplit(x,"°"),"[",2),"'"),"[",1))
  segundos<- as.numeric(sapply(strsplit(sapply(strsplit(x,"°"),"[",2),"'"),"[",2))
  return(mult*(grados+(minutos/60)+(segundos/3600)))
}
