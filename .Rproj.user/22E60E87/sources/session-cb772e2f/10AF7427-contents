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

# Traduccion al Español de los nombres de texturas de suelo
Soil_textures_Spanish_names <- function(df, columna) {
  diccionario <- c(
    "Cl" = "Arcilloso",
    "SiCl" = "Arcillo limoso",
    "SaCl" = "Sandy clay",
    "ClLo" = "Franco arcilloso",
    "SiClLo" = "Franco arcillo limoso",
    "SaClLo" = "Franco arcillo arenoso",
    "Lo" = "Franco",
    "SiLo" = "Franco limoso",
    "SaLo" = "Franco arenoso",
    "Si" = "Limoso",
    "LoSa" = "Arenoso franco",
    "Sa" = "Arenoso"
  )
  
  # Dividir cada valor de la columna en palabras individuales
  palabras <- strsplit(df[[columna]], " ")
  
  # Aplicar reemplazos en cada palabra
  for (i in seq_along(palabras)) {
    palabras[[i]] <- sapply(palabras[[i]], function(word) {
      if (word %in% names(diccionario)) {
        return(diccionario[[word]])
      } else {
        return(word)
      }
    })
  }
  
  # Unir las palabras de nuevo en frases
  frases <- sapply(palabras, paste, collapse = " ")
  #df[[columna]] <- frases
  return(frases)
  
}



Normalizacion_texturas <- function(df,Arena,Limo,Arcilla){
  
  Arena <- sym(Arena)
  Limo <- sym(Limo)
  Arcilla <- sym(Arcilla)
  
  # Total
  total <- df %>%
    mutate(Total = !!Arena + !!Limo + !!Arcilla) %>%
    pull(Total)
  print(total[1:6])
  
  # Normalizacion de la suma
  df <- df %>%
    mutate(across(c(!!Arena, !!Limo, !!Arcilla), ~ (. * 100) / total, .names = "{.col}"))
  
  return(df)
}
#######

USDA_triangle_plot <- function(df,Clay="CLAY",
                   Sand="SAND",
                   Silt="SILT",
                   color=T,
                   group=NULL,
                   dot_size=0.3){
  require(ggtern)
  require(ggplot2)
  data(USDA)
  custom_palette <- c(
    "A: Arcilloso" = "#f89e62",
    "A-Ar: Arcillo arenoso" = "#d9d1b4",
    "F-A-Ar: Franco arcillo arenoso" = "#c7a367",
    "F-A: Franco arenoso" = "#bfb56e",
    "Ar-F: Areno franco" = "#a79a70",
    "Ar: Arenoso" = "#e9e1b0",
    "F-A: Franco arcilloso" = "#ae734b",
    "F: Franco" = "#b5a67d",
    "F-L: Franco limoso" = "#e1d4c4",
    "A-L: Arcillo limoso" = "#ba8460",
    "F-A-L: Franco arcillo limoso" = "#9b8578",
    "L: Limoso" = "#525756")
  
  USDA <- USDA%>%
    mutate(
      Etiqueta = case_when(
        Label == "Clay" ~ "Arcilloso",
        Label == "Sandy Clay" ~ "Arcillo arenoso",
        Label == "Sandy Clay Loam" ~ "Franco arcillo arenoso",
        Label == "Sandy Loam" ~ "Franco arenoso",
        Label == "Loamy Sand" ~ "Areno franco",
        Label == "Sand" ~ "Arenoso",
        Label == "Clay Loam" ~ "Franco arcilloso",
        Label == "Loam" ~ "Franco",
        Label == "Silt Loam" ~ "Franco limoso",
        Label == "Silty Clay" ~ "Arcillo limoso",
        Label == "Silty Clay Loam" ~ "Franco arcillo limoso",
        Label == "Silt" ~ "Limoso",
        TRUE ~ Label  # Keep other values unchanged
      ),
      Etiqueta_inside = case_when(
        Label == "Clay" ~ "Arcilloso",
        Label == "Sandy Clay" ~ "Arcillo\narenoso",
        Label == "Sandy Clay Loam" ~ "Franco\narcillo\narenoso",
        Label == "Sandy Loam" ~ "Franco\narenoso",
        Label == "Loamy Sand" ~ "Areno\nfranco",
        Label == "Sand" ~ "Arenoso",
        Label == "Clay Loam" ~ "Franco\narcilloso",
        Label == "Loam" ~ "Franco",
        Label == "Silt Loam" ~ "Franco\nlimoso",
        Label == "Silty Clay" ~ "Arcillo\nlimoso",
        Label == "Silty Clay Loam" ~ "Franco\narcillo\nlimoso",
        Label == "Silt" ~ "Limoso",
        TRUE ~ Label  # Keep other values unchanged
      ),
      Ref = case_when(
        Label == "Clay" ~ "A",
        Label == "Sandy Clay" ~ "A-Ar",
        Label == "Sandy Clay Loam" ~ "F-A-Ar",
        Label == "Sandy Loam" ~ "F-A",
        Label == "Loamy Sand" ~ "Ar-F",
        Label == "Sand" ~ "Ar",
        Label == "Clay Loam" ~ "F-A",
        Label == "Loam" ~ "F",
        Label == "Silt Loam" ~ "F-L",
        Label == "Silty Clay" ~ "A-L",
        Label == "Silty Clay Loam" ~ "F-A-L",
        Label == "Silt" ~ "L",
        TRUE ~ Label  # Keep other values unchanged
      ),
      Ref_etiqueta = paste0(Ref,": ",Etiqueta))
  USDA$Arena <- USDA$Sand
  USDA$Arcilla <- USDA$Clay
  USDA$Limo <- USDA$Silt
  
  USDA_text <- USDA  %>% group_by(Etiqueta) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  
  USDA_text <- USDA %>% group_by(Etiqueta)%>%
    mutate(across(where(is.numeric), ~mean(., na.rm = TRUE)))
  
  
  
  Triangle_color_with_legend <- ggplot(data = USDA, aes(
    y = Arcilla,
    x = Arena,
    z = Limo
  )) +
    coord_tern(L = "x", T = "y", R = "z") +
    geom_polygon(
      aes(fill = Ref_etiqueta),
      alpha = 0.7,
      size = 0.5,
      color = "black"
    ) +
    geom_text(data = USDA_text,
              aes(label = Ref),
              color = 'black',
              size = 2) +
    geom_point(
      data = df,
      aes(
        x = ensym(Sand),#!!ensym(Sand),
        y = ensym(Clay),#!!ensym(Clay),
        z = ensym(Silt)#!!ensym(Silt),
        #color={{!!group}}
      ),
      size=dot_size) +
    theme_showarrows() +
    theme_clockwise() +
    theme(text = element_text(family = "Helvetica"))  +
    labs(fill = "Referencias") +
    scale_fill_manual(values = custom_palette)

  
  return(Triangle_color_with_legend)
}




Multiespectral_Indexes<- function(BLUE,GREEN,RED,NIR,REDEDGE,scale=1){
  # https://www.mdpi.com/2072-4292/14/5/1251
  BNDVI <-(NIR-BLUE)/(NIR+BLUE) 
  CI_GREEN  <- (NIR/GREEN)-1
  CI_RED  <-(NIR/RED)-1
  CI_REG<- (NIR/REDEDGE)-1
  CVI <-(NIR/GREEN)*(RED/GREEN)
  DVI <- NIR-RED
  DVI_GREEN<- NIR-GREEN
  DVI_REG <-NIR-REDEDGE
  EVI	<-2.5*(NIR-RED)/(1+NIR-2.4*RED)
  EVI2  <- 2.5*(NIR-RED)/(NIR+2.4*RED+1)
  GARI  <- (NIR-(GREEN-1.7*(BLUE-RED)))/
    (NIR+(GREEN-1.7*(BLUE-RED))) 
  GNDVI <-(NIR-GREEN)/
    (NIR+GREEN)
  GOSAVI  <-(NIR-GREEN)/
    (NIR+GREEN+0.16)
  GRVI  <-(GREEN-RED)/
    (GREEN+RED)
  LCI <-(NIR- REDEDGE)/(NIR-RED)
  MCARI <-((REDEDGE-RED)- 0.2*(REDEDGE-GREEN))*(REDEDGE/RED)
  
  MCARI1  <-1.2*(2.5*(NIR-RED)-1.3*(NIR-GREEN))
  MCARI2  <-(3.75*(NIR-RED)-1.95*(NIR-GREEN))/
    sqrt((2*NIR+1)**2-6*(NIR-5*RED)-0.5)
  MNLI  <-(1.5*(NIR**2)-(1.5*GREEN))/
    (NIR**2+RED+0.5)
  MSR	<- ((NIR/RED)-1)/
    sqrt((NIR/RED)+1)
  MSR_REG <- ((NIR/REDEDGE)-1)/
    ((NIR/REDEDGE)+1)
  MTCI  <- (NIR-RED)/
    (NIR-RED)
  NDRE  <- (NIR-REDEDGE)/
    (NIR+REDEDGE)
  NDREI <-(REDEDGE-GREEN)/
    (REDEDGE+GREEN)
  NAVI<- 1 - (RED/NIR)
  NDVI<-(NIR-RED)/
    (NIR+RED)
  OSAVI<-1.6*((NIR-RED)/(NIR+RED+0.16))
  OSAVI_GREEN<-1.6*((NIR-GREEN)/(NIR+GREEN+0.16))
  OSAVI_REG<-1.6*((NIR-REDEDGE)/(NIR+REDEDGE+0.16))
  RDVI<-(NIR-RED)/
    (NIR+RED)
  RDVI_REG<-(NIR-REDEDGE)/
    (NIR+REDEDGE)
  RGBVI<-(GREEN**2-BLUE*RED)/
    (GREEN**2+BLUE*RED)
  RTVI_CORE<-100*(NIR - REDEDGE)-10*(NIR-GREEN)
  RVI<-NIR/RED                                       
  SAVI <-(1.5*(NIR-RED))/(NIR+RED+0.5)
  SAVI_GREEN <- 1.5*(NIR-GREEN)/(NIR+GREEN+0.5)
  S_CCCI <- NDRE/NDVI
  SIPI <- (NIR-BLUE)/(NIR-RED)
  SR_REG <- NIR/REDEDGE
  TCARI<-3*((REDEDGE-RED)-0.2*(REDEDGE-GREEN)*(REDEDGE/RED))
  TCARI_OSAVI<- TCARI/OSAVI
  TVI<-(120*(NIR-GREEN)-200*(RED-GREEN))/2
  VARI<-(GREEN-RED)/(GREEN+RED-BLUE)
  WDRVI<-(0.2*NIR-RED)/(0.2*NIR+RED)
  output <- c(
    BNDVI,CI_GREEN,CI_RED,CVI,DVI,DVI_GREEN,
    DVI_REG,
    EVI, EVI2 ,GARI ,GNDVI ,GOSAVI,GRVI,
    LCI,MCARI,MCARI1,MCARI2,MNLI, MSR,
    MSR_REG,MTCI,NDRE,NDREI,NAVI, NDVI,
    OSAVI,OSAVI_GREEN,OSAVI_REG,RDVI,
    RDVI_REG,RGBVI,RTVI_CORE,RVI,SAVI, 
    SAVI_GREEN,S_CCCI,SIPI, SR_REG,TCARI,
    TCARI_OSAVI,TVI,VARI,WDRVI)*scale
  names <- c(
    "BNDVI", "CI_GREEN", "CI_RED", "CVI", "DVI", "DVI_GREEN",
    "DVI_REG", "EVI", "EVI2", "GARI", "GNDVI", "GOSAVI", "GRVI",
    "LCI", "MCARI", "MCARI1", "MCARI2", "MNLI", "MSR",
    "MSR_REG", "MTCI", "NDRE", "NDREI", "NAVI", "NDVI",
    "OSAVI", "OSAVI_GREEN", "OSAVI_REG", "RDVI",
    "RDVI_REG", "RGBVI", "RTVI_CORE", "RVI", "SAVI", 
    "SAVI_GREEN", "S_CCCI", "SIPI", "SR_REG", "TCARI",
    "TCARI_OSAVI", "TVI", "VARI", "WDRVI"
  )
  names(output) <- names
  return(output)
}

# TT.plot( class.sys = "USDA.TT",tri.data    = Soil_data )
# 
# Soil_data %>% ggplot(aes(y=SAND,x=sp_dominante,fill = sp_dominante))+
#   geom_boxplot()










# Soil_data <- Soil_data %>% mutate("Sp dominante"=sp_dominante)
# Triangle_color_with_legend <- ggplot(data = USDA, aes(
#   y = Arcilla,
#   x = Arena,
#   z = Limo
# )) +
#   coord_tern(L = "x", T = "y", R = "z") +
#   geom_polygon(
#     aes(fill = Ref_etiqueta),
#     alpha = 0.6,
#     size = 0.5,
#     color = "black"
#   ) +
#   geom_text(data = USDA_text,
#             aes(label = Ref),
#             color = 'black',
#             size = 2) +
#   geom_point(
#     data = Soil_data,
#     aes(
#       x = SAND,
#       y = CLAY,
#       z = SILT,
#       color=`Sp dominante`
#       #size=Carbono.Orgánico....
#     ),size=2) +
#   theme_showarrows() +
#   theme_clockwise() +
#   labs(
#     yarrow = "Arcilla < 2 µm",
#     zarrow = "Limo 2 - 50 µm (%)",
#     xarrow = "Arena 50 - 2000 µm (%)",
#     fill = "Referencias"
#   ) +
#   theme(text = element_text(family = "Helvetica"))  +
#   scale_fill_manual(values = custom_palette)
# guides(fill=FALSE, color=FALSE)+

# Triangle_color_with_legend
# jpeg("./03_Results/01_Plots/Soil_texture.jpg",res=200,width = 2000,height = 1600)
# Triangle_color_with_legend
# dev.off()

