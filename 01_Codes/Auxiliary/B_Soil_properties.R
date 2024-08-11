
# Soil data analysis
dependencies<- c("aqp","stringr","Hmisc","lattice","MASS","dplyr","knitr",
                 "soiltexture","ggplot2","ggtern","kableExtra","ggtern")
lapply(dependencies, require,character.only =T)

source("./01_Codes/Functions/Generic_functions.R")
Soil_data <- read.csv2("./02_Data/01_Samples/Original_data/Soil_samples_24-04-2024.csv")
Sites <-read.csv2("./02_Data/01_Samples/Processed_data/Site_locations_processed.csv") 

Soil_data <- Soil_data %>% mutate(Sitio=gsub("-","",Sitio))

Soil_data <- merge(Soil_data,Sites,by.x="Sitio",by.y = "ID")

Soil_data <- Soil_data %>% mutate(SAND=Arena...2.0.005,
                                  SILT=Limo.0.05.0.002,
                                  CLAY=Arcilla..0.002) %>% 
  filter(complete.cases(CLAY))

Soil_data<- Normalizacion_texturas(Soil_data, "SAND", "SILT", "CLAY")
#horizons <- Normalizacion_texturas(Soil_data, "SAND", "SILT", "CLAY")

Soil_data$Clase_textural<- TT.points.in.classes( 
  tri.data    = Soil_data, 
  class.sys   = "USDA.TT", 
  PiC.type    = "t" 
) 


Soil_data$Clase_textural_B<- Soil_textures_Spanish_names(Soil_data,"Clase_textural")
USDA_triangle_plot <- function(df,Clay="CLAY",
                               Sand="SAND",
                               Silt="SILT",
                               color=T,
                               group=NULL,
                               dot_size=0.3){

  source("./01_Codes/Functions/Generic_functions.R")
USDA_triangle_plot(Soil_data)
############## Triángulos texturales

library()
# soil_data <- data.frame(
#   soil= c("a", "b", "c", "d"),
#   sand = c(15, 18, 57, 32),
#   silt = c(52, 70, 8, 26),
#   clay = c(33, 12, 35, 42),
#   om = c(1, 3, 4, 11),
#   bd = c(1.33, 1.38, 1.76, 1.15)
# )
data(USDA)

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
    data = Soil_data,
    aes(
      x = SAND,
      y = CLAY,
      z = SILT
    ),
    size=0.3) +
  theme_showarrows() +
  theme_clockwise() +
  theme(text = element_text(family = "Helvetica"))  +
  labs(fill = "Referencias") +
  scale_fill_manual(values = custom_palette)
}
# guides(fill=FALSE, color=FALSE)+

Triangle_color_with_legend

