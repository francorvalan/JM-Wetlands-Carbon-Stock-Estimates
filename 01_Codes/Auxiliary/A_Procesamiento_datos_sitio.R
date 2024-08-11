# 

Sys.setlocale("LC_ALL", "en_US.UTF-8")
libraries <- c("dplyr","tidyr","sf","tmap","soiltexture")
lapply(libraries, require,character.only =T)
tmap_mode("view")

source("./01_Codes/Functions/Generic_functions.R")
Sites<- read.csv2("./02_Data/01_Samples/Original_data/Site_locations.csv", fileEncoding = "latin1")

Sites <- Sites %>% mutate(Latitud=to_dec(Latitud),
                          Longitud=to_dec(Longitud))

#%>% mutate_at(c("Latitud","Longitud"),to_dec)
Sites_sf <-Sites %>% st_as_sf(coords = c("Longitud","Latitud"),
                              crs=4326) %>% 
            st_transform(5344)

tm_shape(Sites_sf)+tm_dots()
Sites<- cbind(Sites,Sites_sf %>% st_coordinates())
write.csv2(Sites,"./02_Data/01_Samples/Site_locations_processed.csv")

# Soil data

Soil_data<- read.csv2("./02_Data/01_Samples/Original_data/Soil_samples_24-04-2024.csv")
Soil_data <- Soil_data %>% mutate(Sitio=gsub("-","",Sitio))
Soil_data <- merge(Soil_data,Sites,by.x="Sitio",by.y = "ID")




Soil_data_complete <- Soil_data %>% mutate(SAND=Arena...2.0.005,
                                  SILT=Limo.0.05.0.002,
                                  CLAY=Arcilla..0.002) %>% 
  filter(complete.cases(CLAY))

Soil_data_complete<- Normalizacion_texturas(Soil_data_complete, "SAND", "SILT", "CLAY")

#Soil_data_complete %>% group_by(Sitio) %>% summarise(sum=sum(SAND,SILT,CLAY)) %>% select(sum) %>% pull()
#horizons <- Normalizacion_texturas(Soil_data, "SAND", "SILT", "CLAY")

Soil_data_complete$Clase_textural<- TT.points.in.classes( 
  tri.data    = Soil_data_complete, 
  class.sys   = "USDA.TT", 
  PiC.type    = "t" 
) 

Soil_data_complete <- Soil_data_complete %>% select(!c(Clase_textural_B,Clase_textural_FC))
Soil_data_complete$Clase_textural_USDA<- Soil_textures_Spanish_names(Soil_data_complete,"Clase_textural")
write.csv2(Soil_data_complete,"./02_Data/01_Samples/Processed_data/Soil_data.csv",
           fileEncoding ="Latin1")

Soil_data_st <- Soil_data %>% st_as_sf(coords = c("X","Y"),crs=5344)
st_write(Soil_data_st,"../03_Results/")
