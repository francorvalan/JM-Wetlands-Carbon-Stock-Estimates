# ver https://statsandr.com/blog/anova-in-r/

Sys.setlocale("LC_ALL", "en_US.UTF-8")
libraries <- c("dplyr","tidyr","sf","tmap","soiltexture","ggplot2")
lapply(libraries, require,character.only =T)
# tmap_mode("view")

source("./01_Codes/Functions/Generic_functions.R")
# Sites<- read.csv2("./02_Data/01_Samples/Original_data/Site_locations.csv", fileEncoding = "latin1")
# 
# Sites <- Sites %>% mutate(Latitud=to_dec(Latitud),
#                           Longitud=to_dec(Longitud))
# 
# #%>% mutate_at(c("Latitud","Longitud"),to_dec)
# Sites_sf <-Sites %>% st_as_sf(coords = c("Longitud","Latitud"),
#                               crs=4326) %>% 
#             st_transform(5344)
# 
# tm_shape(Sites_sf)+tm_dots()
# Sites<- cbind(Sites,Sites_sf %>% st_coordinates())
# #write.csv2(Sites,"./02_Data/01_Samples/Site_locations_processed.csv")
# 
# # Soil data
# Soil_data<- read.csv2("./02_Data/01_Samples/Original_data/Soil_samples_24-04-2024.csv")
# Soil_data <- Soil_data %>% mutate(Sitio=gsub("-","",Sitio))
# Soil_data <- merge(Soil_data,Sites,by.x="Sitio",by.y = "ID")
# 
# 
# Soil_data_complete <- Soil_data %>% mutate(SAND=Arena...2.0.005,
#                                   SILT=Limo.0.05.0.002,
#                                   CLAY=Arcilla..0.002) %>% 
#   filter(complete.cases(CLAY))
# 
# Soil_data_complete<- Normalizacion_texturas(Soil_data_complete, "SAND", "SILT", "CLAY")
# 
# 
# Soil_data_complete$Clase_textural<- TT.points.in.classes( 
#   tri.data    = Soil_data_complete, 
#   class.sys   = "USDA.TT", 
#   PiC.type    = "t" 
# ) 
# 
# Soil_data_complete <- Soil_data_complete %>% select(!c(Clase_textural_B,Clase_textural_FC))
# Soil_data_complete$Clase_textural_USDA<- Soil_textures_Spanish_names(Soil_data_complete,"Clase_textural")
# #write.csv2(Soil_data_complete,"./02_Data/01_Samples/Processed_data/Soil_data.csv",
# #           fileEncoding ="Latin1")
# 
# Soil_data_st <- Soil_data %>% st_as_sf(coords = c("X","Y"),crs=5344)
# 
# #st_write(Soil_data_st,"./02_Data/01_Samples/Processed_data/Soil_data.gpkg")
# #st_write(Sites_sf,"./02_Data/01_Samples/Processed_data/Sites.gpkg")
# 
# 

# Biomass data
Biomasa <- read.csv2("./02_Data/01_Samples/Processed_data/Biomass_samples.csv",
                     fileEncoding ="Latin1")
ggplot(Biomasa) +
  aes(x = sp_dominante, y = PS_Verano, color = sp_dominante) +
  geom_jitter() +
  theme(legend.position = "none")


Biomasa_st <- Biomasa %>% 
  st_as_sf(coords = c("Este","Norte"),crs=5344)
st_write(Biomasa_st,"./02_Data/01_Samples/Processed_data/Biomasa.gpkg")
library(ggplot2)
Biomasa %>% ggplot(aes(x=sp_dominante,y = PS_Verano,fill=sp_dominante))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5,
                                   size = 10),
        plot.title = element_text(hjust = 0.5))

fisher.test(x = tabla, alternative = "two.sided")


modelo <- lm(PS_Verano ~ sp_dominante, data = Biomasa)
resultado_anova <- anova(modelo)
print(resultado_anova)

library(multcomp)
test_fisher <- glht(modelo, linfct = mcp(sp_dominante = "Tukey"))
summary(test_fisher)

# Realizar el test de Tukey HSD
tukey <- TukeyHSD(aov(PS_Verano ~ sp_dominante, data = Biomasa))
print(tukey)
plot(tukey)
stats::plot


tukey_df <- as.data.frame(tukey$sp_dominante)

# Agregar nombres de comparaciones como columna separada
tukey_df$Comparison <- rownames(tukey_df)

# Graficar con ggplot2
ggplot(tukey_df, aes(x = Comparison, y = diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() + 
  labs(title = "Tukey HSD Test - Diferencias de medias entre especies",
       x = "Comparaciones",
       y = "Diferencia de medias")

########################################################
anova <- aov(PS_Verano ~ sp_dominante, data = Biomasa)
summary(anova)
# Tukey's test
tukey <- TukeyHSD(anova)
anova

# compact letter display
library(multcompView)


tukey <- TukeyHSD(modelo)

cld <- multcompLetters4(anova, tukey)
print(cld)
# table with factors and 3rd quantile
Tk <- group_by(Biomasa,sp_dominante) %>%
  summarise(mean=mean(PS_Verano), quant = quantile(PS_Verano, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$sp_dominante)
Tk$cld <- cld$Letters

# Tuckey test representation :
plot(tukey , las=1 , col="brown")

print(Tk)

# I need to group the treatments that are not different each other together.
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

# Apply the function on my dataset
LABELS <- generate_label_df(tukey , "sp_dominante")


# A panel of colors to draw each group with the same color :
my_colors <- c( 
  rgb(143,199,74,maxColorValue = 255),
  rgb(242,104,34,maxColorValue = 255), 
  rgb(111,145,202,maxColorValue = 255)
)

# Draw the basic boxplot
a <- boxplot(data$value ~ data$treatment , ylim=c(min(data$value) , 1.1*max(data$value)) , col=my_colors[as.numeric(LABELS[,1])] , ylab="value" , main="")

# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(data$treatment)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] )


# boxplot
ggplot(Biomasa, aes(sp_dominante, PS_Verano)) + 
  geom_boxplot(aes(fill = sp_dominante), show.legend = FALSE) +
  labs(x="sp dominante", y="Peso seco (g)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = Tk, aes(x = sp_dominante, y = quant, label = cld), size = 3, vjust=-1, hjust =-1) +
  scale_fill_brewer(palette = "Pastel1")
################################################


# Obtener las letras de significancia
letras <- multcompLetters4(modelo, tukey)

# Convertir en un dataframe para usar con ggplot2
letras_df <- as.data.frame(letras$sp_dominante)
letras_df$sp_dominante <- rownames(letras_df)
colnames(letras_df)[1] <- "Letter"
