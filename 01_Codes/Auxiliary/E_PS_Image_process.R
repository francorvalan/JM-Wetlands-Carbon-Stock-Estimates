# 

dependencies <- c("sf","terra","tmap")
lapply(dependencies,require,character.only=T)

PS_Images<- list.files("./02_Data/02_Covs/PS_Images/",pattern = ".tif$",full.names = T,recursive = T)
PS_Images <- grep("8b",PS_Images,value = T)
PS_Enero <- grep("20240125",PS_Images,value = T)
PS_Abril <- grep("20240424",PS_Images,value = T)

PS_Enero <- lapply(PS_Enero,rast)
PS_Abril <- lapply(PS_Abril,rast)

PS_Enero <- do.call(mosaic,c(PS_Enero, fun = "mean"))
PS_Abril <- do.call(mosaic,c(PS_Abril, fun = "mean"))
writeRaster(PS_Abril,"./02_Data/02_Covs/PS_Images/20240125/PS_abril.tif")
writeRaster(PS_Enero,"./02_Data/02_Covs/PS_Images/20240125/PS_enero.tif")

PS_Index <- function(RED,GREEN,BLUE,REDEDGE,NIR){
  BNDVI <-(NIR-BLUE)/(NIR+BLUE) 
  # CI_GREEN  <- (NIR/GREEN)-1
  # CI_RED  <-(NIR/RED)-1
  # CI_REG<- (NIR/REDEDGE)-1
  # CVI <-(NIR/GREEN)*(RED/GREEN)
  # DVI <- NIR-RED
  # DVI_GREEN<- NIR-GREEN
  # DVI_REG <-NIR-REDEDGE
  EVI	<-2.5*(NIR-RED)/(1+NIR-2.4*RED)
  # EVI2  <- 2.5*(NIR-RED)/(NIR+2.4*RED+1)
  GARI  <- (NIR-(GREEN-1.7*(BLUE-RED)))/
    (NIR+(GREEN-1.7*(BLUE-RED))) 
  # GNDVI <-(NIR-GREEN)/
  #   (NIR+GREEN)
  # GOSAVI  <-(NIR-GREEN)/
  #   (NIR+GREEN+0.16)
  GRVI  <-(GREEN-RED)/
    (GREEN+RED)
  LCI <-(NIR- REDEDGE)/(NIR-RED)
  MCARI <-((REDEDGE-RED)- 0.2*(REDEDGE-GREEN))*(REDEDGE/RED)
  
  MCARI1  <-1.2*(2.5*(NIR-RED)-1.3*(NIR-GREEN))
  # MCARI2  <-(3.75*(NIR-RED)-1.95*(NIR-GREEN))/
  #   sqrt((2*NIR+1)**2-6*(NIR-5*RED)-0.5)
  # MNLI  <-(1.5*(NIR**2)-(1.5*GREEN))/
  #   (NIR**2+RED+0.5)
  # MSR	<- ((NIR/RED)-1)/
  #   sqrt((NIR/RED)+1)
  # MSR_REG <- ((NIR/REDEDGE)-1)/
  #   ((NIR/REDEDGE)+1)
  # MTCI  <- (NIR-RED)/
  #   (NIR-RED)
  NDRE  <- (NIR-REDEDGE)/
    (NIR+REDEDGE)
  NDREI <-(REDEDGE-GREEN)/
    (REDEDGE+GREEN)
  # NAVI<- 1 - (RED/NIR)
  NDVI<-(NIR-RED)/
    (NIR+RED)
  OSAVI<-1.6*((NIR-RED)/(NIR+RED+0.16))
  # OSAVI_GREEN<-1.6*((NIR-GREEN)/(NIR+GREEN+0.16))
  # OSAVI_REG<-1.6*((NIR-REDEDGE)/(NIR+REDEDGE+0.16))
  # RDVI<-(NIR-RED)/
  #   (NIR+RED)
  # RDVI_REG<-(NIR-REDEDGE)/
  #   (NIR+REDEDGE)
  RGBVI<-(GREEN**2-BLUE*RED)/
    (GREEN**2+BLUE*RED)
  RTVI_CORE<-100*(NIR - REDEDGE)-10*(NIR-GREEN)
  #RVI<-NIR/RED                                       
  SAVI <-(1.5*(NIR-RED))/(NIR+RED+0.5)
  SAVI_GREEN <- 1.5*(NIR-GREEN)/(NIR+GREEN+0.5)
  # S_CCCI <- NDRE/NDVI
  # SIPI <- (NIR-BLUE)/(NIR-RED)
  # SR_REG <- NIR/REDEDGE
  # TCARI<-3*((REDEDGE-RED)-0.2*(REDEDGE-GREEN)*(REDEDGE/RED))
  # TCARI_OSAVI<- TCARI/OSAVI
  # TVI<-(120*(NIR-GREEN)-200*(RED-GREEN))/2
  VARI<-(GREEN-RED)/(GREEN+RED-BLUE)
  # WDRVI<-(0.2*NIR-RED)/(0.2*NIR+RED)
  output <- c(PS_Enero,BNDVI,EVI,GARI,GRVI,LCI,MCARI,
             MCARI1,NDRE,NDREI,NDVI,OSAVI,RGBVI,
             RTVI_CORE,SAVI,SAVI_GREEN,VARI)
  names(output)<- c("PS_Enero","BNDVI","EVI","GARI","GRVI","LCI","MCARI",
                    "MCARI1","NDRE","NDREI","NDVI","OSAVI","RGBVI",
                    "RTVI_CORE","SAVI","SAVI_GREEN","VARI")
  
  return(output)
}
start <- Sys.time()
PS_Abril_Index <- PS_Index(PS_Abril$red,
         PS_Abril$green,
         PS_Abril$blue,
         PS_Abril$rededge,
         PS_Abril$nir)
print(Sys.time() - start)

writeRaster(PS_Abril_Index,"./")
