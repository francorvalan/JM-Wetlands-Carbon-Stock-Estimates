#

Sys.setlocale("LC_ALL", "en_US.UTF-8")
dependencies <- c("terra","sf","stars","tmap")
lapply(dependencies,require,character.only=T)
options(digits = 10)

setwd("C:/Users/francisco.corvalan/OneDrive - Ausenco/Working Documents/06-Geomática/10_Drone/02_Processed_data/GSD_7cm/")
source("C:/Users/francisco.corvalan/JM Wetlands Carbon Stock Estimates/01_Codes/Functions/Generic_functions.R")
files<- list.files("./",pattern = ".tif$",full.names = T,recursive = T)
files <- grep("Auxiliares|DEM",files,invert = T,value = T)

Sitios<- lapply(files,function(x) strsplit(x,"/")[[1]][2]) %>% unlist() %>% unique()
RGB <- grep("RGB",files,value = T)
outputdir <- "../../../08_Results/03_Image_process/01_Drone/B_and_Multiespectral_stacks/"
if(!dir.exists(outputdir))(dir.create(outputdir))
for(i in Sitios[-2]){
  #i=Sitios[[1]]
  files_i<- grep(i,files,value = T)
  # RGB files are in 8 bits and Multiespectral in 16, so I transform it to 
  # syntetic 16 bits
  Blue <- rast(grep("RGB",files_i,value = T))[[3]]
  Blue <- Blue*(2^8)
  Multiestral_i_files <- grep("espectral",files_i,value = T)
  if(length(Multiestral_i_files)>1)({
    Multiestral_i <- lapply(Multiestral_i_files, rast)
    Multiestral_i <- do.call(mosaic,c(Multiestral_i, fun = "mean"))
    }) else({
      Multiestral_i <- rast(Multiestral_i_files)
    })
  Multiestral_i <- Multiestral_i[[1:4]] %>% 
    project("EPSG:5344",method="cubicspline",res=c(0.2,0.2),
            threads=8)
  names(Multiestral_i) <- c("Green","Red","RedEdge","NIR")
  
  Blue <- Blue %>% project(Multiestral_i, mask=T,
                          threads=8)
  
  Blue[Blue==65280] <- NA
  
  
  Multiestral_i[Multiestral_i==65535] <- NA
  names(Blue) <- "Blue"
  
  output<- c(Blue,Multiestral_i)
  writeRaster(output,paste0(outputdir,i,".tif"),datatype="INT2U",overwrite=T)
}

files <- list.files("../../../08_Results/03_Image_process/01_Drone/B_and_Multiespectral_stacks/",
                  pattern = ".tif",full.names = T)


for (i in files) {
  image <- rast(i) %>% 
    project("EPSG:5344",method="cubicspline",res=c(0.4,0.4),
            threads=8)/2^16
  index_i <- Multiespectral_Indexes(image$Blue,image$Green,image$Red,image$NIR,image$RedEdge,scale=1000)
  writeRaster(index_i,paste0("../../../08_Results/03_Image_process/01_Drone/Indices/",
                             gsub(".tif","_scale1000.tif",basename(i))),
              datatype="INT2S",overwrite=T)
  index_i <- NULL
  gc()
}
       