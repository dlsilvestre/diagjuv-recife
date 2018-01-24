#====================================================#
# DIAGNÓSTICO DA JUVENTUDE NA CIDADE DO RECIFE       #              
#====================================================#
# MOBILIDADE URBANA                                  #
#----------------------------------------------------#
# Secretaria Executiva de Juventude                  #
#----------------------------------------------------#
# Recife 2017                                        #
#----------------------------------------------------#
# Claudio A. Monteiro                                #
# claudiomonteiro@gmail.com                          #
#----------------------------------------------------#
# Qualquer duvida contate o desenvolvedor            #
# #UseSoftwareLivre                                  #
#----------------------------------------------------#

# instalar pacotes necessarios
# install.packages(c("readr","plyr", "rgdal", "ggplot2", "ggmap", "maps", "mapdata", "raster"), dependencies = T )

# carregar pacotes
library(readr)
library(plyr)
library(rgdal)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(raster)

# carregar dados
setwd("C:/Users/Monteiro-DataPC/Documents/Research/Juventude OIT PCR/Mobilidade/Dados Originais/")
shp_recife <- shapefile("Bairros.shp")
estacoesbike <- read_delim("estacoesbike.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
acidentes_2015 <-  read_delim("acidentes-2015.csv",  ";", escape_double = FALSE, trim_ws = TRUE)

# pasta de destino dos graficos
setwd("C:/Users/Monteiro-DataPC/Documents/Research/Juventude OIT PCR/Mobilidade/Resultados/")

#---- carregar shape dos bairros ----#

# converter shapefile em dataframe para ser usado pelo ggplot2
shp_recife@data$id <- rownames(shp_recife@data)
shapefile_points <- fortify(shp_recife, region = "id")

# juntar dados e shapedata
shp_recife = join(shapefile_points, shp_recife@data, by="id")

#==== Ciclovias e BikePE ====#

#---- BikePE ----#

# Google Map Recife
gg_recife <- get_map(location = c(lon = -34.896964, lat = -8.067562 ),
               zoom = 12, maptype = 'terrain')

# visualizar estacoes
rec_bike <- ggmap(gg_recife) +
  geom_point(data=estacoesbike,  aes(x= longitude , y = latitude),shape= 18, size = 3, color = "orange")
rec_bike

# salvar
ggsave("rec_bike.png", width = 8, height = 8, units = "in")


#-------------------#
# shape ciclovia
shp_ciclovia <- shapefile("~/layers ciclofaixa/POLYLINE.shp")
shp_ciclovia <- shapefile(file.choose())

# Next the shapefile has to be converted to a dataframe for use in ggplot2
shp_ciclovia@data$id <- rownames(shp_ciclovia@data)
shapefile_points2 <- fortify(shp_ciclovia, region = "id")

# merge data and shapefile 2
shp_ciclovia = join(shapefile_points2, shp_ciclovia@data, by="id")

ggplot(data = shp_recife) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = grey(0.3)) + 
  coord_fixed(1) +
  guides(fill=FALSE) +
  geom_path(data = shp_ciclovia_df, aes(x = long, y = lat, group = group), color = "#40a4df")+
  theme_minimal()

#==== Acidentes com Vitimas ====#

#---- 2015 ----#
acid_bairro2015 <- data.frame(table(acidentes_2015$bairro))
acid_bairro2015 <- acid_bairro2015[order(acid_bairro2015$Freq),]

best_match= function(string_vector,string_replacement){
  s<-string_replacement %>% 
    purrr::map_int(~{
      .x %>% 
        RecordLinkage::levenshteinSim(string_vector) %>%
        match(max(.),.)
    })
  string_vector[s] = string_replacement
  return(string_vector)
}

acid_bairro2015$EBAIRRNOME = acid_bairro2015$Var1
acid_bairro2015$EBAIRRNOME = toupper(acid_bairro2015$EBAIRRNOME)
acid_bairro2015$EBAIRRNOME = stri_trans_general(acid_bairro2015$EBAIRRNOME , "Latin-ASCII")
acid_bairro2015$EBAIRRNOME = best_match(acid_bairro2015$EBAIRRNOME, shape$EBAIRRNOME)

# merge data with shapefile
acid_bairro2015 <- merge(acid_bairro2015, shp_recife@data[,3:4], by = "EBAIRRNOME", all = T)
acid_bairro2015$Localidade <- acid_bairro2015$EBAIRRNOME


#==== MAP FUNCTION ====#

mapa.funcao <- function(shape, data, variable) { 
  library(ggrepel)
  library(purrr)
  library(ggplot2)
  library(stringi)
  
  
  # function to create merge string based on similarity
  best_match= function(string_vector,string_replacement){
    s<-string_replacement %>% 
      purrr::map_int(~{
        .x %>% 
          RecordLinkage::levenshteinSim(string_vector) %>%
          match(max(.),.)
      })
    string_vector[s] = string_replacement
    return(string_vector)
  }
  
  data$EBAIRRNOME = data$Localidade
  data$EBAIRRNOME = toupper(data$EBAIRRNOME)
  data$EBAIRRNOME = stri_trans_general(data$EBAIRRNOME , "Latin-ASCII")
  data$EBAIRRNOME = best_match(data$EBAIRRNOME, shape$EBAIRRNOME)
  data$variavel <- variable
  data$variavel[is.na(data$variavel)] <- 0

  
  # merge data with shapefile
  shp_data <- merge(shape, data, by = "EBAIRRNOME", all = T)
  
  # definir labels no mapa
  shp_data <- shp_data[order(shp_data$variavel),]
  shp_data$bairros_detasq <- 1
  #shp_data$bairros_detasq[1:5] <- ""
  shp_data$bairros_detasq[c(length(shp_data)-5):c(length(shp_data))] <- ""
  
  shp_data$bairros_detasq <- with(shp_data, paste0(shp_data$bairros_detasq, shp_data$Localidade))
  shp_data$bairros_detasq_cod <- grepl(shp_data$bairros_detasq, pattern = "1")
  shp_data$bairros_detasq[shp_data$bairros_detasq_cod == TRUE ] <- ""
  
  # tranformar shapefile em polygonsdataframe
  data_fortity <- fortify(shp_data, region = "Localidade")
  Localidade <- shp_data@data$Localidade
  
  # extrair centroides dos poligonos
  centroids.df <- as.data.frame(coordinates(shp_data))
  names(centroids.df) <- c("Longitude", "Latitude")  #more sensible column Localidades
  
  # This shapefile contained population data, let's plot it.
  variavel <- shp_data@data$variavel
  nomes_centroides <- shp_data$bairros_detasq
  
  map_dataframe <- data.frame(Localidade, variavel, centroids.df, nomes_centroides)
  
  plot <- ggplot(data = map_dataframe, aes(map_id = Localidade)) + 
    geom_map(aes(fill = shp_data$variavel), colour = grey(0.85),  map = data_fortity) +
    expand_limits(x = data_fortity$long, y = data_fortity$lat) + 
    # scale_fill_gradient(colours=inferno(10, alpha = 1, begin = 1, end = 0))+
    scale_fill_gradient(name = "" , low=	"#ffad60", high= "#4c0000")+
    geom_label_repel(aes(label = nomes_centroides, x = Longitude, y = Latitude),
                     size = 3, color = "black") + #add labels at centroids
    coord_fixed(1) +
    #labs(title = title)
    theme_nothing(legend = T)
  return(plot)
}

acid2 <- mapa.funcao(shp_recife, acid_bairro2015, acid_bairro2015$Freq)
ggsave("scid_bairros_2015_2.png", acid2, width = 6, height = 9, units = "in")


