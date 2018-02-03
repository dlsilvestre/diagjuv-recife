#====================================================#
# DIAGNOSTICO DA CONDICAO JUVENIL NO RECIFE          #              
#====================================================#
# FUNCOES GERAIS                                     #
#----------------------------------------------------#
# Prefeitura da Cidade do Recife                     #
# Secretaria Executiva de Juventude                  #
#----------------------------------------------------#
# Recife 2018                                        #
#----------------------------------------------------#
# Claudio A. Monteiro                                #
# claudiomonteiro@gmail.com                          #
#----------------------------------------------------#
# Qualquer duvida contate o desenvolvedor            #
# #UseSoftwareLivre                                  #
#----------------------------------------------------#


# tema ggplot2 para graficos
tema_massa <- function (base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(colour= "black",size=11,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=11,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=12,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="bold"),
          panel.grid.major = element_line(colour = grey(0.85)), 
          panel.grid.minor = element_line(colour = grey(1)),
          legend.key.size = unit(9, "mm"),
          legend.text = element_text(size = 9, hjust = 3, vjust = 3),
          legend.title = element_text(size = 9),
          axis.line = element_line(size = 1, colour = "grey70"))
}


data <- jovem_morte_bairro
variable <- jovem_morte_bairro$Freq
shape <- shp_recife1

mapa.funcao <- function(shape, data, variable, legendtitle, pallete) { 
  library(stringi); library(ggplot2)
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
  
  data$EBAIRRNOME = data$localidade
  data$EBAIRRNOME = toupper(data$EBAIRRNOME)
  data$EBAIRRNOME = stri_trans_general(data$EBAIRRNOME , "Latin-ASCII")
  data$EBAIRRNOME = best_match(data$EBAIRRNOME, shape$EBAIRRNOME)
  data$variavel   = variable
  
  # merge data with shapefile
  shp_data <- merge(shape, data, by = "EBAIRRNOME", all = T)
  
  # definir labels no mapa (4 maiores, 4 menores)
  shp_data$variavel[is.na(shp_data$variavel)] = 0
  shp_data = shp_data[order(shp_data$variavel),]
  shp_data$bairros_detasq = 1
  shp_data$bairros_detasq[1:4] = ""
  shp_data$bairros_detasq[c(length(shp_data)-4):c(length(shp_data))] = ""
  
  shp_data$bairros_detasq = with(shp_data, paste0(shp_data$bairros_detasq, shp_data$EBAIRRNOME))
  shp_data$bairros_detasq_cod = grepl(shp_data$bairros_detasq, pattern = "1")
  shp_data$bairros_detasq[shp_data$bairros_detasq_cod == TRUE ] = ""
  
  # tranformar shapefile em polygonsdataframe
  data_fortity = fortify(shp_data, region = "EBAIRRNOME")
  localidade = shp_data@data$EBAIRRNOME
  
  # extrair centroides dos poligonos
  centroids.df = as.data.frame(coordinates(shp_data))
  names(centroids.df) = c("Longitude", "Latitude")  #more sensible column localidades
  
  # base para plotagem
  variavel = shp_data@data$variavel
  nomes_centroides = shp_data$bairros_detasq
  map_dataframe = data.frame(localidade, variavel, centroids.df, nomes_centroides)
  
  plot = ggplot(data = map_dataframe, aes(map_id = localidade)) + 
    geom_map(aes(fill = shp_data$variavel),colour = grey(0.85),  map = data_fortity) +
    expand_limits(x = data_fortity$long, y = data_fortity$lat) +
    scale_fill_viridis(name = legendtitle, option= pallete, direction = -1) +
    geom_label_repel(aes(label = nomes_centroides, x = Longitude, y = Latitude), size = 3, color = "black") + 
    coord_fixed(1) +
    theme_nothing(legend = T)

    return(plot)
}







