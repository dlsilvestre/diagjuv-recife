#====================================================#
# DIAGNÓSTICO DA JUVENTUDE NA CIDADE DO RECIFE       #              
#====================================================#
# SAÚDE E MEIO AMBIENTE                              #
#----------------------------------------------------#
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

# instalar pacotes necessarios
# install.packages(c("devtools", "readxl"))

library(readxl); library(devtools); library(dplyr);  library(ggrepel); 
library(purrr); library(ggplot2);library(stringi); library(rgdal); library(ggplot2); 
library(maps); library(mapdata); library(raster); library(ggmap)


# carregar shapefile 1 (completo)
shp_recife1 <- shapefile("Dados Gerais/bases_cartograficas/Bairros.shp")

#=================================#

#==== MAP FUNCTION ====#
cc <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=100))

mapa.funcao <- function(shape, data, variable, title) { 
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
  data$variavel <- variable

  # merge data with shapefile
  shp_data <- merge(shape, data, by = "EBAIRRNOME", all = T)
  
  # definir labels no mapa
  shp_data <- shp_data[order(shp_data$variavel),]
  shp_data$bairros_detasq <- 1
  #shp_data$bairros_detasq[1:5] <- ""
  shp_data$bairros_detasq[c(length(shp_data)-5):c(length(shp_data))] <- ""
  
  shp_data$bairros_detasq <- with(shp_data, paste0(shp_data$bairros_detasq, shp_data$localidade))
  shp_data$bairros_detasq_cod <- grepl(shp_data$bairros_detasq, pattern = "1")
  shp_data$bairros_detasq[shp_data$bairros_detasq_cod == TRUE ] <- ""
  
  # tranformar shapefile em polygonsdataframe
  data_fortity <- fortify(shp_data, region = "localidade")
  localidade <- shp_data@data$localidade
  
  # extrair centroides dos poligonos
  centroids.df <- as.data.frame(coordinates(shp_data))
  names(centroids.df) <- c("Longitude", "Latitude")  #more sensible column localidades
  
  # This shapefile contained population data, let's plot it.
  variavel <- shp_data@data$variavel
  nomes_centroides <- shp_data$bairros_detasq
  map_dataframe <- data.frame(localidade, variavel, centroids.df, nomes_centroides)
  
  plot <- ggplot(data = map_dataframe, aes(map_id = localidade)) + 
    geom_map(aes(fill = shp_data$variavel), colour = grey(0.85),  map = data_fortity) +
    expand_limits(x = data_fortity$long, y = data_fortity$lat) + 
    scale_fill_viridis() +
    geom_label_repel(aes(label = nomes_centroides, x = Longitude, y = Latitude),
                     size = 3, color = "black") + #add labels at centroids
    coord_fixed(1) +
   labs(title = title)+
   theme_nothing(legend = T)
  return(plot)
}


#=================================#
#           MORTALIDADE    
#=================================# 

# instalar pacote 'datasus'
devtools::install_github("danicat/datasus")
library(datasus)

# selecionar pasta de trabalho do pacote
datasus.init('Saúde e Meio Ambiente/dados')

# baixar dados de mortalidade por causas externas para PE
df <- sim.load("DOEXT", c(2010:2014), "PE")

# PROBLEMA COM O ACESSO AOS ARQUIVOS - ENVIAR MENSAGEM A DANICAT

#=================================#
#            SANEAMENTO
#=================================#

# carregar bases
esgoto2000 <- read_excel("Saúde e Meio Ambiente/dados/esgotamento-CENSO2000.xls", col_names = FALSE)
esgoto2010 <- read_excel("Saúde e Meio Ambiente/dados/esgotamento-CENSO2010.xls", col_names = FALSE)

# selecionar dados por bairros
esgoto2000 <- esgoto2000[712:805,]
esgoto2010 <- esgoto2010[1018:1111,]

# recodificar variaveis
colnames(esgoto2000) 
colnames(esgoto2010) <- c("localidade", "total_domicilios", "possui_banheiro_total", "esgotamento_geral_pluvial", 
                          "esgotamento_fossa_septica", "esgotamento_outro", "nao_possui_banheiro", "codigo")

# transformar em numeric
esgoto2010 <- data.frame(esgoto2010[,1], lapply(esgoto2010[,2:8], as.numeric))

# calcular prop. de domicilios com esgotamento sanitario
esgoto2010 <- mutate(esgoto2010, taxa_esgotamento = round((possui_banheiro_total / total_domicilios), 3)*100 )

# mapa esgotamento absoluto
mapa.funcao(shp_recife1, esgoto2010, esgoto2010$possui_banheiro_total, "Domicilios com Esgotamento Sanitário")
ggsave("esgotamento_absoluto.png", path = "Saúde e Meio Ambiente/resultados", acid2, width = 15, height = 9, units = "in")

# mapa prop. esgotamento
mapa.funcao(shp_recife1, esgoto2010, esgoto2010$taxa_esgotamento, "Taxa de Domicilios com Esgotamento Sanitário")
ggsave("esgotamento_taxa.png", path = "Saúde e Meio Ambiente/resultados", acid2, width = 15, height = 9, units = "in")







