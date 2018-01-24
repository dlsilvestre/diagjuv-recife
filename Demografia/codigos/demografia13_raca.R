#================================================#
# JUVENTUDE NA CIDADE DO RECIFE                  #
# Demografia 1.3 - Raça e Cor                    #
#================================================#
# Recife - Pernambuco - Brasil                   #
# Prefeitura da Cidade do Recife                 #
#------------------------------------------------#
# Secretaria Executiva de Juventude              #
#------------------------------------------------#
# Claudio A. Monteiro                            #
# claudiomonteiro@gmail.com                      #
#------------------------------------------------#
# Qualquer duvida contate o desenvolvedor        #
# #UseSoftwareLivre                              #
#------------------------------------------------#

# instalar pacotes necessarios
# install.packages(c("readxl", "dplyr", "reshape"))

# carregar pacotes
library(readxl); library(dplyr); library(reshape)

#----carregar banco de dados----#
pop_idaderaca_2010 <- read_excel("Dados Originais/Demografia/pop_idaderaca_2010.xlsx")


#============================#
# Manipulação dos dados      #
#============================#

# remover linhas desnecessarias
pop_idaderaca_2010 <- pop_idaderaca_2010[-c(1:6,686),]

# renomear colunas
colnames(pop_idaderaca_2010) <- c("localidade", "raca_cor", "pop_total", "pop_15a19", "pop_20a24", "pop_25a29")

# substituir NA por 0
pop_idaderaca_2010 <- data.frame(pop_idaderaca_2010[1:2], sapply(pop_idaderaca_2010[3:6], function(x) gsub("-", "0", x)), stringsAsFactors = F)

# transformar em numeric
pop_idaderaca_2010 <- data.frame(pop_idaderaca_2010[1:2], sapply(pop_idaderaca_2010[3:6], function(x) as.numeric(x)), stringsAsFactors = F)

# criar variavel 'pop_jovem'
pop_idaderaca_2010 <- mutate(pop_idaderaca_2010, pop_jovem = pop_15a19 + pop_20a24 + pop_25a29)


#----Substituir NAs por Nomes----#

# transformar 'raca_cor' em factor
pop_idaderaca_2010$raca_cor <- as.factor(pop_idaderaca_2010$raca_cor)

# dividir dados com base na raça
pop_idaderaca_amarela <- pop_idaderaca_2010[pop_idaderaca_2010$raca_cor == "Amarela",]
pop_idaderaca_branca <- pop_idaderaca_2010[pop_idaderaca_2010$raca_cor == "Branca",]
pop_idaderaca_indigena <- pop_idaderaca_2010[pop_idaderaca_2010$raca_cor == "Indígena",]
pop_idaderaca_parda <- pop_idaderaca_2010[pop_idaderaca_2010$raca_cor == "Parda",]
pop_idaderaca_preta <- pop_idaderaca_2010[pop_idaderaca_2010$raca_cor == "Preta",]
pop_idaderaca_semdecla <- pop_idaderaca_2010[pop_idaderaca_2010$raca_cor == "Sem declaração",]
pop_idaderaca_total <- pop_idaderaca_2010[pop_idaderaca_2010$raca_cor == "Total",]

# nomear 
pop_idaderaca_amarela$localidade <- pop_idaderaca_branca$localidade <- pop_idaderaca_indigena$localidade <- 
pop_idaderaca_parda$localidade <- pop_idaderaca_preta$localidade <- pop_idaderaca_semdecla$localidade <- 
pop_idaderaca_total$localidade 

# combinar
pop_idaderaca_2010 <- rbind(pop_idaderaca_amarela, 
                            pop_idaderaca_branca,
                            pop_idaderaca_indigena,  
                            pop_idaderaca_parda,
                            pop_idaderaca_preta, 
                            pop_idaderaca_semdecla, 
                            pop_idaderaca_total)
# remover 
pop_idaderaca_2010 <- pop_idaderaca_2010[,-c(4:6)]

#----transformar banco e criar variaveis----#

# transformar de long para wide
pop_idaderaca_2010 <- reshape(pop_idaderaca_2010, idvar = "localidade", 
              timevar = "raca_cor", direction = "wide")

# proporcao de jovens pretos do total de jovens
pop_idaderaca_2010 <- mutate(pop_idaderaca_2010, 
                             prop_preto_jovem = (pop_jovem.Preta / pop_jovem.Total)*100)

# proporcao de jovens pardos do total de jovens
pop_idaderaca_2010 <- mutate(pop_idaderaca_2010, 
                             prop_pardo_jovem = (pop_jovem.Parda / pop_jovem.Total)*100)

# proporcao de jovens indigenas do total de jovens
pop_idaderaca_2010 <- mutate(pop_idaderaca_2010, 
                             prop_indigena_jovem = (pop_jovem.Indígena / pop_jovem.Total)*100)

#===============================
# VISUALIZACAO GRAFICA 
#===============================

#--- arretado theme ---#
theme_arretado<- function (base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(colour= "black",size=13,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=13,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=13,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=13,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
          panel.grid.major = element_line(colour = grey(0.85)), 
          panel.grid.minor = element_line(colour = grey(1)),
          legend.key.size = unit(9, "mm"),
          legend.text = element_text(size = 9, hjust = 3, vjust = 3),
          legend.title = element_text(size = 9),
          axis.line = element_line(size = 1, colour = "grey70"))
}

#----------------------------------------#
#  MAPA RACA/COR DOS JOVENS DO RECIFE
#----------------------------------------#

# remover outros niveis


#=================
# Raca/cor preta
shape_recife <- shapefile("Dados Originais/Geodata/Bairros.shp")

#
#
shape <- shape_recife
data <- pop_idaderaca_2010

#==== MAP FUNCTION ====#

# mapa.funcao <- function(shape, data) { 
  library(ggrepel)
  
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
  
  # merge data with shapefile
  shp_data <- merge(shape, data, by = "EBAIRRNOME")
  
  # definir labels no mapa
  shp_data <- shp_data[order(shp_data$pop_jovem.Indígena),]
  shp_data$bairros_detasq <- 1
  shp_data$bairros_detasq[1:3] <- ""
  shp_data$bairros_detasq[c(length(shp_data)-2):c(length(shp_data))] <- ""
  
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
  variavel <- shp_data@data$pop_jovem.Indígena
  nomes_centroides <- shp_data$bairros_detasq
  
  map_dataframe <- data.frame(localidade, variavel, centroids.df, nomes_centroides)
  
  plot <- ggplot(data = map_dataframe, aes(map_id = localidade)) + 
    geom_map(aes(fill = variavel), colour= grey(0.8), map = data_fortity) +
    expand_limits(x = data_fortity$long, y = data_fortity$lat) + 
    scale_fill_gradient(name = "Porcentagem", low="lightgreen", high= "darkblue")+
    geom_label_repel(aes(label = nomes_centroides, x = Longitude, y = Latitude),
                     size = 5, color = "black") + #add labels at centroids
  coord_fixed(1) +
    labs(title = "População Absoluta de Jovens Indígenas")+
    theme_nothing(legend = T)+
    theme(legend.key.size = unit(1.1, "cm"),
          legend.text = element_text(size = 14, hjust = 3, vjust = 3),
          legend.title = element_text(size = 14),
          title = element_text(colour="black",size=18,angle=0,face="bold"))
  
  print(plot)
  
  ggsave("jovens_indigenas_POP.png",  width = 8.5, height = 11, units = "in")
#  return(plot)
# }

mapa.funcao(shape_recife, demojovem_2000)










