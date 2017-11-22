#====================================================#
# DIAGNÓSTICO DA JUVENTUDE NA CIDADE DO RECIFE       #              
#====================================================#
# MOBILIDADE URBANA - Pesquisa Origem-Destino        #
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
library(readr); library(plyr); library(plyr); library(rgdal); library(ggplot2); 
library(ggmap); library(maps); library(mapdata); library(raster)

#==== pesquisa origem destino ====#
setwd("~/Documents/Claudio/PCR/")
shp_recife2 <- shapefile(file.choose())

origem_dest <- read_delim("~/Documents/Claudio/untitled folder/diagjuv-recife/Dados Originais/Mobilidade e Planejamento Urbano/pesquisaodrecife2016.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

#---- visual recife
gg_recife <- get_map(location = c(lon = -34.946964, lat = -8.027562 ),
                     zoom = 11, maptype = 'roadmap')
ggmap(gg_recife)+
  geom_polygon(data = shp_recife2, aes(x = long, y = lat, group = group), fill=NA , color = grey(0.2))+ 
  coord_fixed() +
  theme_minimal()

#----- jovens para escola
jovem_dest <- origem_dest[origem_dest$faixa_etaria == 3,]
#jov_dest_cdu <- jovem_dest[jovem_dest$]

#----- de onde vem os recifense estudantes da UFPE
ufpe_dest <- origem_dest[origem_dest$nome_instituicao_ensino == 3,]

levels(as.factor(origem_dest$nome_instituicao_ensino))

ufpe <- c("UNIVERSIDADE FEDERAL ADE PERNAMBUCO", "UNIVERSIDADE FEDERAL D PERNAMBUCO" , 
          "UNIVERSIDADE FEDERAL DE EPERNAMBUCO",                                                                              
          "UNIVERSIDADE FEDERAL DE FERNAMBUCO",                                                                               
          "UNIVERSIDADE FEDERAL DE PARNAMBUCO",                                                                               
          "UNIVERSIDADE FEDERAL DE PE",
          "UNIVERSIDADE FEDERAL DE PERAMBUCO",                                                                                
          "UNIVERSIDADE FEDERAL DE PERANAMBUCO",                                                                              
          "UNIVERSIDADE FEDERAL DE PERNABUCO",                                                                                
          "UNIVERSIDADE FEDERAL DE PERNAMBCO",                                                                                
          "UNIVERSIDADE FEDERAL DE PERNAMBUC",                                                                                
          "UNIVERSIDADE FEDERAL DE PERNAMBUCO", 
          "UNIVERSIDADE FEDERAL DE PERNAMBUCO -UFPE",                                                                         
          "UNIVERSIDADE FEDERAL DE PERNAMBUCO (UFPE)",
          "UNIVERSIDADE FEDERAL DE PERNAMBUCO - UFPE",
          "UNIVERSIDADE FEDERAL DE PERNAMBUCO08",                                                                             
          "UNIVERSIDADE FEDERAL DE PERNAMUBUCO",                                                                              
          "UNIVERSIDADE FEDERAL DE PERNANBUCO",                                                                               
          "UNIVERSIDADE FEDERAL DE PERNMABUCO",                                                                               
          "UNIVERSIDADE FEDERAL DE PRNAMBUCO",
          "UNIVERSIDADE FEDEREAL DE PERNAMBUCO",                                                                              
          "UNIVERSIDADE FEDRAL RURAL DE PERNAMBUCO",                                                                          
          "UNIVERSIDADE FEDRRAL DE PERNAMBUCO",                                                                               
          "UNIVERSIDADE FERDERAL DE PERNAMBUCO",                                                                              
          "UNIVERSIDADE FEREDAL DE PERNAMBUCO",                                                                               
          "UNIVERSIDADE FERERAL DE PERNAMBUCO",
          "UFPE",                                                                                                             
          "UFPE _ UNIVERSIDADE FEDERAL DE PERNAMBUCO",
          "UFPE - CAMPUS RECIFE",
          "UFPE - CENTRO DE INFORMATICA",
          "UFPE - UNIVERSIDADE FEDERAL DE PARMAMBUCO",                                                                        
          "UFPE - UNIVERSIDADE FEDERAL DE PERNAMBUCO",                                                                        
          "UFPE -CCJ",
          "UFPE / UNINASSAU",
          "UFPE E FG",                                                                                                        
          "UFPE E IFPE",                                                                                                      
          "UFPE E UNICAP",                                                                                                    
          "UFPE FDR",                                                                                                         
          "UFPE RECIFE",
          "UFPE UNIVERSIDADE FEDERAL DE PERNAMBUCO",
          "UFPE-CIN",
          "UFPE-MDU",                                                                                                         
          "UFPE-MESTRADO",                                                                                                    
          "UFPE/CAA",                                                                                                         
          "UFPE/CAV",                                                                                                         
          "UFPE/LIKA"
)

ufpe_dest<- origem_dest[origem_dest$nome_instituicao_ensino %in% ufpe, ] 
levels(as.factor(ufpe_dest$cidade_residencia))

#--------------------------------#
#==== cidades origem da ufpe ====#

ufpe_cidade <- data.frame(table(ufpe_dest$cidade_residencia))
ufpe_cidade <- ufpe_cidade[order(ufpe_cidade$Freq),]

# em proporcao
sum(ufpe_cidade$Freq)
ufpe_cidade <- mutate(ufpe_cidade, prop_cidade = Freq / sum(ufpe_cidade$Freq))
ufpe_cidade$prop_cidade <- round(ufpe_cidade$prop_cidade, 4)
ufpe_cidade$Var1 <- factor(ufpe_cidade$Var1, levels = ufpe_cidade$Var1)
ufpe_cidadeplot <- ufpe_cidade[32:46,]

# grafico barra
bar_ufpe_cid <- ggplot(ufpe_cidadeplot, aes(x = Var1, y = prop_cidade))+
  geom_bar(stat = "identity", fill = "#1c3c40") +
  geom_label(label = ufpe_cidadeplot$prop_cidade, 
             size = 4, color = "black", fontface = "bold") +
  labs(y = "Proporção do Total da Amostra", x = "", title = "Cidades Origem da UFPE") +
  coord_flip() +
  theme_massa()
bar_ufpe_cid

# salvar grafico
ggsave("bar_ufpe_cid.png", bar_ufpe_cid, width = 12, height = 6, units = "in")

#-------------------------------------------#
#==== cidades nao-recife origem da ufpe ====#

ufpe_cidfora <- data.frame(table(ufpe_dest$cidade_residencia))
ufpe_cidfora <- ufpe_cidfora[order(ufpe_cidfora$Freq),]
ufpe_cidfora <- ufpe_cidfora[ !ufpe_cidfora$Var1 == "RECIFE",]

# em proporcao
sum(ufpe_cidfora$Freq)
ufpe_cidfora <- mutate(ufpe_cidfora, prop_cidade = Freq / sum(ufpe_cidfora$Freq))
ufpe_cidfora$prop_cidade <- round(ufpe_cidfora$prop_cidade, 4)

ufpe_cidfora$Var1 <- factor(ufpe_cidfora$Var1, levels = ufpe_cidfora$Var1)
ufpe_cidfora <- ufpe_cidfora[21:45,]

# grafico barra
bar_ufpe_fora <- ggplot(ufpe_cidfora, aes(x = Var1, y = prop_cidade))+
  geom_bar(stat = "identity", fill = "#1c3c40") +
  geom_label(label = ufpe_cidfora$prop_cidade, 
             size = 4, color = "black", fontface = "bold") +
  labs(y = "Proporção da Amostra Sem Recife", x = "", title = "Cidades Origem da UFPE Fora de Recife") +
  coord_flip() +
  theme_massa()
bar_ufpe_fora

# salvar grafico
ggsave("bar_ufpe_fora.png", bar_ufpe_fora, width = 12, height = 6, units = "in")

#================================#
#==== bairros origem da UFPE ====#
library(readxl)
latlong_bairros_recife <- read_excel("~/Documents/latlong_bairros_recife.xls")
shp_recife2 <- shapefile(file.choose())

ufpe_bairros <- ufpe_dest[ufpe_dest$cidade_residencia == "RECIFE",]
ufpe_bairros <- data.frame(table(ufpe_bairros$bairro_residencia))
ufpe_bairros <- ufpe_bairros[order(ufpe_bairros$Freq),]

# em proporcao
sum(ufpe_bairros$Freq)
ufpe_bairros <- mutate(ufpe_bairros, prop_bairro = Freq / sum(ufpe_bairros$Freq))

library(stringi)
library(magrittr)
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

latlong_bairros_recife$bairro <- toupper(latlong_bairros_recife$bairro)
latlong_bairros_recife$bairro = stri_trans_general(latlong_bairros_recife$bairro , "Latin-ASCII")
ufpe_bairros$bairro = best_match(ufpe_bairros$bairro_n, latlong_bairros_recife$bairro)

# merge data with lat long recife
ufpe_latlon <- merge(ufpe_bairros, latlong_bairros_recife, by = "bairro")
ufpe_latlon <- ufpe_latlon[-87,]

# merge data with shapefile@data
shp_recife3 <- merge(shp_recife2, ufpe_latlon, by = "bairro_n", all = T)

mapa.funcao(shp_recife3, ufpe_bairros2, ufpe_bairros2$Freq)


















